# Relay HTTP

## Abstract

This document describes a protocol for tunnelling HTTP traffic over
HTTP, with the goal of providing portable, general, securable access
to the World Wide Web for programs running in restricted environments,
including Javascript programs running in browsers.

The defined protocol is similar to the widely used HTTP proxying
protocol, but differs in that the proxied traffic is carried over an
ordinary HTTP connection; the special syntax used by an HTTP proxy is
avoided here.

----

@TOC@

## Introduction

Javascript programs running in a browser have severely restricted
access to the network, to avoid [cross-site scripting][XSS]
attacks. This makes developing browser-based applications that
interact with multiple services on the web difficult, leading to
problematic workarounds such as [dynamic script
tags](http://www.xml.com/pub/a/2005/12/21/json-dynamic-script-tag.html)
for loading JSON data from third-party services. Many applications
have therefore introduced methods of relaying HTTP requests to other
HTTP servers on an ad-hoc, case-by-case basis.

This specification defines instead a general mechanism for tunnelling
outbound HTTP requests over an HTTP transport, which, if implemented
by the site hosting a browser-based application, gives the Javascript
component of the application full (but controllable) access to the
entirety of the web in a standard way.

### Requirements

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in [RFC 2119][].

## Definitions

### Relay Endpoints

The HTTP server that hosts a web application should expose one or more
Relay Endpoints. These are URLs, under the control of and served by
the HTTP server, that implement the protocol described below for
relaying HTTP requests to other HTTP servers.

The Javascript part of the hosted web application should be configured
with the URLs of one or more Relay Endpoints that it can use.

### Pipelines, Envelopes, and Payloads

RFC 2616
[defines](http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html#sec19.1)
the `application/http` [MIME
type](http://www.iana.org/assignments/media-types/) for representing
pipelines of HTTP requests or responses, and the `message/http` MIME
type for single HTTP requests or responses. In order to carry HTTP
over HTTP, we define a simple enveloping protocol for these pipelines
and messages.

 - An *Embedded Pipeline* is a pipeline being tunnelled. It is of
   type `application/http`, and is either a pipeline of HTTP requests,
   or a pipeline of HTTP responses.

 - An *Embedded Message* is a single HTTP request or HTTP response
   being tunnelled. It is of type `message/http`.

 - An *Envelope HTTP Request* is an HTTP request that carries an
   Embedded Pipeline or Message in its body.

 - An *Envelope HTTP Response* is an HTTP response that carries an
   Embedded Pipeline or Message in its body.

In cases where there is no resulting ambiguity, the term Pipeline is
used to mean either a Pipeline or a Message in the text below.

## Running Example

In the following sections, we will be tracking the progress of a web
application that

 - has been given the URL `http://10.11.12.13/myrelay/` as its Relay
   Endpoint, and

 - wishes to contact an HTTP service at `http://www.example.com/service`.

## Making a request

Instead of the application making a direct HTTP request to the service
desired, it constructs an HTTP request Pipeline, embeds it in an
Envelope Request, and sends the Envelope Request to the Relay
Endpoint.

The application MUST use the HTTP `POST` method when sending the
Envelope Request to the Relay Endpoint, even when the method(s) in the
Embedded Pipeline are not `POST`.

The application MUST append the host and port number that the Relay
Endpoint should contact to the Relay Endpoint URL it has been
given. The string appended to the Relay Endpoint URL MUST be of the
form "`hostname:portnumber`". The "`:portnumber`" part MAY be omitted,
in which case the actual port number used by the Relay Endpoint when
making the outbound HTTP connection SHOULD be the standard
IANA-registered `http` port number.

For our running example, this makes the URL to which the Envelope
Request is `POST`ed `http://10.11.12.13/myrelay/www.example.com`. If
the port had been number 8080, then the URL would have been
`http://10.11.12.13/myrelay/www.example.com:8080`.

The body of the `POST`ed Envelope Request request sent to the Relay
Endpoint MUST be a properly-formatted HTTP request Pipeline or
Message, as defined by [RFC 2616][] or [RFC 1945][].

If the Embedded Pipeline only contains a single message (making it, in
fact, an Embedded Message), then the `Content-Type` header of the
Envelope Request SHOULD be `message/http`. If it contains more than one
message (making it a true Embedded Pipeline), then the `Content-Type`
header of the Envelope Request MUST be `application/http`.

> _Rationale_: Requiring clear indication of the number of pipelined
> requests contained in an Envelope Request makes it possible to
> implement a RelayHttp service that does not parse the Embedded
> Pipelines it sends back and forth at all, simply relaying them
> verbatim to and from the remote server.

## Processing a request

When the Relay Endpoint receives an Envelope Request as described
above, it MUST extract the hostname and port numbers encoded in the
Envelope Request's URL as described above. If the hostname and port
number are not valid, the Embedded Pipeline MUST NOT be relayed, a
status code of 400 MUST be sent in the response to the Envelope
Request, and processing of the Envelope Request MUST be terminated.

A Relay Endpoint MUST support Embedded Messages, that is, Envelope
Requests with `Content-Type` of `message/http`. It MAY also support
Embedded Pipelines proper, with `Content-Type` of
`application/http`. If it does not support Pipelines,
`application/http`, then it MUST reply to any `application/http`
Envelope Request with a status code of 415, and MUST NOT relay the
Embedded Pipeline. If an Envelope Request has an unrecognised
`Content-Type` (such as `application/x-www-form-urlencoded`), the
Relay Endpoint SHOULD treat it as if it were `message/http`.

> _Rationale_: Naive RelayHttp services that do not parse Embedded
> Pipelines cannot in general tell when to stop collecting a response
> pipeline for a given request pipeline. Therefore, support for
> multiple-requests-per-Embedded-Pipeline is made optional.

The Relay Endpoint MAY examine the Embedded Pipeline, in combination
with the `relay` parameter, in order to decide whether to perform the
actual relaying or not. If it decides not to proceed with the relay
after such an examination, because of security policy configuration or
otherwise, it MAY reply to the Envelope Request (NB: not the Embedded
Request(s)) with status codes 401 or 403 as appropriate.

Otherwise, the Relay Endpoint opens an outbound HTTP connection to the
hostname and port number specified as part of the Envelope Request's
URL and sends the Pipeline that it received, unmodified. The
response(s) from the remote server are recorded verbatim by the Relay
Endpoint.

## Responding to a request

If the Relay Endpoint fails before it reaches the point where it has
collected a response from the remote server, it SHOULD respond to the
original Envelope Request with a 5xx-series status code.

Otherwise, it should reply to the Envelope Request with the collected
response Pipeline, unmodified, embedded in an Envelope Response with a
status code of 200.

If the Envelope Request was of type `application/http`, a true
Pipeline, then the Envelope Response MUST be given a `Content-Type` of
`application/http`, and SHOULD carry the same number of HTTP responses
as there were HTTP requests in the Envelope Request's Pipeline, but
MAY carry fewer. In the case where fewer responses than expected are
sent back by the Relay Endpoint, the sender of the Envelope Request
should treat the missing responses as it would for a dropped TCP
connection if it were accessing the remote server directly.

If the Envelope Request was of type `message/http` (or was treated as
if this were the case), then the Envelope Response MUST be given a
`Content-Type` of `message/http`, and MUST contain exactly one HTTP
response message.

## Security Considerations

Normal HTTP access-control mechanisms can be used to restrict access
to Relay Endpoints, including [HTTP authentication][RFC 2617] and
[HTTPS][RFC 2818].

A Relay Endpoint is not required to relay every request; it may choose
to reject requests to certain blacklisted hosts, for instance, or may
only allow requests to a set of whitelisted destinations. Mechanisms
for controlling such policies are outside the scope of this document.

## Normative References

 - [RFC 2119][] (BCP 14), Key words for use in RFCs to Indicate
   Requirement Levels, S. Bradner

 - [RFC 1945][], Hypertext Transfer Protocol -- HTTP/1.0,
   T. Berners-Lee et al.

 - [RFC 2616][], Hypertext Transfer Protocol -- HTTP/1.1, R. Fielding
   et al.

  [Comet]: http://en.wikipedia.org/wiki/Comet_(programming)
  [RFC 2119]: http://www.ietf.org/rfc/rfc2119.txt
  [RFC 2616]: http://www.w3.org/Protocols/rfc2616/rfc2616.html
  [RFC 2617]: http://www.ietf.org/rfc/rfc2617.txt
  [RFC 2818]: http://www.ietf.org/rfc/rfc2818.txt
  [RFC 1945]: http://www.w3.org/Protocols/rfc1945/rfc1945
  [XSS]: http://en.wikipedia.org/wiki/Cross-site_scripting

## Author's Address

Tony Garnock-Jones  
tonygarnockjones@gmail.com  
<http://homepages.kcbbs.gen.nz/tonyg/>

## Copyright Statement

Copyright &copy; 2009, 2010 Tony Garnock-Jones <tonygarnockjones@gmail.com>  
Copyright &copy; 2009 LShift Ltd. <query@lshift.net>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this documentation (the "Documentation"), to deal in the
Documentation without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Documentation, and to permit persons to whom
the Documentation is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Documentation.

THE DOCUMENTATION IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE DOCUMENTATION OR THE USE OR OTHER DEALINGS IN THE
DOCUMENTATION.

## Appendix: Example messages

The following sections provide example `message/http` Envelope
Requests and Responses that correspond to the running example given in
the main body of the text.

### Envelope Request (message/http)

The client connects to IP address `10.11.12.13`, on port 80, and sends
the following HTTP message:

<pre>GET /myrelay/www.example.com HTTP/1.0
Host: 10.11.12.13
Content-Type: message/http
Content-Length: 48

GET /service HTTP/1.0
Host: www.example.com

</pre>

### Envelope Response (message/http)

We imagine that the service requested from `www.example.com` is not
actually available. The Relay Endpoint then responds with the
following HTTP message:

<pre>HTTP/1.0 200 Ok
Server: RelayHttp/1.0
Content-Type: message/http
Content-Length: 146

HTTP/1.0 404 Not found
Server: SomeExampleServer/2.3.4
Content-Type: text/plain
Content-Length: 41

The requested service is not available.
</pre>
