# Reverse HTTP

## Abstract

This document describes a dynamic, ReST-style means of enrolment and
participation in an HTTP network. The `message/http` and
`application/http` MIME types defined by [RFC 2616][] are used to
build a dynamically-configurable "Remote CGI" service.

Joining the World Wide Web as an HTTP server has been an ad-hoc,
manual process. By using the protocol defined here, programs can
provide services to the Web just as easily as they request services
from the Web.

----

@TOC@

  [Streaming]: #streaming

## Introduction

To date, joining the World Wide Web as an HTTP server has been a
complicated process, managed in an ad-hoc way. Almost every piece of
server configuration is manual: DNS records, firewall and NAT
configuration, load balancing, access control configuration,
operating-system configuration, installation and management of server
software and extensions, CGI scripts and their interpreters, and so
on. Even simple HTTP services are static and difficult to change.

By contrast, joining the World Wide Web as an HTTP client is extremely
easy. No DNS configuration is required, as clients do not have names
within the network; no complicated server software is required; no
firewall or NAT configuration needs to be changed. HTTP clients come
and go dynamically.

The World Wide Web is evolving from being a protocol for naming and
copying statically-configured resources into being a communications
platform for participants in a [general-purpose distributed object
system][Web hooks]. As this has been happening, ad-hoc techniques such
as long-polling/[Comet][] have developed that serve to make HTTP less
asymmetric and more dynamically configurable.

This document describes a ReST-style protocol that permits programs
with access to an HTTP client library to

 - dynamically claim and relinquish pieces of URL-space in which to
   provide HTTP-accessible services, and

 - respond to HTTP requests within their claimed portion of URL-space.

The protocol can be seen as "Remote CGI": a means of extending an HTTP
service that, in contrast to ordinary CGI, does not require special
access to the file system and configuration of the server.

By splitting out the concerns of allocation of URL-space and of
responding to incoming requests from the concerns of DNS, firewall and
NAT management, HTTP-based services can come and go without needing
the extensive manual configuration that is currently
required. Programs can provide services to the Web just as easily as
they request services from the Web. The protocol defined here gives
control over enrolment to the very programs wishing to enrol.

Long-polling techniques are used by many Web services to provide
timely event notification while avoiding the need for event recipients
to expose addressable HTTP endpoints. This specification lowers the
barriers to universal use of HTTP push and [web hooks][], and makes it
possible to push the use of long-polling out to the very edges of the
network.

### Requirements

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in [RFC 2119][].

### Existing server extension mechanisms

There are several widely-deployed methods for extending web servers,
all of which require some changes to the server's static configuration
to be made in order to add or remove a service. Some are complex
protocols in their own right.

 - Apache's [mod_proxy][] in reverse-proxy mode lets many HTTP servers
   be composed to appear as a single server.

 - [CGI][] (and variants such as [FastCGI](http://en.wikipedia.org/wiki/FastCGI) and [SCGI](http://en.wikipedia.org/wiki/Simple_Common_Gateway_Interface)) lets many small programs be individually named and made accessible via HTTP.

 - More generally, web server extension modules are often used to
   extend web servers with new functionality.

## Definitions and Service Overview

The participants in the protocol defined in this document are
Applications and the Gateway Service. Interactions between the Gateway
Server and HTTP clients requesting services provided by Applications
are normal HTTP requests and so are not specified in this document.

The following message sequence diagram illustrates the processing of a
single request:

    Original Requestor    Gateway Service        Application "foo"
            |                      |                      |
            |                      |<---- GET ------------|
            |                      |                      |
            |----- GET /foo/a ---->|                      |
            |                      |----- 200 ----------->|
            |                      |      "GET /a"        |
            |                      |                      |
            |                      |<---- POST -----------|
            |                      |      "404"           |
            |                      |                      |
            |<---- 404 ------------|----- 202 ----------->|
            |                      |                      |

An *Application* is a program that wishes to provide HTTP service to
third parties, and that registers with a Gateway Service in order to
do so.

A *Gateway Server* is an ordinary HTTP server that provides Gateway
Service to Applications. A *Gateway Service*

 - responds to Application requests for claiming or relinquishing
   pieces of the URL-space under the control of the Gateway Server,
   and

 - relays incoming requests for claimed pieces of URL-space on to the
   corresponding Applications, and relays Application responses to
   such requests on to the original requestors.

The *Gateway Service URL* is used by Applications to claim and
configure pieces of URL-space under the control of the Gateway Server.

The *Application Name* is the identifier that Applications use when
claiming URL-space from a Gateway Service. Application Names are
embedded into the Public Application URL given to the Application on
registration.  Application Names must be valid DNS labels, as
specified by [RFC 1034][].

When an Application claims a portion of URL-space from a Gateway
Service, the service sends back three URLs:

 - One, the *Public Application URL*, is a URL that lies within the
   portion of URL-space under control of the Gateway Server that the
   Application can give out to third parties.

 - One, the *Private Application URL*, is the URL that the Application
   uses to control its registration with the Gateway Service.

 - The final URL is the *Request URL* that the Application should use
   to receive incoming requests. When HTTP requests arrive for the
   Public Application URL, they are placed in a queue, and sent on to
   the Application for processing when the Application accesses a
   Request URL.

Private Application URLs are for controlling active registrations with
the Gateway Service, and SHOULD be unguessable (for instance, by
incorporating a UUID or other nonce value) and SHOULD NOT be visible
in any kind of public index. Private Application URLs are effectively
capabilities granting full access to an application's registration
with the Gateway Service.

Public Application URLs are for wide distribution and use, and so MAY
be guessable or placed in a public index.

HTTP requests relayed to Applications are named by unique Request
URLs. When an Application has finished processing a request, it
`POST`s an appropriate HTTP response to the request's Request URL. The
response is then relayed back to the original requestor.

Since Request URLs give access to the incoming request stream for an
Application, they SHOULD be unguessable (for instance, by
incorporating a UUID or other nonce value) and SHOULD NOT be visible
in any kind of public index.

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

In the context of this specification,

 - Envelope HTTP *Responses* carry the Embedded HTTP *request*
   pipelines or messages from the Gateway Service to Applications, and

 - Envelope HTTP *Requests* carry the corresponding Embedded HTTP
   *response* pipelines or messages from Applications to the Gateway
   Service.

## Registration: Claiming a piece of URL-space

To claim a piece of URL-space, `POST` to the Gateway Service URL with
a normal `application/x-www-form-urlencoded` entity body. The
parameters sent should be

 - `name`: a valid DNS label, as specified by [RFC 1034][]. This is
   the Application Name to register.

 - `token`: any string of characters. Optional. If present, used as a
   shared-secret to control access to the registration process: if, on
   two subsequent registrations, the tokens do not match, then the
   later registration will fail with response code 403.

 - `lease`: any string of digits. Optional. If present, and a
   registered service is left dormant (i.e. without clients collecting
   incoming requests) for the specified number of seconds, the
   registration will be deleted.

If the `token` parameter is omitted, the service SHOULD act as if a
fresh, random token was supplied, thereby ensuring a
first-in-first-served policy on application name registrations.

If the `lease` parameter is omitted, the service should choose a value
for the lease-expiry-time long enough to give the newly-registered
service time to start polling for incoming requests. If the `lease`
parameter is present, the service should try to honour it as far as
possible, however overly-short or overly-long leases MAY be altered by
the service without notice to the application.

Possible responses to a registration request include:

 - 201, if a new registration was made.

 - 204, if an existing registration was refreshed.

 - 400, if
    - the `lease` parameter is present but not a parseable integer number of seconds, or
    - the `name` is missing or is not a legal DNS label

 - 403, if the token does not match a token held in an existing
   registration record for this application name.

If a 2xx-series response is sent, the response MUST have the following headers set:

 - `Link`, with `rel="first"` and the URL being a Request URL.

 - `Link`, with `rel="related"` and the URL being the Public
   Application URL.

 - `Location` being the Private Application URL.

Applications should issue a `GET` against the Request URL (given in
the `rel="first"` `Link` header) to begin polling for incoming
requests.

Applications may hand out the Public Application URL (in the
`rel="related"` `Link` header) to third parties.

The URL given in the `Location` header can be used to query and update
the configuration of the registration.

### Virtual-hosts vs. Paths

The Public Application URL is chosen by the server on an
application-by-application basis, and can be of any form, though it
should include the application name. Two major possibilities exist:

 - virtual-host-based: `http://applicationname.example.com/` (perhaps
   using wildcard `CNAME`s in DNS to support arbitrary application
   labels)

 - path-based: `http://example.com/x/y/applicationname`

Applications MUST NOT assume a particular scheme for construction of
the Public Application URL, and MUST instead use the `rel="related"`
`Link` header in the registration response as their public address.

If virtual-host-based URL construction is used, care must be taken to
treat the Application Name case-insensitively, as DNS labels are
case-insensitive.

## Waiting for a request to arrive

To poll for incoming requests, issue an HTTP `GET` to a Request URL
sent from the Gateway Service in a `Link` header (either `rel="first"`
or `rel="next"`). The Gateway Service should hold the request open
until a request arrives for the registered application or an internal
timeout occurs.

The `Accept` header on the `GET` request, if present, lets the Gateway
Service know whether use of `application/http` is permissible (see the
section on [Streaming][] below). Unless the Gateway Service
is sure that the requesting Application can handle `application/http`,
it MUST use `message/http` (i.e. it must send requests one-at-a-time
without streaming or pipelining).

Possible responses include:

 - 200, if a request arrives before the poll times out. The entity
   body is the incoming request(s).

 - 204, if the poll times out.

 - 404, if no such Request URL exists.

 - 410, if the registration is deleted during the poll.

If a 2xx-series response carrying an entity body is sent, the response
will be an Envelope HTTP Response, carrying an Embedded HTTP Pipeline
or Embedded HTTP Message in its entity body. The `Content-type` header
of the Envelope Response will be set to `application/http` or
`message/http`. Embedded Pipelines (`application/http`) MUST NOT be
used unless the Gateway Service supports them, the Application has
explicitly requested them via use of the `Accept` header, and the
Gateway Service has decided to use them.

Embedded Pipelines will carry the HTTP `Method`, `Request-URI` and
`HTTP-version` values that were sent by the original requestor. The
Gateway Service MUST NOT alter the embedded HTTP headers or the
`Request-URI` when it relays requests on to applications.

All 2xx-series responses MUST have a `Link` header with `rel="next"`
pointing to the next Request URL the application should use. If an
application sends a `GET` more than once for the same Request URL,
undefined behaviour results; Applications MUST NOT assume that a
Request URL can be used more than once, and MUST instead follow the
linked-list formed by successive `Link` `rel="next"` headers in
responses from the Gateway Service. The only known-safe reuse of a
Request URL is when the Gateway Service decides to send a
previously-used Request URL in a `Link` header.

Responses with code 200 MUST also have a `Requesting-Client` header
containing the IP address and port number of the original requestor
for the Embedded Pipeline or Message being delivered. The header
should be formatted as `A.B.C.D:PORT`; for example, if the requestor
had IP address 10.1.2.3 and remote TCP port number 45678, the
`Requesting-Client` header would contain the string `10.1.2.3:45678`.

### Work distribution

If an application is waiting on more than a single Request URL for
incoming requests, the Gateway Service SHOULD send requests out to
each polling client in as close to a round-robin fashion as possible,
except where doing so could result in requests being delivered to the
Application out of order. (See the section on "[Streaming][]" below
for details on ordering constraints.)

## Replying to a received request

Once an Application has processed a received HTTP request, and
constructed an appropriate HTTP response, it uses `POST` to the
Request URL from which it received the request to cause the reply to
be relayed on to the original requestor.

The `Content-type` of the `POST` SHOULD be the same as the
`Content-type` received on the Envelope HTTP Response from the `GET`,
and SHOULD NOT be absent; that is, if the Gateway Service sent an
Embedded HTTP Request Pipeline, the `POST`ed response should be an
Embedded HTTP Response Pipeline labelled as `application/http`, and if
the Gateway Service sent an Embedded HTTP Request Message, the
`POST`ed response should be an Embedded HTTP Response Message labelled
as `message/http`. The Gateway Service MAY permit other
`Content-type`s, such as `application/x-www-form-urlencoded`, since
some HTTP client libraries do not offer control over the
`Content-type` header.

The submitted entity body MUST be a valid HTTP response Pipeline or
Message, as appropriate.

Possible responses include:

 - 202, if the reply is accepted and relayed on to the original
   requestor. (NB: the requestor may have given up in the
   meantime. Status code 202 is no guarantee that the original
   requestor will receive the application's reply.)

 - 404, if no such request URL exists.

 - 400, if the HTTP response in the request body is invalid.

## Third-party requests to services registered with the gateway

Ordinary HTTP requests intended for Applications registered with the
Gateway Service (that is, sent to portions of URL-space rooted at
Public Application URLs) are simply enqueued and delivered to the
application when it next polls for requests, in a first-in, first-out
ordering. (NB: The Gateway Service MUST NOT reorder requests arriving
on a single connection, but MAY arbitrarily interleave requests
arriving on different connections.)

Normally, the application will generate its own HTTP responses to send
back, but in some cases the Gateway Service has to supply a
response. Cases where the Gateway Service may supply an HTTP response
include:

 - 400, if the IP address and port of the requestor could not be
   determined.

 - 404, if no registration covering the requested URL exists.

 - 503, if there was an error relaying the request.

 - 504, if no pollers were available within a certain timeout (see
   below).

 - 504, if there was a timeout waiting for a reply from the
   application.

 - 502, if the reply from the poller was invalid.

If a request for an Application is sent to the Gateway Service during
a period when no polling clients are waiting for incoming requests,
the Gateway Service SHOULD time out the request with an appropriate
response code, and SHOULD indicate to the requestor that no
application servers were available. Some care should be taken to
distinguish between unavailability of the registered Application and a
busy Application that is working through a backlog of requests. Only
in the former situation should the request be timed out for this reason.

A second kind of timeout MAY also be implemented by the Gateway
Service: if a request is delivered to the Application, but the
Application does not respond, the Gateway Service MAY time out the
request. If it does, it SHOULD indicate to the requestor that the
request was delivered to the Application, but that a reply was not
received quickly enough. Notice that this kind of timeout should also
apply to requests in a busy Application's request queue: if the
Application is working, but not working fast enough, this kind of
timeout MAY be signalled.

Note that the timeout for unavailability of the Application can be
short -- even as short as 1 second -- but that the timeout for a
missing reply from the Application should be much longer, certainly no
shorter than 60 seconds, if a timeout for a missing reply is
implemented at all.

## Managing existing registrations

### Querying Gateway Service status

Issuing a `GET` to the Gateway Service URL should, after appropriate
authentication and authorisation checks, retrieve a representation of
the state of the entire Gateway Service, either in HTML or in a
machine-readable format. The `Accept` header may be used to control
the format of the returned information.

### Querying registration status

Issuing a `GET` to the Private Application URL SHOULD retrieve a
representation of the state of the registration. The `Accept` header
MAY be used to control the format of the returned information.

Gateway Services SHOULD support returning information about registrations in

 - `application/x-www-form-urlencoded` format. The parameters included
   should include at least `name` and `lease`, as described for the
   registration process above.

 - HTML format, for human use.

### Reconfiguring a registration

Issuing a `PUT` to the Private Application URL with a body of
`Content-type` `application/x-www-form-urlencoded` SHOULD update the
configuration of a registration just as if the registration were
deleted and recreated. In particular, `lease` and `token` should be
updated, if supplied.

If the `name` parameter is supplied, it MUST be ignored.

### Deleting a registration

To cancel a registration, applications should issue an HTTP `DELETE`
to the Private Application URL.

Possible responses to such a `DELETE` include:

 - 204, if the registration was successfully deleted

 - 404, if no such registration existed

If, at the time of the `DELETE`, one or more `GET`s to Request URLs
were active for the registration to be deleted, the `GET`s SHOULD be
terminated as soon as possible with HTTP status code 410 sent back to
the `GET`ting application.

Any requests that were being processed by an application at the time
of the `DELETE` SHOULD NOT be affected by the deletion, and the
Gateway Service SHOULD relay replies received from the application to
the original requestors as usual.

## Streaming

HERE

Can begin talking to a poller immediately and use chunked transfer
coding. May timeout the poller and end the request -- this results in
a 200 response with an empty body in a corner case!

Queue ordering constraints for streams: either one connection, one
stream, or wait for a reply before issuing the next request. Must not
round-robin where doing so could reorder requests.

## Transient service availability

Not all services are available all the time. At times, a registered
Application will not be in a position to receive inbound requests from
the Gateway Service. This corresponds to the equivalent of XMPP's
"unavailable" presence indication. Further work will address the issue
of presence state-change notification over HTTP.

In general, service unavailability is a problem shared by all HTTP
services, not just those exposed via a Gateway Service. The difference
here is that a 5xx-series response code is returned by the Gateway
Service to the requestor in cases of unavailability, rather than the
more usual case of simply no listening socket being available to take
requests.

## Security Considerations

Securing the registration of pieces of URL-space is important. Gateway
Servers can use any means of HTTP or HTTPS authentication and
authorisation to control access to the Gateway Service URL and to
Private Application URLs and Request URLs. In particular, on any of
the interactions described above between the Gateway Server and the
Application, all the usual HTTP response codes apply, including 401
and 403 for authentication and authorisation control.

Besides traditional HTTP/HTTPS access control, unguessable URLs
(including UUIDs or similar) can be used to provide a capability-style
model of access control.

Interactions between third parties and Applications are to be relayed
through the Gateway Service: it is not the Service's role to control
access to Application resources. Instead, Applications themselves
should check the credentials offered in each third-party HTTP request,
and should return 401 and 403 responses as appropriate to the third
parties.

## Normative References

 - [RFC 1034][], Domain Names - Concepts And Facilities, P. Mockapetris

 - [RFC 1945][], Hypertext Transfer Protocol -- HTTP/1.0,
   T. Berners-Lee et al.

 - [RFC 2119][] (BCP 14), Key words for use in RFCs to Indicate
   Requirement Levels, S. Bradner

 - [RFC 2616][], Hypertext Transfer Protocol -- HTTP/1.1, R. Fielding
   et al.

## Informative References

 - [RFC 3857][CGI], The Common Gateway Interface (CGI) Version 1.1,
   D. Robinson and K. Coar

 - [draft-nottingham-http-link-header-04][], Link Relations and HTTP
   Header Linking, M. Nottingham; Internet-Draft, expires August 29,
   2009

 - [draft-lentczner-rhttp-00][], Reverse HTTP, M. Lentczner and
   D. Preston; Internet-Draft, expires September 5, 2009

  [Comet]: http://en.wikipedia.org/wiki/Comet_(programming)
  [mod_proxy]: http://httpd.apache.org/docs/2.0/mod/mod_proxy.html
  [Web hooks]: http://webhooks.org/about

  [RFC 1034]: http://www.ietf.org/rfc/rfc1034.txt
  [RFC 1945]: http://www.w3.org/Protocols/rfc1945/rfc1945
  [RFC 2119]: http://www.ietf.org/rfc/rfc2119.txt
  [RFC 2616]: http://www.w3.org/Protocols/rfc2616/rfc2616.html
  [CGI]: http://www.ietf.org/rfc/rfc3875.txt
  [draft-nottingham-http-link-header-04]: http://tools.ietf.org/id/draft-nottingham-http-link-header-04.txt
  [draft-lentczner-rhttp-00]: http://www.ietf.org/internet-drafts/draft-lentczner-rhttp-00.txt

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
