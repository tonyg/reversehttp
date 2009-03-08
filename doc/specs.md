# Draft Specs

## Specifications

So far, there are two relevant specifications being developed as part
of ReverseHttp.

### ReverseHttp

The specification is [<b>here</b>][rev].

Abstract:

> "This document describes a dynamic, ReST-style means of enrolment
> and participation in an HTTP network. The `message/http` and
> `application/http` MIME types defined by [RFC 2616][] are used to
> build a dynamically-configurable "Remote CGI" service.

> "Joining the World Wide Web as an HTTP server has been an ad-hoc,
> manual process. By using the protocol defined here, programs can
> provide services to the Web just as easily as they request services
> from the Web."

### RelayHttp

The specification is [<b>here</b>][relay].

Abstract:

> "This document describes a protocol for tunnelling HTTP traffic over
> HTTP, with the goal of providing portable, general, securable access
> to the World Wide Web for programs running in restricted
> environments, including Javascript programs running in browsers.

> "The defined protocol is similar to the widely used HTTP proxying
> protocol, but differs in that the proxied traffic is carried over an
> ordinary HTTP connection; the special syntax used by an HTTP proxy
> is avoided here."

## General considerations when tunnelling HTTP

HTTP is defined to run over reliable, in-order, connected,
bidirectional byte-stream transports, such as TCP, by [RFC 2616][]. It
depends on the underlying transport in a few different ways:

 - Reliable: no low-level retransmission or acknowledgement protocols
   are needed.

 - In-order: multiple requests issued one after the other on a
   connection are processed in order, and replied to in order, without
   explicit identifiers correlating responses with requests.

 - Bidirectional: responses are transmitted via the same underlying
   connection that the requests were transmitted through. The
   requesting party is anonymous; the underlying connection is the
   only name an HTTP server knows for the requester.

In other words, HTTP relies on the underlying transport to provide

 - automatic byte-level retransmission and acknowledgement,
 - request/response correlation, and
 - an "address" at which an HTTP server can reply to an HTTP client.

To generalize HTTP to run over a different transport protocol, these
features need to be provided in the new setting.

  [rev]: reverse-http-spec.html
  [relay]: relay-http-spec.html
  [RFC 2616]: http://www.w3.org/Protocols/rfc2616/rfc2616.html
