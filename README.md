# Making the web symmetric

[ReverseHttp](http://github.com/tonyg/reversehttp) is an
implementation of the [specifications][specs] hosted at
<http://www.reversehttp.net>:

 - [ReverseHttp][rev]: a dynamic, ReST-style means of enrolment and
   participation in an HTTP network. The `message/http` and
   `application/http` MIME types defined by [RFC 2616][] are used to
   build a dynamically-configurable "Remote CGI" service.

 - [RelayHttp][relay]: a protocol for tunnelling HTTP traffic over
   HTTP, with the goal of providing portable, general, securable
   access to the World Wide Web for programs running in restricted
   environments, including Javascript programs running in browsers.

Joining the World Wide Web as an HTTP server has been an ad-hoc,
manual process. By using the [ReverseHttp protocol][rev], programs can
provide services to the Web just as easily as they request services
from the Web.

## Code overview

 - `src/` holds the server implementation, written in Erlang using
   Mochiweb.

 - `priv/www/` holds the Javascript client implementation and demos.

 - `priv/java/` holds the Java client implementation.

 - `priv/python/` holds the Python client implementation.

## Building and running ReverseHttp

Run `make`. If this succeeds, run `./start-dev.sh`.

By default, the server will now be running on port 8000, serving
content from `priv/www`. Try <http://localhost:8000/>: you should see
an index of demos.

To change the port number, edit `reversehttp.app`. The version in
`src/` is the master copy, but the version in `ebin/` is the one
that's used to actually run the software.

## Software License

ReverseHttp is [open-source](http://www.opensource.org/) code,
licensed under the very liberal [MIT
license](http://www.opensource.org/licenses/mit-license.php):

    Copyright (c) 2008, 2009, 2010 Tony Garnock-Jones <tonygarnockjones@gmail.com>
    Copyright (c) 2008, 2009 LShift Ltd. <query@lshift.net>

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.

  [rev]: http://www.reversehttp.net/reverse-http-spec.html
  [relay]: http://www.reversehttp.net/relay-http-spec.html
  [specs]: http://www.reversehttp.net/specs.html
  [RFC 2616]: http://www.w3.org/Protocols/rfc2616/rfc2616.html
