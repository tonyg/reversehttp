# Downloading ReverseHttp

<a href="http://www.lshift.net/"><img border="0" width="680" src="http://www.lshift.net/images/banner.jpg" alt="LShift Logo"></a>

The original development of ReverseHttp was supported by
[LShift](http://www.lshift.net/).

----

## How can I get a copy?

To download the code, you can either

 - use [Git](http://git-scm.com/) to check out the source repository:  
   `git clone git://github.com/tonyg/reversehttp.git`

 - [browse the code online](http://github.com/tonyg/reversehttp/tree/master)

 - or download the [most recent snapshot](http://github.com/tonyg/reversehttp/zipball/master)
   (automatically generated from the Github web interface upon request).

## What does it do?

The ReverseHttp software bundle contains

 - A server written in [Erlang](http://www.erlang.org/) ([browse
   source](http://github.com/tonyg/reversehttp/blob/master/src/reflect_request_queue.erl))
   that implements both

   - The [draft ReverseHttp specification][revspec], which lets
     arbitrary HTTP clients receive and respond to HTTP requests, and

   - The [draft RelayHttp specification][relayspec], which lets
     Javascript programs issue outbound HTTP requests without running
     afoul of the usual [cross-site scripting][XSS] attacks.

 - Client libraries for

    - Javascript in the browser
      ([browse source](http://github.com/tonyg/reversehttp/blob/master/priv/www/httpd.js))

    - Python
      ([browse source](http://github.com/tonyg/reversehttp/blob/master/priv/python/reversehttp.py))

    - Java
      ([browse source](http://github.com/tonyg/reversehttp/tree/master/priv/java/src/main/java/net/reversehttp))

## Are other languages supported?

Besides the software in the main downloadable bundle, the following
libraries have been developed by other contributors:

 - Hookout, Paul Jones's gem for Ruby ([browse
   source](http://github.com/paulj/hookout), [blog
   post](http://www.lshift.net/blog/2009/07/21/webhooks-behind-the-firewall-with-reverse-http))

## How is it licensed?

It is [open-source](http://www.opensource.org/) code,
licensed under the very liberal [MIT license](http://www.opensource.org/licenses/mit-license.php):

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

  [revspec]: reverse-http-spec.html
  [relayspec]: relay-http-spec.html
  [XSS]: http://en.wikipedia.org/wiki/Cross-site_scripting
