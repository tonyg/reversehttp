---
layout: default
---
# ReverseHttp

## Introduction

Polling for updates is bad. We've known this for about as long as
computers have existed. So why are so many web-based services
([SUP][], RSS and Atom feeds, Twitter, etc.) based around polling?

The answer lies, first and foremost, in the asymmetry of HTTP. The web
is split into two pieces: programs that make requests, and programs
that handle them. It's very rare to see a single program behaving as
both a client and a server.

To fix the asymmetry, we need to be able to act as if being able to
respond to HTTP requests was within easy reach of every program, so
that we can notify interested parties of changes by simply sending an
HTTP request to them. This is the core idea of [web hooks][].

We need to push the messy business of dealing with long-polling away
from the core and toward the edge of the network, where it should be.

We need to let programs dynamically claim a piece of URL space using a
plain old HTTP client and handle requests sent to URLs in that space
using that same HTTP client.

Once that's done suddenly asynchronous notification of events is
within reach of any program that has access to an HTTP client library,
and protocols and services layered on top of HTTP no longer have to
contort themselves to deal with the asymmetry of HTTP. They can assume
that all the world's a server, and simply push and pull content to and
from whereever they please.

## The Solution

Tunnel HTTP over HTTP, in a structured, controllable, securable
way. Let programs claim part of URL space, and serve HTTP, all by
using an ordinary HTTP client library.

 - Read the [draft specifications](specs.html).
 - Check out the [implementation][].
 - Try [the demos][demos].

Even programs running in very restrictive environments, such as
Javascript programs in the browser, can take advantage of a
ReverseHttp service. The current implementation provides small, simple
libraries for Python, Java and in-browser Javascript.

&nbsp;

<div class="imagecenter"><img src="overview.svg"></div>

&nbsp;

## Why hasn't this been done before?

This isn't a completely new idea, it seems, though it doesn't seem to
be widely used &mdash; yet.

At around the same time as I was writing ReverseHttp (i.e. May 2008),
Donovan Preston at [Second Life](http://secondlife.com/) was writing
up his take on the same idea, which he also called [Reverse
HTTP](http://wiki.secondlife.com/wiki/Reverse_HTTP) (a.k.a
"PTTH"). His idea is to use the HTTP 1.1 `Upgrade` header to switch
the direction of the protocol, which works well for non-browser
environments. He also has a [Comet][]-based solution very similar to
mine, except that it uses [JSON](http://www.ietf.org/rfc/rfc4627.txt)
objects for describing the HTTP requests and responses where mine uses
the actual HTTP message formats. [Donovan Preston writes more about
the Second Life variant
here](http://ulaluma.com/pyx/archives/2008/12/ptth_reverse_ht.html),
and has, with Mark Lentczner, produced [an
Internet-Draft](http://www.ietf.org/internet-drafts/draft-lentczner-rhttp-00.txt)
for the `Upgrade`-header based protocol.

*Update:* I've only just now (August 2012) discovered [Eric N. Sit's
SB/MEng thesis from 2000, "Reverse HTTP tunneling for firewall
traversal"](http://dspace.mit.edu/handle/1721.1/9086). This is the
earliest presentation of the idea that I'm aware of. I think it's
pretty neat that Sit was emphasising firewall traversal, which is one
particular situation where enrolment is really important.

## Download and links

 - Instructions for downloading the implementation are [here][implementation]

 - There's lots of thinking from others along the same lines:
    - <http://www.eflorenzano.com/blog/post/reverse-http/>
    - <http://ulaluma.com/pyx/archives/2008/12/ptth_reverse_ht.html>
    - <http://t0rxon.blogspot.com/2009/02/making-real-time-web-real-time.html>
    - <http://kirkwylie.blogspot.com/2008/12/scoble-joins-real-time-web-conversation.html>
    - <http://kirkwylie.blogspot.com/2008/12/rest-requires-asynchronous-notification.html>
    - <http://www.webhooks.org/>

  [XSS]: http://en.wikipedia.org/wiki/Cross-site_scripting
  [implementation]: download.html
  [SUP]: http://blog.friendfeed.com/2008/08/simple-update-protocol-fetch-updates.html
  [Comet]: http://en.wikipedia.org/wiki/Comet_(programming)
  [demos]: offline.html
  [web hooks]: http://web.archive.org/web/20101031131824/http://blog.webhooks.org/about/

## Demo service

Up until mid-2010, there was a free demo ReverseHttp service running
at `reversehttp.net`, but I have since taken it offline because the
traffic bill was getting too high. If anyone would like to sponsor or
host the publicly-available service, or would like advice on
configuring a service instance of their own for their own use, please
[get in
touch](mailto:tonygarnockjones+reversehttp@gmail.com?Subject=ReverseHTTP+Service).

Please [see here](offline.html) for more information.
