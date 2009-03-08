HTTP/1.1 is a lovely protocol. Text-based, sophisticated, flexible. It
does tend toward the verbose though. What if we wanted to use HTTP's
semantics in a very high-speed messaging situation? How could we
mitigate the overhead of all those headers?

Now, bandwidth is pretty cheap: cheap enough that for most
applications the kind of approach I suggest below is ridiculously far
over the top. Some situations, though, really do need a more efficient
protocol: I'm thinking of people having to consume the [OPRA][] feed,
which is fast approaching 1 million messages per second ([1][], [2][],
[3][]). What if, in some bizarre situation, HTTP was the protocol used
to deliver a full OPRA feed?

### Being Stateful

Instead of having each HTTP request start with a clean slate after the
previous request on a given connection has been processed, how about
giving connections a memory?

Let's invent a syntax for HTTP that is easy to translate back to
regular HTTP syntax, but that [avoids repeating ourselves][DRY] quite
so much.

Each line starts with an opcode and a colon. The rest of the line is
interpreted depending on the opcode. Each opcode-line is terminated
with CRLF.

    V:HTTP/1.x                          Set HTTP version identifier.
    B:/some/base/url                    Set base URL for requests.
    M:GET                               Set method for requests.
    <:somename                          Retrieve a named configuration
    >:somename                          Give the current configuration a name
    H:Header: value                     Set a header
    -:/url/suffix                       Issue a bodyless request
    +:/url/suffix 12345                 Issue a request with a body

Opcodes `V`, `B`, `M` and `H` are hopefully self-explanatory. I'll
explore `<` and `>` below. The opcodes `-` and `+` actually complete
each request and tell the server to process the message.

Opcode `-` takes as its argument a URL fragment that gets appended to
the base URL set by opcode `B`. Opcode `+` does the same, but also
takes an ASCII `Content-Length` value, which tells the server to read
that many bytes after the CRLF of the `+` line, and to use the bytes
read as the entity body of the HTTP request.

`Content-Length` is a slightly weird header, more properly associated
with the entity body than the headers proper, which is why it gets
special treatment. (We could also come up with a syntax for indicating
chunked transfer encoding for the entity body.)

As an example, let's encode the following `POST` request:

    POST /someurl HTTP/1.1
    Host: relay.localhost.lshift.net:8000
    Content-Type: text/plain
    Accept-Encoding: identity
    Content-Length: 13
    
    hello world

Encoded, this becomes

    V:HTTP/1.1
    B:/someurl
    M:POST
    H:Host: relay.localhost.lshift.net:8000
    H:Content-Type: text/plain
    H:Accept-Encoding: identity
    +: 13
    hello world

Not an obvious improvement. However, consider issuing 100 copies of
that same request on a single connection. With plain HTTP, all the
headers are repeated; with our encoded HTTP, the only part that is
repeated is:

    +: 13
    hello world

Instead of sending (151 * 100) = 15100 bytes, we now send 130 + (20 *
100) = 2130 bytes.

The scheme as described so far takes care of the unchanging parts of
repeated HTTP requests; for the changing parts, such as `Accept` and
`Referer` headers, we need to make use of the `<` and `>`
opcodes. Before I get into that, though, let's take a look at how the
scheme so far might work in the case of OPRA.

### Measuring OPRA

Each OPRA quote update is [on average 66 bytes
long](http://www.nanex.net/OPRADirect/OPRADirect.htm#3), making for
around 63MB/s of raw content.

Let's imagine that each delivery appears as a separate HTTP request:

    POST /receiver HTTP/1.1
    Host: opra-receiver.example.com
    Content-Type: application/x-opra-quote
    Accept-Encoding: identity
    Content-Length: 66
    
    blablablablablablablablablablablablablablablablablablablablablabla

That's 213 bytes long: an overhead of 220% over the raw message
content.

Encoded using the stateful scheme above, the first request appears on
the wire as

    V:HTTP/1.1
    B:/receiver
    M:POST
    H:Host: opra-receiver.example.com
    H:Content-Type: application/x-opra-quote
    H:Accept-Encoding: identity
    +: 66
    blablablablablablablablablablablablablablablablablablablablablabla

and subsequent requests as

    +: 66
    blablablablablablablablablablablablablablablablablablablablablabla

for an amortized per-request size of 73 bytes: a much less problematic
overhead of 11%. In summary:

<table>
<tr>
  <th>Encoding</th>
  <th>Bytes per message body</th>
  <th>Per-message overhead (bytes)</th>
  <th>Size increase over raw content</th>
  <th>Bandwidth at 1M msgs/sec</th>
</tr>
<tr>
  <td>Plain HTTP</td>
  <td>66</td>
  <td>147</td>
  <td>220%</td>
  <td>203.1 MBy/s</td>
</tr>
<tr>
  <td>Encoded HTTP</td>
  <td>66</td>
  <td>7</td>
  <td>11%</td>
  <td>69.6 MBy/s</td>
</tr>
</table>

Using plain HTTP, the feed doesn't fit on a gigabit ethernet. Using
our encoding scheme, it does.

Besides the savings in terms of bandwidth, the encoding scheme could
also help with saving CPU. After processing the headers once, the
results of the processing could be cached, avoiding unnecessary
repetition of potentially expensive calculations such as routing,
authentication, and authorisation.

### Almost-identical requests

Above, I mentioned that some headers changed, while others stayed the
same from request to request. The `<` and `>` opcodes are intended to
deal with just this situation.

The `>` opcode stores the current state in a named register, and the
`<` opcode loads the current state from a register. Headers that don't
change between requests are placed into a register, and each request
loads from that register before setting its request-specific headers.

To illustrate, imagine the following two requests:

    GET / HTTP/1.1
    Host: www.example.com
    Cookie: key=value
    Accept: HTTP Accept=text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
    
    GET /style.css HTTP/1.1
    Host: www.example.com
    Cookie: key=value
    Referer: http://www.example.com/
    Accept: text/css,*/*;q=0.1

One possible encoding is:

    V:HTTP/1.1
    B:/
    M:GET
    H:Host: www.example.com
    H:Cookie: key=value
    >:config1
    H:Accept: HTTP Accept=text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
    -:
    <:config1
    H:Referer: http://www.example.com/
    H:Accept: text/css,*/*;q=0.1
    -:style.css

By using `<:config1`, the second request reuses the stored settings
for the method, base URL, HTTP version, and `Host` and `Cookie`
headers.

### It'll never catch on, of course &mdash; and I don't mean for it to

Most applications of HTTP do fine using ordinary HTTP syntax. I'm not
suggesting changing HTTP, or trying to get an encoding scheme like
this deployed in any browser or webserver at all. The point of the
exercise is to consider how low one might make the bandwidth overheads
of a text-based protocol like HTTP for the specific case of a
high-speed messaging scenario.

In situations where the semantics of HTTP make sense, but the syntax
is just too verbose, schemes like this one can be useful on a
point-to-point link. There's no need for global support for an
alternative syntax, since people who are already forming very specific
contracts with each other for the exchange of information can choose
to use it, or not, on a case-by-case basis.

Instead of specifying a whole new transport protocol for high-speed
links, people can reuse the considerable amount of work that's gone
into HTTP, without paying the bandwidth price.

### Aside: AMQP 0-8 / 0-9

Just as a throwaway comparison, I computed the minimum possible
overhead for sending a 66-byte message using AMQP 0-8 or 0-9. Using a
single-letter queue name, "`q`", the overhead is 69 bytes per message,
or 105% of the message body. For our OPRA example at 1M messages per
second, that works out at 128.7 megabytes per second, and we're back
over the limit of a single gigabit ethernet again. Interestingly,
despite AMQP's binary nature, its overhead is much higher than a
simple syntactic rearrangement of a text-based protocol in this case.

### Conclusion

We considered the overhead of using plain HTTP in a high-speed
messaging scenario, and invented a simple alternative syntax for HTTP
that drastically reduces the wasted bandwidth.

For the specific example of the OPRA feed, the computed bandwidth
requirement of the experimental syntax is only 11% higher than the raw
data itself &mdash; nearly 3 times less than ordinary HTTP.

----

Note: [this][3] is a local mirror of [this](http://www.jandj.com/presentations/SIFMATech2008-CapacityGrowth.pdf).

  [DRY]: http://en.wikipedia.org/wiki/Don%27t_repeat_yourself
  [OPRA]: http://www.opradata.com/

  [1]: http://www.wallstreetandtech.com/news/trading/showArticle.jhtml?articleID=163101596
  [2]: http://en.wikipedia.org/wiki/Options_Price_Reporting_Authority#Messages_per_Second
  [3]: http://www.lshift.net/blog/wp-content/uploads/2009/02/sifmatech2008-capacitygrowth.pdf
