

# Module scmi_iodev_server_std #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme interpreter i/o device stdio and stderr server</p>.
__Behaviours:__ [`gen_server`](gen_server.html).

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="types"></a>

## Data Types ##




### <a name="type-mode">mode()</a> ###



<pre><code>
mode() = read | write
</code></pre>





### <a name="type-mode_error">mode_error()</a> ###



<pre><code>
mode_error() = write
</code></pre>





### <a name="type-opt">opt()</a> ###



<pre><code>
opt() = {read, boolean()} | {write, boolean()} | {binary, false} | {test, boolean()}
</code></pre>





### <a name="type-server">server()</a> ###



<pre><code>
server() = pid()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#close-2">close/2</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#flush-1">flush/1</a></td><td></td></tr><tr><td valign="top"><a href="#getopts-1">getopts/1</a></td><td></td></tr><tr><td valign="top"><a href="#getopts-2">getopts/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_ready-1">is_ready/1</a></td><td></td></tr><tr><td valign="top"><a href="#peek-2">peek/2</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td></td></tr><tr><td valign="top"><a href="#read_all-1">read_all/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_line-1">read_line/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###


<pre><code>
close(Server::<a href="#type-server">server()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="close-2"></a>

### close/2 ###


<pre><code>
close(Server::<a href="#type-server">server()</a>, Mode::read | write) -&gt; boolean()
</code></pre>

<br></br>



<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="flush-1"></a>

### flush/1 ###


<pre><code>
flush(Server::<a href="#type-server">server()</a>) -&gt; ok | {error, Reason::term()}
</code></pre>

<br></br>



<a name="getopts-1"></a>

### getopts/1 ###


<pre><code>
getopts(Server::<a href="#type-server">server()</a>) -&gt; [<a href="#type-opt">opt()</a>]
</code></pre>

<br></br>



<a name="getopts-2"></a>

### getopts/2 ###


<pre><code>
getopts(Server::<a href="#type-server">server()</a>, Mode::read | write | binary) -&gt; boolean()
</code></pre>

<br></br>



<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Msg, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`


<a name="init-1"></a>

### init/1 ###


<pre><code>
init(X1::{iodata() | standard_io | standard_error, [<a href="#type-mode">mode()</a>]}) -&gt; {ok, #state{}}
</code></pre>

<br></br>



<a name="is_ready-1"></a>

### is_ready/1 ###


<pre><code>
is_ready(Server::<a href="#type-server">server()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="peek-2"></a>

### peek/2 ###


<pre><code>
peek(Server::<a href="#type-server">server()</a>, K::non_neg_integer()) -&gt; string() | eof | {error, Reason::term()}
</code></pre>

<br></br>



<a name="read-2"></a>

### read/2 ###


<pre><code>
read(Server::<a href="#type-server">server()</a>, K::non_neg_integer()) -&gt; string() | eof | {error, Reason::term()}
</code></pre>

<br></br>



<a name="read_all-1"></a>

### read_all/1 ###


<pre><code>
read_all(Server::<a href="#type-server">server()</a>) -&gt; string() | eof | {error, Reason::term()}
</code></pre>

<br></br>



<a name="read_line-1"></a>

### read_line/1 ###


<pre><code>
read_line(Server::<a href="#type-server">server()</a>) -&gt; string() | eof | {error, Reason::term()}
</code></pre>

<br></br>



<a name="start-2"></a>

### start/2 ###


<pre><code>
start(Term::standard_io, Modes::[<a href="#type-mode">mode()</a>]) -&gt; {ok, pid()} | ignore | {error, Reason::term()}
</code></pre>

<br></br>



<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


<a name="write-2"></a>

### write/2 ###


<pre><code>
write(Server::<a href="#type-server">server()</a>, Data::string()) -&gt; ok | {error, Reason::term()}
</code></pre>

<br></br>



