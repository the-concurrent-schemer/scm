

# Module scmd_parse #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-filename">filename()</a> ###


<pre><code>
filename() = <a href="file.md#type-filename">file:filename()</a>
</code></pre>




### <a name="type-posix">posix()</a> ###


<pre><code>
posix() = <a href="file.md#type-posix">file:posix()</a>
</code></pre>




### <a name="type-yecc_ret">yecc_ret()</a> ###


<pre><code>
yecc_ret() = {error, term()} | {ok, term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#binary-1">binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary-2">binary/2</a></td><td></td></tr><tr><td valign="top"><a href="#binary-3">binary/3</a></td><td></td></tr><tr><td valign="top"><a href="#binary-4">binary/4</a></td><td></td></tr><tr><td valign="top"><a href="#file-1">file/1</a></td><td></td></tr><tr><td valign="top"><a href="#file-3">file/3</a></td><td></td></tr><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#iolist-1">iolist/1</a></td><td></td></tr><tr><td valign="top"><a href="#iolist-2">iolist/2</a></td><td></td></tr><tr><td valign="top"><a href="#iolist-3">iolist/3</a></td><td></td></tr><tr><td valign="top"><a href="#iolist-4">iolist/4</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_and_scan-1">parse_and_scan/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-1">string/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-2">string/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-3">string/3</a></td><td></td></tr><tr><td valign="top"><a href="#string-4">string/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="binary-1"></a>

### binary/1 ###

<pre><code>
binary(X::binary()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="binary-2"></a>

### binary/2 ###

<pre><code>
binary(X::binary(), LineNo::pos_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="binary-3"></a>

### binary/3 ###

<pre><code>
binary(Leex::module(), Yecc::module(), X::binary()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="binary-4"></a>

### binary/4 ###

<pre><code>
binary(Leex::module(), Yecc::module(), X::binary(), LineNo::pos_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="file-1"></a>

### file/1 ###

<pre><code>
file(Filename::<a href="#type-filename">filename()</a>) -&gt; {ok, term()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = <a href="#type-posix">posix()</a> | badarg | terminated | system_limit | term()</code></li></ul>

<a name="file-3"></a>

### file/3 ###

<pre><code>
file(Leex::module(), Yecc::module(), Filename::<a href="#type-filename">filename()</a>) -&gt; {ok, term()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Reason = <a href="#type-posix">posix()</a> | badarg | terminated | system_limit | term()</code></li></ul>

<a name="format_error-1"></a>

### format_error/1 ###

<pre><code>
format_error(Message::any()) -&gt; [char() | list()]
</code></pre>
<br />

<a name="iolist-1"></a>

### iolist/1 ###

<pre><code>
iolist(X::iolist()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="iolist-2"></a>

### iolist/2 ###

<pre><code>
iolist(X::iolist(), LineNo::pos_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="iolist-3"></a>

### iolist/3 ###

<pre><code>
iolist(Leex::module(), Yecc::module(), X::iolist()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="iolist-4"></a>

### iolist/4 ###

<pre><code>
iolist(Leex::module(), Yecc::module(), X::iolist(), LineNo::pos_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Tokens::list()) -&gt; <a href="#type-yecc_ret">yecc_ret()</a>
</code></pre>
<br />

<a name="parse_and_scan-1"></a>

### parse_and_scan/1 ###

<pre><code>
parse_and_scan(X1::{function() | {atom(), atom()}, [term()]} | {atom(), atom(), [term()]}) -&gt; <a href="#type-yecc_ret">yecc_ret()</a>
</code></pre>
<br />

<a name="string-1"></a>

### string/1 ###

<pre><code>
string(X::string()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="string-2"></a>

### string/2 ###

<pre><code>
string(X::string(), LineNo::pos_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="string-3"></a>

### string/3 ###

<pre><code>
string(Leex::module(), Yecc::module(), X::string()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

<a name="string-4"></a>

### string/4 ###

<pre><code>
string(Leex::module(), Yecc::module(), X::string(), LineNo::pos_integer()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

