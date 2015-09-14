

# Module scmi_iodev #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme interpreter i/o device resource and client</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="types"></a>

## Data Types ##




### <a name="type-iodev">iodev()</a> ###


__abstract datatype__: `iodev()`




### <a name="type-mode_file">mode_file()</a> ###


<pre><code>
mode_file() = <a href="scmi_iodev_server_file.md#type-mode">scmi_iodev_server_file:mode()</a>
</code></pre>




### <a name="type-mode_ram">mode_ram()</a> ###


<pre><code>
mode_ram() = <a href="scmi_iodev_server_ram.md#type-mode">scmi_iodev_server_ram:mode()</a>
</code></pre>




### <a name="type-mode_std">mode_std()</a> ###


<pre><code>
mode_std() = <a href="scmi_iodev_server_std.md#type-mode">scmi_iodev_server_std:mode()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#close-2">close/2</a></td><td></td></tr><tr><td valign="top"><a href="#flush-1">flush/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_ready-1">is_ready/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_resource-1">is_resource/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_resource_alive-1">is_resource_alive/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_resource_alive-2">is_resource_alive/2</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr><tr><td valign="top"><a href="#notify_when_destroyed-2">notify_when_destroyed/2</a></td><td></td></tr><tr><td valign="top"><a href="#notify_when_destroyed-3">notify_when_destroyed/3</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#peek-2">peek/2</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td></td></tr><tr><td valign="top"><a href="#read_all-1">read_all/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_line-1">read_line/1</a></td><td></td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(IODev::<a href="#type-iodev">iodev()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="close-2"></a>

### close/2 ###

<pre><code>
close(IODev::<a href="#type-iodev">iodev()</a>, Mode::read | write) -&gt; boolean()
</code></pre>
<br />

<a name="flush-1"></a>

### flush/1 ###

<pre><code>
flush(IODev::<a href="#type-iodev">iodev()</a>) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

<a name="is_ready-1"></a>

### is_ready/1 ###

<pre><code>
is_ready(IODev::<a href="#type-iodev">iodev()</a>) -&gt; boolean()
</code></pre>
<br />

<a name="is_resource-1"></a>

### is_resource/1 ###

<pre><code>
is_resource(Term::term()) -&gt; boolean()
</code></pre>
<br />

<a name="is_resource_alive-1"></a>

### is_resource_alive/1 ###

<pre><code>
is_resource_alive(Term::term()) -&gt; boolean()
</code></pre>
<br />

<a name="is_resource_alive-2"></a>

### is_resource_alive/2 ###

<pre><code>
is_resource_alive(Term::term(), Mode::read | write | binary) -&gt; boolean()
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Pid::pid(), Mod::module()) -&gt; <a href="#type-iodev">iodev()</a>
</code></pre>
<br />

<a name="notify_when_destroyed-2"></a>

### notify_when_destroyed/2 ###

<pre><code>
notify_when_destroyed(Msg::term(), Iodev::<a href="#type-iodev">iodev()</a>) -&gt; true
</code></pre>
<br />

<a name="notify_when_destroyed-3"></a>

### notify_when_destroyed/3 ###

<pre><code>
notify_when_destroyed(Pid::pid(), Msg::term(), X3::<a href="#type-iodev">iodev()</a>) -&gt; true
</code></pre>
<br />

<a name="open-2"></a>

### open/2 ###

<pre><code>
open(Term::standard_io, Modes::[<a href="#type-mode_std">mode_std()</a>]) -&gt; <a href="#type-iodev">iodev()</a>
</code></pre>
<br />

<a name="peek-2"></a>

### peek/2 ###

<pre><code>
peek(IODev::<a href="#type-iodev">iodev()</a>, K::non_neg_integer()) -&gt; string() | binary() | eof | {error, Reason::term()}
</code></pre>
<br />

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(IODev::<a href="#type-iodev">iodev()</a>, K::non_neg_integer()) -&gt; string() | binary() | eof | {error, Reason::term()}
</code></pre>
<br />

<a name="read_all-1"></a>

### read_all/1 ###

<pre><code>
read_all(IODev::<a href="#type-iodev">iodev()</a>) -&gt; string() | binary() | eof | {error, Reason::term()}
</code></pre>
<br />

<a name="read_line-1"></a>

### read_line/1 ###

<pre><code>
read_line(IODev::<a href="#type-iodev">iodev()</a>) -&gt; string() | binary() | eof | {error, Reason::term()}
</code></pre>
<br />

<a name="write-2"></a>

### write/2 ###

<pre><code>
write(IODev::<a href="#type-iodev">iodev()</a>, Data::string() | binary()) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />

