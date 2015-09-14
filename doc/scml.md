

# Module scml #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme library</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="types"></a>

## Data Types ##




### <a name="type-unichar">unichar()</a> ###


<pre><code>
unichar() = <a href="scmd_types_impl.md#type-unichar">scmd_types_impl:unichar()</a>
</code></pre>




### <a name="type-utf8">utf8()</a> ###


<pre><code>
utf8() = <a href="scmd_types_impl.md#type-utf8">scmd_types_impl:utf8()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#binary_part-2">binary_part/2</a></td><td></td></tr><tr><td valign="top"><a href="#binary_part-3">binary_part/3</a></td><td></td></tr><tr><td valign="top"><a href="#libraries-0">libraries/0</a></td><td><p>This function is a temporary place holder</p>.</td></tr><tr><td valign="top"><a href="#list_part-2">list_part/2</a></td><td></td></tr><tr><td valign="top"><a href="#list_part-3">list_part/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_unicode-1">to_unicode/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_utf8-1">to_utf8/1</a></td><td></td></tr><tr><td valign="top"><a href="#tuple_part-2">tuple_part/2</a></td><td></td></tr><tr><td valign="top"><a href="#tuple_part-3">tuple_part/3</a></td><td></td></tr><tr><td valign="top"><a href="#unicode_to_utf8-1">unicode_to_utf8/1</a></td><td></td></tr><tr><td valign="top"><a href="#unicode_to_utf8-2">unicode_to_utf8/2</a></td><td></td></tr><tr><td valign="top"><a href="#unicode_to_utf8-3">unicode_to_utf8/3</a></td><td></td></tr><tr><td valign="top"><a href="#utf8_to_unicode-1">utf8_to_unicode/1</a></td><td></td></tr><tr><td valign="top"><a href="#utf8_to_unicode-2">utf8_to_unicode/2</a></td><td></td></tr><tr><td valign="top"><a href="#utf8_to_unicode-3">utf8_to_unicode/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="binary_part-2"></a>

### binary_part/2 ###

<pre><code>
binary_part(X::binary(), Start::<a href="#type-scm_start">scm_start()</a>) -&gt; binary()
</code></pre>
<br />

<a name="binary_part-3"></a>

### binary_part/3 ###

<pre><code>
binary_part(X::binary(), Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; binary()
</code></pre>
<br />

<a name="libraries-0"></a>

### libraries/0 ###

<pre><code>
libraries() -&gt; [<a href="#type-scm_symbol">scm_symbol()</a>]
</code></pre>
<br />

<p>This function is a temporary place holder</p>

<a name="list_part-2"></a>

### list_part/2 ###

<pre><code>
list_part(X::<a href="#type-scm_list">scm_list()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>
<br />

<a name="list_part-3"></a>

### list_part/3 ###

<pre><code>
list_part(X::<a href="#type-scm_list">scm_list()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>
<br />

<a name="to_unicode-1"></a>

### to_unicode/1 ###

<pre><code>
to_unicode(X::iodata()) -&gt; [<a href="#type-unichar">unichar()</a>]
</code></pre>
<br />

<a name="to_utf8-1"></a>

### to_utf8/1 ###

<pre><code>
to_utf8(X::iodata()) -&gt; <a href="#type-utf8">utf8()</a>
</code></pre>
<br />

<a name="tuple_part-2"></a>

### tuple_part/2 ###

<pre><code>
tuple_part(X::tuple(), Start::<a href="#type-scm_start">scm_start()</a>) -&gt; tuple()
</code></pre>
<br />

<a name="tuple_part-3"></a>

### tuple_part/3 ###

<pre><code>
tuple_part(X::tuple(), Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; tuple()
</code></pre>
<br />

<a name="unicode_to_utf8-1"></a>

### unicode_to_utf8/1 ###

<pre><code>
unicode_to_utf8(X::[<a href="#type-unichar">unichar()</a>]) -&gt; <a href="#type-utf8">utf8()</a>
</code></pre>
<br />

<a name="unicode_to_utf8-2"></a>

### unicode_to_utf8/2 ###

<pre><code>
unicode_to_utf8(X::[<a href="#type-unichar">unichar()</a>], Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-utf8">utf8()</a>
</code></pre>
<br />

<a name="unicode_to_utf8-3"></a>

### unicode_to_utf8/3 ###

<pre><code>
unicode_to_utf8(X::[<a href="#type-unichar">unichar()</a>], Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-utf8">utf8()</a>
</code></pre>
<br />

<a name="utf8_to_unicode-1"></a>

### utf8_to_unicode/1 ###

<pre><code>
utf8_to_unicode(X::<a href="#type-utf8">utf8()</a>) -&gt; [<a href="#type-unichar">unichar()</a>]
</code></pre>
<br />

<a name="utf8_to_unicode-2"></a>

### utf8_to_unicode/2 ###

<pre><code>
utf8_to_unicode(X::<a href="#type-utf8">utf8()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; [<a href="#type-unichar">unichar()</a>]
</code></pre>
<br />

<a name="utf8_to_unicode-3"></a>

### utf8_to_unicode/3 ###

<pre><code>
utf8_to_unicode(X::<a href="#type-utf8">utf8()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; [<a href="#type-unichar">unichar()</a>]
</code></pre>
<br />

