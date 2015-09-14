

# Module scml_base_bytevector #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme base library for bytevectors</p>.

__Authors:__ CSCM Contributor ([`the-concurrent-schemer@googlegroups.com`](mailto:the-concurrent-schemer@googlegroups.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#bytevector-append-1">'bytevector-append'/1</a></td><td><p>Returns a bytevector whose bytes are the concatenation of the
bytes in the given bytevectors.</p>.</td></tr><tr><td valign="top"><a href="#bytevector-copy%21-3">'bytevector-copy!'/3</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#bytevector-copy%21-4">'bytevector-copy!'/4</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#bytevector-copy%21-5">'bytevector-copy!'/5</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#bytevector-copy-1">'bytevector-copy'/1</a></td><td>Equivalent to <a href="#bytevector-copy-3"><tt>'bytevector-copy'(V, 0, 'bytevector-length'(V))</tt></a>.</td></tr><tr><td valign="top"><a href="#bytevector-copy-2">'bytevector-copy'/2</a></td><td>Equivalent to <a href="#bytevector-copy-3"><tt>'bytevector-copy'(V, Start, 'bytevector-length'(V))</tt></a>.</td></tr><tr><td valign="top"><a href="#bytevector-copy-3">'bytevector-copy'/3</a></td><td><p>Returns a bytevector constructed from the bytes of bytevector
beginning with index start and ending with index end.</p>.</td></tr><tr><td valign="top"><a href="#bytevector-length-1">'bytevector-length'/1</a></td><td><p>Returns the number of bytes in the given bytevector.</p>.</td></tr><tr><td valign="top"><a href="#bytevector-u8-ref-2">'bytevector-u8-ref'/2</a></td><td><p>Returns byte k of bytevector using zero-origin indexing.  It
is an error if k is not a valid index of bytevector.</p>.</td></tr><tr><td valign="top"><a href="#bytevector-u8-set%21-3">'bytevector-u8-set!'/3</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#bytevector%3f-1">'bytevector?'/1</a></td><td><p>Returns #t if obj is a bytevector, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#make-bytevector-1">'make-bytevector'/1</a></td><td>Equivalent to <a href="#make-bytevector-2"><tt>'make-bytevector'(K, 0)</tt></a>.</td></tr><tr><td valign="top"><a href="#make-bytevector-2">'make-bytevector'/2</a></td><td><p>Returns a bytevector of k bytes.</p>.</td></tr><tr><td valign="top"><a href="#string-%3eutf8-1">'string->utf8'/1</a></td><td>Equivalent to <a href="#string-%3eutf8-3"><tt>'string-&gt;utf8'(S, 0, 'string-length'(S))</tt></a>.</td></tr><tr><td valign="top"><a href="#string-%3eutf8-2">'string->utf8'/2</a></td><td>Equivalent to <a href="#string-%3eutf8-3"><tt>'string-&gt;utf8'(S, Start, 'string-length'(S))</tt></a>.</td></tr><tr><td valign="top"><a href="#string-%3eutf8-3">'string->utf8'/3</a></td><td><p>encodes the characters of a string between start and end and
returns the corresponding bytevector.</p>.</td></tr><tr><td valign="top"><a href="#utf8-%3estring-1">'utf8->string'/1</a></td><td>Equivalent to <a href="#utf8-%3estring-3"><tt>'utf8-&gt;string'(V, 0, 'bytevector-length'(V))</tt></a>.</td></tr><tr><td valign="top"><a href="#utf8-%3estring-2">'utf8->string'/2</a></td><td>Equivalent to <a href="#utf8-%3estring-3"><tt>'utf8-&gt;string'(V, Start, 'bytevector-length'(V))</tt></a>.</td></tr><tr><td valign="top"><a href="#utf8-%3estring-3">'utf8->string'/3</a></td><td><p>Decodes the bytes of a bytevector between start and end and
returns the corresponding string.</p>.</td></tr><tr><td valign="top"><a href="#bytevector-1">bytevector/1</a></td><td><p>Returns a bytevector whose bytes contain the given arguments.</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###

<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>
<br />

<a name="bytevector-append-1"></a>

### 'bytevector-append'/1 ###

<pre><code>
'bytevector-append'(Vs::[<a href="#type-scm_bytevector">scm_bytevector()</a>, ...]) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

<p>Returns a bytevector whose bytes are the concatenation of the
bytes in the given bytevectors.</p>

<a name="bytevector-copy%21-3"></a>

### 'bytevector-copy!'/3 ###

<pre><code>
'bytevector-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p><em>unsupported</em></p>

<a name="bytevector-copy%21-4"></a>

### 'bytevector-copy!'/4 ###

<pre><code>
'bytevector-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_bytevector">scm_bytevector()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p><em>unsupported</em></p>

<a name="bytevector-copy%21-5"></a>

### 'bytevector-copy!'/5 ###

<pre><code>
'bytevector-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_bytevector">scm_bytevector()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p><em>unsupported</em></p>

<a name="bytevector-copy-1"></a>

### 'bytevector-copy'/1 ###

<pre><code>
'bytevector-copy'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

Equivalent to [`'bytevector-copy'(V, 0, 'bytevector-length'(V))`](#bytevector-copy-3).

<a name="bytevector-copy-2"></a>

### 'bytevector-copy'/2 ###

<pre><code>
'bytevector-copy'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

Equivalent to [`'bytevector-copy'(V, Start, 'bytevector-length'(V))`](#bytevector-copy-3).

<a name="bytevector-copy-3"></a>

### 'bytevector-copy'/3 ###

<pre><code>
'bytevector-copy'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

<p>Returns a bytevector constructed from the bytes of bytevector
beginning with index start and ending with index end.</p>

<a name="bytevector-length-1"></a>

### 'bytevector-length'/1 ###

<pre><code>
'bytevector-length'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_k">scm_k()</a>
</code></pre>
<br />

<p>Returns the number of bytes in the given bytevector.</p>

<a name="bytevector-u8-ref-2"></a>

### 'bytevector-u8-ref'/2 ###

<pre><code>
'bytevector-u8-ref'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>, K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_byte">scm_byte()</a>
</code></pre>
<br />

<p>Returns byte k of bytevector using zero-origin indexing.  It
is an error if k is not a valid index of bytevector.</p>

<a name="bytevector-u8-set%21-3"></a>

### 'bytevector-u8-set!'/3 ###

<pre><code>
'bytevector-u8-set!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, K::<a href="#type-scm_k">scm_k()</a>, Byte::<a href="#type-scm_byte">scm_byte()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p><em>unsupported</em></p>

<a name="bytevector%3f-1"></a>

### 'bytevector?'/1 ###

<pre><code>
'bytevector?'(Bytevector::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if obj is a bytevector, otherwise returns #f.</p>

<a name="make-bytevector-1"></a>

### 'make-bytevector'/1 ###

<pre><code>
'make-bytevector'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

Equivalent to [`'make-bytevector'(K, 0)`](#make-bytevector-2).

<a name="make-bytevector-2"></a>

### 'make-bytevector'/2 ###

<pre><code>
'make-bytevector'(K::<a href="#type-scm_k">scm_k()</a>, Byte::<a href="#type-scm_byte">scm_byte()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

<p>Returns a bytevector of k bytes.</p>

<a name="string-%3eutf8-1"></a>

### 'string->utf8'/1 ###

<pre><code>
'string-&gt;utf8'(String::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

Equivalent to [`'string->utf8'(S, 0, 'string-length'(S))`](#string-%3eutf8-3).

<a name="string-%3eutf8-2"></a>

### 'string->utf8'/2 ###

<pre><code>
'string-&gt;utf8'(String::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

Equivalent to [`'string->utf8'(S, Start, 'string-length'(S))`](#string-%3eutf8-3).

<a name="string-%3eutf8-3"></a>

### 'string->utf8'/3 ###

<pre><code>
'string-&gt;utf8'(String::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

<p>encodes the characters of a string between start and end and
returns the corresponding bytevector.</p>

<a name="utf8-%3estring-1"></a>

### 'utf8->string'/1 ###

<pre><code>
'utf8-&gt;string'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>
<br />

Equivalent to [`'utf8->string'(V, 0, 'bytevector-length'(V))`](#utf8-%3estring-3).

<a name="utf8-%3estring-2"></a>

### 'utf8->string'/2 ###

<pre><code>
'utf8-&gt;string'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>
<br />

Equivalent to [`'utf8->string'(V, Start, 'bytevector-length'(V))`](#utf8-%3estring-3).

<a name="utf8-%3estring-3"></a>

### 'utf8->string'/3 ###

<pre><code>
'utf8-&gt;string'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>
<br />

<p>Decodes the bytes of a bytevector between start and end and
returns the corresponding string.</p>

<a name="bytevector-1"></a>

### bytevector/1 ###

<pre><code>
bytevector(Bytes::[<a href="#type-scm_byte">scm_byte()</a>, ...]) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>
<br />

<p>Returns a bytevector whose bytes contain the given arguments.</p>

