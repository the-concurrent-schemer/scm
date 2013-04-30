

# Module scml_base_string #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme base library for strings</p>.
__Authors:__ CSCM Contributor ([`the-concurrent-schemer@googlegroups.com`](mailto:the-concurrent-schemer@googlegroups.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#list-%3estring-1">'list->string'/1</a></td><td><p>Returns a string constructed from the characters in the list.</p>.</td></tr><tr><td valign="top"><a href="#make-string-1">'make-string'/1</a></td><td>Equivalent to <a href="#make-string-2"><tt>'make-string'(K, '#\\null')</tt></a>.</td></tr><tr><td valign="top"><a href="#make-string-2">'make-string'/2</a></td><td><p>Returns a string of k characters.</p>.</td></tr><tr><td valign="top"><a href="#string-%3elist-1">'string->list'/1</a></td><td>Equivalent to <a href="#string-%3elist-3"><tt>'string-&gt;list'(S, 0, 'string-length'(S))</tt></a>.</td></tr><tr><td valign="top"><a href="#string-%3elist-2">'string->list'/2</a></td><td>Equivalent to <a href="#string-%3elist-3"><tt>'string-&gt;list'(S, Start, 'string-length'(S))</tt></a>.</td></tr><tr><td valign="top"><a href="#string-%3elist-3">'string->list'/3</a></td><td><p>Returns a list of the characters of string between start and
end.</p>.</td></tr><tr><td valign="top"><a href="#string-append-1">'string-append'/1</a></td><td><p>Returns a string whose characters are the concatenation of the
characters in the given strings.</p>.</td></tr><tr><td valign="top"><a href="#string-copy%21-3">'string-copy!'/3</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#string-copy%21-4">'string-copy!'/4</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#string-copy%21-5">'string-copy!'/5</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#string-copy-1">'string-copy'/1</a></td><td>Equivalent to <a href="#string-copy-3"><tt>'string-copy'(S, 0, 'string-length'(S))</tt></a>.</td></tr><tr><td valign="top"><a href="#string-copy-2">'string-copy'/2</a></td><td>Equivalent to <a href="#string-copy-3"><tt>'string-copy'(S, Start, 'string-length'(S))</tt></a>.</td></tr><tr><td valign="top"><a href="#string-copy-3">'string-copy'/3</a></td><td><p>Returns a string constructed from the characters of string
beginning with index start and ending with index end.</p>.</td></tr><tr><td valign="top"><a href="#string-fill%21-2">'string-fill!'/2</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#string-fill%21-3">'string-fill!'/3</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#string-fill%21-4">'string-fill!'/4</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#string-length-1">'string-length'/1</a></td><td><p>Returns the number of characters in the given string.</p>.</td></tr><tr><td valign="top"><a href="#string-ref-2">'string-ref'/2</a></td><td><p>Returns character k of string using zero-origin indexing.  It
is an error if k is not a valid index of string.</p>.</td></tr><tr><td valign="top"><a href="#string-set%21-3">'string-set!'/3</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#string%3c%3d%3f-1">'string<=?'/1</a></td><td><p>Returns #t if all the strings are monotonically
non-decreasing, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#string%3c%3f-1">'string<?'/1</a></td><td><p>Returns #t if all the strings are monotonically increasing,
otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#string%3d%3f-1">'string=?'/1</a></td><td><p>Returns #t if all the strings are the same length and contain
exactly the same characters in the same positions, otherwise
returns #f.</p>.</td></tr><tr><td valign="top"><a href="#string%3e%3d%3f-1">'string>=?'/1</a></td><td><p>Returns #t if all the strings are monotonically
non-increasing, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#string%3e%3f-1">'string>?'/1</a></td><td><p>Returns #t if all the strings are monotonically decreasing,
otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#string%3f-1">'string?'/1</a></td><td><p>Returns #t if obj is a string, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#string-1">string/1</a></td><td><p>Returns a string composed of the arguments.</p>.</td></tr><tr><td valign="top"><a href="#substring-3">substring/3</a></td><td>Equivalent to <a href="#string-copy-3"><tt>'string-copy'(S, Start, End)</tt></a>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="list-%3estring-1"></a>

### 'list->string'/1 ###


<pre><code>
'list-&gt;string'(Cs::[<a href="#type-scm_char">scm_char()</a>]) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


<p>Returns a string constructed from the characters in the list.</p>

<a name="make-string-1"></a>

### 'make-string'/1 ###


<pre><code>
'make-string'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


Equivalent to [`'make-string'(K, '#\\null')`](#make-string-2).
<a name="make-string-2"></a>

### 'make-string'/2 ###


<pre><code>
'make-string'(K::<a href="#type-scm_k">scm_k()</a>, Character::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


<p>Returns a string of k characters.</p>

<a name="string-%3elist-1"></a>

### 'string->list'/1 ###


<pre><code>
'string-&gt;list'(String::<a href="#type-scm_string">scm_string()</a>) -&gt; [<a href="#type-scm_char">scm_char()</a>]
</code></pre>

<br></br>


Equivalent to [`'string->list'(S, 0, 'string-length'(S))`](#string-%3elist-3).
<a name="string-%3elist-2"></a>

### 'string->list'/2 ###


<pre><code>
'string-&gt;list'(String::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; [<a href="#type-scm_char">scm_char()</a>]
</code></pre>

<br></br>


Equivalent to [`'string->list'(S, Start, 'string-length'(S))`](#string-%3elist-3).
<a name="string-%3elist-3"></a>

### 'string->list'/3 ###


<pre><code>
'string-&gt;list'(String::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; [<a href="#type-scm_char">scm_char()</a>]
</code></pre>

<br></br>


<p>Returns a list of the characters of string between start and
end.</p>

<a name="string-append-1"></a>

### 'string-append'/1 ###


<pre><code>
'string-append'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


<p>Returns a string whose characters are the concatenation of the
characters in the given strings.</p>

<a name="string-copy%21-3"></a>

### 'string-copy!'/3 ###


<pre><code>
'string-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="string-copy%21-4"></a>

### 'string-copy!'/4 ###


<pre><code>
'string-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="string-copy%21-5"></a>

### 'string-copy!'/5 ###


<pre><code>
'string-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="string-copy-1"></a>

### 'string-copy'/1 ###


<pre><code>
'string-copy'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


Equivalent to [`'string-copy'(S, 0, 'string-length'(S))`](#string-copy-3).
<a name="string-copy-2"></a>

### 'string-copy'/2 ###


<pre><code>
'string-copy'(String::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


Equivalent to [`'string-copy'(S, Start, 'string-length'(S))`](#string-copy-3).
<a name="string-copy-3"></a>

### 'string-copy'/3 ###


<pre><code>
'string-copy'(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


<p>Returns a string constructed from the characters of string
beginning with index start and ending with index end.</p>

<a name="string-fill%21-2"></a>

### 'string-fill!'/2 ###


<pre><code>
'string-fill!'(S::<a href="#type-scm_string">scm_string()</a>, Fill::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="string-fill%21-3"></a>

### 'string-fill!'/3 ###


<pre><code>
'string-fill!'(S::<a href="#type-scm_string">scm_string()</a>, Fill::<a href="#type-scm_char">scm_char()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="string-fill%21-4"></a>

### 'string-fill!'/4 ###


<pre><code>
'string-fill!'(S::<a href="#type-scm_string">scm_string()</a>, Fill::<a href="#type-scm_char">scm_char()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="string-length-1"></a>

### 'string-length'/1 ###


<pre><code>
'string-length'(String::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_k">scm_k()</a>
</code></pre>

<br></br>


<p>Returns the number of characters in the given string.</p>

<a name="string-ref-2"></a>

### 'string-ref'/2 ###


<pre><code>
'string-ref'(String::<a href="#type-scm_string">scm_string()</a>, K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_char">scm_char()</a>
</code></pre>

<br></br>


<p>Returns character k of string using zero-origin indexing.  It
is an error if k is not a valid index of string.</p>

<a name="string-set%21-3"></a>

### 'string-set!'/3 ###


<pre><code>
'string-set!'(S::<a href="#type-scm_string">scm_string()</a>, K::<a href="#type-scm_k">scm_k()</a>, C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="string%3c%3d%3f-1"></a>

### 'string<=?'/1 ###


<pre><code>
'string&lt;=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if all the strings are monotonically
non-decreasing, otherwise returns #f.</p>

<a name="string%3c%3f-1"></a>

### 'string<?'/1 ###


<pre><code>
'string&lt;?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if all the strings are monotonically increasing,
otherwise returns #f.</p>

<a name="string%3d%3f-1"></a>

### 'string=?'/1 ###


<pre><code>
'string=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if all the strings are the same length and contain
exactly the same characters in the same positions, otherwise
returns #f.</p>

<a name="string%3e%3d%3f-1"></a>

### 'string>=?'/1 ###


<pre><code>
'string&gt;=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if all the strings are monotonically
non-increasing, otherwise returns #f.</p>

<a name="string%3e%3f-1"></a>

### 'string>?'/1 ###


<pre><code>
'string&gt;?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if all the strings are monotonically decreasing,
otherwise returns #f.</p>

<a name="string%3f-1"></a>

### 'string?'/1 ###


<pre><code>
'string?'(String::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is a string, otherwise returns #f.</p>

<a name="string-1"></a>

### string/1 ###


<pre><code>
string(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


<p>Returns a string composed of the arguments.</p>

<a name="substring-3"></a>

### substring/3 ###


<pre><code>
substring(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


Equivalent to [`'string-copy'(S, Start, End)`](#string-copy-3).
