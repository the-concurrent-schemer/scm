

# Module scml_base_string #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#list-%3estring-1">'list->string'/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-string-1">'make-string'/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-string-2">'make-string'/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-%3elist-1">'string->list'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-%3elist-2">'string->list'/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-%3elist-3">'string->list'/3</a></td><td></td></tr><tr><td valign="top"><a href="#string-append-1">'string-append'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-copy%21-3">'string-copy!'/3</a></td><td></td></tr><tr><td valign="top"><a href="#string-copy%21-4">'string-copy!'/4</a></td><td></td></tr><tr><td valign="top"><a href="#string-copy%21-5">'string-copy!'/5</a></td><td></td></tr><tr><td valign="top"><a href="#string-copy-1">'string-copy'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-copy-2">'string-copy'/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-copy-3">'string-copy'/3</a></td><td></td></tr><tr><td valign="top"><a href="#string-fill%21-2">'string-fill!'/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-fill%21-3">'string-fill!'/3</a></td><td></td></tr><tr><td valign="top"><a href="#string-fill%21-4">'string-fill!'/4</a></td><td></td></tr><tr><td valign="top"><a href="#string-length-1">'string-length'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-ref-2">'string-ref'/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-set%21-3">'string-set!'/3</a></td><td></td></tr><tr><td valign="top"><a href="#string%3c%3d%3f-1">'string<=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string%3c%3f-1">'string<?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string%3d%3f-1">'string=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string%3e%3d%3f-1">'string>=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string%3e%3f-1">'string>?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string%3f-1">'string?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#imports-0">imports/0</a></td><td></td></tr><tr><td valign="top"><a href="#string-1">string/1</a></td><td></td></tr><tr><td valign="top"><a href="#substring-3">substring/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="list-%3estring-1"></a>

### 'list->string'/1 ###


<pre><code>
'list-&gt;string'(Cs::[<a href="#type-scm_char">scm_char()</a>]) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="make-string-1"></a>

### 'make-string'/1 ###


<pre><code>
'make-string'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="make-string-2"></a>

### 'make-string'/2 ###


<pre><code>
'make-string'(K::<a href="#type-scm_k">scm_k()</a>, C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="string-%3elist-1"></a>

### 'string->list'/1 ###


<pre><code>
'string-&gt;list'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; [<a href="#type-scm_char">scm_char()</a>]
</code></pre>

<br></br>



<a name="string-%3elist-2"></a>

### 'string->list'/2 ###


<pre><code>
'string-&gt;list'(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; [<a href="#type-scm_char">scm_char()</a>]
</code></pre>

<br></br>



<a name="string-%3elist-3"></a>

### 'string->list'/3 ###


<pre><code>
'string-&gt;list'(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; [<a href="#type-scm_char">scm_char()</a>]
</code></pre>

<br></br>



<a name="string-append-1"></a>

### 'string-append'/1 ###


<pre><code>
'string-append'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="string-copy%21-3"></a>

### 'string-copy!'/3 ###


<pre><code>
'string-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string-copy%21-4"></a>

### 'string-copy!'/4 ###


<pre><code>
'string-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string-copy%21-5"></a>

### 'string-copy!'/5 ###


<pre><code>
'string-copy!'(To::<a href="#type-scm_bytevector">scm_bytevector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string-copy-1"></a>

### 'string-copy'/1 ###


<pre><code>
'string-copy'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="string-copy-2"></a>

### 'string-copy'/2 ###


<pre><code>
'string-copy'(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="string-copy-3"></a>

### 'string-copy'/3 ###


<pre><code>
'string-copy'(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="string-fill%21-2"></a>

### 'string-fill!'/2 ###


<pre><code>
'string-fill!'(S::<a href="#type-scm_string">scm_string()</a>, Fill::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string-fill%21-3"></a>

### 'string-fill!'/3 ###


<pre><code>
'string-fill!'(S::<a href="#type-scm_string">scm_string()</a>, Fill::<a href="#type-scm_char">scm_char()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string-fill%21-4"></a>

### 'string-fill!'/4 ###


<pre><code>
'string-fill!'(S::<a href="#type-scm_string">scm_string()</a>, Fill::<a href="#type-scm_char">scm_char()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string-length-1"></a>

### 'string-length'/1 ###


<pre><code>
'string-length'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_k">scm_k()</a>
</code></pre>

<br></br>



<a name="string-ref-2"></a>

### 'string-ref'/2 ###


<pre><code>
'string-ref'(S::<a href="#type-scm_string">scm_string()</a>, K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_char">scm_char()</a>
</code></pre>

<br></br>



<a name="string-set%21-3"></a>

### 'string-set!'/3 ###


<pre><code>
'string-set!'(S::<a href="#type-scm_string">scm_string()</a>, K::<a href="#type-scm_k">scm_k()</a>, C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string%3c%3d%3f-1"></a>

### 'string<=?'/1 ###


<pre><code>
'string&lt;=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string%3c%3f-1"></a>

### 'string<?'/1 ###


<pre><code>
'string&lt;?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string%3d%3f-1"></a>

### 'string=?'/1 ###


<pre><code>
'string=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string%3e%3d%3f-1"></a>

### 'string>=?'/1 ###


<pre><code>
'string&gt;=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string%3e%3f-1"></a>

### 'string>?'/1 ###


<pre><code>
'string&gt;?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string%3f-1"></a>

### 'string?'/1 ###


<pre><code>
'string?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="imports-0"></a>

### imports/0 ###


<pre><code>
imports() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="string-1"></a>

### string/1 ###


<pre><code>
string(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="substring-3"></a>

### substring/3 ###


<pre><code>
substring(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_char">scm_char()</a>
</code></pre>

<br></br>



