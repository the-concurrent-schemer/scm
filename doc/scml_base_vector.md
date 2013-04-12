

# Module scml_base_vector #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#list-%3evector-1">'list->vector'/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-vector-1">'make-vector'/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-vector-2">'make-vector'/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-%3evector-1">'string->vector'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-%3evector-2">'string->vector'/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-%3evector-3">'string->vector'/3</a></td><td></td></tr><tr><td valign="top"><a href="#vector-%3elist-1">'vector->list'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-%3elist-2">'vector->list'/2</a></td><td></td></tr><tr><td valign="top"><a href="#vector-%3elist-3">'vector->list'/3</a></td><td></td></tr><tr><td valign="top"><a href="#vector-%3estring-1">'vector->string'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-%3estring-2">'vector->string'/2</a></td><td></td></tr><tr><td valign="top"><a href="#vector-%3estring-3">'vector->string'/3</a></td><td></td></tr><tr><td valign="top"><a href="#vector-append-1">'vector-append'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-copy%21-3">'vector-copy!'/3</a></td><td></td></tr><tr><td valign="top"><a href="#vector-copy%21-4">'vector-copy!'/4</a></td><td></td></tr><tr><td valign="top"><a href="#vector-copy%21-5">'vector-copy!'/5</a></td><td></td></tr><tr><td valign="top"><a href="#vector-copy-1">'vector-copy'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-copy-2">'vector-copy'/2</a></td><td></td></tr><tr><td valign="top"><a href="#vector-copy-3">'vector-copy'/3</a></td><td></td></tr><tr><td valign="top"><a href="#vector-fill%21-2">'vector-fill!'/2</a></td><td></td></tr><tr><td valign="top"><a href="#vector-fill%21-3">'vector-fill!'/3</a></td><td></td></tr><tr><td valign="top"><a href="#vector-fill%21-4">'vector-fill!'/4</a></td><td></td></tr><tr><td valign="top"><a href="#vector-length-1">'vector-length'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-ref-2">'vector-ref'/2</a></td><td></td></tr><tr><td valign="top"><a href="#vector-set%21-3">'vector-set!'/3</a></td><td></td></tr><tr><td valign="top"><a href="#vector%3f-1">'vector?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#imports-0">imports/0</a></td><td></td></tr><tr><td valign="top"><a href="#vector-1">vector/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="list-%3evector-1"></a>

### 'list->vector'/1 ###


<pre><code>
'list-&gt;vector'(Objs::[<a href="#type-scm_obj">scm_obj()</a>]) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="make-vector-1"></a>

### 'make-vector'/1 ###


<pre><code>
'make-vector'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="make-vector-2"></a>

### 'make-vector'/2 ###


<pre><code>
'make-vector'(K::<a href="#type-scm_k">scm_k()</a>, Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="string-%3evector-1"></a>

### 'string->vector'/1 ###


<pre><code>
'string-&gt;vector'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="string-%3evector-2"></a>

### 'string->vector'/2 ###


<pre><code>
'string-&gt;vector'(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="string-%3evector-3"></a>

### 'string->vector'/3 ###


<pre><code>
'string-&gt;vector'(S::<a href="#type-scm_string">scm_string()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="vector-%3elist-1"></a>

### 'vector->list'/1 ###


<pre><code>
'vector-&gt;list'(V::<a href="#type-scm_vector">scm_vector()</a>) -&gt; [<a href="#type-scm_obj">scm_obj()</a>]
</code></pre>

<br></br>



<a name="vector-%3elist-2"></a>

### 'vector->list'/2 ###


<pre><code>
'vector-&gt;list'(V::<a href="#type-scm_vector">scm_vector()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; [<a href="#type-scm_obj">scm_obj()</a>]
</code></pre>

<br></br>



<a name="vector-%3elist-3"></a>

### 'vector->list'/3 ###


<pre><code>
'vector-&gt;list'(V::<a href="#type-scm_vector">scm_vector()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; [<a href="#type-scm_obj">scm_obj()</a>]
</code></pre>

<br></br>



<a name="vector-%3estring-1"></a>

### 'vector->string'/1 ###


<pre><code>
'vector-&gt;string'(V::<a href="#type-scm_vector">scm_vector()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="vector-%3estring-2"></a>

### 'vector->string'/2 ###


<pre><code>
'vector-&gt;string'(V::<a href="#type-scm_vector">scm_vector()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="vector-%3estring-3"></a>

### 'vector->string'/3 ###


<pre><code>
'vector-&gt;string'(V::<a href="#type-scm_vector">scm_vector()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="vector-append-1"></a>

### 'vector-append'/1 ###


<pre><code>
'vector-append'(Vs::[<a href="#type-scm_vector">scm_vector()</a>, ...]) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="vector-copy%21-3"></a>

### 'vector-copy!'/3 ###


<pre><code>
'vector-copy!'(To::<a href="#type-scm_vector">scm_vector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_vector">scm_vector()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="vector-copy%21-4"></a>

### 'vector-copy!'/4 ###


<pre><code>
'vector-copy!'(To::<a href="#type-scm_vector">scm_vector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_vector">scm_vector()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="vector-copy%21-5"></a>

### 'vector-copy!'/5 ###


<pre><code>
'vector-copy!'(To::<a href="#type-scm_vector">scm_vector()</a>, At::<a href="#type-scm_k">scm_k()</a>, From::<a href="#type-scm_vector">scm_vector()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="vector-copy-1"></a>

### 'vector-copy'/1 ###


<pre><code>
'vector-copy'(V::<a href="#type-scm_vector">scm_vector()</a>) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="vector-copy-2"></a>

### 'vector-copy'/2 ###


<pre><code>
'vector-copy'(V::<a href="#type-scm_vector">scm_vector()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="vector-copy-3"></a>

### 'vector-copy'/3 ###


<pre><code>
'vector-copy'(V::<a href="#type-scm_vector">scm_vector()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="vector-fill%21-2"></a>

### 'vector-fill!'/2 ###


<pre><code>
'vector-fill!'(V::<a href="#type-scm_vector">scm_vector()</a>, Fill::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="vector-fill%21-3"></a>

### 'vector-fill!'/3 ###


<pre><code>
'vector-fill!'(V::<a href="#type-scm_vector">scm_vector()</a>, Fill::<a href="#type-scm_obj">scm_obj()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="vector-fill%21-4"></a>

### 'vector-fill!'/4 ###


<pre><code>
'vector-fill!'(V::<a href="#type-scm_vector">scm_vector()</a>, Fill::<a href="#type-scm_obj">scm_obj()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="vector-length-1"></a>

### 'vector-length'/1 ###


<pre><code>
'vector-length'(V::<a href="#type-scm_vector">scm_vector()</a>) -&gt; <a href="#type-scm_k">scm_k()</a>
</code></pre>

<br></br>



<a name="vector-ref-2"></a>

### 'vector-ref'/2 ###


<pre><code>
'vector-ref'(V::<a href="#type-scm_vector">scm_vector()</a>, K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="vector-set%21-3"></a>

### 'vector-set!'/3 ###


<pre><code>
'vector-set!'(V::<a href="#type-scm_vector">scm_vector()</a>, K::<a href="#type-scm_k">scm_k()</a>, Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="vector%3f-1"></a>

### 'vector?'/1 ###


<pre><code>
'vector?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="imports-0"></a>

### imports/0 ###


<pre><code>
imports() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="vector-1"></a>

### vector/1 ###


<pre><code>
vector(Objs::[<a href="#type-scm_obj">scm_obj()</a>, ...]) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



