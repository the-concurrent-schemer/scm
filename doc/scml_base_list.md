

# Module scml_base_list #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#list-copy-1">'list-copy'/1</a></td><td></td></tr><tr><td valign="top"><a href="#list-ref-2">'list-ref'/2</a></td><td></td></tr><tr><td valign="top"><a href="#list-set%21-3">'list-set!'/3</a></td><td></td></tr><tr><td valign="top"><a href="#list-tail-2">'list-tail'/2</a></td><td></td></tr><tr><td valign="top"><a href="#list%3f-1">'list?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-list-1">'make-list'/1</a></td><td></td></tr><tr><td valign="top"><a href="#make-list-2">'make-list'/2</a></td><td></td></tr><tr><td valign="top"><a href="#null%3f-1">'null?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#pair%3f-1">'pair?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#set-car%21-2">'set-car!'/2</a></td><td></td></tr><tr><td valign="top"><a href="#set-cdr%21-2">'set-cdr!'/2</a></td><td></td></tr><tr><td valign="top"><a href="#append-1">append/1</a></td><td></td></tr><tr><td valign="top"><a href="#assoc-2">assoc/2</a></td><td></td></tr><tr><td valign="top"><a href="#assoc-3">assoc/3</a></td><td></td></tr><tr><td valign="top"><a href="#assq-2">assq/2</a></td><td></td></tr><tr><td valign="top"><a href="#assv-2">assv/2</a></td><td></td></tr><tr><td valign="top"><a href="#caar-1">caar/1</a></td><td></td></tr><tr><td valign="top"><a href="#cadr-1">cadr/1</a></td><td></td></tr><tr><td valign="top"><a href="#car-1">car/1</a></td><td></td></tr><tr><td valign="top"><a href="#cdar-1">cdar/1</a></td><td></td></tr><tr><td valign="top"><a href="#cddr-1">cddr/1</a></td><td></td></tr><tr><td valign="top"><a href="#cdr-1">cdr/1</a></td><td></td></tr><tr><td valign="top"><a href="#cons-2">cons/2</a></td><td></td></tr><tr><td valign="top"><a href="#length-1">length/1</a></td><td></td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td></td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td></td></tr><tr><td valign="top"><a href="#member-3">member/3</a></td><td></td></tr><tr><td valign="top"><a href="#memq-2">memq/2</a></td><td></td></tr><tr><td valign="top"><a href="#memv-2">memv/2</a></td><td></td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="list-copy-1"></a>

### 'list-copy'/1 ###


<pre><code>
'list-copy'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="list-ref-2"></a>

### 'list-ref'/2 ###


<pre><code>
'list-ref'(List::<a href="#type-scm_list">scm_list()</a>, K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="list-set%21-3"></a>

### 'list-set!'/3 ###


<pre><code>
'list-set!'(List::<a href="#type-scm_list">scm_list()</a>, K::<a href="#type-scm_k">scm_k()</a>, Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="list-tail-2"></a>

### 'list-tail'/2 ###


<pre><code>
'list-tail'(List::<a href="#type-scm_list">scm_list()</a>, K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>

<br></br>



<a name="list%3f-1"></a>

### 'list?'/1 ###


<pre><code>
'list?'(T::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="make-list-1"></a>

### 'make-list'/1 ###


<pre><code>
'make-list'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>

<br></br>



<a name="make-list-2"></a>

### 'make-list'/2 ###


<pre><code>
'make-list'(K::<a href="#type-scm_k">scm_k()</a>, Fill::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>

<br></br>



<a name="null%3f-1"></a>

### 'null?'/1 ###


<pre><code>
'null?'(X1::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="pair%3f-1"></a>

### 'pair?'/1 ###


<pre><code>
'pair?'(X1::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="set-car%21-2"></a>

### 'set-car!'/2 ###


<pre><code>
'set-car!'(Pair::<a href="#type-scm_pair">scm_pair()</a>, Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="set-cdr%21-2"></a>

### 'set-cdr!'/2 ###


<pre><code>
'set-cdr!'(Pair::<a href="#type-scm_pair">scm_pair()</a>, Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="append-1"></a>

### append/1 ###


<pre><code>
append(Lists::<a href="#type-scmi_vargs">scmi_vargs()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>

<br></br>



<a name="assoc-2"></a>

### assoc/2 ###


<pre><code>
assoc(Obj::<a href="#type-scm_obj">scm_obj()</a>, Alist::<a href="#type-scm_alist">scm_alist()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="assoc-3"></a>

### assoc/3 ###


<pre><code>
assoc(Obj::<a href="#type-scm_obj">scm_obj()</a>, T::<a href="#type-scm_alist">scm_alist()</a>, Compare::<a href="#type-scm_proc">scm_proc()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="assq-2"></a>

### assq/2 ###


<pre><code>
assq(Obj::<a href="#type-scm_obj">scm_obj()</a>, Alist::<a href="#type-scm_alist">scm_alist()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="assv-2"></a>

### assv/2 ###


<pre><code>
assv(Obj::<a href="#type-scm_obj">scm_obj()</a>, Alist::<a href="#type-scm_alist">scm_alist()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="caar-1"></a>

### caar/1 ###


<pre><code>
caar(Pair::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="cadr-1"></a>

### cadr/1 ###


<pre><code>
cadr(Pair::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="car-1"></a>

### car/1 ###


<pre><code>
car(X1::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="cdar-1"></a>

### cdar/1 ###


<pre><code>
cdar(Pair::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="cddr-1"></a>

### cddr/1 ###


<pre><code>
cddr(Pair::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="cdr-1"></a>

### cdr/1 ###


<pre><code>
cdr(T::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="cons-2"></a>

### cons/2 ###


<pre><code>
cons(Obj1::<a href="#type-scm_obj">scm_obj()</a>, Obj2::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a>
</code></pre>

<br></br>



<a name="length-1"></a>

### length/1 ###


<pre><code>
length(List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_k">scm_k()</a>
</code></pre>

<br></br>



<a name="list-1"></a>

### list/1 ###


<pre><code>
list(List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>

<br></br>



<a name="member-2"></a>

### member/2 ###


<pre><code>
member(Obj::<a href="#type-scm_obj">scm_obj()</a>, List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="member-3"></a>

### member/3 ###


<pre><code>
member(Obj::<a href="#type-scm_obj">scm_obj()</a>, T::<a href="#type-scm_list">scm_list()</a>, Compare::<a href="#type-scm_proc">scm_proc()</a>) -&gt; <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="memq-2"></a>

### memq/2 ###


<pre><code>
memq(Obj::<a href="#type-scm_obj">scm_obj()</a>, List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="memv-2"></a>

### memv/2 ###


<pre><code>
memv(Obj::<a href="#type-scm_obj">scm_obj()</a>, List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="reverse-1"></a>

### reverse/1 ###


<pre><code>
reverse(List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>

<br></br>



