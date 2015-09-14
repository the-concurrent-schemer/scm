

# Module scml_base_list #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme base library for pairs and lists</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#list-copy-1">'list-copy'/1</a></td><td>Equivalent to <a href="#list-1"><tt>list(obj)</tt></a>.</td></tr><tr><td valign="top"><a href="#list-ref-2">'list-ref'/2</a></td><td><p>Returns the kth element of list.  It is an error if list has
fewer than k elements.</p>.</td></tr><tr><td valign="top"><a href="#list-set%21-3">'list-set!'/3</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#list-tail-2">'list-tail'/2</a></td><td><p>Returns the sublist of list obtained by omitting the first k
elements.  It is an error if list has fewer than k elements.</p>.</td></tr><tr><td valign="top"><a href="#list%3f-1">'list?'/1</a></td><td><p>Returns #t if obj is a (proper) list, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#make-list-1">'make-list'/1</a></td><td>Equivalent to <a href="#make-list-2"><tt>'make-list'(K, '#f')</tt></a>.</td></tr><tr><td valign="top"><a href="#make-list-2">'make-list'/2</a></td><td><p>Returns a list of k elements.</p>.</td></tr><tr><td valign="top"><a href="#null%3f-1">'null?'/1</a></td><td><p>Returns #t if obj is the empty list, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#pair%3f-1">'pair?'/1</a></td><td><p>Returns #t if obj is a pair, and otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#set-car%21-2">'set-car!'/2</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#set-cdr%21-2">'set-cdr!'/2</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#append-1">append/1</a></td><td><p>Returns a list consisting of the elements of the first list
followed by the elements of the other lists.  If there are no
arguments, the empty list is returned.  If there is exactly one
argument, it is returned.  An improper list results if the last
argument is not a proper list.  The last argument, if there is one,
can be of any type.</p>.</td></tr><tr><td valign="top"><a href="#assoc-5">assoc/5</a></td><td>Equivalent to <a href="#assoc-3"><tt>assoc(Obj, List, 'equal?')</tt></a>.</td></tr><tr><td valign="top"><a href="#assoc-6">assoc/6</a></td><td><p>Return the first pair of alist whose car field is obj.  If no
pair in alist has obj as its car, then #f is returned.</p>.</td></tr><tr><td valign="top"><a href="#assq-5">assq/5</a></td><td>Equivalent to <a href="#assq-3"><tt>assq(Obj, List, 'eq?')</tt></a>.</td></tr><tr><td valign="top"><a href="#assv-5">assv/5</a></td><td>Equivalent to <a href="#assv-3"><tt>assv(Obj, List, 'eqv?')</tt></a>.</td></tr><tr><td valign="top"><a href="#caar-1">caar/1</a></td><td><p>Returns the composition of car.</p>.</td></tr><tr><td valign="top"><a href="#cadr-1">cadr/1</a></td><td><p>Returns the composition of car and cdr.</p>.</td></tr><tr><td valign="top"><a href="#car-1">car/1</a></td><td><p>Returns the contents of the car field of pair.  Note that it
is an error to take the car of the empty list.</p>.</td></tr><tr><td valign="top"><a href="#cdar-1">cdar/1</a></td><td><p>Returns the composition of cdr and car.</p>.</td></tr><tr><td valign="top"><a href="#cddr-1">cddr/1</a></td><td><p>Returns the composition of cdr and cdr.</p>.</td></tr><tr><td valign="top"><a href="#cdr-1">cdr/1</a></td><td><p>Returns the contents of the cdr field of pair.  Note that it
is an error to take the car of the empty list.</p>.</td></tr><tr><td valign="top"><a href="#cons-2">cons/2</a></td><td><p>Returns a pair whose car is obj1 and whose cdr is obj2.  The
pair constructed by <code>cons</code> is not guaranteed to be different (in
the sense of <code>eqv?</code>) from every existing object.</p>.</td></tr><tr><td valign="top"><a href="#length-1">length/1</a></td><td><p>Returns the length of list.</p>.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td><p>Returns a list of the arguments.</p>.</td></tr><tr><td valign="top"><a href="#member-5">member/5</a></td><td>Equivalent to <a href="#memq-3"><tt>memq(Obj, List, 'equal?')</tt></a>.</td></tr><tr><td valign="top"><a href="#member-6">member/6</a></td><td><p>Return the first sublist of list whose car is obj where the
sublists of list are the non-empty lists returned by (list-tail
list k) for k less than the length of list.  If obj does not occur
in the list, the #f is returned.</p>.</td></tr><tr><td valign="top"><a href="#memq-5">memq/5</a></td><td>Equivalent to <a href="#memq-3"><tt>memq(Obj, List, 'eq?')</tt></a>.</td></tr><tr><td valign="top"><a href="#memv-5">memv/5</a></td><td>Equivalent to <a href="#memq-3"><tt>memq(Obj, List, 'eqv?')</tt></a>.</td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td><p>Returns a list consisting of the elements of list in reverse
order.</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###

<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>
<br />

<a name="list-copy-1"></a>

### 'list-copy'/1 ###

<pre><code>
'list-copy'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

Equivalent to [`list(obj)`](#list-1).

<a name="list-ref-2"></a>

### 'list-ref'/2 ###

<pre><code>
'list-ref'(List::<a href="#type-scm_list">scm_list()</a>, K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<p>Returns the kth element of list.  It is an error if list has
fewer than k elements.</p>

<a name="list-set%21-3"></a>

### 'list-set!'/3 ###

<pre><code>
'list-set!'(List::<a href="#type-scm_list">scm_list()</a>, K::<a href="#type-scm_k">scm_k()</a>, Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p><em>unsupported</em></p>

<a name="list-tail-2"></a>

### 'list-tail'/2 ###

<pre><code>
'list-tail'(List::<a href="#type-scm_list">scm_list()</a>, K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>
<br />

<p>Returns the sublist of list obtained by omitting the first k
elements.  It is an error if list has fewer than k elements.</p>

<a name="list%3f-1"></a>

### 'list?'/1 ###

<pre><code>
'list?'(T::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if obj is a (proper) list, otherwise returns #f.</p>

<a name="make-list-1"></a>

### 'make-list'/1 ###

<pre><code>
'make-list'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>
<br />

Equivalent to [`'make-list'(K, '#f')`](#make-list-2).

<a name="make-list-2"></a>

### 'make-list'/2 ###

<pre><code>
'make-list'(K::<a href="#type-scm_k">scm_k()</a>, Fill::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>
<br />

<p>Returns a list of k elements.</p>

<a name="null%3f-1"></a>

### 'null?'/1 ###

<pre><code>
'null?'(X1::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if obj is the empty list, otherwise returns #f.</p>

<a name="pair%3f-1"></a>

### 'pair?'/1 ###

<pre><code>
'pair?'(X1::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if obj is a pair, and otherwise returns #f.</p>

<a name="set-car%21-2"></a>

### 'set-car!'/2 ###

<pre><code>
'set-car!'(Pair::<a href="#type-scm_pair">scm_pair()</a>, Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p><em>unsupported</em></p>

<a name="set-cdr%21-2"></a>

### 'set-cdr!'/2 ###

<pre><code>
'set-cdr!'(Pair::<a href="#type-scm_pair">scm_pair()</a>, Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p><em>unsupported</em></p>

<a name="append-1"></a>

### append/1 ###

<pre><code>
append(Lists::<a href="#type-scmi_vargs">scmi_vargs()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>
<br />

<p>Returns a list consisting of the elements of the first list
followed by the elements of the other lists.  If there are no
arguments, the empty list is returned.  If there is exactly one
argument, it is returned.  An improper list results if the last
argument is not a proper list.  The last argument, if there is one,
can be of any type.</p>

<a name="assoc-5"></a>

### assoc/5 ###

<pre><code>
assoc(Obj::<a href="#type-scm_obj">scm_obj()</a>, Alist::<a href="#type-scm_alist">scm_alist()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

Equivalent to [`assoc(Obj, List, 'equal?')`](#assoc-3).

<a name="assoc-6"></a>

### assoc/6 ###

<pre><code>
assoc(Obj::<a href="#type-scm_obj">scm_obj()</a>, T::<a href="#type-scm_alist">scm_alist()</a>, Compare::<a href="#type-scm_proc">scm_proc()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p>Return the first pair of alist whose car field is obj.  If no
pair in alist has obj as its car, then #f is returned.</p>

<a name="assq-5"></a>

### assq/5 ###

<pre><code>
assq(Obj::<a href="#type-scm_obj">scm_obj()</a>, Alist::<a href="#type-scm_alist">scm_alist()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

Equivalent to [`assq(Obj, List, 'eq?')`](#assq-3).

<a name="assv-5"></a>

### assv/5 ###

<pre><code>
assv(Obj::<a href="#type-scm_obj">scm_obj()</a>, Alist::<a href="#type-scm_alist">scm_alist()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

Equivalent to [`assv(Obj, List, 'eqv?')`](#assv-3).

<a name="caar-1"></a>

### caar/1 ###

<pre><code>
caar(Pair::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<p>Returns the composition of car.</p>

<a name="cadr-1"></a>

### cadr/1 ###

<pre><code>
cadr(Pair::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<p>Returns the composition of car and cdr.</p>

<a name="car-1"></a>

### car/1 ###

<pre><code>
car(X1::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<p>Returns the contents of the car field of pair.  Note that it
is an error to take the car of the empty list.</p>

<a name="cdar-1"></a>

### cdar/1 ###

<pre><code>
cdar(Pair::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<p>Returns the composition of cdr and car.</p>

<a name="cddr-1"></a>

### cddr/1 ###

<pre><code>
cddr(Pair::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<p>Returns the composition of cdr and cdr.</p>

<a name="cdr-1"></a>

### cdr/1 ###

<pre><code>
cdr(T::<a href="#type-scm_pair">scm_pair()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<p>Returns the contents of the cdr field of pair.  Note that it
is an error to take the car of the empty list.</p>

<a name="cons-2"></a>

### cons/2 ###

<pre><code>
cons(Obj1::<a href="#type-scm_obj">scm_obj()</a>, Obj2::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_pair">scm_pair()</a>
</code></pre>
<br />

<p>Returns a pair whose car is obj1 and whose cdr is obj2.  The
pair constructed by <code>cons</code> is not guaranteed to be different (in
the sense of <code>eqv?</code>) from every existing object.</p>

<a name="length-1"></a>

### length/1 ###

<pre><code>
length(List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_k">scm_k()</a>
</code></pre>
<br />

<p>Returns the length of list.</p>

<a name="list-1"></a>

### list/1 ###

<pre><code>
list(List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>
<br />

<p>Returns a list of the arguments.</p>

<a name="member-5"></a>

### member/5 ###

<pre><code>
member(Obj::<a href="#type-scm_obj">scm_obj()</a>, List::<a href="#type-scm_list">scm_list()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

Equivalent to [`memq(Obj, List, 'equal?')`](#memq-3).

<a name="member-6"></a>

### member/6 ###

<pre><code>
member(Obj::<a href="#type-scm_obj">scm_obj()</a>, T::<a href="#type-scm_list">scm_list()</a>, Compare::<a href="#type-scm_proc">scm_proc()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<p>Return the first sublist of list whose car is obj where the
sublists of list are the non-empty lists returned by (list-tail
list k) for k less than the length of list.  If obj does not occur
in the list, the #f is returned.</p>

<a name="memq-5"></a>

### memq/5 ###

<pre><code>
memq(Obj::<a href="#type-scm_obj">scm_obj()</a>, List::<a href="#type-scm_list">scm_list()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

Equivalent to [`memq(Obj, List, 'eq?')`](#memq-3).

<a name="memv-5"></a>

### memv/5 ###

<pre><code>
memv(Obj::<a href="#type-scm_obj">scm_obj()</a>, List::<a href="#type-scm_list">scm_list()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

Equivalent to [`memq(Obj, List, 'eqv?')`](#memq-3).

<a name="reverse-1"></a>

### reverse/1 ###

<pre><code>
reverse(List::<a href="#type-scm_list">scm_list()</a>) -&gt; <a href="#type-scm_list">scm_list()</a>
</code></pre>
<br />

<p>Returns a list consisting of the elements of list in reverse
order.</p>

