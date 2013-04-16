

# Module scml_base_control #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#call-with-current-continuation-1">'call-with-current-continuation'/1</a></td><td></td></tr><tr><td valign="top"><a href="#call-with-values-2">'call-with-values'/2</a></td><td></td></tr><tr><td valign="top"><a href="#dynamic-wind-3">'dynamic-wind'/3</a></td><td></td></tr><tr><td valign="top"><a href="#for-each-1">'for-each'/1</a></td><td></td></tr><tr><td valign="top"><a href="#procedure%3f-1">'procedure?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-for-each-1">'string-for-each'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-map-1">'string-map'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-for-each-1">'vector-for-each'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-map-1">'vector-map'/1</a></td><td></td></tr><tr><td valign="top"><a href="#apply-1">apply/1</a></td><td></td></tr><tr><td valign="top"><a href="#map-1">map/1</a></td><td></td></tr><tr><td valign="top"><a href="#values-1">values/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="call-with-current-continuation-1"></a>

### 'call-with-current-continuation'/1 ###


<pre><code>
'call-with-current-continuation'(Proc::<a href="#type-scm_proc">scm_proc()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>



<a name="call-with-values-2"></a>

### 'call-with-values'/2 ###


<pre><code>
'call-with-values'(Producer::<a href="#type-scm_thunk">scm_thunk()</a>, Consumer::<a href="#type-scm_proc">scm_proc()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>



<a name="dynamic-wind-3"></a>

### 'dynamic-wind'/3 ###


<pre><code>
'dynamic-wind'(Before::<a href="#type-scm_thunk">scm_thunk()</a>, Thunk::<a href="#type-scm_thunk">scm_thunk()</a>, After::<a href="#type-scm_thunk">scm_thunk()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>



<a name="for-each-1"></a>

### 'for-each'/1 ###


<pre><code>
'for-each'(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="procedure%3f-1"></a>

### 'procedure?'/1 ###


<pre><code>
'procedure?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string-for-each-1"></a>

### 'string-for-each'/1 ###


<pre><code>
'string-for-each'(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string-map-1"></a>

### 'string-map'/1 ###


<pre><code>
'string-map'(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="vector-for-each-1"></a>

### 'vector-for-each'/1 ###


<pre><code>
'vector-for-each'(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="vector-map-1"></a>

### 'vector-map'/1 ###


<pre><code>
'vector-map'(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; <a href="#type-scm_vector">scm_vector()</a>
</code></pre>

<br></br>



<a name="apply-1"></a>

### apply/1 ###


<pre><code>
apply(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>



<a name="map-1"></a>

### map/1 ###


<pre><code>
map(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; [<a href="#type-scm_any">scm_any()</a>]
</code></pre>

<br></br>



<a name="values-1"></a>

### values/1 ###


<pre><code>
values(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>



