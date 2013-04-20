

# Module scml_base_control #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#call-with-values-5">'call-with-values'/5</a></td><td><p>Calls its producer argument with no values and a continuation
that, when passed some values, calls the consumer procedure with
those values as arguments. The continuation for the call to
consumer is the continuation of the call to <code>call-with-values</code>.</p>.</td></tr><tr><td valign="top"><a href="#call%2fcc-4">'call/cc'/4</a></td><td><p>Packages the current continuation as an "escape procedure" and
passes it as an argument to <code>Proc</code>. The escape procedure is a
Scheme procedure that, if it is later called, will abandon whatever
continuation is in effect at that later time and will instead use
the continuation that was in effect when the escape procedure was
created. Calling the escape procedure will cause the invocation of
before and after thunks installed using <code>dynamic-wind</code>.  @equiv
<em>call-with-current-continuation</em></p>.</td></tr><tr><td valign="top"><a href="#dynamic-wind-6">'dynamic-wind'/6</a></td><td><p>Calls <code>Thunk</code> without arguments, returning the result(s) of
this call. <code>Before</code> and <code>After</code> are called, also without arguments,
as required.  Note that, in the absence of calls to continuations
captured using <code>call/cc</code>, the three arguments are called once each,
in order.<code>Before</code> is called whenever execution enters the dynamic
extent of the call to <code>Thunk</code> and <code>After</code> is called whenever it
exits that dynamic extent.  The <code>Before</code> and <code>After</code> thunks are
called in the same dynamic environment as the call to
<code>dynamic-wind</code>.</p>


<pre><code>No special handling is given for escapes that might occur inside
the +Before+ and +After+ thunks. It protects the +Thunk+ by its
continuation that enforces the following three rules.</code></pre>

<ol class="arabic">
<li>
<p>
Its normal continuation is for <code>Before</code> to be called before
<code>Thunk</code>, which is called before <code>After</code>, and finally to return
the value of the evaluation of <code>Thunk</code> as the value of the
entire dynamic-wind expression.
</p>
</li>
<li>
<p>
If an escape is made out of the <code>Thunk</code>, <code>dynamic-wind</code>
guarantees that the <code>After</code> will be called before the escape
occurs.
</p>
</li>
<li>
<p>
If an escape is made into the <code>Thunk</code>, it guarantees that the
<code>Before</code> will be called before control is returned to the place
of initial escape in the <code>Thunk</code>, and finally the <code>After</code> is
called.
</p>
</li>
</ol>.</td></tr><tr><td valign="top"><a href="#for-each-1">'for-each'/1</a></td><td></td></tr><tr><td valign="top"><a href="#procedure%3f-1">'procedure?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-for-each-1">'string-for-each'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-map-1">'string-map'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-for-each-1">'vector-for-each'/1</a></td><td></td></tr><tr><td valign="top"><a href="#vector-map-1">'vector-map'/1</a></td><td></td></tr><tr><td valign="top"><a href="#apply-4">apply/4</a></td><td><p>Calls <code>Proc</code> with the elements of the list <code>(append (list arg1
[8230,41,32,97,114,103,115,41]</code> as the actual arguments.</p>.</td></tr><tr><td valign="top"><a href="#map-1">map/1</a></td><td></td></tr><tr><td valign="top"><a href="#values-4">values/4</a></td><td><p>Delivers all of it[8217,115,32,97,114,103,117,109,101,110,116,115,32,116,111,32,105,116,115,32,
 99,111,110,116,105,110,117,97,116,105,111,110,46]</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="call-with-values-5"></a>

### 'call-with-values'/5 ###


<pre><code>
'call-with-values'(Producer::<a href="#type-scm_thunk">scm_thunk()</a>, Consumer::<a href="#type-scm_proc">scm_proc()</a>, Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>


<p>Calls its producer argument with no values and a continuation
that, when passed some values, calls the consumer procedure with
those values as arguments. The continuation for the call to
consumer is the continuation of the call to <code>call-with-values</code>.</p>

<a name="call%2fcc-4"></a>

### 'call/cc'/4 ###


<pre><code>
'call/cc'(Proc::<a href="#type-scm_proc">scm_proc()</a>, Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>


<p>Packages the current continuation as an "escape procedure" and
passes it as an argument to <code>Proc</code>. The escape procedure is a
Scheme procedure that, if it is later called, will abandon whatever
continuation is in effect at that later time and will instead use
the continuation that was in effect when the escape procedure was
created. Calling the escape procedure will cause the invocation of
before and after thunks installed using <code>dynamic-wind</code>.  @equiv
<em>call-with-current-continuation</em></p>

<a name="dynamic-wind-6"></a>

### 'dynamic-wind'/6 ###


<pre><code>
'dynamic-wind'(Before::<a href="#type-scm_thunk">scm_thunk()</a>, Thunk::<a href="#type-scm_thunk">scm_thunk()</a>, After::<a href="#type-scm_thunk">scm_thunk()</a>, Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>


<p>Calls <code>Thunk</code> without arguments, returning the result(s) of
this call. <code>Before</code> and <code>After</code> are called, also without arguments,
as required.  Note that, in the absence of calls to continuations
captured using <code>call/cc</code>, the three arguments are called once each,
in order.<code>Before</code> is called whenever execution enters the dynamic
extent of the call to <code>Thunk</code> and <code>After</code> is called whenever it
exits that dynamic extent.  The <code>Before</code> and <code>After</code> thunks are
called in the same dynamic environment as the call to
<code>dynamic-wind</code>.</p>


<pre><code>No special handling is given for escapes that might occur inside
the +Before+ and +After+ thunks. It protects the +Thunk+ by its
continuation that enforces the following three rules.</code></pre>

<ol class="arabic">
<li>
<p>
Its normal continuation is for <code>Before</code> to be called before
<code>Thunk</code>, which is called before <code>After</code>, and finally to return
the value of the evaluation of <code>Thunk</code> as the value of the
entire dynamic-wind expression.
</p>
</li>
<li>
<p>
If an escape is made out of the <code>Thunk</code>, <code>dynamic-wind</code>
guarantees that the <code>After</code> will be called before the escape
occurs.
</p>
</li>
<li>
<p>
If an escape is made into the <code>Thunk</code>, it guarantees that the
<code>Before</code> will be called before control is returned to the place
of initial escape in the <code>Thunk</code>, and finally the <code>After</code> is
called.
</p>
</li>
</ol>

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



<a name="apply-4"></a>

### apply/4 ###


<pre><code>
apply(Arg::[<a href="#type-scm_any">scm_any()</a>, ...], Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>


<p>Calls <code>Proc</code> with the elements of the list <code>(append (list arg1
[8230,41,32,97,114,103,115,41]</code> as the actual arguments.</p>

<a name="map-1"></a>

### map/1 ###


<pre><code>
map(Args::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; [<a href="#type-scm_any">scm_any()</a>]
</code></pre>

<br></br>



<a name="values-4"></a>

### values/4 ###


<pre><code>
values(Args::[<a href="#type-scm_any">scm_any()</a>, ...], Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>


<p>Delivers all of it[8217,115,32,97,114,103,117,109,101,110,116,115,32,116,111,32,105,116,115,32,
 99,111,110,116,105,110,117,97,116,105,111,110,46]</p>

