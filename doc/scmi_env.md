

# Module scmi_env #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme interpreter environment resource</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="types"></a>

## Data Types ##




### <a name="type-env">env()</a> ###


__abstract datatype__: `env()`




### <a name="type-val">val()</a> ###



<pre><code>
val() = <a href="#type-scm_obj">scm_obj()</a>
</code></pre>





### <a name="type-var">var()</a> ###



<pre><code>
var() = <a href="#type-scm_symbol">scm_symbol()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#define_variable-3">define_variable/3</a></td><td></td></tr><tr><td valign="top"><a href="#extend-3">extend/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_immutable-1">is_immutable/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_resource-1">is_resource/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_variable-2">lookup_variable/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_immutable-1">make_immutable/1</a></td><td></td></tr><tr><td valign="top"><a href="#notify_when_destroyed-2">notify_when_destroyed/2</a></td><td></td></tr><tr><td valign="top"><a href="#notify_when_destroyed-3">notify_when_destroyed/3</a></td><td></td></tr><tr><td valign="top"><a href="#safe_lookup_variable-2">safe_lookup_variable/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_variable-3">set_variable/3</a></td><td></td></tr><tr><td valign="top"><a href="#the_empty-0">the_empty/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="define_variable-3"></a>

### define_variable/3 ###


<pre><code>
define_variable(Var::<a href="#type-var">var()</a>, Val::<a href="#type-val">val()</a>, X3::<a href="#type-env">env()</a>) -&gt; true
</code></pre>

<br></br>



<a name="extend-3"></a>

### extend/3 ###


<pre><code>
extend(Vars::[<a href="#type-var">var()</a>], Vals::[<a href="#type-val">val()</a>], X3::<a href="#type-env">env()</a>) -&gt; <a href="#type-val">val()</a>
</code></pre>

<br></br>



<a name="is_immutable-1"></a>

### is_immutable/1 ###


<pre><code>
is_immutable(X1::<a href="#type-env">env()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="is_resource-1"></a>

### is_resource/1 ###


<pre><code>
is_resource(Term::term()) -&gt; boolean()
</code></pre>

<br></br>



<a name="lookup_variable-2"></a>

### lookup_variable/2 ###


<pre><code>
lookup_variable(Var::<a href="#type-var">var()</a>, Env::<a href="#type-env">env()</a>) -&gt; <a href="#type-val">val()</a>
</code></pre>

<br></br>



<a name="make_immutable-1"></a>

### make_immutable/1 ###


<pre><code>
make_immutable(X1::<a href="#type-env">env()</a>) -&gt; true
</code></pre>

<br></br>



<a name="notify_when_destroyed-2"></a>

### notify_when_destroyed/2 ###


<pre><code>
notify_when_destroyed(Msg::term(), Env::<a href="#type-env">env()</a>) -&gt; true
</code></pre>

<br></br>



<a name="notify_when_destroyed-3"></a>

### notify_when_destroyed/3 ###


<pre><code>
notify_when_destroyed(Pid::pid(), Msg::term(), X3::<a href="#type-env">env()</a>) -&gt; true
</code></pre>

<br></br>



<a name="safe_lookup_variable-2"></a>

### safe_lookup_variable/2 ###


<pre><code>
safe_lookup_variable(Var::<a href="#type-var">var()</a>, X2::<a href="#type-env">env()</a>) -&gt; <a href="#type-val">val()</a>
</code></pre>

<br></br>



<a name="set_variable-3"></a>

### set_variable/3 ###


<pre><code>
set_variable(Var::<a href="#type-var">var()</a>, Val::<a href="#type-val">val()</a>, X3::<a href="#type-env">env()</a>) -&gt; true
</code></pre>

<br></br>



<a name="the_empty-0"></a>

### the_empty/0 ###


<pre><code>
the_empty() -&gt; <a href="#type-env">env()</a>
</code></pre>

<br></br>



