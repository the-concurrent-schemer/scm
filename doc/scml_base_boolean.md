

# Module scml_base_boolean #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme base library for booleans</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#boolean%3d%3f-1">'boolean=?'/1</a></td><td><p>Returns #t if all the arguments are booleans and all are #t or
all are #f.</p>.</td></tr><tr><td valign="top"><a href="#boolean%3f-1">'boolean?'/1</a></td><td><p>Returns #t if obj is either #t or #f, and returns #f
otherwise.</p>.</td></tr><tr><td valign="top"><a href="#not-1">'not'/1</a></td><td><p>Returns #t if obj is false, and returns #f otherwise.</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="boolean%3d%3f-1"></a>

### 'boolean=?'/1 ###


<pre><code>
'boolean=?'(Bs::[<a href="#type-scm_boolean">scm_boolean()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if all the arguments are booleans and all are #t or
all are #f.</p>

<a name="boolean%3f-1"></a>

### 'boolean?'/1 ###


<pre><code>
'boolean?'(X1::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is either #t or #f, and returns #f
otherwise.</p>

<a name="not-1"></a>

### 'not'/1 ###


<pre><code>
'not'(X1::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is false, and returns #f otherwise.</p>

