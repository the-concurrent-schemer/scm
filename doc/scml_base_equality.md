

# Module scml_base_equality #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme base library for equivalence predicates</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#eq%3f-2">'eq?'/2</a></td><td>Equivalent to <tt>'eqv?' / 2</tt>.</td></tr><tr><td valign="top"><a href="#equal%3f-2">'equal?'/2</a></td><td><p>Returns #t if obj1 and obj2 have the same display
representation.  Otherwise, #f.</p>.</td></tr><tr><td valign="top"><a href="#eqv%3f-2">'eqv?'/2</a></td><td><p>Returns #t if obj1 and obj2 are normally regarded as the same
object.  Otherwise, #f.</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###

<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>
<br />

<a name="eq%3f-2"></a>

### 'eq?'/2 ###

<pre><code>
'eq?'(O1::<a href="#type-scm_obj">scm_obj()</a>, O2::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

Equivalent to `'eqv?' / 2`.

<a name="equal%3f-2"></a>

### 'equal?'/2 ###

<pre><code>
'equal?'(Boolean::<a href="#type-scm_obj">scm_obj()</a>, O2::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if obj1 and obj2 have the same display
representation.  Otherwise, #f.</p>

<a name="eqv%3f-2"></a>

### 'eqv?'/2 ###

<pre><code>
'eqv?'(Boolean::<a href="#type-scm_obj">scm_obj()</a>, O2::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if obj1 and obj2 are normally regarded as the same
object.  Otherwise, #f.</p>

