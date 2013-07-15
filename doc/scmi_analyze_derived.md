

# Module scmi_analyze_derived #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme interpreter syntactic analyzer for derived expressions</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scmi_exports-0">'$scmi_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_and-2">analyze_and/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_begin-2">analyze_begin/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_case-2">analyze_case/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_cond-2">analyze_cond/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_cond_expand-2">analyze_cond_expand/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_do-2">analyze_do/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_guard-2">analyze_guard/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_let-2">analyze_let/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_let_values-2">analyze_let_values/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_letrec-2">analyze_letrec/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_letrec_values-2">analyze_letrec_values/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_letrecs-2">analyze_letrecs/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_lets-2">analyze_lets/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_lets_values-2">analyze_lets_values/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_make_parameter-2">analyze_make_parameter/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_or-2">analyze_or/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_parameterize-2">analyze_parameterize/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_quasiquote-2">analyze_quasiquote/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_unless-2">analyze_unless/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_unquote-2">analyze_unquote/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_unquote_splicing-2">analyze_unquote_splicing/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_when-2">analyze_when/2</a></td><td></td></tr><tr><td valign="top"><a href="#scan_out_internal_definitions-1">scan_out_internal_definitions/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scmi_exports-0"></a>

### '$scmi_exports'/0 ###


<pre><code>
'$scmi_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_sugar">scmi_sugar()</a>}]
</code></pre>

<br></br>



<a name="analyze_and-2"></a>

### analyze_and/2 ###


<pre><code>
analyze_and(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_begin-2"></a>

### analyze_begin/2 ###


<pre><code>
analyze_begin(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_case-2"></a>

### analyze_case/2 ###


<pre><code>
analyze_case(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_cond-2"></a>

### analyze_cond/2 ###


<pre><code>
analyze_cond(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_cond_expand-2"></a>

### analyze_cond_expand/2 ###


<pre><code>
analyze_cond_expand(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_do-2"></a>

### analyze_do/2 ###


<pre><code>
analyze_do(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_guard-2"></a>

### analyze_guard/2 ###


<pre><code>
analyze_guard(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_let-2"></a>

### analyze_let/2 ###


<pre><code>
analyze_let(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_let_values-2"></a>

### analyze_let_values/2 ###


<pre><code>
analyze_let_values(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_letrec-2"></a>

### analyze_letrec/2 ###


<pre><code>
analyze_letrec(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_letrec_values-2"></a>

### analyze_letrec_values/2 ###


<pre><code>
analyze_letrec_values(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_letrecs-2"></a>

### analyze_letrecs/2 ###


<pre><code>
analyze_letrecs(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_lets-2"></a>

### analyze_lets/2 ###


<pre><code>
analyze_lets(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_lets_values-2"></a>

### analyze_lets_values/2 ###


<pre><code>
analyze_lets_values(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_make_parameter-2"></a>

### analyze_make_parameter/2 ###


<pre><code>
analyze_make_parameter(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_or-2"></a>

### analyze_or/2 ###


<pre><code>
analyze_or(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_parameterize-2"></a>

### analyze_parameterize/2 ###


<pre><code>
analyze_parameterize(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_quasiquote-2"></a>

### analyze_quasiquote/2 ###


<pre><code>
analyze_quasiquote(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_unless-2"></a>

### analyze_unless/2 ###


<pre><code>
analyze_unless(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_unquote-2"></a>

### analyze_unquote/2 ###


<pre><code>
analyze_unquote(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_unquote_splicing-2"></a>

### analyze_unquote_splicing/2 ###


<pre><code>
analyze_unquote_splicing(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="analyze_when-2"></a>

### analyze_when/2 ###


<pre><code>
analyze_when(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scmi_exec">scmi_exec()</a>
</code></pre>

<br></br>



<a name="scan_out_internal_definitions-1"></a>

### scan_out_internal_definitions/1 ###


<pre><code>
scan_out_internal_definitions(Body::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; [<a href="#type-scm_any">scm_any()</a>]
</code></pre>

<br></br>



