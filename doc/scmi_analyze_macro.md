

# Module scmi_analyze_macro #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme interpreter syntactic analyzer for syntax expressions</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scmi_exports-0">'$scmi_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_assignment_syntax-2">analyze_assignment_syntax/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_lambda_syntax-2">analyze_lambda_syntax/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_let_syntax-2">analyze_let_syntax/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_letrec_syntax-2">analyze_letrec_syntax/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_letrecs_syntax-2">analyze_letrecs_syntax/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_lets_syntax-2">analyze_lets_syntax/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_sequence_syntax-2">analyze_sequence_syntax/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_syntax_error-2">analyze_syntax_error/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_syntax_rules-2">analyze_syntax_rules/2</a></td><td></td></tr><tr><td valign="top"><a href="#scan_out_internal_definitions-2">scan_out_internal_definitions/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scmi_exports-0"></a>

### '$scmi_exports'/0 ###

<pre><code>
'$scmi_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_expander">scmi_expander()</a>}]
</code></pre>
<br />

<a name="analyze_assignment_syntax-2"></a>

### analyze_assignment_syntax/2 ###

<pre><code>
analyze_assignment_syntax(Exp0::<a href="#type-scmi_exp">scmi_exp()</a>, Senv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_lambda_syntax-2"></a>

### analyze_lambda_syntax/2 ###

<pre><code>
analyze_lambda_syntax(Body::<a href="#type-scmi_exp">scmi_exp()</a>, Senv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_expander">scmi_expander()</a>
</code></pre>
<br />

<a name="analyze_let_syntax-2"></a>

### analyze_let_syntax/2 ###

<pre><code>
analyze_let_syntax(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_letrec_syntax-2"></a>

### analyze_letrec_syntax/2 ###

<pre><code>
analyze_letrec_syntax(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_letrecs_syntax-2"></a>

### analyze_letrecs_syntax/2 ###

<pre><code>
analyze_letrecs_syntax(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_lets_syntax-2"></a>

### analyze_lets_syntax/2 ###

<pre><code>
analyze_lets_syntax(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_sequence_syntax-2"></a>

### analyze_sequence_syntax/2 ###

<pre><code>
analyze_sequence_syntax(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_syntax_error-2"></a>

### analyze_syntax_error/2 ###

<pre><code>
analyze_syntax_error(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; no_return()
</code></pre>
<br />

<a name="analyze_syntax_rules-2"></a>

### analyze_syntax_rules/2 ###

<pre><code>
analyze_syntax_rules(Rules::<a href="#type-scmi_exp">scmi_exp()</a>, Senv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_expander">scmi_expander()</a>
</code></pre>
<br />

<a name="scan_out_internal_definitions-2"></a>

### scan_out_internal_definitions/2 ###

<pre><code>
scan_out_internal_definitions(Body::[<a href="#type-scmi_exp">scmi_exp()</a>, ...], Senv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; [<a href="#type-scmi_exp">scmi_exp()</a>]
</code></pre>
<br />

