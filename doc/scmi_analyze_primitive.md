

# Module scmi_analyze_primitive #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme interpreter syntactic analyzer for primitive expressions</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scmi_exports-0">'$scmi_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_application-2">analyze_application/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_assignment-2">analyze_assignment/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_if-2">analyze_if/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_include-2">analyze_include/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_include_ci-2">analyze_include_ci/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_include_lib-2">analyze_include_lib/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_include_lib_ci-2">analyze_include_lib_ci/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_lambda-2">analyze_lambda/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_proc_application-3">analyze_proc_application/3</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_quote-2">analyze_quote/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_sequence-2">analyze_sequence/2</a></td><td></td></tr><tr><td valign="top"><a href="#apply-5">apply/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scmi_exports-0"></a>

### '$scmi_exports'/0 ###

<pre><code>
'$scmi_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_expander">scmi_expander()</a>}]
</code></pre>
<br />

<a name="analyze_application-2"></a>

### analyze_application/2 ###

<pre><code>
analyze_application(Operands::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_assignment-2"></a>

### analyze_assignment/2 ###

<pre><code>
analyze_assignment(X1::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_if-2"></a>

### analyze_if/2 ###

<pre><code>
analyze_if(X1::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_include-2"></a>

### analyze_include/2 ###

<pre><code>
analyze_include(Ss::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_include_ci-2"></a>

### analyze_include_ci/2 ###

<pre><code>
analyze_include_ci(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; no_return()
</code></pre>
<br />

<a name="analyze_include_lib-2"></a>

### analyze_include_lib/2 ###

<pre><code>
analyze_include_lib(Ss::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_include_lib_ci-2"></a>

### analyze_include_lib_ci/2 ###

<pre><code>
analyze_include_lib_ci(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; no_return()
</code></pre>
<br />

<a name="analyze_lambda-2"></a>

### analyze_lambda/2 ###

<pre><code>
analyze_lambda(Body::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_proc_application-3"></a>

### analyze_proc_application/3 ###

<pre><code>
analyze_proc_application(Proc::<a href="#type-scmi_proc">scmi_proc()</a>, Operands::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_quote-2"></a>

### analyze_quote/2 ###

<pre><code>
analyze_quote(X1::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_sequence-2"></a>

### analyze_sequence/2 ###

<pre><code>
analyze_sequence(Exps::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="apply-5"></a>

### apply/5 ###

`apply(Nip0, Args, Env, Ok, Ng) -> any()`

