

# Module scmi_analyze_program #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme interpreter syntactic analyzer for program expressions</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scmi_exports-0">'$scmi_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_define-2">analyze_define/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_define_library-2">analyze_define_library/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_define_record_type-2">analyze_define_record_type/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_define_syntax-2">analyze_define_syntax/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_define_values-2">analyze_define_values/2</a></td><td></td></tr><tr><td valign="top"><a href="#analyze_import-2">analyze_import/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scmi_exports-0"></a>

### '$scmi_exports'/0 ###

<pre><code>
'$scmi_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_expander">scmi_expander()</a>}]
</code></pre>
<br />

<a name="analyze_define-2"></a>

### analyze_define/2 ###

<pre><code>
analyze_define(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_define_library-2"></a>

### analyze_define_library/2 ###

<pre><code>
analyze_define_library(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_define_record_type-2"></a>

### analyze_define_record_type/2 ###

<pre><code>
analyze_define_record_type(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_define_syntax-2"></a>

### analyze_define_syntax/2 ###

<pre><code>
analyze_define_syntax(Exp0::<a href="#type-scmi_exp">scmi_exp()</a>, Senv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_define_values-2"></a>

### analyze_define_values/2 ###

<pre><code>
analyze_define_values(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

<a name="analyze_import-2"></a>

### analyze_import/2 ###

<pre><code>
analyze_import(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, SEnv::<a href="#type-scmi_senv">scmi_senv()</a>) -&gt; <a href="#type-scmi_dexec">scmi_dexec()</a>
</code></pre>
<br />

