

# Module scml_r5rs #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme r5rs library</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#call-with-current-continuation-4">'call-with-current-continuation'/4</a></td><td></td></tr><tr><td valign="top"><a href="#exact-%3einexact-1">'exact->inexact'/1</a></td><td></td></tr><tr><td valign="top"><a href="#inexact-%3eexact-1">'inexact->exact'/1</a></td><td></td></tr><tr><td valign="top"><a href="#null-environment-1">'null-environment'/1</a></td><td></td></tr><tr><td valign="top"><a href="#scheme-report-environment-1">'scheme-report-environment'/1</a></td><td></td></tr><tr><td valign="top"><a href="#transcript-off-0">'transcript-off'/0</a></td><td></td></tr><tr><td valign="top"><a href="#transcript-on-1">'transcript-on'/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="call-with-current-continuation-4"></a>

### 'call-with-current-continuation'/4 ###


<pre><code>
'call-with-current-continuation'(Proc::<a href="#type-scm_proc">scm_proc()</a>, Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>



<a name="exact-%3einexact-1"></a>

### 'exact->inexact'/1 ###


<pre><code>
'exact-&gt;inexact'(Z::<a href="#type-scm_z">scm_z()</a>) -&gt; <a href="#type-scm_z">scm_z()</a>
</code></pre>

<br></br>



<a name="inexact-%3eexact-1"></a>

### 'inexact->exact'/1 ###


<pre><code>
'inexact-&gt;exact'(Z::<a href="#type-scm_z">scm_z()</a>) -&gt; <a href="#type-scm_z">scm_z()</a>
</code></pre>

<br></br>



<a name="null-environment-1"></a>

### 'null-environment'/1 ###


<pre><code>
'null-environment'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scmi_env">scmi_env()</a>
</code></pre>

<br></br>



<a name="scheme-report-environment-1"></a>

### 'scheme-report-environment'/1 ###


<pre><code>
'scheme-report-environment'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scmi_env">scmi_env()</a>
</code></pre>

<br></br>



<a name="transcript-off-0"></a>

### 'transcript-off'/0 ###


<pre><code>
'transcript-off'() -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="transcript-on-1"></a>

### 'transcript-on'/1 ###


<pre><code>
'transcript-on'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



