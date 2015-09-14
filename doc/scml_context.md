

# Module scml_context #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme process-context library</p>.

__Authors:__ CSCM Contributor ([`the-concurrent-schemer@googlegroups.com`](mailto:the-concurrent-schemer@googlegroups.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#command-line-0">'command-line'/0</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-exit-0">'emergency-exit'/0</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-exit-1">'emergency-exit'/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-environment-variable-1">'get-environment-variable'/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-environment-variables-0">'get-environment-variables'/0</a></td><td></td></tr><tr><td valign="top"><a href="#exit-0">exit/0</a></td><td></td></tr><tr><td valign="top"><a href="#exit-1">exit/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###

<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>
<br />

<a name="command-line-0"></a>

### 'command-line'/0 ###

<pre><code>
'command-line'() -&gt; [<a href="#type-scm_string">scm_string()</a>, ...]
</code></pre>
<br />

<a name="emergency-exit-0"></a>

### 'emergency-exit'/0 ###

<pre><code>
'emergency-exit'() -&gt; no_return()
</code></pre>
<br />

<a name="emergency-exit-1"></a>

### 'emergency-exit'/1 ###

<pre><code>
'emergency-exit'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; no_return()
</code></pre>
<br />

<a name="get-environment-variable-1"></a>

### 'get-environment-variable'/1 ###

<pre><code>
'get-environment-variable'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<a name="get-environment-variables-0"></a>

### 'get-environment-variables'/0 ###

<pre><code>
'get-environment-variables'() -&gt; [[<a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_string">scm_string()</a>]]
</code></pre>
<br />

<a name="exit-0"></a>

### exit/0 ###

<pre><code>
exit() -&gt; no_return()
</code></pre>
<br />

<a name="exit-1"></a>

### exit/1 ###

<pre><code>
exit(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; no_return()
</code></pre>
<br />

