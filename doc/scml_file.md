

# Module scml_file #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme file library</p>.

__Authors:__ CSCM Contributor ([`the-concurrent-schemer@googlegroups.com`](mailto:the-concurrent-schemer@googlegroups.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#call-with-input-file-2">'call-with-input-file'/2</a></td><td></td></tr><tr><td valign="top"><a href="#call-with-output-file-2">'call-with-output-file'/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete-file-1">'delete-file'/1</a></td><td></td></tr><tr><td valign="top"><a href="#file-exists%3f-1">'file-exists?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-binary-input-file-1">'open-binary-input-file'/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-binary-output-file-1">'open-binary-output-file'/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-input-file-1">'open-input-file'/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-output-file-1">'open-output-file'/1</a></td><td></td></tr><tr><td valign="top"><a href="#with-input-from-file-2">'with-input-from-file'/2</a></td><td></td></tr><tr><td valign="top"><a href="#with-output-to-file-2">'with-output-to-file'/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###

<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>
<br />

<a name="call-with-input-file-2"></a>

### 'call-with-input-file'/2 ###

<pre><code>
'call-with-input-file'(S::<a href="#type-scm_string">scm_string()</a>, Proc::<a href="#type-scm_proc">scm_proc()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<a name="call-with-output-file-2"></a>

### 'call-with-output-file'/2 ###

<pre><code>
'call-with-output-file'(S::<a href="#type-scm_string">scm_string()</a>, Proc::<a href="#type-scm_proc">scm_proc()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<a name="delete-file-1"></a>

### 'delete-file'/1 ###

<pre><code>
'delete-file'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>
<br />

<a name="file-exists%3f-1"></a>

### 'file-exists?'/1 ###

<pre><code>
'file-exists?'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<a name="open-binary-input-file-1"></a>

### 'open-binary-input-file'/1 ###

<pre><code>
'open-binary-input-file'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>
<br />

<a name="open-binary-output-file-1"></a>

### 'open-binary-output-file'/1 ###

<pre><code>
'open-binary-output-file'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>
<br />

<a name="open-input-file-1"></a>

### 'open-input-file'/1 ###

<pre><code>
'open-input-file'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>
<br />

<a name="open-output-file-1"></a>

### 'open-output-file'/1 ###

<pre><code>
'open-output-file'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>
<br />

<a name="with-input-from-file-2"></a>

### 'with-input-from-file'/2 ###

<pre><code>
'with-input-from-file'(S::<a href="#type-scm_string">scm_string()</a>, Thunk::<a href="#type-scm_thunk">scm_thunk()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

<a name="with-output-to-file-2"></a>

### 'with-output-to-file'/2 ###

<pre><code>
'with-output-to-file'(S::<a href="#type-scm_string">scm_string()</a>, Thunk::<a href="#type-scm_thunk">scm_thunk()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>
<br />

