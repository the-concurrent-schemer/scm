

# Module scml_base_symbol #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme base library for symbols</p>.
__Authors:__ CSCM Contributor ([`the-concurrent-schemer@googlegroups.com`](mailto:the-concurrent-schemer@googlegroups.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#string-%3esymbol-1">'string->symbol'/1</a></td><td><p>Returns the symbol whose name is string.  This procedure can
create symbols with names containing special characters that would
require escaping with written, but does not interpret escapes in
its input.</p>.</td></tr><tr><td valign="top"><a href="#symbol-%3estring-1">'symbol->string'/1</a></td><td><p>Returns the name of symbol as a string, but without adding
escapes.</p>.</td></tr><tr><td valign="top"><a href="#symbol%3d%3f-1">'symbol=?'/1</a></td><td><p>Returns #t if all the arguments are symbols and all have the
same names in the sense of string=?.</p>.</td></tr><tr><td valign="top"><a href="#symbol%3f-1">'symbol?'/1</a></td><td><p>Returns #t if obj is a symbol, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#symbol_to_unicode-1">symbol_to_unicode/1</a></td><td></td></tr><tr><td valign="top"><a href="#unicode_to_symbol-1">unicode_to_symbol/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="string-%3esymbol-1"></a>

### 'string->symbol'/1 ###


<pre><code>
'string-&gt;symbol'(String::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_symbol">scm_symbol()</a>
</code></pre>

<br></br>


<p>Returns the symbol whose name is string.  This procedure can
create symbols with names containing special characters that would
require escaping with written, but does not interpret escapes in
its input.</p>

<a name="symbol-%3estring-1"></a>

### 'symbol->string'/1 ###


<pre><code>
'symbol-&gt;string'(S::<a href="#type-scm_symbol">scm_symbol()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


<p>Returns the name of symbol as a string, but without adding
escapes.</p>

<a name="symbol%3d%3f-1"></a>

### 'symbol=?'/1 ###


<pre><code>
'symbol=?'(Ss::[<a href="#type-scm_symbol">scm_symbol()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if all the arguments are symbols and all have the
same names in the sense of string=?.</p>

<a name="symbol%3f-1"></a>

### 'symbol?'/1 ###


<pre><code>
'symbol?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is a symbol, otherwise returns #f.</p>

<a name="symbol_to_unicode-1"></a>

### symbol_to_unicode/1 ###


<pre><code>
symbol_to_unicode(S::<a href="#type-scm_symbol">scm_symbol()</a>) -&gt; [<a href="scmd_types_impl.md#type-unichar">scmd_types_impl:unichar()</a>]
</code></pre>

<br></br>



<a name="unicode_to_symbol-1"></a>

### unicode_to_symbol/1 ###


<pre><code>
unicode_to_symbol(L::[<a href="scmd_types_impl.md#type-unichar">scmd_types_impl:unichar()</a>]) -&gt; <a href="#type-scm_symbol">scm_symbol()</a>
</code></pre>

<br></br>



