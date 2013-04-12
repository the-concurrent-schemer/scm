

# Module scml_base_symbol #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#string-%3esymbol-1">'string->symbol'/1</a></td><td></td></tr><tr><td valign="top"><a href="#symbol-%3estring-1">'symbol->string'/1</a></td><td></td></tr><tr><td valign="top"><a href="#symbol%3d%3f-1">'symbol=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#symbol%3f-1">'symbol?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#imports-0">imports/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="string-%3esymbol-1"></a>

### 'string->symbol'/1 ###


<pre><code>
'string-&gt;symbol'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_symbol">scm_symbol()</a>
</code></pre>

<br></br>



<a name="symbol-%3estring-1"></a>

### 'symbol->string'/1 ###


<pre><code>
'symbol-&gt;string'(S::<a href="#type-scm_symbol">scm_symbol()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="symbol%3d%3f-1"></a>

### 'symbol=?'/1 ###


<pre><code>
'symbol=?'(Ss::[<a href="#type-scm_symbol">scm_symbol()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="symbol%3f-1"></a>

### 'symbol?'/1 ###


<pre><code>
'symbol?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="imports-0"></a>

### imports/0 ###


<pre><code>
imports() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



