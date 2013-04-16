

# Module scml_char #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#char-alphabetic%3f-1">'char-alphabetic?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-ci%3c%3d%3f-1">'char-ci<=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-ci%3c%3f-1">'char-ci<?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-ci%3d%3f-1">'char-ci=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-ci%3e%3d%3f-1">'char-ci>=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-ci%3e%3f-1">'char-ci>?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-downcase-1">'char-downcase'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-foldcase-1">'char-foldcase'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-lower-case%3f-1">'char-lower-case?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-numeric%3f-1">'char-numeric?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-upcase-1">'char-upcase'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-upper-case%3f-1">'char-upper-case?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#char-whitespace%3f-1">'char-whitespace?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#digit-value-1">'digit-value'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-ci%3c%3d%3f-1">'string-ci<=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-ci%3c%3f-1">'string-ci<?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-ci%3d%3f-1">'string-ci=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-ci%3e%3d%3f-1">'string-ci>=?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-ci%3e%3f-1">'string-ci>?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-downcase-1">'string-downcase'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-foldcase-1">'string-foldcase'/1</a></td><td></td></tr><tr><td valign="top"><a href="#string-upcase-1">'string-upcase'/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="char-alphabetic%3f-1"></a>

### 'char-alphabetic?'/1 ###


<pre><code>
'char-alphabetic?'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-ci%3c%3d%3f-1"></a>

### 'char-ci<=?'/1 ###


<pre><code>
'char-ci&lt;=?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-ci%3c%3f-1"></a>

### 'char-ci<?'/1 ###


<pre><code>
'char-ci&lt;?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-ci%3d%3f-1"></a>

### 'char-ci=?'/1 ###


<pre><code>
'char-ci=?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-ci%3e%3d%3f-1"></a>

### 'char-ci>=?'/1 ###


<pre><code>
'char-ci&gt;=?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-ci%3e%3f-1"></a>

### 'char-ci>?'/1 ###


<pre><code>
'char-ci&gt;?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-downcase-1"></a>

### 'char-downcase'/1 ###


<pre><code>
'char-downcase'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_char">scm_char()</a>
</code></pre>

<br></br>



<a name="char-foldcase-1"></a>

### 'char-foldcase'/1 ###


<pre><code>
'char-foldcase'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_char">scm_char()</a>
</code></pre>

<br></br>



<a name="char-lower-case%3f-1"></a>

### 'char-lower-case?'/1 ###


<pre><code>
'char-lower-case?'(L::<a href="#type-scm_letter">scm_letter()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-numeric%3f-1"></a>

### 'char-numeric?'/1 ###


<pre><code>
'char-numeric?'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-upcase-1"></a>

### 'char-upcase'/1 ###


<pre><code>
'char-upcase'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_char">scm_char()</a>
</code></pre>

<br></br>



<a name="char-upper-case%3f-1"></a>

### 'char-upper-case?'/1 ###


<pre><code>
'char-upper-case?'(L::<a href="#type-scm_letter">scm_letter()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-whitespace%3f-1"></a>

### 'char-whitespace?'/1 ###


<pre><code>
'char-whitespace?'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="digit-value-1"></a>

### 'digit-value'/1 ###


<pre><code>
'digit-value'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="string-ci%3c%3d%3f-1"></a>

### 'string-ci<=?'/1 ###


<pre><code>
'string-ci&lt;=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string-ci%3c%3f-1"></a>

### 'string-ci<?'/1 ###


<pre><code>
'string-ci&lt;?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string-ci%3d%3f-1"></a>

### 'string-ci=?'/1 ###


<pre><code>
'string-ci=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string-ci%3e%3d%3f-1"></a>

### 'string-ci>=?'/1 ###


<pre><code>
'string-ci&gt;=?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string-ci%3e%3f-1"></a>

### 'string-ci>?'/1 ###


<pre><code>
'string-ci&gt;?'(Ss::[<a href="#type-scm_string">scm_string()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="string-downcase-1"></a>

### 'string-downcase'/1 ###


<pre><code>
'string-downcase'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="string-foldcase-1"></a>

### 'string-foldcase'/1 ###


<pre><code>
'string-foldcase'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="string-upcase-1"></a>

### 'string-upcase'/1 ###


<pre><code>
'string-upcase'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



