

# Module scml_base_char #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

<p>Scheme base library for characters</p>.

__Authors:__ CSCM Contributor ([`the-concurrent-schemer@googlegroups.com`](mailto:the-concurrent-schemer@googlegroups.com)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#char-%3einteger-1">'char->integer'/1</a></td><td><p>Returns an exact integer equal to the Unicode scalar value of
the given character.</p>.</td></tr><tr><td valign="top"><a href="#char%3c%3d%3f-1">'char<=?'/1</a></td><td><p>Returns #t if all the characters are monotonically
non-decreasing, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#char%3c%3f-1">'char<?'/1</a></td><td><p>Returns #t if all the characters are monotonically increasing,
otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#char%3d%3f-1">'char=?'/1</a></td><td><p>Returns #t if all the characters are equal, otherwise returns
#f.</p>.</td></tr><tr><td valign="top"><a href="#char%3e%3d%3f-1">'char>=?'/1</a></td><td><p>Returns #t if all the characters are monotonically
non-increasing, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#char%3e%3f-1">'char>?'/1</a></td><td><p>Returns #t if all the characters are monotonically decreasing,
otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#char%3f-1">'char?'/1</a></td><td><p>Returns #t if obj is a character, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#integer-%3echar-1">'integer->char'/1</a></td><td><p>Returns a character equal to the Unicode scalar value of the
given exact integer.</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###

<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>
<br />

<a name="char-%3einteger-1"></a>

### 'char->integer'/1 ###

<pre><code>
'char-&gt;integer'(Character::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_n">scm_n()</a>
</code></pre>
<br />

<p>Returns an exact integer equal to the Unicode scalar value of
the given character.</p>

<a name="char%3c%3d%3f-1"></a>

### 'char<=?'/1 ###

<pre><code>
'char&lt;=?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if all the characters are monotonically
non-decreasing, otherwise returns #f.</p>

<a name="char%3c%3f-1"></a>

### 'char<?'/1 ###

<pre><code>
'char&lt;?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if all the characters are monotonically increasing,
otherwise returns #f.</p>

<a name="char%3d%3f-1"></a>

### 'char=?'/1 ###

<pre><code>
'char=?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if all the characters are equal, otherwise returns
#f.</p>

<a name="char%3e%3d%3f-1"></a>

### 'char>=?'/1 ###

<pre><code>
'char&gt;=?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if all the characters are monotonically
non-increasing, otherwise returns #f.</p>

<a name="char%3e%3f-1"></a>

### 'char>?'/1 ###

<pre><code>
'char&gt;?'(Cs::[<a href="#type-scm_char">scm_char()</a>, ...]) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if all the characters are monotonically decreasing,
otherwise returns #f.</p>

<a name="char%3f-1"></a>

### 'char?'/1 ###

<pre><code>
'char?'(Character::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>
<br />

<p>Returns #t if obj is a character, otherwise returns #f.</p>

<a name="integer-%3echar-1"></a>

### 'integer->char'/1 ###

<pre><code>
'integer-&gt;char'(N::<a href="#type-scm_n">scm_n()</a>) -&gt; <a href="#type-scm_char">scm_char()</a>
</code></pre>
<br />

<p>Returns a character equal to the Unicode scalar value of the
given exact integer.</p>

