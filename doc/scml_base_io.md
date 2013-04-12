

# Module scml_base_io #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#binary-port%3f-1">'binary-port?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#call-with-port-2">'call-with-port'/2</a></td><td></td></tr><tr><td valign="top"><a href="#char-ready%3f-0">'char-ready?'/0</a></td><td></td></tr><tr><td valign="top"><a href="#char-ready%3f-1">'char-ready?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#close-input-port-1">'close-input-port'/1</a></td><td></td></tr><tr><td valign="top"><a href="#close-output-port-1">'close-output-port'/1</a></td><td></td></tr><tr><td valign="top"><a href="#close-port-1">'close-port'/1</a></td><td></td></tr><tr><td valign="top"><a href="#current-error-port-0">'current-error-port'/0</a></td><td></td></tr><tr><td valign="top"><a href="#current-input-port-0">'current-input-port'/0</a></td><td></td></tr><tr><td valign="top"><a href="#current-output-port-0">'current-output-port'/0</a></td><td></td></tr><tr><td valign="top"><a href="#eof-object-0">'eof-object'/0</a></td><td></td></tr><tr><td valign="top"><a href="#eof-object%3f-1">'eof-object?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#flush-output-port-0">'flush-output-port'/0</a></td><td></td></tr><tr><td valign="top"><a href="#flush-output-port-1">'flush-output-port'/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-output-bytevector-1">'get-output-bytevector'/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-output-string-1">'get-output-string'/1</a></td><td></td></tr><tr><td valign="top"><a href="#input-port-open%3f-1">'input-port-open?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#input-port%3f-1">'input-port?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-input-bytevector-1">'open-input-bytevector'/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-input-string-1">'open-input-string'/1</a></td><td></td></tr><tr><td valign="top"><a href="#open-output-bytevector-0">'open-output-bytevector'/0</a></td><td></td></tr><tr><td valign="top"><a href="#open-output-string-0">'open-output-string'/0</a></td><td></td></tr><tr><td valign="top"><a href="#output-port-open%3f-1">'output-port-open?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#output-port%3f-1">'output-port?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#peek-char-0">'peek-char'/0</a></td><td></td></tr><tr><td valign="top"><a href="#peek-char-1">'peek-char'/1</a></td><td></td></tr><tr><td valign="top"><a href="#peek-u8-0">'peek-u8'/0</a></td><td></td></tr><tr><td valign="top"><a href="#peek-u8-1">'peek-u8'/1</a></td><td></td></tr><tr><td valign="top"><a href="#port%3f-1">'port?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#read-bytevector%21-1">'read-bytevector!'/1</a></td><td></td></tr><tr><td valign="top"><a href="#read-bytevector%21-2">'read-bytevector!'/2</a></td><td></td></tr><tr><td valign="top"><a href="#read-bytevector%21-3">'read-bytevector!'/3</a></td><td></td></tr><tr><td valign="top"><a href="#read-bytevector%21-4">'read-bytevector!'/4</a></td><td></td></tr><tr><td valign="top"><a href="#read-bytevector-1">'read-bytevector'/1</a></td><td></td></tr><tr><td valign="top"><a href="#read-bytevector-2">'read-bytevector'/2</a></td><td></td></tr><tr><td valign="top"><a href="#read-char-0">'read-char'/0</a></td><td></td></tr><tr><td valign="top"><a href="#read-char-1">'read-char'/1</a></td><td></td></tr><tr><td valign="top"><a href="#read-line-0">'read-line'/0</a></td><td></td></tr><tr><td valign="top"><a href="#read-line-1">'read-line'/1</a></td><td></td></tr><tr><td valign="top"><a href="#read-string-1">'read-string'/1</a></td><td></td></tr><tr><td valign="top"><a href="#read-string-2">'read-string'/2</a></td><td></td></tr><tr><td valign="top"><a href="#read-u8-0">'read-u8'/0</a></td><td></td></tr><tr><td valign="top"><a href="#read-u8-1">'read-u8'/1</a></td><td></td></tr><tr><td valign="top"><a href="#textual-port%3f-1">'textual-port?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#u8-ready%3f-0">'u8-ready?'/0</a></td><td></td></tr><tr><td valign="top"><a href="#u8-ready%3f-1">'u8-ready?'/1</a></td><td></td></tr><tr><td valign="top"><a href="#write-bytevector-1">'write-bytevector'/1</a></td><td></td></tr><tr><td valign="top"><a href="#write-bytevector-2">'write-bytevector'/2</a></td><td></td></tr><tr><td valign="top"><a href="#write-bytevector-3">'write-bytevector'/3</a></td><td></td></tr><tr><td valign="top"><a href="#write-bytevector-4">'write-bytevector'/4</a></td><td></td></tr><tr><td valign="top"><a href="#write-char-1">'write-char'/1</a></td><td></td></tr><tr><td valign="top"><a href="#write-char-2">'write-char'/2</a></td><td></td></tr><tr><td valign="top"><a href="#write-string-1">'write-string'/1</a></td><td></td></tr><tr><td valign="top"><a href="#write-string-2">'write-string'/2</a></td><td></td></tr><tr><td valign="top"><a href="#write-string-3">'write-string'/3</a></td><td></td></tr><tr><td valign="top"><a href="#write-string-4">'write-string'/4</a></td><td></td></tr><tr><td valign="top"><a href="#write-u8-1">'write-u8'/1</a></td><td></td></tr><tr><td valign="top"><a href="#write-u8-2">'write-u8'/2</a></td><td></td></tr><tr><td valign="top"><a href="#imports-0">imports/0</a></td><td></td></tr><tr><td valign="top"><a href="#newline-0">newline/0</a></td><td></td></tr><tr><td valign="top"><a href="#newline-1">newline/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="binary-port%3f-1"></a>

### 'binary-port?'/1 ###


<pre><code>
'binary-port?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="call-with-port-2"></a>

### 'call-with-port'/2 ###


<pre><code>
'call-with-port'(Port::<a href="#type-scm_port">scm_port()</a>, Proc::<a href="#type-scm_proc">scm_proc()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>



<a name="char-ready%3f-0"></a>

### 'char-ready?'/0 ###


<pre><code>
'char-ready?'() -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="char-ready%3f-1"></a>

### 'char-ready?'/1 ###


<pre><code>
'char-ready?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="close-input-port-1"></a>

### 'close-input-port'/1 ###


<pre><code>
'close-input-port'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="close-output-port-1"></a>

### 'close-output-port'/1 ###


<pre><code>
'close-output-port'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="close-port-1"></a>

### 'close-port'/1 ###


<pre><code>
'close-port'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="current-error-port-0"></a>

### 'current-error-port'/0 ###


<pre><code>
'current-error-port'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>



<a name="current-input-port-0"></a>

### 'current-input-port'/0 ###


<pre><code>
'current-input-port'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>



<a name="current-output-port-0"></a>

### 'current-output-port'/0 ###


<pre><code>
'current-output-port'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>



<a name="eof-object-0"></a>

### 'eof-object'/0 ###


<pre><code>
'eof-object'() -&gt; <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="eof-object%3f-1"></a>

### 'eof-object?'/1 ###


<pre><code>
'eof-object?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="flush-output-port-0"></a>

### 'flush-output-port'/0 ###


<pre><code>
'flush-output-port'() -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="flush-output-port-1"></a>

### 'flush-output-port'/1 ###


<pre><code>
'flush-output-port'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="get-output-bytevector-1"></a>

### 'get-output-bytevector'/1 ###


<pre><code>
'get-output-bytevector'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>

<br></br>



<a name="get-output-string-1"></a>

### 'get-output-string'/1 ###


<pre><code>
'get-output-string'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>



<a name="input-port-open%3f-1"></a>

### 'input-port-open?'/1 ###


<pre><code>
'input-port-open?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="input-port%3f-1"></a>

### 'input-port?'/1 ###


<pre><code>
'input-port?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="open-input-bytevector-1"></a>

### 'open-input-bytevector'/1 ###


<pre><code>
'open-input-bytevector'(S::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>



<a name="open-input-string-1"></a>

### 'open-input-string'/1 ###


<pre><code>
'open-input-string'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>



<a name="open-output-bytevector-0"></a>

### 'open-output-bytevector'/0 ###


<pre><code>
'open-output-bytevector'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>



<a name="open-output-string-0"></a>

### 'open-output-string'/0 ###


<pre><code>
'open-output-string'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>



<a name="output-port-open%3f-1"></a>

### 'output-port-open?'/1 ###


<pre><code>
'output-port-open?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="output-port%3f-1"></a>

### 'output-port?'/1 ###


<pre><code>
'output-port?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="peek-char-0"></a>

### 'peek-char'/0 ###


<pre><code>
'peek-char'() -&gt; <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="peek-char-1"></a>

### 'peek-char'/1 ###


<pre><code>
'peek-char'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="peek-u8-0"></a>

### 'peek-u8'/0 ###


<pre><code>
'peek-u8'() -&gt; <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="peek-u8-1"></a>

### 'peek-u8'/1 ###


<pre><code>
'peek-u8'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="port%3f-1"></a>

### 'port?'/1 ###


<pre><code>
'port?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="read-bytevector%21-1"></a>

### 'read-bytevector!'/1 ###


<pre><code>
'read-bytevector!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-bytevector%21-2"></a>

### 'read-bytevector!'/2 ###


<pre><code>
'read-bytevector!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-bytevector%21-3"></a>

### 'read-bytevector!'/3 ###


<pre><code>
'read-bytevector!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-bytevector%21-4"></a>

### 'read-bytevector!'/4 ###


<pre><code>
'read-bytevector!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-bytevector-1"></a>

### 'read-bytevector'/1 ###


<pre><code>
'read-bytevector'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-bytevector-2"></a>

### 'read-bytevector'/2 ###


<pre><code>
'read-bytevector'(K::<a href="#type-scm_k">scm_k()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-char-0"></a>

### 'read-char'/0 ###


<pre><code>
'read-char'() -&gt; <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-char-1"></a>

### 'read-char'/1 ###


<pre><code>
'read-char'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-line-0"></a>

### 'read-line'/0 ###


<pre><code>
'read-line'() -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-line-1"></a>

### 'read-line'/1 ###


<pre><code>
'read-line'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-string-1"></a>

### 'read-string'/1 ###


<pre><code>
'read-string'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-string-2"></a>

### 'read-string'/2 ###


<pre><code>
'read-string'(K::<a href="#type-scm_k">scm_k()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-u8-0"></a>

### 'read-u8'/0 ###


<pre><code>
'read-u8'() -&gt; <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="read-u8-1"></a>

### 'read-u8'/1 ###


<pre><code>
'read-u8'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>



<a name="textual-port%3f-1"></a>

### 'textual-port?'/1 ###


<pre><code>
'textual-port?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="u8-ready%3f-0"></a>

### 'u8-ready?'/0 ###


<pre><code>
'u8-ready?'() -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="u8-ready%3f-1"></a>

### 'u8-ready?'/1 ###


<pre><code>
'u8-ready?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>



<a name="write-bytevector-1"></a>

### 'write-bytevector'/1 ###


<pre><code>
'write-bytevector'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-bytevector-2"></a>

### 'write-bytevector'/2 ###


<pre><code>
'write-bytevector'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-bytevector-3"></a>

### 'write-bytevector'/3 ###


<pre><code>
'write-bytevector'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-bytevector-4"></a>

### 'write-bytevector'/4 ###


<pre><code>
'write-bytevector'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-char-1"></a>

### 'write-char'/1 ###


<pre><code>
'write-char'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-char-2"></a>

### 'write-char'/2 ###


<pre><code>
'write-char'(C::<a href="#type-scm_char">scm_char()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-string-1"></a>

### 'write-string'/1 ###


<pre><code>
'write-string'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-string-2"></a>

### 'write-string'/2 ###


<pre><code>
'write-string'(S::<a href="#type-scm_string">scm_string()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-string-3"></a>

### 'write-string'/3 ###


<pre><code>
'write-string'(S::<a href="#type-scm_string">scm_string()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-string-4"></a>

### 'write-string'/4 ###


<pre><code>
'write-string'(S::<a href="#type-scm_string">scm_string()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-u8-1"></a>

### 'write-u8'/1 ###


<pre><code>
'write-u8'(B::<a href="#type-scm_byte">scm_byte()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="write-u8-2"></a>

### 'write-u8'/2 ###


<pre><code>
'write-u8'(B::<a href="#type-scm_byte">scm_byte()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="imports-0"></a>

### imports/0 ###


<pre><code>
imports() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="newline-0"></a>

### newline/0 ###


<pre><code>
newline() -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



<a name="newline-1"></a>

### newline/1 ###


<pre><code>
newline(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>



