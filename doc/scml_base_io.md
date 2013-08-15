

# Module scml_base_io #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme base library for input and output</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#binary-port%3f-1">'binary-port?'/1</a></td><td><p>Returns #t if obj is a binary port, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#call-with-port-5">'call-with-port'/5</a></td><td><p>Calls proc with port as an argument.  If proc returns, then
the port is closed and the values yielded by the proc are returned.
Otherwise, the port is automatically closed when there are no
longer any references to the port.</p>.</td></tr><tr><td valign="top"><a href="#char-ready%3f-0">'char-ready?'/0</a></td><td>Equivalent to <a href="#char-ready%3f-1"><tt>'char-ready?'('current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#char-ready%3f-1">'char-ready?'/1</a></td><td><p>Returns #t if a character is ready on the textual input port
or if port is at the end of the file, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#close-input-port-1">'close-input-port'/1</a></td><td><p>Close the resource associated with port, rendering the input
port incapable of delivering data.  This procedure has no effect if
the input port has already been closed.  It is an error to apply
this procedure to a port which is not an input port.</p>.</td></tr><tr><td valign="top"><a href="#close-output-port-1">'close-output-port'/1</a></td><td><p>Close the resource associated with port, rendering the input
port incapable of delivering data.  This procedure has no effect if
the input port has already been closed.  It is an error to apply
this procedure to a port which is not an input port.</p>.</td></tr><tr><td valign="top"><a href="#close-port-1">'close-port'/1</a></td><td><p>Close the resource associated with port, rendering the port
incapable of delivering or accepting data.  This procedure has no
effect if the port has already been closed.</p>.</td></tr><tr><td valign="top"><a href="#current-error-port-0">'current-error-port'/0</a></td><td><p>Returns the current error port.  The initial value is the
default error port.  The default error port is a textual port.  The
<code>current-error-port</code> procedures behave like a parameter object,
which can be overridden with <code>parameterize</code>.</p>.</td></tr><tr><td valign="top"><a href="#current-error-port-1">'current-error-port'/1</a></td><td><p>Returns the converter procedure for the current error port.</p>.</td></tr><tr><td valign="top"><a href="#current-error-port-2">'current-error-port'/2</a></td><td><p>Saves the given port as the current error port.  Returns #f.</p>.</td></tr><tr><td valign="top"><a href="#current-input-port-0">'current-input-port'/0</a></td><td><p>Returns the current input port.  The initial value is the
default input port.  The default input port is a textual port.  The
<code>current-input-port</code> procedures behave like a parameter object,
which can be overridden with <code>parameterize</code>.</p>.</td></tr><tr><td valign="top"><a href="#current-input-port-1">'current-input-port'/1</a></td><td><p>Returns the converter procedure for the current input port.</p>.</td></tr><tr><td valign="top"><a href="#current-input-port-2">'current-input-port'/2</a></td><td><p>Saves the given port as the current input port.  Returns #f.</p>.</td></tr><tr><td valign="top"><a href="#current-output-port-0">'current-output-port'/0</a></td><td><p>Returns the current output port.  The initial value is the
default output port.  The default output port is a textual port.
The <code>current-output-port</code> procedures behave like a parameter
object, which can be overridden with <code>parameterize</code>.</p>.</td></tr><tr><td valign="top"><a href="#current-output-port-1">'current-output-port'/1</a></td><td><p>Returns the converter procedure for the current output port.</p>.</td></tr><tr><td valign="top"><a href="#current-output-port-2">'current-output-port'/2</a></td><td><p>Saves the given port as the current output port.  Returns #f.</p>.</td></tr><tr><td valign="top"><a href="#eof-object-0">'eof-object'/0</a></td><td><p>Returns an end-of-file object.</p>.</td></tr><tr><td valign="top"><a href="#eof-object%3f-1">'eof-object?'/1</a></td><td><p>Returns #t if obj is an end-of-file object, otherwise returns
#f.</p>.</td></tr><tr><td valign="top"><a href="#flush-output-port-0">'flush-output-port'/0</a></td><td>Equivalent to <a href="#flush-output-port-1"><tt>'flush-output-port'('current-output-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#flush-output-port-1">'flush-output-port'/1</a></td><td><p>Flushes any buffered output from the buffer of output port to
the underlying file or device.</p>.</td></tr><tr><td valign="top"><a href="#get-output-bytevector-1">'get-output-bytevector'/1</a></td><td><p>Returns a bytevector consisting of the bytes that have been
output to the port so far in the order they were output.  It is an
error if port is not a binary output port.</p>.</td></tr><tr><td valign="top"><a href="#get-output-string-1">'get-output-string'/1</a></td><td><p>Returns a string consisting of the characters that have been
output to the port so far in the order they were output.  It is an
error if port is not a textual output port.</p>.</td></tr><tr><td valign="top"><a href="#input-port-open%3f-1">'input-port-open?'/1</a></td><td><p>Returns #t if port is still open and capable of performing
input, otherwise returns #t.</p>.</td></tr><tr><td valign="top"><a href="#input-port%3f-1">'input-port?'/1</a></td><td><p>Returns #t if obj is an input port, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#open-input-bytevector-1">'open-input-bytevector'/1</a></td><td><p>Returns a binary input port that delivers bytes from the given
bytevector.</p>.</td></tr><tr><td valign="top"><a href="#open-input-string-1">'open-input-string'/1</a></td><td><p>Returns a textual input port that delivers characters from the
given string.</p>.</td></tr><tr><td valign="top"><a href="#open-output-bytevector-0">'open-output-bytevector'/0</a></td><td><p>Returns a binary output port that will accumulate bytes for
retrieval by <code>get-output-bytevector</code>.</p>.</td></tr><tr><td valign="top"><a href="#open-output-string-0">'open-output-string'/0</a></td><td><p>Returns a textual output port that will accumulate characters
for retrieval by <code>get-output-string</code>.</p>.</td></tr><tr><td valign="top"><a href="#output-port-open%3f-1">'output-port-open?'/1</a></td><td><p>Returns #t if port is still open and capable of performing
output, otherwise returns #t.</p>.</td></tr><tr><td valign="top"><a href="#output-port%3f-1">'output-port?'/1</a></td><td><p>Returns #t if obj is an output port, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#peek-char-0">'peek-char'/0</a></td><td>Equivalent to <a href="#peek-char-1"><tt>'peek-char'('current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#peek-char-1">'peek-char'/1</a></td><td><p>Returns the next character available from the textual input
port, but without updating the port to point to the following
character.  If no more characters are available, an end-of-file
object is returned.</p>.</td></tr><tr><td valign="top"><a href="#peek-u8-0">'peek-u8'/0</a></td><td>Equivalent to <a href="#peek-u8-1"><tt>'peek-u8'('current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#peek-u8-1">'peek-u8'/1</a></td><td><p>Returns the next byte available from the binary input port,
but without updating the port to point to the following byte.  If
not more bytes are available, an end-of-file object is returned.</p>.</td></tr><tr><td valign="top"><a href="#port%3f-1">'port?'/1</a></td><td><p>Returns #t if obj is a port, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#read-bytevector%21-1">'read-bytevector!'/1</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#read-bytevector%21-2">'read-bytevector!'/2</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#read-bytevector%21-3">'read-bytevector!'/3</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#read-bytevector%21-4">'read-bytevector!'/4</a></td><td><p><em>unsupported</em></p>.</td></tr><tr><td valign="top"><a href="#read-bytevector-1">'read-bytevector'/1</a></td><td>Equivalent to <a href="#read-bytevector-2"><tt>'read-bytevector'(K, 'current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#read-bytevector-2">'read-bytevector'/2</a></td><td><p>Reads the next k bytes, or as many as are available before the
end of file, from the binary input port into a bytevector in
left-to-right order and returns the bytevector.  If no bytes are
available before the end of file, an end-of-file object is
returned.</p>.</td></tr><tr><td valign="top"><a href="#read-char-0">'read-char'/0</a></td><td>Equivalent to <a href="#read-char-1"><tt>'read-char'('current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#read-char-1">'read-char'/1</a></td><td><p>Returns the next character available from the textual input
port, updating the port to point to the following character.  If no
more characters are available, an end-of-file object is returned.</p>.</td></tr><tr><td valign="top"><a href="#read-line-0">'read-line'/0</a></td><td>Equivalent to <a href="#read-line-1"><tt>'read-line'('current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#read-line-1">'read-line'/1</a></td><td><p>Returns the next line of text available from the textual input
port, updating the port to point to the following character.  If an
end of line is read, a string containing all of the text up to (but
not including) the end of file is returned, and the port is updated
to point just past the end of line.  If an end of file is
encountered before any end of line is read, but some characters
have been read, a string containing those characters is returned.
If an end of file is encountered before any characters are read, an
end-of-file object is returned.  For the purpose of this procedure,
an end of line consists of either a linefeed character, a carriage
return character, or a sequence of a carriage return character
followed by a linefeed character.</p>.</td></tr><tr><td valign="top"><a href="#read-string-1">'read-string'/1</a></td><td>Equivalent to <a href="#read-string-2"><tt>'read-string'(K, 'current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#read-string-2">'read-string'/2</a></td><td><p>Reads the next k characters, or as many as are available
before the end of file, from the textual input port into a string
in left-to-right order and returns the string.  If no characters
are available before the end of file, an end-of-file object is
returned.</p>.</td></tr><tr><td valign="top"><a href="#read-u8-0">'read-u8'/0</a></td><td>Equivalent to <a href="#read-u8-1"><tt>'read-u8'('current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#read-u8-1">'read-u8'/1</a></td><td><p>Returns the next byte available from the binary input port,
updating the port to point to the following byte.  If no more bytes
are available, an end-of-file object is returned.</p>.</td></tr><tr><td valign="top"><a href="#textual-port%3f-1">'textual-port?'/1</a></td><td><p>Returns #t if obj is a textual port, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#u8-ready%3f-0">'u8-ready?'/0</a></td><td>Equivalent to <a href="#u8-ready%3f-1"><tt>'u8-ready?'('current-input-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#u8-ready%3f-1">'u8-ready?'/1</a></td><td><p>Returns #t if a byte is ready on the binary input port or if
port is at the end of the file, otherwise returns #f.</p>.</td></tr><tr><td valign="top"><a href="#write-bytevector-1">'write-bytevector'/1</a></td><td>Equivalent to <a href="#write-bytevector-2"><tt>'write-bytevector'(V, 'current-output-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#write-bytevector-2">'write-bytevector'/2</a></td><td>Equivalent to <a href="#write-bytevector-4"><tt>'write-bytevector'(V, 'current-output-port'(), 0,
'bytevector-length'(V))</tt></a>.</td></tr><tr><td valign="top"><a href="#write-bytevector-3">'write-bytevector'/3</a></td><td>Equivalent to <a href="#write-bytevector-4"><tt>'write-bytevector'(V, 'current-output-port'(), Start,
'bytevector-length'(V))</tt></a>.</td></tr><tr><td valign="top"><a href="#write-bytevector-4">'write-bytevector'/4</a></td><td><p>Writes the bytes of bytevector from start to end in
left-to-right order to the binary output port.</p>.</td></tr><tr><td valign="top"><a href="#write-char-1">'write-char'/1</a></td><td>Equivalent to <a href="#write-char-2"><tt>'write-char'(C, 'current-output-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#write-char-2">'write-char'/2</a></td><td><p>Writes the character char to the given textual output port.</p>.</td></tr><tr><td valign="top"><a href="#write-string-1">'write-string'/1</a></td><td>Equivalent to <a href="#write-string-2"><tt>'write-string'(S, 'current-output-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#write-string-2">'write-string'/2</a></td><td>Equivalent to <a href="#write-string-3"><tt>'write-string'(S, 'current-output-port'(), 0)</tt></a>.</td></tr><tr><td valign="top"><a href="#write-string-3">'write-string'/3</a></td><td>Equivalent to <a href="#write-string-4"><tt>'write-string'(S, 'current-output-port'(), Start,
'string-length'(S))</tt></a>.</td></tr><tr><td valign="top"><a href="#write-string-4">'write-string'/4</a></td><td><p>Writes the characters of string from start to end in
left-right order to the textual output port.</p>.</td></tr><tr><td valign="top"><a href="#write-u8-1">'write-u8'/1</a></td><td>Equivalent to <a href="#write-u8-2"><tt>'write-u8'(B, 'current-output-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#write-u8-2">'write-u8'/2</a></td><td><p>Writes the byte to the given binary output port.</p>.</td></tr><tr><td valign="top"><a href="#newline-0">newline/0</a></td><td>Equivalent to <a href="#newline-1"><tt>newline('current-output-port'())</tt></a>.</td></tr><tr><td valign="top"><a href="#newline-1">newline/1</a></td><td><p>Writes an end of line to the textual output port.</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="binary-port%3f-1"></a>

### 'binary-port?'/1 ###


<pre><code>
'binary-port?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is a binary port, otherwise returns #f.</p>

<a name="call-with-port-5"></a>

### 'call-with-port'/5 ###


<pre><code>
'call-with-port'(Port::<a href="#type-scm_port">scm_port()</a>, Proc::<a href="#type-scm_proc">scm_proc()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>


<p>Calls proc with port as an argument.  If proc returns, then
the port is closed and the values yielded by the proc are returned.
Otherwise, the port is automatically closed when there are no
longer any references to the port.</p>

<a name="char-ready%3f-0"></a>

### 'char-ready?'/0 ###


<pre><code>
'char-ready?'() -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


Equivalent to [`'char-ready?'('current-input-port'())`](#char-ready%3f-1).
<a name="char-ready%3f-1"></a>

### 'char-ready?'/1 ###


<pre><code>
'char-ready?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if a character is ready on the textual input port
or if port is at the end of the file, otherwise returns #f.</p>

<a name="close-input-port-1"></a>

### 'close-input-port'/1 ###


<pre><code>
'close-input-port'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Close the resource associated with port, rendering the input
port incapable of delivering data.  This procedure has no effect if
the input port has already been closed.  It is an error to apply
this procedure to a port which is not an input port.</p>

<a name="close-output-port-1"></a>

### 'close-output-port'/1 ###


<pre><code>
'close-output-port'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Close the resource associated with port, rendering the input
port incapable of delivering data.  This procedure has no effect if
the input port has already been closed.  It is an error to apply
this procedure to a port which is not an input port.</p>

<a name="close-port-1"></a>

### 'close-port'/1 ###


<pre><code>
'close-port'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Close the resource associated with port, rendering the port
incapable of delivering or accepting data.  This procedure has no
effect if the port has already been closed.</p>

<a name="current-error-port-0"></a>

### 'current-error-port'/0 ###


<pre><code>
'current-error-port'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>


<p>Returns the current error port.  The initial value is the
default error port.  The default error port is a textual port.  The
<code>current-error-port</code> procedures behave like a parameter object,
which can be overridden with <code>parameterize</code>.</p>

<a name="current-error-port-1"></a>

### 'current-error-port'/1 ###


<pre><code>
'current-error-port'(X1::'?SCMIPARAMCVT') -&gt; <a href="#type-scm_proc">scm_proc()</a>
</code></pre>

<br></br>


<p>Returns the converter procedure for the current error port.</p>

<a name="current-error-port-2"></a>

### 'current-error-port'/2 ###


<pre><code>
'current-error-port'(X1::'?SCMIPARAMSET', Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Saves the given port as the current error port.  Returns #f.</p>

<a name="current-input-port-0"></a>

### 'current-input-port'/0 ###


<pre><code>
'current-input-port'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>


<p>Returns the current input port.  The initial value is the
default input port.  The default input port is a textual port.  The
<code>current-input-port</code> procedures behave like a parameter object,
which can be overridden with <code>parameterize</code>.</p>

<a name="current-input-port-1"></a>

### 'current-input-port'/1 ###


<pre><code>
'current-input-port'(X1::'?SCMIPARAMCVT') -&gt; <a href="#type-scm_proc">scm_proc()</a>
</code></pre>

<br></br>


<p>Returns the converter procedure for the current input port.</p>

<a name="current-input-port-2"></a>

### 'current-input-port'/2 ###


<pre><code>
'current-input-port'(X1::'?SCMIPARAMSET', Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Saves the given port as the current input port.  Returns #f.</p>

<a name="current-output-port-0"></a>

### 'current-output-port'/0 ###


<pre><code>
'current-output-port'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>


<p>Returns the current output port.  The initial value is the
default output port.  The default output port is a textual port.
The <code>current-output-port</code> procedures behave like a parameter
object, which can be overridden with <code>parameterize</code>.</p>

<a name="current-output-port-1"></a>

### 'current-output-port'/1 ###


<pre><code>
'current-output-port'(X1::'?SCMIPARAMCVT') -&gt; <a href="#type-scm_proc">scm_proc()</a>
</code></pre>

<br></br>


<p>Returns the converter procedure for the current output port.</p>

<a name="current-output-port-2"></a>

### 'current-output-port'/2 ###


<pre><code>
'current-output-port'(X1::'?SCMIPARAMSET', Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Saves the given port as the current output port.  Returns #f.</p>

<a name="eof-object-0"></a>

### 'eof-object'/0 ###


<pre><code>
'eof-object'() -&gt; <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p>Returns an end-of-file object.</p>

<a name="eof-object%3f-1"></a>

### 'eof-object?'/1 ###


<pre><code>
'eof-object?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is an end-of-file object, otherwise returns
#f.</p>

<a name="flush-output-port-0"></a>

### 'flush-output-port'/0 ###


<pre><code>
'flush-output-port'() -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'flush-output-port'('current-output-port'())`](#flush-output-port-1).
<a name="flush-output-port-1"></a>

### 'flush-output-port'/1 ###


<pre><code>
'flush-output-port'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Flushes any buffered output from the buffer of output port to
the underlying file or device.</p>

<a name="get-output-bytevector-1"></a>

### 'get-output-bytevector'/1 ###


<pre><code>
'get-output-bytevector'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a>
</code></pre>

<br></br>


<p>Returns a bytevector consisting of the bytes that have been
output to the port so far in the order they were output.  It is an
error if port is not a binary output port.</p>

<a name="get-output-string-1"></a>

### 'get-output-string'/1 ###


<pre><code>
'get-output-string'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_string">scm_string()</a>
</code></pre>

<br></br>


<p>Returns a string consisting of the characters that have been
output to the port so far in the order they were output.  It is an
error if port is not a textual output port.</p>

<a name="input-port-open%3f-1"></a>

### 'input-port-open?'/1 ###


<pre><code>
'input-port-open?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if port is still open and capable of performing
input, otherwise returns #t.</p>

<a name="input-port%3f-1"></a>

### 'input-port?'/1 ###


<pre><code>
'input-port?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is an input port, otherwise returns #f.</p>

<a name="open-input-bytevector-1"></a>

### 'open-input-bytevector'/1 ###


<pre><code>
'open-input-bytevector'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>


<p>Returns a binary input port that delivers bytes from the given
bytevector.</p>

<a name="open-input-string-1"></a>

### 'open-input-string'/1 ###


<pre><code>
'open-input-string'(String::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>


<p>Returns a textual input port that delivers characters from the
given string.</p>

<a name="open-output-bytevector-0"></a>

### 'open-output-bytevector'/0 ###


<pre><code>
'open-output-bytevector'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>


<p>Returns a binary output port that will accumulate bytes for
retrieval by <code>get-output-bytevector</code>.</p>

<a name="open-output-string-0"></a>

### 'open-output-string'/0 ###


<pre><code>
'open-output-string'() -&gt; <a href="#type-scm_port">scm_port()</a>
</code></pre>

<br></br>


<p>Returns a textual output port that will accumulate characters
for retrieval by <code>get-output-string</code>.</p>

<a name="output-port-open%3f-1"></a>

### 'output-port-open?'/1 ###


<pre><code>
'output-port-open?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if port is still open and capable of performing
output, otherwise returns #t.</p>

<a name="output-port%3f-1"></a>

### 'output-port?'/1 ###


<pre><code>
'output-port?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is an output port, otherwise returns #f.</p>

<a name="peek-char-0"></a>

### 'peek-char'/0 ###


<pre><code>
'peek-char'() -&gt; <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


Equivalent to [`'peek-char'('current-input-port'())`](#peek-char-1).
<a name="peek-char-1"></a>

### 'peek-char'/1 ###


<pre><code>
'peek-char'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p>Returns the next character available from the textual input
port, but without updating the port to point to the following
character.  If no more characters are available, an end-of-file
object is returned.</p>

<a name="peek-u8-0"></a>

### 'peek-u8'/0 ###


<pre><code>
'peek-u8'() -&gt; <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


Equivalent to [`'peek-u8'('current-input-port'())`](#peek-u8-1).
<a name="peek-u8-1"></a>

### 'peek-u8'/1 ###


<pre><code>
'peek-u8'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p>Returns the next byte available from the binary input port,
but without updating the port to point to the following byte.  If
not more bytes are available, an end-of-file object is returned.</p>

<a name="port%3f-1"></a>

### 'port?'/1 ###


<pre><code>
'port?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is a port, otherwise returns #f.</p>

<a name="read-bytevector%21-1"></a>

### 'read-bytevector!'/1 ###


<pre><code>
'read-bytevector!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="read-bytevector%21-2"></a>

### 'read-bytevector!'/2 ###


<pre><code>
'read-bytevector!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="read-bytevector%21-3"></a>

### 'read-bytevector!'/3 ###


<pre><code>
'read-bytevector!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="read-bytevector%21-4"></a>

### 'read-bytevector!'/4 ###


<pre><code>
'read-bytevector!'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p><em>unsupported</em></p>

<a name="read-bytevector-1"></a>

### 'read-bytevector'/1 ###


<pre><code>
'read-bytevector'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


Equivalent to [`'read-bytevector'(K, 'current-input-port'())`](#read-bytevector-2).
<a name="read-bytevector-2"></a>

### 'read-bytevector'/2 ###


<pre><code>
'read-bytevector'(K::<a href="#type-scm_k">scm_k()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_bytevector">scm_bytevector()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p>Reads the next k bytes, or as many as are available before the
end of file, from the binary input port into a bytevector in
left-to-right order and returns the bytevector.  If no bytes are
available before the end of file, an end-of-file object is
returned.</p>

<a name="read-char-0"></a>

### 'read-char'/0 ###


<pre><code>
'read-char'() -&gt; <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


Equivalent to [`'read-char'('current-input-port'())`](#read-char-1).
<a name="read-char-1"></a>

### 'read-char'/1 ###


<pre><code>
'read-char'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p>Returns the next character available from the textual input
port, updating the port to point to the following character.  If no
more characters are available, an end-of-file object is returned.</p>

<a name="read-line-0"></a>

### 'read-line'/0 ###


<pre><code>
'read-line'() -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


Equivalent to [`'read-line'('current-input-port'())`](#read-line-1).
<a name="read-line-1"></a>

### 'read-line'/1 ###


<pre><code>
'read-line'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p>Returns the next line of text available from the textual input
port, updating the port to point to the following character.  If an
end of line is read, a string containing all of the text up to (but
not including) the end of file is returned, and the port is updated
to point just past the end of line.  If an end of file is
encountered before any end of line is read, but some characters
have been read, a string containing those characters is returned.
If an end of file is encountered before any characters are read, an
end-of-file object is returned.  For the purpose of this procedure,
an end of line consists of either a linefeed character, a carriage
return character, or a sequence of a carriage return character
followed by a linefeed character.</p>

<a name="read-string-1"></a>

### 'read-string'/1 ###


<pre><code>
'read-string'(K::<a href="#type-scm_k">scm_k()</a>) -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


Equivalent to [`'read-string'(K, 'current-input-port'())`](#read-string-2).
<a name="read-string-2"></a>

### 'read-string'/2 ###


<pre><code>
'read-string'(K::<a href="#type-scm_k">scm_k()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p>Reads the next k characters, or as many as are available
before the end of file, from the textual input port into a string
in left-to-right order and returns the string.  If no characters
are available before the end of file, an end-of-file object is
returned.</p>

<a name="read-u8-0"></a>

### 'read-u8'/0 ###


<pre><code>
'read-u8'() -&gt; <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


Equivalent to [`'read-u8'('current-input-port'())`](#read-u8-1).
<a name="read-u8-1"></a>

### 'read-u8'/1 ###


<pre><code>
'read-u8'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_eof">scm_eof()</a>
</code></pre>

<br></br>


<p>Returns the next byte available from the binary input port,
updating the port to point to the following byte.  If no more bytes
are available, an end-of-file object is returned.</p>

<a name="textual-port%3f-1"></a>

### 'textual-port?'/1 ###


<pre><code>
'textual-port?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if obj is a textual port, otherwise returns #f.</p>

<a name="u8-ready%3f-0"></a>

### 'u8-ready?'/0 ###


<pre><code>
'u8-ready?'() -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


Equivalent to [`'u8-ready?'('current-input-port'())`](#u8-ready%3f-1).
<a name="u8-ready%3f-1"></a>

### 'u8-ready?'/1 ###


<pre><code>
'u8-ready?'(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if a byte is ready on the binary input port or if
port is at the end of the file, otherwise returns #f.</p>

<a name="write-bytevector-1"></a>

### 'write-bytevector'/1 ###


<pre><code>
'write-bytevector'(V::<a href="#type-scm_bytevector">scm_bytevector()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'write-bytevector'(V, 'current-output-port'())`](#write-bytevector-2).
<a name="write-bytevector-2"></a>

### 'write-bytevector'/2 ###


<pre><code>
'write-bytevector'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'write-bytevector'(V, 'current-output-port'(), 0,'bytevector-length'(V))`](#write-bytevector-4).
<a name="write-bytevector-3"></a>

### 'write-bytevector'/3 ###


<pre><code>
'write-bytevector'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'write-bytevector'(V, 'current-output-port'(), Start,'bytevector-length'(V))`](#write-bytevector-4).
<a name="write-bytevector-4"></a>

### 'write-bytevector'/4 ###


<pre><code>
'write-bytevector'(Bytevector::<a href="#type-scm_bytevector">scm_bytevector()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Writes the bytes of bytevector from start to end in
left-to-right order to the binary output port.</p>

<a name="write-char-1"></a>

### 'write-char'/1 ###


<pre><code>
'write-char'(C::<a href="#type-scm_char">scm_char()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'write-char'(C, 'current-output-port'())`](#write-char-2).
<a name="write-char-2"></a>

### 'write-char'/2 ###


<pre><code>
'write-char'(Character::<a href="#type-scm_char">scm_char()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Writes the character char to the given textual output port.</p>

<a name="write-string-1"></a>

### 'write-string'/1 ###


<pre><code>
'write-string'(S::<a href="#type-scm_string">scm_string()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'write-string'(S, 'current-output-port'())`](#write-string-2).
<a name="write-string-2"></a>

### 'write-string'/2 ###


<pre><code>
'write-string'(String::<a href="#type-scm_string">scm_string()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'write-string'(S, 'current-output-port'(), 0)`](#write-string-3).
<a name="write-string-3"></a>

### 'write-string'/3 ###


<pre><code>
'write-string'(String::<a href="#type-scm_string">scm_string()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'write-string'(S, 'current-output-port'(), Start,'string-length'(S))`](#write-string-4).
<a name="write-string-4"></a>

### 'write-string'/4 ###


<pre><code>
'write-string'(S::<a href="#type-scm_string">scm_string()</a>, Port::<a href="#type-scm_port">scm_port()</a>, Start::<a href="#type-scm_start">scm_start()</a>, End::<a href="#type-scm_end">scm_end()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Writes the characters of string from start to end in
left-right order to the textual output port.</p>

<a name="write-u8-1"></a>

### 'write-u8'/1 ###


<pre><code>
'write-u8'(B::<a href="#type-scm_byte">scm_byte()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`'write-u8'(B, 'current-output-port'())`](#write-u8-2).
<a name="write-u8-2"></a>

### 'write-u8'/2 ###


<pre><code>
'write-u8'(B::<a href="#type-scm_byte">scm_byte()</a>, Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Writes the byte to the given binary output port.</p>

<a name="newline-0"></a>

### newline/0 ###


<pre><code>
newline() -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


Equivalent to [`newline('current-output-port'())`](#newline-1).
<a name="newline-1"></a>

### newline/1 ###


<pre><code>
newline(Port::<a href="#type-scm_port">scm_port()</a>) -&gt; <a href="#type-scm_false">scm_false()</a>
</code></pre>

<br></br>


<p>Writes an end of line to the textual output port.</p>

