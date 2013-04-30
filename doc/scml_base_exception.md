

# Module scml_base_exception #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme base library for exceptions</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#%24scml_exports-0">'$scml_exports'/0</a></td><td></td></tr><tr><td valign="top"><a href="#error-object-irritants-1">'error-object-irritants'/1</a></td><td><p>Returns the list of the irritants encapsulated by the
<code>error-object</code>.</p>.</td></tr><tr><td valign="top"><a href="#error-object-message-1">'error-object-message'/1</a></td><td><p>Returns the message encapsulated by the <code>error-object</code>.</p>.</td></tr><tr><td valign="top"><a href="#error-object%3f-1">'error-object?'/1</a></td><td><p>Returns #t if the given object is an object created by
<code>error</code>, raised by the <code>read</code> procedure, or raised by the inability
to open an input or output port on a file.</p>.</td></tr><tr><td valign="top"><a href="#file-error%3f-1">'file-error?'/1</a></td><td><p>Returns #t if the given object is an object raised by the
inability to open an input or output port on a file.</p>.</td></tr><tr><td valign="top"><a href="#raise-continuable-4">'raise-continuable'/4</a></td><td><p>Raises an exception by invoking the current exception handler
on <code>Obj</code>.  The handler is called with the same dynamic environment
as the call to <code>raise-continuable</code>, except that: (1) the current
exception handler is the one that was in place when the handler
being called was installed, and (2) if the handler being called
returns, then it will again become the current exception handler.
If the handler returns, the values it returns become the values
returned by the call to <code>raise-continuable</code>.</p>.</td></tr><tr><td valign="top"><a href="#read-error%3f-1">'read-error?'/1</a></td><td><p>Returns #t if the given object is an object raised by the
<code>read</code> procedure.</p>.</td></tr><tr><td valign="top"><a href="#with-exception-handler-5">'with-exception-handler'/5</a></td><td><p>Returns the results of invoking <code>Thunk</code>. <code>Handler</code> is
installed as the current exception handler in the dynamic
environment used for the invocation of <code>Thunk</code>.  It is an error if
<code>Handler</code> does not accept one argument.  It is also an error if
<code>Thunk</code> does not accept zero arguments.</p>.</td></tr><tr><td valign="top"><a href="#error-4">error/4</a></td><td><p>Raises an exception as if by calling <code>raise</code> on a newly
allocated implementation-defined object which encapsulates the
information provided by <code>Message</code>, as well as any objects, known as
the <code>Irritants</code>. The procedure <code>error-object?</code> must return #t on
such objects.</p>.</td></tr><tr><td valign="top"><a href="#raise-4">raise/4</a></td><td><p>Raises an exception by invoking the current exception handler
on <code>Obj</code>.  The handler is called with the same dynamic environment
as that of the call to <code>raise</code>, except that the current exception
handler is the one that was in place when the handler being called
was installed.  If the handler returns, a secondary exception is
raised in the same dynamic environment as the handler.</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="%24scml_exports-0"></a>

### '$scml_exports'/0 ###


<pre><code>
'$scml_exports'() -&gt; [{<a href="#type-scm_symbol">scm_symbol()</a>, <a href="#type-scmi_nip">scmi_nip()</a>}]
</code></pre>

<br></br>



<a name="error-object-irritants-1"></a>

### 'error-object-irritants'/1 ###


<pre><code>
'error-object-irritants'(Error_file::<a href="#type-scm_error">scm_error()</a>) -&gt; [<a href="#type-scm_obj">scm_obj()</a>]
</code></pre>

<br></br>


<p>Returns the list of the irritants encapsulated by the
<code>error-object</code>.</p>

<a name="error-object-message-1"></a>

### 'error-object-message'/1 ###


<pre><code>
'error-object-message'(Error_file::<a href="#type-scm_error">scm_error()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>


<p>Returns the message encapsulated by the <code>error-object</code>.</p>

<a name="error-object%3f-1"></a>

### 'error-object?'/1 ###


<pre><code>
'error-object?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if the given object is an object created by
<code>error</code>, raised by the <code>read</code> procedure, or raised by the inability
to open an input or output port on a file.</p>

<a name="file-error%3f-1"></a>

### 'file-error?'/1 ###


<pre><code>
'file-error?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if the given object is an object raised by the
inability to open an input or output port on a file.</p>

<a name="raise-continuable-4"></a>

### 'raise-continuable'/4 ###


<pre><code>
'raise-continuable'(Obj::<a href="#type-scm_obj">scm_obj()</a>, Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>


<p>Raises an exception by invoking the current exception handler
on <code>Obj</code>.  The handler is called with the same dynamic environment
as the call to <code>raise-continuable</code>, except that: (1) the current
exception handler is the one that was in place when the handler
being called was installed, and (2) if the handler being called
returns, then it will again become the current exception handler.
If the handler returns, the values it returns become the values
returned by the call to <code>raise-continuable</code>.</p>

<a name="read-error%3f-1"></a>

### 'read-error?'/1 ###


<pre><code>
'read-error?'(Obj::<a href="#type-scm_obj">scm_obj()</a>) -&gt; <a href="#type-scm_boolean">scm_boolean()</a>
</code></pre>

<br></br>


<p>Returns #t if the given object is an object raised by the
<code>read</code> procedure.</p>

<a name="with-exception-handler-5"></a>

### 'with-exception-handler'/5 ###


<pre><code>
'with-exception-handler'(Handler::<a href="#type-scm_proc">scm_proc()</a>, Thunk::<a href="#type-scm_thunk">scm_thunk()</a>, Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>


<p>Returns the results of invoking <code>Thunk</code>. <code>Handler</code> is
installed as the current exception handler in the dynamic
environment used for the invocation of <code>Thunk</code>.  It is an error if
<code>Handler</code> does not accept one argument.  It is also an error if
<code>Thunk</code> does not accept zero arguments.</p>

<a name="error-4"></a>

### error/4 ###


<pre><code>
error(Irritants::[<a href="#type-scm_obj">scm_obj()</a>, ...], Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>


<p>Raises an exception as if by calling <code>raise</code> on a newly
allocated implementation-defined object which encapsulates the
information provided by <code>Message</code>, as well as any objects, known as
the <code>Irritants</code>. The procedure <code>error-object?</code> must return #t on
such objects.</p>

<a name="raise-4"></a>

### raise/4 ###


<pre><code>
raise(Obj::<a href="#type-scm_obj">scm_obj()</a>, Env::<a href="#type-scmi_env">scmi_env()</a>, Ok::<a href="#type-scmi_ccok">scmi_ccok()</a>, Ng::<a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-scm_obj">scm_obj()</a>
</code></pre>

<br></br>


<p>Raises an exception by invoking the current exception handler
on <code>Obj</code>.  The handler is called with the same dynamic environment
as that of the call to <code>raise</code>, except that the current exception
handler is the one that was in place when the handler being called
was installed.  If the handler returns, a secondary exception is
raised in the same dynamic environment as the handler.</p>

