

# Module scmd_types #
* [Description](#description)
* [Data Types](#types)

<p>Scheme datum types (for Erlang types and specs)</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="types"></a>

## Data Types ##




### <a name="type-scm_alist">scm_alist()</a> ###


<pre><code>
scm_alist() = [<a href="#type-scm_pair">scm_pair()</a>]
</code></pre>


<pre><code>association list (list of pairs)</code></pre>




### <a name="type-scm_any">scm_any()</a> ###


<pre><code>
scm_any() = <a href="scmd_types_impl.md#type-s_any">scmd_types_impl:s_any</a>(<a href="#type-scm_any">scm_any()</a>) | <a href="#type-scm_alist">scm_alist()</a> | <a href="#type-scm_boolean">scm_boolean()</a> | <a href="#type-scm_byte">scm_byte()</a> | <a href="#type-scm_bytevector">scm_bytevector()</a> | <a href="#type-scm_char">scm_char()</a> | <a href="#type-scm_end">scm_end()</a> | <a href="#type-scm_k">scm_k()</a> | <a href="#type-scm_letter">scm_letter()</a> | <a href="#type-scm_list">scm_list()</a> | <a href="#type-scm_list_nonempty">scm_list_nonempty()</a> | <a href="#type-scm_n">scm_n()</a> | <a href="#type-scm_n_nonzero">scm_n_nonzero()</a> | <a href="#type-scm_n_pos">scm_n_pos()</a> | <a href="#type-scm_pair">scm_pair()</a> | <a href="#type-scm_port">scm_port()</a> | <a href="#type-scm_eof">scm_eof()</a> | <a href="#type-scm_proc">scm_proc()</a> | <a href="#type-scm_q">scm_q()</a> | <a href="#type-scm_start">scm_start()</a> | <a href="#type-scm_string">scm_string()</a> | <a href="#type-scm_symbol">scm_symbol()</a> | <a href="#type-scm_thunk">scm_thunk()</a> | <a href="#type-scm_vector">scm_vector()</a> | <a href="#type-scm_x">scm_x()</a> | <a href="#type-scm_y">scm_y()</a> | <a href="#type-scm_z">scm_z()</a> | <a href="#type-scm_exception">scm_exception()</a> | <a href="#type-scm_error">scm_error()</a>
</code></pre>




### <a name="type-scm_boolean">scm_boolean()</a> ###


<pre><code>
scm_boolean() = <a href="scmd_types_impl.md#type-s_boolean">scmd_types_impl:s_boolean()</a>
</code></pre>


<pre><code>boolean value (#t or #f)</code></pre>




### <a name="type-scm_byte">scm_byte()</a> ###


<pre><code>
scm_byte() = <a href="scmd_types_impl.md#type-s_byte">scmd_types_impl:s_byte()</a>
</code></pre>


<pre><code>exact integer 0 =< byte < 256</code></pre>




### <a name="type-scm_bytevector">scm_bytevector()</a> ###


<pre><code>
scm_bytevector() = <a href="scmd_types_impl.md#type-s_bytevector">scmd_types_impl:s_bytevector()</a>
</code></pre>


<pre><code>bytevector</code></pre>




### <a name="type-scm_char">scm_char()</a> ###


<pre><code>
scm_char() = <a href="scmd_types_impl.md#type-s_character">scmd_types_impl:s_character()</a>
</code></pre>


<pre><code>character</code></pre>




### <a name="type-scm_end">scm_end()</a> ###


<pre><code>
scm_end() = <a href="scmd_types_impl.md#type-s_integer_exact_non_neg">scmd_types_impl:s_integer_exact_non_neg()</a>
</code></pre>


<pre><code>exact non-negative integer</code></pre>




### <a name="type-scm_eof">scm_eof()</a> ###


<pre><code>
scm_eof() = <a href="scmi_types.md#type-eof">scmi_types:eof()</a>
</code></pre>


<pre><code>end-of-file</code></pre>




### <a name="type-scm_error">scm_error()</a> ###


<pre><code>
scm_error() = <a href="scmi_types.md#type-error">scmi_types:error()</a>
</code></pre>




### <a name="type-scm_exception">scm_exception()</a> ###


<pre><code>
scm_exception() = <a href="scmi_types.md#type-exception">scmi_types:exception()</a>
</code></pre>




### <a name="type-scm_false">scm_false()</a> ###


<pre><code>
scm_false() = <a href="scmd_types_impl.md#type-s_false">scmd_types_impl:s_false()</a>
</code></pre>


<pre><code>false</code></pre>




### <a name="type-scm_k">scm_k()</a> ###


<pre><code>
scm_k() = <a href="scmd_types_impl.md#type-s_integer_exact_non_neg">scmd_types_impl:s_integer_exact_non_neg()</a>
</code></pre>


<pre><code>exact non-negative integer</code></pre>




### <a name="type-scm_k_pos">scm_k_pos()</a> ###


<pre><code>
scm_k_pos() = <a href="scmd_types_impl.md#type-s_integer_exact_pos">scmd_types_impl:s_integer_exact_pos()</a>
</code></pre>


<pre><code>exact positive integer</code></pre>




### <a name="type-scm_letter">scm_letter()</a> ###


<pre><code>
scm_letter() = <a href="scmd_types_impl.md#type-s_letter">scmd_types_impl:s_letter()</a>
</code></pre>


<pre><code>alphabetic character</code></pre>




### <a name="type-scm_list">scm_list()</a> ###


<pre><code>
scm_list() = <a href="scmd_types_impl.md#type-s_list">scmd_types_impl:s_list</a>(<a href="#type-scm_any">scm_any()</a>)
</code></pre>


<pre><code>list</code></pre>




### <a name="type-scm_list_nonempty">scm_list_nonempty()</a> ###


<pre><code>
scm_list_nonempty() = <a href="scmd_types_impl.md#type-s_list_nonempty">scmd_types_impl:s_list_nonempty</a>(<a href="#type-scm_any">scm_any()</a>)
</code></pre>


<pre><code>non-empty list</code></pre>




### <a name="type-scm_n">scm_n()</a> ###


<pre><code>
scm_n() = <a href="scmd_types_impl.md#type-s_integer">scmd_types_impl:s_integer()</a>
</code></pre>


<pre><code>integer</code></pre>




### <a name="type-scm_n_nonzero">scm_n_nonzero()</a> ###


<pre><code>
scm_n_nonzero() = <a href="scmd_types_impl.md#type-s_integer_nonzero">scmd_types_impl:s_integer_nonzero()</a>
</code></pre>


<pre><code>non-zero integer</code></pre>




### <a name="type-scm_n_pos">scm_n_pos()</a> ###


<pre><code>
scm_n_pos() = <a href="scmd_types_impl.md#type-s_integer_pos">scmd_types_impl:s_integer_pos()</a>
</code></pre>


<pre><code>positive integer</code></pre>




### <a name="type-scm_obj">scm_obj()</a> ###


<pre><code>
scm_obj() = <a href="#type-scm_any">scm_any()</a>
</code></pre>


<pre><code>any object</code></pre>




### <a name="type-scm_pair">scm_pair()</a> ###


<pre><code>
scm_pair() = <a href="scmd_types_impl.md#type-s_pair">scmd_types_impl:s_pair</a>(<a href="#type-scm_any">scm_any()</a>)
</code></pre>


<pre><code>pair</code></pre>




### <a name="type-scm_port">scm_port()</a> ###


<pre><code>
scm_port() = <a href="scmi_types.md#type-iodev">scmi_types:iodev()</a>
</code></pre>


<pre><code>port</code></pre>




### <a name="type-scm_proc">scm_proc()</a> ###


<pre><code>
scm_proc() = <a href="scmi_types.md#type-proc">scmi_types:proc()</a>
</code></pre>


<pre><code>proc</code></pre>




### <a name="type-scm_q">scm_q()</a> ###


<pre><code>
scm_q() = <a href="scmd_types_impl.md#type-s_rational">scmd_types_impl:s_rational()</a>
</code></pre>


<pre><code>rational</code></pre>




### <a name="type-scm_start">scm_start()</a> ###


<pre><code>
scm_start() = <a href="scmd_types_impl.md#type-s_integer_exact_non_neg">scmd_types_impl:s_integer_exact_non_neg()</a>
</code></pre>


<pre><code>exact non-negative integer</code></pre>




### <a name="type-scm_string">scm_string()</a> ###


<pre><code>
scm_string() = <a href="scmd_types_impl.md#type-s_string">scmd_types_impl:s_string()</a>
</code></pre>


<pre><code>string</code></pre>




### <a name="type-scm_symbol">scm_symbol()</a> ###


<pre><code>
scm_symbol() = <a href="scmd_types_impl.md#type-s_symbol">scmd_types_impl:s_symbol()</a>
</code></pre>


<pre><code>symbol</code></pre>




### <a name="type-scm_thunk">scm_thunk()</a> ###


<pre><code>
scm_thunk() = <a href="scmi_types.md#type-thunk">scmi_types:thunk()</a>
</code></pre>


<pre><code>thunk</code></pre>




### <a name="type-scm_true">scm_true()</a> ###


<pre><code>
scm_true() = <a href="scmd_types_impl.md#type-s_true">scmd_types_impl:s_true()</a>
</code></pre>


<pre><code>true</code></pre>




### <a name="type-scm_vector">scm_vector()</a> ###


<pre><code>
scm_vector() = <a href="scmd_types_impl.md#type-s_vector">scmd_types_impl:s_vector</a>(<a href="#type-scm_any">scm_any()</a>)
</code></pre>


<pre><code>vector</code></pre>




### <a name="type-scm_x">scm_x()</a> ###


<pre><code>
scm_x() = <a href="scmd_types_impl.md#type-s_real">scmd_types_impl:s_real()</a>
</code></pre>


<pre><code>real number</code></pre>




### <a name="type-scm_y">scm_y()</a> ###


<pre><code>
scm_y() = <a href="scmd_types_impl.md#type-s_real">scmd_types_impl:s_real()</a>
</code></pre>


<pre><code>real number</code></pre>




### <a name="type-scm_z">scm_z()</a> ###


<pre><code>
scm_z() = <a href="scmd_types_impl.md#type-s_complex">scmd_types_impl:s_complex()</a>
</code></pre>


<pre><code>complex number</code></pre>


