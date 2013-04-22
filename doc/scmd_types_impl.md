

# Module scmd_types_impl #
* [Description](#description)
* [Data Types](#types)


<p>Scheme datum implementation types (for Erlang types and specs)</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="types"></a>

## Data Types ##




### <a name="type-bytes">bytes()</a> ###



<pre><code>
bytes() = &lt;&lt;_:_*8&gt;&gt;
</code></pre>





<pre><code>0..255</code></pre>





### <a name="type-integer_non_zero">integer_non_zero()</a> ###



<pre><code>
integer_non_zero() = neg_integer() | pos_integer()
</code></pre>





### <a name="type-lineno">lineno()</a> ###



<pre><code>
lineno() = non_neg_integer()
</code></pre>





### <a name="type-s_any">s_any()</a> ###



<pre><code>
s_any(T) = <a href="#type-s_boolean">s_boolean()</a> | <a href="#type-s_number">s_number()</a> | <a href="#type-s_character">s_character()</a> | <a href="#type-s_string">s_string()</a> | <a href="#type-s_symbol">s_symbol()</a> | <a href="#type-s_bytevector">s_bytevector()</a> | <a href="#type-s_list">s_list</a>(T) | <a href="#type-s_list_nonempty">s_list_nonempty</a>(T) | <a href="#type-s_pair">s_pair</a>(T) | <a href="#type-s_vector">s_vector</a>(T) | <a href="#type-s_label">s_label</a>(T) | <a href="#type-s_labelref">s_labelref()</a> | <a href="#type-s_quote">s_quote</a>(T) | <a href="#type-s_quasiquote">s_quasiquote</a>(T) | <a href="#type-s_unquote">s_unquote</a>(T) | <a href="#type-s_unquote_splicing">s_unquote_splicing</a>(T)
</code></pre>





### <a name="type-s_boolean">s_boolean()</a> ###



<pre><code>
s_boolean() = #boolean{val = <a href="#type-t_boolean">t_boolean()</a>}
</code></pre>





### <a name="type-s_bytevector">s_bytevector()</a> ###



<pre><code>
s_bytevector() = #bytevector{val = <a href="#type-t_bytevector">t_bytevector()</a>}
</code></pre>





### <a name="type-s_character">s_character()</a> ###



<pre><code>
s_character() = #character{val = <a href="#type-t_character">t_character()</a>}
</code></pre>





### <a name="type-s_complex">s_complex()</a> ###



<pre><code>
s_complex() = <a href="#type-s_rectangular">s_rectangular()</a> | <a href="#type-s_polar">s_polar()</a> | <a href="#type-s_real">s_real()</a>
</code></pre>





### <a name="type-s_datum">s_datum()</a> ###



<pre><code>
s_datum() = <a href="#type-s_any">s_any</a>(<a href="#type-s_datum">s_datum()</a>)
</code></pre>





### <a name="type-s_denominator">s_denominator()</a> ###



<pre><code>
s_denominator() = <a href="#type-s_integer_exact_pos">s_integer_exact_pos()</a>
</code></pre>





### <a name="type-s_false">s_false()</a> ###



<pre><code>
s_false() = #boolean{val = <a href="#type-t_false">t_false()</a>}
</code></pre>





### <a name="type-s_finite">s_finite()</a> ###



<pre><code>
s_finite() = <a href="#type-s_finite_inexact">s_finite_inexact()</a> | <a href="#type-s_finite_exact">s_finite_exact()</a>
</code></pre>





### <a name="type-s_finite_exact">s_finite_exact()</a> ###



<pre><code>
s_finite_exact() = <a href="#type-s_rational">s_rational()</a> | <a href="#type-s_integer_exact">s_integer_exact()</a>
</code></pre>





### <a name="type-s_finite_inexact">s_finite_inexact()</a> ###



<pre><code>
s_finite_inexact() = <a href="#type-s_negzero">s_negzero()</a> | float() | <a href="#type-s_integer_inexact">s_integer_inexact()</a>
</code></pre>





### <a name="type-s_identifier">s_identifier()</a> ###



<pre><code>
s_identifier() = <a href="#type-t_identifier">t_identifier()</a>
</code></pre>





### <a name="type-s_infnan">s_infnan()</a> ###



<pre><code>
s_infnan() = '?PINF' | '?NINF' | '?PNAN' | '?NNAN'
</code></pre>





### <a name="type-s_integer">s_integer()</a> ###



<pre><code>
s_integer() = <a href="#type-s_integer_inexact">s_integer_inexact()</a> | <a href="#type-s_integer_exact">s_integer_exact()</a>
</code></pre>





### <a name="type-s_integer_exact">s_integer_exact()</a> ###



<pre><code>
s_integer_exact() = integer()
</code></pre>





### <a name="type-s_integer_exact_neg">s_integer_exact_neg()</a> ###



<pre><code>
s_integer_exact_neg() = neg_integer()
</code></pre>





### <a name="type-s_integer_exact_non_neg">s_integer_exact_non_neg()</a> ###



<pre><code>
s_integer_exact_non_neg() = non_neg_integer()
</code></pre>





### <a name="type-s_integer_exact_non_zero">s_integer_exact_non_zero()</a> ###



<pre><code>
s_integer_exact_non_zero() = <a href="#type-integer_non_zero">integer_non_zero()</a>
</code></pre>





### <a name="type-s_integer_exact_pos">s_integer_exact_pos()</a> ###



<pre><code>
s_integer_exact_pos() = pos_integer()
</code></pre>





### <a name="type-s_integer_inexact">s_integer_inexact()</a> ###



<pre><code>
s_integer_inexact() = float()
</code></pre>





<pre><code>trunc(float()) == float()</code></pre>





### <a name="type-s_label">s_label()</a> ###



<pre><code>
s_label(T) = #label{val = <a href="#type-t_label">t_label</a>(T)}
</code></pre>





### <a name="type-s_labelref">s_labelref()</a> ###



<pre><code>
s_labelref() = #labelref{val = <a href="#type-t_labelref">t_labelref()</a>}
</code></pre>





### <a name="type-s_letter">s_letter()</a> ###



<pre><code>
s_letter() = #character{val = <a href="#type-t_letter">t_letter()</a>}
</code></pre>





### <a name="type-s_list">s_list()</a> ###



<pre><code>
s_list(T) = <a href="#type-t_list">t_list</a>(T)
</code></pre>





### <a name="type-s_list_nonempty">s_list_nonempty()</a> ###



<pre><code>
s_list_nonempty(T) = <a href="#type-t_list_nonempty">t_list_nonempty</a>(T)
</code></pre>





### <a name="type-s_negzero">s_negzero()</a> ###



<pre><code>
s_negzero() = '?NZER'
</code></pre>





### <a name="type-s_number">s_number()</a> ###



<pre><code>
s_number() = <a href="#type-s_complex">s_complex()</a>
</code></pre>





### <a name="type-s_numerator">s_numerator()</a> ###



<pre><code>
s_numerator() = <a href="#type-s_integer">s_integer()</a>
</code></pre>





### <a name="type-s_pair">s_pair()</a> ###



<pre><code>
s_pair(T) = <a href="#type-t_pair">t_pair</a>(T)
</code></pre>





### <a name="type-s_polar">s_polar()</a> ###



<pre><code>
s_polar() = {polar, {Mag::<a href="#type-s_real">s_real()</a>, Ang::<a href="#type-s_rational">s_rational()</a>}}
</code></pre>





### <a name="type-s_quasiquote">s_quasiquote()</a> ###



<pre><code>
s_quasiquote(T) = #quasiquote{val = <a href="#type-t_quasiquote">t_quasiquote</a>(T)}
</code></pre>





### <a name="type-s_quote">s_quote()</a> ###



<pre><code>
s_quote(T) = #quote{val = <a href="#type-t_quote">t_quote</a>(T)}
</code></pre>





### <a name="type-s_rational">s_rational()</a> ###



<pre><code>
s_rational() = {<a href="#type-s_numerator">s_numerator()</a>, <a href="#type-s_denominator">s_denominator()</a>}
</code></pre>





### <a name="type-s_real">s_real()</a> ###



<pre><code>
s_real() = <a href="#type-s_infnan">s_infnan()</a> | <a href="#type-s_finite">s_finite()</a>
</code></pre>





### <a name="type-s_rectangular">s_rectangular()</a> ###



<pre><code>
s_rectangular() = {rectangular, {Real::<a href="#type-s_real">s_real()</a>, Imag::<a href="#type-s_real">s_real()</a>}}
</code></pre>





### <a name="type-s_string">s_string()</a> ###



<pre><code>
s_string() = #string{val = <a href="#type-t_string">t_string()</a>}
</code></pre>





### <a name="type-s_symbol">s_symbol()</a> ###



<pre><code>
s_symbol() = <a href="#type-t_symbol">t_symbol()</a>
</code></pre>





### <a name="type-s_true">s_true()</a> ###



<pre><code>
s_true() = #boolean{val = <a href="#type-t_true">t_true()</a>}
</code></pre>





### <a name="type-s_unquote">s_unquote()</a> ###



<pre><code>
s_unquote(T) = #unquote{val = <a href="#type-t_unquote">t_unquote</a>(T)}
</code></pre>





### <a name="type-s_unquote_splicing">s_unquote_splicing()</a> ###



<pre><code>
s_unquote_splicing(T) = #unquote_splicing{val = <a href="#type-t_unquote_splicing">t_unquote_splicing</a>(T)}
</code></pre>





### <a name="type-s_vector">s_vector()</a> ###



<pre><code>
s_vector(T) = #vector{val = <a href="#type-t_vector">t_vector</a>(T)}
</code></pre>





### <a name="type-t_boolean">t_boolean()</a> ###



<pre><code>
t_boolean() = boolean()
</code></pre>





### <a name="type-t_bytevector">t_bytevector()</a> ###



<pre><code>
t_bytevector() = <a href="#type-bytes">bytes()</a>
</code></pre>





### <a name="type-t_character">t_character()</a> ###



<pre><code>
t_character() = <a href="#type-unichar">unichar()</a>
</code></pre>





### <a name="type-t_false">t_false()</a> ###



<pre><code>
t_false() = false
</code></pre>





### <a name="type-t_identifier">t_identifier()</a> ###



<pre><code>
t_identifier() = atom() | {atom(), <a href="#type-utf8">utf8()</a>}
</code></pre>





<pre><code>atom(utf-8()) | {atom(sha(utf-8())), utf-8()}</code></pre>





### <a name="type-t_label">t_label()</a> ###



<pre><code>
t_label(T) = {non_neg_integer(), T}
</code></pre>





### <a name="type-t_labelref">t_labelref()</a> ###



<pre><code>
t_labelref() = non_neg_integer()
</code></pre>





### <a name="type-t_letter">t_letter()</a> ###



<pre><code>
t_letter() = 65..90 | 97..122
</code></pre>





### <a name="type-t_list">t_list()</a> ###



<pre><code>
t_list(T) = maybe_improper_list(T, T | [])
</code></pre>





### <a name="type-t_list_nonempty">t_list_nonempty()</a> ###



<pre><code>
t_list_nonempty(T) = nonempty_maybe_improper_list(T, T | [])
</code></pre>





### <a name="type-t_pair">t_pair()</a> ###



<pre><code>
t_pair(T) = nonempty_maybe_improper_list(T, T)
</code></pre>





### <a name="type-t_quasiquote">t_quasiquote()</a> ###



<pre><code>
t_quasiquote(T) = <a href="#type-t_list">t_list</a>(T)
</code></pre>





### <a name="type-t_quote">t_quote()</a> ###



<pre><code>
t_quote(T) = <a href="#type-t_list">t_list</a>(T)
</code></pre>





### <a name="type-t_string">t_string()</a> ###



<pre><code>
t_string() = <a href="#type-utf8">utf8()</a>
</code></pre>





### <a name="type-t_symbol">t_symbol()</a> ###



<pre><code>
t_symbol() = <a href="#type-t_identifier">t_identifier()</a>
</code></pre>





### <a name="type-t_true">t_true()</a> ###



<pre><code>
t_true() = true
</code></pre>





### <a name="type-t_unquote">t_unquote()</a> ###



<pre><code>
t_unquote(T) = <a href="#type-t_list">t_list</a>(T)
</code></pre>





### <a name="type-t_unquote_splicing">t_unquote_splicing()</a> ###



<pre><code>
t_unquote_splicing(T) = <a href="#type-t_list">t_list</a>(T)
</code></pre>





### <a name="type-t_vector">t_vector()</a> ###



<pre><code>
t_vector(T) = {T}
</code></pre>





<pre><code>tuple of any size</code></pre>





### <a name="type-unichar">unichar()</a> ###



<pre><code>
unichar() = <a href="#type-unichar_low">unichar_low()</a> | <a href="#type-unichar_high">unichar_high()</a>
</code></pre>





<pre><code>unicode "code point"</code></pre>





### <a name="type-unichar_high">unichar_high()</a> ###



<pre><code>
unichar_high() = 57344..1114111
</code></pre>





### <a name="type-unichar_low">unichar_low()</a> ###



<pre><code>
unichar_low() = 0..55295
</code></pre>





### <a name="type-utf8">utf8()</a> ###



<pre><code>
utf8() = &lt;&lt;_:_*8&gt;&gt;
</code></pre>





<pre><code>utf-8</code></pre>


