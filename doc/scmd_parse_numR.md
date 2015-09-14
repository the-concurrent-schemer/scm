

# Module scmd_parse_numR #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-yecc_ret">yecc_ret()</a> ###


<pre><code>
yecc_ret() = {error, term()} | {ok, term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_and_scan-1">parse_and_scan/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_number-3">to_number/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format_error-1"></a>

### format_error/1 ###

<pre><code>
format_error(Message::any()) -&gt; [char() | list()]
</code></pre>
<br />

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Tokens::list()) -&gt; <a href="#type-yecc_ret">yecc_ret()</a>
</code></pre>
<br />

<a name="parse_and_scan-1"></a>

### parse_and_scan/1 ###

<pre><code>
parse_and_scan(X1::{function() | {atom(), atom()}, [term()]} | {atom(), atom(), [term()]}) -&gt; <a href="#type-yecc_ret">yecc_ret()</a>
</code></pre>
<br />

<a name="to_number-3"></a>

### to_number/3 ###

<pre><code>
to_number(N::<a href="scmd_types_impl.md#type-lineno">scmd_types_impl:lineno()</a>, X::string(), Env::<a href="#type-scmi_denv">scmi_denv()</a>) -&gt; {ok, <a href="scmd_types_impl.md#type-e_number">scmd_types_impl:e_number()</a>} | {error, <a href="scmd_types_impl.md#type-lineno">scmd_types_impl:lineno()</a>, Msg::iolist()}
</code></pre>
<br />

