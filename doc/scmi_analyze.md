

# Module scmi_analyze #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme interpreter syntactic analyzer</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="types"></a>

## Data Types ##




### <a name="type-ana">ana()</a> ###



<pre><code>
ana() = #ana{}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#analyze-1">analyze/1</a></td><td></td></tr><tr><td valign="top"><a href="#analyze-2">analyze/2</a></td><td></td></tr><tr><td valign="top"><a href="#are_valid_variables-1">are_valid_variables/1</a></td><td></td></tr><tr><td valign="top"><a href="#classify-1">classify/1</a></td><td></td></tr><tr><td valign="top"><a href="#flatten_variables-1">flatten_variables/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_reserved_symbol-1">is_reserved_symbol/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_valid_variable-1">is_valid_variable/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_tmp_variables-1">make_tmp_variables/1</a></td><td></td></tr><tr><td valign="top"><a href="#splitnv_arguments-2">splitnv_arguments/2</a></td><td></td></tr><tr><td valign="top"><a href="#validate_variable-1">validate_variable/1</a></td><td></td></tr><tr><td valign="top"><a href="#validate_variables-1">validate_variables/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="analyze-1"></a>

### analyze/1 ###


<pre><code>
analyze(Exp::<a href="#type-scm_any">scm_any()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>



<a name="analyze-2"></a>

### analyze/2 ###


<pre><code>
analyze(Exp::<a href="#type-scm_any">scm_any()</a>, Ana::<a href="#type-scmi_ana">scmi_ana()</a>) -&gt; <a href="#type-scm_any">scm_any()</a>
</code></pre>

<br></br>



<a name="are_valid_variables-1"></a>

### are_valid_variables/1 ###


<pre><code>
are_valid_variables(Variable::[<a href="#type-scmi_var">scmi_var()</a>]) -&gt; boolean()
</code></pre>

<br></br>



<a name="classify-1"></a>

### classify/1 ###


<pre><code>
classify(Exp::<a href="#type-scm_any">scm_any()</a>) -&gt; atom() | {rectangular | polar, {atom(), atom()}}
</code></pre>

<br></br>



<a name="flatten_variables-1"></a>

### flatten_variables/1 ###


<pre><code>
flatten_variables(L::<a href="#type-scmi_var">scmi_var()</a> | [<a href="#type-scmi_var">scmi_var()</a>]) -&gt; [<a href="#type-scmi_var">scmi_var()</a>]
</code></pre>

<br></br>



<a name="is_reserved_symbol-1"></a>

### is_reserved_symbol/1 ###


<pre><code>
is_reserved_symbol(X1::<a href="#type-scmi_var">scmi_var()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="is_valid_variable-1"></a>

### is_valid_variable/1 ###


<pre><code>
is_valid_variable(Variable::<a href="#type-scmi_var">scmi_var()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="make_tmp_variables-1"></a>

### make_tmp_variables/1 ###


<pre><code>
make_tmp_variables(Formal::<a href="#type-scmi_var">scmi_var()</a> | [<a href="#type-scmi_var">scmi_var()</a>]) -&gt; <a href="#type-scmi_var">scmi_var()</a> | [<a href="#type-scmi_var">scmi_var()</a>]
</code></pre>

<br></br>



<a name="splitnv_arguments-2"></a>

### splitnv_arguments/2 ###


<pre><code>
splitnv_arguments(N::pos_integer(), L::[<a href="#type-scm_any">scm_any()</a>, ...]) -&gt; [<a href="#type-scm_any">scm_any()</a>, ...]
</code></pre>

<br></br>



<a name="validate_variable-1"></a>

### validate_variable/1 ###


<pre><code>
validate_variable(Variable::<a href="#type-scmi_var">scmi_var()</a>) -&gt; true
</code></pre>

<br></br>



<a name="validate_variables-1"></a>

### validate_variables/1 ###


<pre><code>
validate_variables(Variables::[<a href="#type-scmi_var">scmi_var()</a>]) -&gt; true
</code></pre>

<br></br>



