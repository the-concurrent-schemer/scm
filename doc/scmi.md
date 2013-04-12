

# Module scmi #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-ana">ana()</a> ###



<pre><code>
ana() = <a href="scmi_analyze.md#type-ana">scmi_analyze:ana()</a>
</code></pre>





### <a name="type-arg">arg()</a> ###



<pre><code>
arg() = <a href="#type-scm_any">scm_any()</a>
</code></pre>





### <a name="type-body">body()</a> ###



<pre><code>
body() = <a href="#type-exec">exec()</a>
</code></pre>





### <a name="type-ccng">ccng()</a> ###



<pre><code>
ccng() = fun((<a href="#type-val">val()</a>) -&gt; no_return())
</code></pre>





### <a name="type-ccok">ccok()</a> ###



<pre><code>
ccok() = fun((<a href="#type-val">val()</a>, <a href="#type-ccng">ccng()</a>) -&gt; <a href="#type-val">val()</a>)
</code></pre>





### <a name="type-env">env()</a> ###



<pre><code>
env() = <a href="scmi_env.md#type-env">scmi_env:env()</a>
</code></pre>





### <a name="type-exec">exec()</a> ###



<pre><code>
exec() = fun((<a href="#type-env">env()</a>, <a href="#type-ccok">ccok()</a>, <a href="#type-ccng">ccng()</a>) -&gt; <a href="#type-val">val()</a>)
</code></pre>





### <a name="type-f">f()</a> ###



<pre><code>
f() = <a href="#type-f0">f0()</a> | <a href="#type-fn">fn()</a> | <a href="#type-fv">fv()</a> | <a href="#type-fnv">fnv()</a>
</code></pre>





### <a name="type-f0">f0()</a> ###



<pre><code>
f0() = fun(() -&gt; <a href="#type-val">val()</a>)
</code></pre>





### <a name="type-fn">fn()</a> ###



<pre><code>
fn() = fun((...) -&gt; <a href="#type-val">val()</a>)
</code></pre>





<pre><code>fun((arg(),...) -> val()).</code></pre>





### <a name="type-fnv">fnv()</a> ###



<pre><code>
fnv() = fun((...) -&gt; <a href="#type-val">val()</a>)
</code></pre>





<pre><code>fun((arg(),...,vargs()) -> val()).</code></pre>





### <a name="type-fv">fv()</a> ###



<pre><code>
fv() = fun((<a href="#type-vargs">vargs()</a>) -&gt; <a href="#type-val">val()</a>)
</code></pre>





### <a name="type-nip">nip()</a> ###



<pre><code>
nip() = <a href="#type-nip0">nip0()</a> | <a href="#type-nipn">nipn()</a> | <a href="#type-nipv">nipv()</a> | <a href="#type-nipnv">nipnv()</a>
</code></pre>





### <a name="type-nip0">nip0()</a> ###



<pre><code>
nip0() = #nip0{val = <a href="#type-f0">f0()</a>}
</code></pre>





### <a name="type-nipn">nipn()</a> ###



<pre><code>
nipn() = #nipn{val = <a href="#type-fn">fn()</a> | [<a href="#type-fn">fn()</a>]}
</code></pre>





### <a name="type-nipnv">nipnv()</a> ###



<pre><code>
nipnv() = #nipnv{val = <a href="#type-fnv">fnv()</a>}
</code></pre>





### <a name="type-nipv">nipv()</a> ###



<pre><code>
nipv() = #nipv{val = <a href="#type-fv">fv()</a>}
</code></pre>





### <a name="type-p">p()</a> ###



<pre><code>
p() = <a href="#type-p0">p0()</a> | <a href="#type-pn">pn()</a> | <a href="#type-pv">pv()</a> | <a href="#type-pnv">pnv()</a>
</code></pre>





### <a name="type-p0">p0()</a> ###



<pre><code>
p0() = {<a href="#type-body">body()</a>, <a href="#type-env">env()</a>}
</code></pre>





### <a name="type-param">param()</a> ###



<pre><code>
param() = <a href="#type-var">var()</a>
</code></pre>





### <a name="type-params">params()</a> ###



<pre><code>
params() = [<a href="#type-param">param()</a>]
</code></pre>





### <a name="type-pn">pn()</a> ###



<pre><code>
pn() = {<a href="#type-params">params()</a>, <a href="#type-body">body()</a>, <a href="#type-env">env()</a>}
</code></pre>





### <a name="type-pnv">pnv()</a> ###



<pre><code>
pnv() = {pos_integer(), <a href="#type-params">params()</a>, <a href="#type-body">body()</a>, <a href="#type-env">env()</a>}
</code></pre>





### <a name="type-proc">proc()</a> ###



<pre><code>
proc() = <a href="#type-proc0">proc0()</a> | <a href="#type-procn">procn()</a> | <a href="#type-procv">procv()</a> | <a href="#type-procnv">procnv()</a>
</code></pre>





### <a name="type-proc0">proc0()</a> ###



<pre><code>
proc0() = #proc0{val = <a href="#type-p0">p0()</a>}
</code></pre>





### <a name="type-procn">procn()</a> ###



<pre><code>
procn() = #procn{val = <a href="#type-pn">pn()</a> | [<a href="#type-pn">pn()</a>]}
</code></pre>





### <a name="type-procnv">procnv()</a> ###



<pre><code>
procnv() = #procnv{val = <a href="#type-pnv">pnv()</a>}
</code></pre>





### <a name="type-procv">procv()</a> ###



<pre><code>
procv() = #procv{val = <a href="#type-pv">pv()</a>}
</code></pre>





### <a name="type-pv">pv()</a> ###



<pre><code>
pv() = {<a href="#type-param">param()</a>, <a href="#type-body">body()</a>, <a href="#type-env">env()</a>}
</code></pre>





### <a name="type-val">val()</a> ###



<pre><code>
val() = <a href="#type-scm_any">scm_any()</a>
</code></pre>





### <a name="type-var">var()</a> ###



<pre><code>
var() = <a href="#type-scm_symbol">scm_symbol()</a> | reference()
</code></pre>





### <a name="type-vargs">vargs()</a> ###



<pre><code>
vargs() = [<a href="#type-arg">arg()</a>]
</code></pre>





### <a name="type-xf">xf()</a> ###



<pre><code>
xf() = <a href="#type-xf0">xf0()</a> | <a href="#type-xfn">xfn()</a> | <a href="#type-xfv">xfv()</a> | <a href="#type-xfnv">xfnv()</a>
</code></pre>





### <a name="type-xf0">xf0()</a> ###



<pre><code>
xf0() = fun((<a href="#type-scmi_env">scmi_env()</a>, <a href="#type-scmi_ccok">scmi_ccok()</a>, <a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-val">val()</a>)
</code></pre>





### <a name="type-xfn">xfn()</a> ###



<pre><code>
xfn() = fun((...) -&gt; <a href="#type-val">val()</a>)
</code></pre>





<pre><code>fun((arg(),... ,scmi_env(), scmi_ccok(), scmi_ccng()) -> val()).</code></pre>





### <a name="type-xfnv">xfnv()</a> ###



<pre><code>
xfnv() = fun((...) -&gt; <a href="#type-val">val()</a>)
</code></pre>





<pre><code>fun((arg(),... ,vargs(), scmi_env(), scmi_ccok(), scmi_ccng()) -> val()).</code></pre>





### <a name="type-xfv">xfv()</a> ###



<pre><code>
xfv() = fun((<a href="#type-vargs">vargs()</a>, <a href="#type-scmi_env">scmi_env()</a>, <a href="#type-scmi_ccok">scmi_ccok()</a>, <a href="#type-scmi_ccng">scmi_ccng()</a>) -&gt; <a href="#type-val">val()</a>)
</code></pre>





### <a name="type-xnip">xnip()</a> ###



<pre><code>
xnip() = <a href="#type-xnip0">xnip0()</a> | <a href="#type-xnipn">xnipn()</a> | <a href="#type-xnipv">xnipv()</a> | <a href="#type-xnipnv">xnipnv()</a>
</code></pre>





### <a name="type-xnip0">xnip0()</a> ###



<pre><code>
xnip0() = #xnip0{val = <a href="#type-f0">f0()</a>}
</code></pre>





### <a name="type-xnipn">xnipn()</a> ###



<pre><code>
xnipn() = #xnipn{val = <a href="#type-fn">fn()</a> | [<a href="#type-fn">fn()</a>]}
</code></pre>





### <a name="type-xnipnv">xnipnv()</a> ###



<pre><code>
xnipnv() = #xnipnv{val = <a href="#type-fnv">fnv()</a>}
</code></pre>





### <a name="type-xnipv">xnipv()</a> ###



<pre><code>
xnipv() = #xnipv{val = <a href="#type-fv">fv()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#make_and-1">make_and/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_begin-1">make_begin/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_call-2">make_call/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_case-2">make_case/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_case-4">make_case/4</a></td><td></td></tr><tr><td valign="top"><a href="#make_cond-1">make_cond/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_cond-2">make_cond/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_cond_expand-1">make_cond_expand/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_cond_expand-2">make_cond_expand/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_define-2">make_define/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_do-2">make_do/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_do-3">make_do/3</a></td><td></td></tr><tr><td valign="top"><a href="#make_do-4">make_do/4</a></td><td></td></tr><tr><td valign="top"><a href="#make_if-2">make_if/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_if-3">make_if/3</a></td><td></td></tr><tr><td valign="top"><a href="#make_lambda-2">make_lambda/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_let-2">make_let/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_let_named-3">make_let_named/3</a></td><td></td></tr><tr><td valign="top"><a href="#make_letrec-2">make_letrec/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_letrecs-2">make_letrecs/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_lets-2">make_lets/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_not-1">make_not/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_or-1">make_or/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_quote-1">make_quote/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_set-2">make_set/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_unless-2">make_unless/2</a></td><td></td></tr><tr><td valign="top"><a href="#make_variable-0">make_variable/0</a></td><td></td></tr><tr><td valign="top"><a href="#make_when-2">make_when/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="make_and-1"></a>

### make_and/1 ###

`make_and(Exps) -> any()`


<a name="make_begin-1"></a>

### make_begin/1 ###

`make_begin(Exps) -> any()`


<a name="make_call-2"></a>

### make_call/2 ###

`make_call(Proc, Args) -> any()`


<a name="make_case-2"></a>

### make_case/2 ###

`make_case(Exp, Exps) -> any()`


<a name="make_case-4"></a>

### make_case/4 ###

`make_case(Exp, Exps, Else, X4) -> any()`


<a name="make_cond-1"></a>

### make_cond/1 ###

`make_cond(Exps) -> any()`


<a name="make_cond-2"></a>

### make_cond/2 ###

`make_cond(Exps, Else) -> any()`


<a name="make_cond_expand-1"></a>

### make_cond_expand/1 ###

`make_cond_expand(Exps) -> any()`


<a name="make_cond_expand-2"></a>

### make_cond_expand/2 ###

`make_cond_expand(Exps, Else) -> any()`


<a name="make_define-2"></a>

### make_define/2 ###

`make_define(Variable, Value) -> any()`


<a name="make_do-2"></a>

### make_do/2 ###

`make_do(Test, Results) -> any()`


<a name="make_do-3"></a>

### make_do/3 ###

`make_do(Test, Results, Specs) -> any()`


<a name="make_do-4"></a>

### make_do/4 ###

`make_do(Test, Results, Specs, Commands) -> any()`


<a name="make_if-2"></a>

### make_if/2 ###

`make_if(Test, Consequent) -> any()`


<a name="make_if-3"></a>

### make_if/3 ###

`make_if(Test, Consequent, Alternate) -> any()`


<a name="make_lambda-2"></a>

### make_lambda/2 ###

`make_lambda(Formals, Body) -> any()`


<a name="make_let-2"></a>

### make_let/2 ###

`make_let(Bindings, Body) -> any()`


<a name="make_let_named-3"></a>

### make_let_named/3 ###

`make_let_named(Tag, Bindings, Body) -> any()`


<a name="make_letrec-2"></a>

### make_letrec/2 ###

`make_letrec(Bindings, Body) -> any()`


<a name="make_letrecs-2"></a>

### make_letrecs/2 ###

`make_letrecs(Bindings, Body) -> any()`


<a name="make_lets-2"></a>

### make_lets/2 ###

`make_lets(Bindings, Body) -> any()`


<a name="make_not-1"></a>

### make_not/1 ###

`make_not(Exp) -> any()`


<a name="make_or-1"></a>

### make_or/1 ###

`make_or(Exps) -> any()`


<a name="make_quote-1"></a>

### make_quote/1 ###

`make_quote(Exps) -> any()`


<a name="make_set-2"></a>

### make_set/2 ###

`make_set(Variable, Value) -> any()`


<a name="make_unless-2"></a>

### make_unless/2 ###

`make_unless(Test, Exps) -> any()`


<a name="make_variable-0"></a>

### make_variable/0 ###

`make_variable() -> any()`


<a name="make_when-2"></a>

### make_when/2 ###

`make_when(Test, Exps) -> any()`


