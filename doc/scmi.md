

# Module scmi #
* [Data Types](#types)



<a name="types"></a>

## Data Types ##




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
param() = <a href="#type-scm_symbol">scm_symbol()</a>
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





### <a name="type-vargs">vargs()</a> ###



<pre><code>
vargs() = [<a href="#type-arg">arg()</a>]
</code></pre>


