

# Module scmi_types #
* [Description](#description)
* [Data Types](#types)

<p>Scheme interpreter types (for Erlang types and specs)</p>.

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).

<a name="types"></a>

## Data Types ##




### <a name="type-arg">arg()</a> ###


<pre><code>
arg() = <a href="#type-exp">exp()</a>
</code></pre>




### <a name="type-body">body()</a> ###


<pre><code>
body() = <a href="#type-dexec">dexec()</a>
</code></pre>




### <a name="type-denv">denv()</a> ###


<pre><code>
denv() = <a href="scmi_env.md#type-env">scmi_env:env()</a>
</code></pre>




### <a name="type-dexec">dexec()</a> ###


<pre><code>
dexec() = fun((<a href="#type-denv">denv()</a>, <a href="#type-dok">dok()</a>, <a href="#type-dng">dng()</a>) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>




### <a name="type-dng">dng()</a> ###


<pre><code>
dng() = fun((<a href="#type-exp">exp()</a>) -&gt; no_return())
</code></pre>




### <a name="type-dok">dok()</a> ###


<pre><code>
dok() = fun((<a href="#type-exp">exp()</a>, <a href="#type-dng">dng()</a>) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>




### <a name="type-eof">eof()</a> ###


<pre><code>
eof() = '?EOF'
</code></pre>


<pre><code>?EOF</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = <a href="#type-error_user">error_user()</a> | <a href="#type-error_read">error_read()</a> | <a href="#type-error_file">error_file()</a>
</code></pre>




### <a name="type-error_file">error_file()</a> ###


<pre><code>
error_file() = #error_file{val = <a href="#type-exp">exp()</a>}
</code></pre>




### <a name="type-error_read">error_read()</a> ###


<pre><code>
error_read() = #error_read{val = <a href="#type-exp">exp()</a>}
</code></pre>




### <a name="type-error_user">error_user()</a> ###


<pre><code>
error_user() = #error_user{val = [<a href="#type-exp">exp()</a>, ...]}
</code></pre>




### <a name="type-exception">exception()</a> ###


<pre><code>
exception() = #exception{val = [<a href="#type-signal">signal()</a>]} | #cexception{val = [<a href="#type-signal">signal()</a>]}
</code></pre>




### <a name="type-exp">exp()</a> ###


<pre><code>
exp() = <a href="#type-scm_any">scm_any()</a>
</code></pre>




### <a name="type-expander">expander()</a> ###


<pre><code>
expander() = #expander{val = <a href="#type-sexec">sexec()</a>}
</code></pre>




### <a name="type-f">f()</a> ###


<pre><code>
f() = <a href="#type-f0">f0()</a> | <a href="#type-fn">fn()</a> | <a href="#type-fv">fv()</a> | <a href="#type-fnv">fnv()</a>
</code></pre>




### <a name="type-f0">f0()</a> ###


<pre><code>
f0() = fun(() -&gt; <a href="#type-exp">exp()</a>)
</code></pre>




### <a name="type-fn">fn()</a> ###


<pre><code>
fn() = fun((...) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>


<pre><code>fun((arg(),...) -> exp()).</code></pre>




### <a name="type-fnv">fnv()</a> ###


<pre><code>
fnv() = fun((...) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>


<pre><code>fun((arg(),...,vargs()) -> exp()).</code></pre>




### <a name="type-fv">fv()</a> ###


<pre><code>
fv() = fun((<a href="#type-vargs">vargs()</a>) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>




### <a name="type-iodev">iodev()</a> ###


<pre><code>
iodev() = <a href="scmi_iodev.md#type-iodev">scmi_iodev:iodev()</a>
</code></pre>




### <a name="type-l">l()</a> ###


<pre><code>
l() = <a href="#type-l0">l0()</a> | <a href="#type-ln">ln()</a> | <a href="#type-lv">lv()</a> | <a href="#type-lnv">lnv()</a>
</code></pre>




### <a name="type-l0">l0()</a> ###


<pre><code>
l0() = #l0{body = <a href="#type-body">body()</a>, env = <a href="#type-denv">denv()</a>, src = <a href="#type-src">src()</a>}
</code></pre>




### <a name="type-lip">lip()</a> ###


<pre><code>
lip() = <a href="#type-lip0">lip0()</a> | <a href="#type-lipn">lipn()</a> | <a href="#type-lipv">lipv()</a> | <a href="#type-lipnv">lipnv()</a>
</code></pre>




### <a name="type-lip0">lip0()</a> ###


<pre><code>
lip0() = #lip0{val = <a href="#type-l0">l0()</a>}
</code></pre>




### <a name="type-lipn">lipn()</a> ###


<pre><code>
lipn() = #lipn{val = <a href="#type-ln">ln()</a> | [<a href="#type-ln">ln()</a>]}
</code></pre>




### <a name="type-lipnv">lipnv()</a> ###


<pre><code>
lipnv() = #lipnv{val = <a href="#type-lnv">lnv()</a>}
</code></pre>




### <a name="type-lipv">lipv()</a> ###


<pre><code>
lipv() = #lipv{val = <a href="#type-lv">lv()</a>}
</code></pre>




### <a name="type-ln">ln()</a> ###


<pre><code>
ln() = #ln{params = <a href="#type-params">params()</a>, body = <a href="#type-body">body()</a>, env = <a href="#type-denv">denv()</a>, src = <a href="#type-src">src()</a>}
</code></pre>




### <a name="type-lnv">lnv()</a> ###


<pre><code>
lnv() = #lnv{n = pos_integer(), params = <a href="#type-params">params()</a>, body = <a href="#type-body">body()</a>, env = <a href="#type-denv">denv()</a>, src = <a href="#type-src">src()</a>}
</code></pre>




### <a name="type-lv">lv()</a> ###


<pre><code>
lv() = #lv{param = <a href="#type-param">param()</a>, body = <a href="#type-body">body()</a>, env = <a href="#type-denv">denv()</a>, src = <a href="#type-src">src()</a>}
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




### <a name="type-param">param()</a> ###


<pre><code>
param() = <a href="#type-var">var()</a>
</code></pre>




### <a name="type-params">params()</a> ###


<pre><code>
params() = [<a href="#type-param">param()</a>]
</code></pre>




### <a name="type-proc">proc()</a> ###


<pre><code>
proc() = <a href="#type-nip">nip()</a> | <a href="#type-xnip">xnip()</a> | <a href="#type-lip">lip()</a>
</code></pre>




### <a name="type-senv">senv()</a> ###


<pre><code>
senv() = <a href="scmi_analyze.md#type-senv">scmi_analyze:senv()</a>
</code></pre>




### <a name="type-sexec">sexec()</a> ###


<pre><code>
sexec() = fun((<a href="#type-exp">exp()</a>, <a href="#type-senv">senv()</a>) -&gt; <a href="#type-expander">expander()</a> | <a href="#type-dexec">dexec()</a>)
</code></pre>




### <a name="type-signal">signal()</a> ###


<pre><code>
signal() = #signal{obj = <a href="#type-exp">exp()</a>, env = <a href="#type-denv">denv()</a>, ok = <a href="#type-dok">dok()</a>, ng = <a href="#type-dng">dng()</a>}
</code></pre>




### <a name="type-src">src()</a> ###


<pre><code>
src() = fun(() -&gt; [<a href="#type-exp">exp()</a>])
</code></pre>




### <a name="type-thunk">thunk()</a> ###


<pre><code>
thunk() = <a href="#type-nip0">nip0()</a> | <a href="#type-xnip0">xnip0()</a> | <a href="#type-lip0">lip0()</a>
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
xf0() = fun((<a href="#type-denv">denv()</a>, <a href="#type-dok">dok()</a>, <a href="#type-dng">dng()</a>) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>




### <a name="type-xfn">xfn()</a> ###


<pre><code>
xfn() = fun((...) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>


<pre><code>fun((arg(),... ,denv(), dok(), dng()) -> exp()).</code></pre>




### <a name="type-xfnv">xfnv()</a> ###


<pre><code>
xfnv() = fun((...) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>


<pre><code>fun((arg(),... ,vargs(), denv(), dok(), dng()) -> exp()).</code></pre>




### <a name="type-xfv">xfv()</a> ###


<pre><code>
xfv() = fun((<a href="#type-vargs">vargs()</a>, <a href="#type-denv">denv()</a>, <a href="#type-dok">dok()</a>, <a href="#type-dng">dng()</a>) -&gt; <a href="#type-exp">exp()</a>)
</code></pre>




### <a name="type-xnip">xnip()</a> ###


<pre><code>
xnip() = <a href="#type-xnip0">xnip0()</a> | <a href="#type-xnipn">xnipn()</a> | <a href="#type-xnipv">xnipv()</a> | <a href="#type-xnipnv">xnipnv()</a>
</code></pre>




### <a name="type-xnip0">xnip0()</a> ###


<pre><code>
xnip0() = #xnip0{val = <a href="#type-xf0">xf0()</a>}
</code></pre>




### <a name="type-xnipn">xnipn()</a> ###


<pre><code>
xnipn() = #xnipn{val = <a href="#type-xfn">xfn()</a> | [<a href="#type-xfn">xfn()</a>]}
</code></pre>




### <a name="type-xnipnv">xnipnv()</a> ###


<pre><code>
xnipnv() = #xnipnv{val = <a href="#type-xfnv">xfnv()</a>}
</code></pre>




### <a name="type-xnipv">xnipv()</a> ###


<pre><code>
xnipv() = #xnipv{val = <a href="#type-xfv">xfv()</a>}
</code></pre>

