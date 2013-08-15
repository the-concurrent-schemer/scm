

# Module scmi_eval #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


<p>Scheme interpreter expression evaluator</p>.
__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_ng-1">default_ng/1</a></td><td></td></tr><tr><td valign="top"><a href="#default_ok-2">default_ok/2</a></td><td></td></tr><tr><td valign="top"><a href="#eval-1">eval/1</a></td><td></td></tr><tr><td valign="top"><a href="#eval-2">eval/2</a></td><td></td></tr><tr><td valign="top"><a href="#eval-3">eval/3</a></td><td></td></tr><tr><td valign="top"><a href="#eval-4">eval/4</a></td><td></td></tr><tr><td valign="top"><a href="#exec-1">exec/1</a></td><td></td></tr><tr><td valign="top"><a href="#exec-2">exec/2</a></td><td></td></tr><tr><td valign="top"><a href="#exec-3">exec/3</a></td><td></td></tr><tr><td valign="top"><a href="#exec-4">exec/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_ng-1"></a>

### default_ng/1 ###


<pre><code>
default_ng(Error::<a href="#type-scmi_exp">scmi_exp()</a>) -&gt; no_return()
</code></pre>

<br></br>



<a name="default_ok-2"></a>

### default_ok/2 ###


<pre><code>
default_ok(Value::<a href="#type-scmi_exp">scmi_exp()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



<a name="eval-1"></a>

### eval/1 ###


<pre><code>
eval(Exp::<a href="#type-scmi_exp">scmi_exp()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



<a name="eval-2"></a>

### eval/2 ###


<pre><code>
eval(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



<a name="eval-3"></a>

### eval/3 ###


<pre><code>
eval(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



<a name="eval-4"></a>

### eval/4 ###


<pre><code>
eval(Exp::<a href="#type-scmi_exp">scmi_exp()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



<a name="exec-1"></a>

### exec/1 ###


<pre><code>
exec(Exec::<a href="#type-scmi_dexec">scmi_dexec()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



<a name="exec-2"></a>

### exec/2 ###


<pre><code>
exec(Exec::<a href="#type-scmi_dexec">scmi_dexec()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



<a name="exec-3"></a>

### exec/3 ###


<pre><code>
exec(Exec::<a href="#type-scmi_dexec">scmi_dexec()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



<a name="exec-4"></a>

### exec/4 ###


<pre><code>
exec(Exec::<a href="#type-scmi_dexec">scmi_dexec()</a>, Env::<a href="#type-scmi_denv">scmi_denv()</a>, Ok::<a href="#type-scmi_dok">scmi_dok()</a>, Ng::<a href="#type-scmi_dng">scmi_dng()</a>) -&gt; <a href="#type-scmi_exp">scmi_exp()</a>
</code></pre>

<br></br>



