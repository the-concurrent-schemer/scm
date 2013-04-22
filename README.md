

# The Concurrent Schemer #

Copyright (c) 2013 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<p>"The Concurrent Schemer" (CSCM) is an implementation of the Scheme
programming language built on top of the Erlang Virtual Machine.  CSCM
combines the sequential programming model of Scheme with the
concurrent, distributed, and fault-tolerant programming model of
Erlang.  Simply put, the goals of CSCM are concurrent Scheme,
distributed Scheme, and fault-tolerant Scheme.</p>
<p>Scheme is a statically scoped and properly tail recursive dialect of
the Lisp programming language invented by Guy Lewis Steele Jr. and
Gerald Jay Sussman at MIT.  Scheme was designed to have exceptionally
clear and simple semantics and few different ways to form expressions.
A wide variety of programming paradigms, including imperative,
functional, and object-oriented styles, find convenient expression in
Scheme.</p>
<p>Erlang is a programming language used to build massively scalable,
soft, real-time systems with requirements on high availability.
Erlang's run-time system has built-in support for concurrency,
distribution and fault tolerance.  Erlang is designed at the Ericsson
Computer Science Laboratory and is maintained as an open-source
project and community.</p>
<p>The default language is Scheme R7RS.  The default virtual machine is
Erlang/OTP R16B or higher.</p>
<p>For further information, see
<a href="https://github.com/the-concurrent-schemer">https://github.com/the-concurrent-schemer</a>.</p>


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scm_app.md" class="module">scm_app</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scm_sup.md" class="module">scm_sup</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_parse.md" class="module">scmd_parse</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_parse_numR.md" class="module">scmd_parse_numR</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_scan.md" class="module">scmd_scan</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_scan_num10.md" class="module">scmd_scan_num10</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_scan_num16.md" class="module">scmd_scan_num16</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_scan_num2.md" class="module">scmd_scan_num2</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_scan_num8.md" class="module">scmd_scan_num8</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_types.md" class="module">scmd_types</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmd_types_impl.md" class="module">scmd_types_impl</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmi_analyze.md" class="module">scmi_analyze</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmi_analyze_derived.md" class="module">scmi_analyze_derived</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmi_analyze_primitive.md" class="module">scmi_analyze_primitive</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmi_analyze_program.md" class="module">scmi_analyze_program</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmi_analyze_syntax.md" class="module">scmi_analyze_syntax</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmi_env.md" class="module">scmi_env</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmi_eval.md" class="module">scmi_eval</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmi_types.md" class="module">scmi_types</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml.md" class="module">scml</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_boolean.md" class="module">scml_base_boolean</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_bytevector.md" class="module">scml_base_bytevector</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_char.md" class="module">scml_base_char</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_control.md" class="module">scml_base_control</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_equality.md" class="module">scml_base_equality</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_exception.md" class="module">scml_base_exception</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_io.md" class="module">scml_base_io</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_list.md" class="module">scml_base_list</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_number.md" class="module">scml_base_number</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_string.md" class="module">scml_base_string</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_symbol.md" class="module">scml_base_symbol</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_system.md" class="module">scml_base_system</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_base_vector.md" class="module">scml_base_vector</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_char.md" class="module">scml_char</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_complex.md" class="module">scml_complex</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_context.md" class="module">scml_context</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_cxr.md" class="module">scml_cxr</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_eval.md" class="module">scml_eval</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_file.md" class="module">scml_file</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_inexact.md" class="module">scml_inexact</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_lambda.md" class="module">scml_lambda</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_lazy.md" class="module">scml_lazy</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_load.md" class="module">scml_load</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_r5rs.md" class="module">scml_r5rs</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_read.md" class="module">scml_read</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_repl.md" class="module">scml_repl</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_time.md" class="module">scml_time</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scml_write.md" class="module">scml_write</a></td></tr>
<tr><td><a href="https://github.com/the-concurrent-schemer/scm/blob/master/doc/scmtmp.md" class="module">scmtmp</a></td></tr></table>

