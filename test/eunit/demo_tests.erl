%%% The MIT License
%%%
%%% Copyright (C) 2013-2015 by Joseph Wayne Norton <norton@alum.mit.edu>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

%%% @doc Scheme demo tests
%%% @author CSCM Contributor <the-concurrent-schemer@googlegroups.com>

-module(demo_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("scm/src/scmi.hrl").


%%%===================================================================
%%% API
%%%===================================================================

hello_world_test() ->
    Str = "(define hello-world (lambda () (display \"Hello World!\")))",
    Env = scmi_env:the_empty(),
    False = ?FALSE,
    Proc = {nipv, 0, fun(Args) -> io:format("~p~n", [Args]), False end},
    scmi_env:define_variable('display', Proc, Env),
    {ok, Exp} = scmd_parse:string(Str),
    scmi_eval:eval(Exp, Env),
    R = scmi_eval:eval(['hello-world'], Env),
    ?assertEqual(R, False).

basic_test_() ->
    {"A suite of basic tests (adapted from http://norvig.com/lispy2.html) for demo purposes.",
     {setup,
      fun start_basic_tests/0,
      fun stop_basic_tests/1,
      fun exec_basic_tests/1}}.

start_basic_tests() ->
    BaseEnv = scmi_env:the_empty(),
    scml:import_identifiers(BaseEnv, [scml_base]).

stop_basic_tests(_) ->
    ok.

exec_basic_tests(Env) ->
    [ fun() -> 'assert_parse_eval_equal?'(Env, Status, A, B) end || {Status, A, B} <- basic_tests() ].


%% @TODO
%%   - fix each No Good (ng) case
%%   - double check each Error (error) case
%%   - replace the atom error for each Error case with it's explicit
%%     exit or error reason

basic_tests() ->
    [ {ok, "'(testing 1 (2.0) -3.14e159)", "(testing 1 (2.0) -3.14e+159)"}
    , {ok,  "(+ 2 2)", "4"}
    , {ok, "(+ (* 2 100) (* 1 10))", "210"}
    , {ok, "(if (> 6 5) (+ 1 1) (+ 2 2))", "2"}
    , {ok, "(if (< 6 5) (+ 1 1) (+ 2 2))", "4"}
    , {ok, "(define x 3)", "#f"}
    , {ok, "x", "3"}
    , {ok, "(+ x x)", "6"}
    , {ok, "(begin (define y 1) (set! y (+ y 1)) (+ y 1))", "3"}
    , {ok, "((lambda (x) (+ x x)) 5)", "10"}
    , {ok, "(define twice (lambda (x) (* 2 x)))", "#f"}
    , {ok, "(twice 5)", "10"}
    , {ok, "(define compose (lambda (f g) (lambda (x) (f (g x)))))", "#f"}
    , {ok, "((compose list twice) 5)", "(10)"}
    , {ok, "(define repeat (lambda (f) (compose f f)))", "#f"}
    , {ok, "((repeat twice) 5)", "20"}
    , {ok, "((repeat (repeat twice)) 5)", "80"}
    , {ok, "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))", "#f"}
    , {ok, "(fact 3)", "6"}
    , {ok, "(fact 50)", "30414093201713378043612608166064768844377641568960512000000000000"}
    , {ok, "(define abs (lambda (n) ((if (> n 0) + -) 0 n)))", "#f"}
    , {ok, "(list (abs -3) (abs 0) (abs 3))", "(3 0 3)"}
    , {ok, "(define combine (lambda (f)"
       "    (lambda (x y)"
       "      (if (null? x) (quote ())"
       "          (f (list (car x) (car y))"
       "             ((combine f) (cdr x) (cdr y)))))))", "#f"}
    , {ok, "(define zip (combine cons))", "#f"}
    , {ok, "(zip (list 1 2 3 4) (list 5 6 7 8))", "((1 5) (2 6) (3 7) (4 8))"}
    , {ok, "(define riff-shuffle (lambda (deck) (begin"
       "    (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))"
       "    (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))"
       "    (define mid (lambda (seq) (/ (length seq) 2)))"
       "    ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))", "#f"}
    , {ok, "(riff-shuffle (list 1 2 3 4 5 6 7 8))", "(1 5 2 6 3 7 4 8)"}
    , {ok, "((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))", "(1 3 5 7 2 4 6 8)"}
    , {ok, "(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))", "(1 2 3 4 5 6 7 8)"}
    , {ok, "()", error}
    , {ok, "(set! x)", error}
    , {ok, "(define 3 4)", error}
    , {ok, "(quote 1 2)", error}
    , {ok, "(if 1 2 3 4)", error}
    , {ok, "(lambda 3 3)", error}
    , {ok, "(lambda (x))", error}
    , {ok, "(if (= 1 2) (define-macro a 'a)"
       "    (define-macro a 'b))", error}
    , {ok, "(define (twice-again x) (* 2 x))", "#f"}
    , {ok, "(twice-again 2)", "4"}
    , {ok, "(twice-again 2 2)", error}
    , {ok, "(define lyst (lambda items items))", "#f"}
    , {ok, "(lyst 1 2 3 (+ 2 2))", "(1 2 3 4)"}
    , {ok, "(if 1 2)", "2"}
    , {ok, "(if (= 3 4) 2)", "#f"}
    , {ok, "(define (account bal) (lambda (amt) (set! bal (+ bal amt)) bal))", "#f"}
    , {ok, "(define a1 (account 100))", "#f"}
    , {ok, "(a1 0)", "100"}
    , {ok, "(a1 10)", "110"}
    , {ok, "(a1 10)", "120"}
    , {ok, "(define (newton guess function derivative epsilon)"
       "    (define guess2 (- guess (/ (function guess) (derivative guess))))"
       "    (if (< (abs (- guess guess2)) epsilon) guess2"
       "        (newton guess2 function derivative epsilon)))", "#f"}
    , {ok, "(define (square-root a)"
       "    (newton 1 (lambda (x) (- (* x x) a)) (lambda (x) (* 2 x)) 1e-8))", "#f"}
    , {ng, "(> (square-root 200.) 14.14213)", "#t"}
    , {ok, "(< (square-root 200.) 14.14215)", "#t"}
    , {ng, "(= (square-root 200.) (sqrt 200.))", "#t"}
    , {ok, "(define (sum-squares-range start end)"
       "         (define (sumsq-acc start end acc)"
       "            (if (> start end) acc (sumsq-acc (+ start 1) end (+ (* start start) acc))))"
       "         (sumsq-acc start end 0))", "#f"}
    , {ok, "(sum-squares-range 1 3000)", "9004500500"}
    , {ok, "(call/cc (lambda (throw) (+ 5 (* 10 (throw 1))))) ;; throw", "1"}
    , {ok, "(call/cc (lambda (throw) (+ 5 (* 10 1)))) ;; do not throw", "15"}
    , {ok, "(call/cc (lambda (throw)"
       "         (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (escape 3)))))))) ; 1 level", "35"}
    , {ok, "(call/cc (lambda (throw)"
       "         (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (throw 3)))))))) ; 2 levels", "3"}
    , {ok, "(call/cc (lambda (throw)"
       "         (+ 5 (* 10 (call/cc (lambda (escape) (* 100 1))))))) ; 0 levels", "1005"}
    , {ng, "(* 1i 1i)", "(-1+0i)"}
    , {ng, "(sqrt -1)", "1i"}
    , {ok, "(let ((a 1) (b 2)) (+ a b))", "3"}
    , {ok, "(let ((a 1) (b 2 3)) (+ a b))", error}
    , {ok, "(and 1 2 3)", "3"}
    , {ok, "(and (> 2 1) 2 3)", "3"}
    , {ok, "(and)", "#t"}
    , {ok, "(and (> 2 1) (> 2 3))", "#f"}
    , {ng, "(define-macro unless (lambda args `(if (not ,(car args)) (begin ,@(cdr args))))) ; test `", "#f"}
    , {ng, "(unless (= 2 (+ 1 1)) (display 2) 3 4)", "#f"}
    , {ng, "(unless (= 4 (+ 1 1)) (display 2) (display \"\n\") 3 4)", "4"}
    , {ok, "(quote x)", "x"}
    , {ok, "(quote (1 2 three))", "(1 2 three)"}
    , {ok, "'x", "x"}
    , {ok, "'(one 2 3)", "(one 2 3)"}
    , {ng, "(define L (list 1 2 3))", "#f"}
    , {ng, "`(testing ,@L testing)", "(testing 1 2 3 testing)"}
    , {ng, "`(testing ,L testing)", "(testing (1 2 3) testing)"}
    , {ok, "`,@L", error}
    , {ng, "'(1 ;test comments '"
       "     ;skip this line"
       "     2 ; more ; comments ; ) )"
       "     3) ; final comment", "(1 2 3)"}
    ].

%%%===================================================================
%%% internal helpers
%%%===================================================================

'assert_parse_eval_equal?'(Env, Status, A, B) ->
    try
        case scmd_parse:string(A) of
            {ok, ExpA} ->
                case scmd_parse:string(B) of
                    {ok, ExpB} ->
                        Exec = scmi_eval:eval(ExpA, Env),
                        case scml_base_equality:'equal?'(Exec, ExpB) of
                            ?FALSE when Status == ok ->
                                ?assertEqual({"A", A, Exec}, {"B", B, ExpB});
                            _ ->
                                ?assert(true)
                        end;
                    Err when Status == ok ->
                        ?assertEqual({"B", B, parse}, Err);
                    _ ->
                        ?assert(true)
                end;
            Err when Status == ok ->
                ?assertEqual({"A", A, parse}, Err);
            _ ->
                ?assert(true)
        end
    catch
        _:_ when Status == ok, B == error ->
            ?assert(true);
        _:_ when Status == ng, B /= error ->
            ?assert(true);
        X:Y ->
            ?assertEqual({"A", A, "B", B}, {X, Y})
    end.
