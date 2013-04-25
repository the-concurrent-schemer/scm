%%% The MIT License
%%%
%%% Copyright (C) 2013 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Scheme interpreter syntactic analyzer for primitive expressions
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_analyze_primitive).

%% External exports
-export([analyze_lambda/2
         , analyze_sequence/2
         , analyze_application/2, apply/5
         , analyze_if/2
         , analyze_assignment/2
         , analyze_include/2
         , analyze_include_ci/2
         , analyze_include_lib/2
         , analyze_include_lib_ci/2
        ]).

%% Internal imports
-import(scmi_analyze, [analyze/2, validate_variables/1, validate_variable/1, splitnv_arguments/2]).
-import(scmi_analyze_derived, [scan_out_internal_definitions/1]).

-include("scmi_analyze.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

analyze_lambda([[]|Body], Ana) ->
    Exec = analyze_sequence(scan_out_internal_definitions(Body), Ana),
    Src = fun() -> Body end,
    fun(Env, Ok, Ng) -> Ok(#lip0{val=#l0{body=Exec, env=Env, src=Src}}, Ng) end;
analyze_lambda([Variable|Body], Ana) when not is_list(Variable) ->
    validate_variable(Variable),
    Exec = analyze_sequence(scan_out_internal_definitions(Body), Ana),
    Src = fun() -> Body end,
    fun(Env, Ok, Ng) -> Ok(#lipv{val=#lv{param=Variable, body=Exec, env=Env, src=Src}}, Ng) end;
analyze_lambda([[Variables|Variable]=Vs|Body], Ana) when not is_list(Variable) ->
    validate_variables(Vs),
    AllVariables = Variables ++ [Variable],
    Exec = analyze_sequence(scan_out_internal_definitions(Body), Ana),
    Src = fun() -> Body end,
    fun(Env, Ok, Ng) -> Ok(#lipnv{val=#lnv{n=length(Variables), params=AllVariables, body=Exec, env=Env, src=Src}}, Ng) end;
analyze_lambda([Variables|Body], Ana) when is_list(Variables) ->
    validate_variables(Variables),
    Exec = analyze_sequence(scan_out_internal_definitions(Body), Ana),
    Src = fun() -> Body end,
    fun(Env, Ok, Ng) -> Ok(#lipn{val=#ln{params=Variables, body=Exec, env=Env, src=Src}}, Ng) end.

analyze_sequence(Exps, Ana) ->
    sequentially([ analyze(Exp, Ana) || Exp <- Exps ]).

analyze_application([Operator|Operands], Ana) when is_list(Operands) ->
    FExec = analyze(Operator, Ana),
    AExecs = [ analyze(Operand, Ana) || Operand <- Operands ],
    fun(Env, Ok, Ng) ->
            FExec(Env,
                  fun(Proc, Ng1) ->
                          get_args(AExecs, Env,
                                   fun(Args, Ng2) -> apply(Proc, Args, Env, Ok, Ng2) end,
                                   Ng1)
                  end,
                  Ng)
    end;
analyze_application(Exp, Ana) ->
    erlang:error(badarg, [Exp, Ana]).

apply(#nip0{val=Fun}, [], _Env, Ok, Ng) ->
    Ok(apply_nip0(Fun), Ng);
apply(#nipn{val=FunOrFuns}, Args, _Env, Ok, Ng) ->
    Ok(apply_nipn(FunOrFuns, Args), Ng);
apply(#nipv{val=Fun}, Args, _Env, Ok, Ng) ->
    Ok(apply_nipv(Fun, Args), Ng);
apply(#nipnv{val=Fun}, Args, _Env, Ok, Ng) ->
    Ok(apply_nipnv(Fun, Args), Ng);
apply(#xnip0{val=Fun}, [], Env, Ok, Ng) ->
    apply_xnip0(Fun, Env, Ok, Ng);
apply(#xnipn{val=FunOrFuns}, Args, Env, Ok, Ng) ->
    apply_xnipn(FunOrFuns, Args, Env, Ok, Ng);
apply(#xnipv{val=Fun}, Args, Env, Ok, Ng) ->
    apply_xnipv(Fun, Args, Env, Ok, Ng);
apply(#xnipnv{val=Fun}, Args, Env, Ok, Ng) ->
    apply_xnipnv(Fun, Args, Env, Ok, Ng);
apply(#lip0{val=Proc}, [], _Env, Ok, Ng) ->
    apply_proc0(Proc, Ok, Ng);
apply(#lipn{val=Proc}, Args, _Env, Ok, Ng) ->
    apply_procn(Proc, Args, Ok, Ng);
apply(#lipv{val=Proc}, Args, _Env, Ok, Ng) ->
    apply_procv(Proc, Args, Ok, Ng);
apply(#lipnv{val=Proc}, Args, _Env, Ok, Ng) ->
    apply_procnv(Proc, Args, Ok, Ng);
apply(Proc, Args, Env, Ok, Ng) ->
    erlang:error(badarg, [Proc, Args, Env, Ok, Ng]).

analyze_if([Test, Consequent, Alternate], Ana) ->
    TExec = analyze(Test, Ana),
    CExec = analyze(Consequent, Ana),
    AExec = analyze(Alternate, Ana),
    fun(Env, Ok, Ng) ->
            %% execute test
            TExec(Env,
                  fun(?FALSE, Ng1) ->
                          %% execute alternative
                          AExec(Env, Ok, Ng1);
                     (_, Ng1) ->
                          %% execute consequent
                          CExec(Env, Ok, Ng1)
                  end,
                  Ng)
    end;
analyze_if([Test, Consequent], Ana) ->
    analyze_if([Test, Consequent, ?FALSE], Ana).

analyze_assignment([Variable, Exp], Ana) ->
    validate_variable(Variable),
    Exec = analyze(Exp, Ana),
    fun(Env, Ok, Ng) ->
            %% execute operands
            Exec(Env,
                 %% @TODO optimize get, save, and restore operations
                 fun(Val, Ng1) ->
                         %% get old value
                         OldVal = scmi_env:safe_lookup_variable(Variable, Env),
                         %% save new value
                         scmi_env:set_variable(Variable, Val, Env),
                         Ok(?FALSE,
                            fun(Err) ->
                                    %% restore old value upon error
                                    scmi_env:set_variable(Variable, OldVal, Env),
                                    Ng1(Err)
                            end)
                 end,
                 Ng)
    end.

analyze_include(Ss, Ana) ->
    sequentially([ includer(include_pp(S), Ana) || S <- Ss ]).

analyze_include_ci(Exp, Ana) ->
    erlang:error(unsupported, [Exp, Ana]).

analyze_include_lib(Ss, Ana) ->
    sequentially([ includer_lib(include_pp(S), Ana) || S <- Ss ]).

analyze_include_lib_ci(Exp, Ana) ->
    erlang:error(unsupported, [Exp, Ana]).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

sequentially([]) ->
    erlang:error(badarg, [[]]);
sequentially([A]) ->
    A;
sequentially([A, B|Cs]) ->
    sequentially([sequentially(A, B)|Cs]).

sequentially(A, B) ->
    fun(Env, Ok, Ng) ->
            A(Env,
              fun(_AVal, Ng1) -> B(Env, Ok, Ng1) end,
              Ng)
    end.

get_args([], _Env, Ok, Ng) ->
    Ok([], Ng);
get_args([AExec|AExecs], Env, Ok, Ng) ->
    AExec(Env,
          fun(Arg, Ng1) ->
                  get_args(AExecs, Env,
                           fun(Args, Ng2) -> Ok([Arg|Args], Ng2) end,
                           Ng1)
          end,
          Ng).

apply_nip0(Fun) ->
    Fun().

apply_nipn(Fun, Args) when is_function(Fun) ->
    erlang:apply(Fun, Args);
apply_nipn(Funs, Args) ->
    apply_nipn(Funs, Args, length(Args)).

apply_nipn([Fun|_Funs], Args, Arity) when is_function(Fun, Arity) ->
    erlang:apply(Fun, Args);
apply_nipn([_Fun|Funs], Args, Arity) ->
    apply_nipn(Funs, Args, Arity);
apply_nipn([], Args, Arity) ->
    erlang:error(badarg, [[], Args, Arity]).

apply_nipv(Fun, Args) ->
    Fun(Args).

apply_nipnv(Fun, Args) ->
    {arity, N} = erlang:fun_info(Fun, arity),
    ArgsN = splitnv_arguments(N-1, Args),
    erlang:apply(Fun, ArgsN).

apply_xnip0(Fun, Env, Ok, Ng) ->
    Fun(Env, Ok, Ng).

apply_xnipn(Funs, Args, Env, Ok, Ng) ->
    apply_nipn(Funs, Args ++ [Env, Ok, Ng]).

apply_xnipv(Fun, Args, Env, Ok, Ng) ->
    Fun(Args, Env, Ok, Ng).

apply_xnipnv(Fun, Args, Env, Ok, Ng) ->
    {arity, N} = erlang:fun_info(Fun, arity),
    ArgsN = splitnv_arguments(N-4, Args),
    erlang:apply(Fun, ArgsN ++ [Env, Ok, Ng]).

apply_proc0(#l0{body=Exec, env=BaseEnv}, Ok, Ng) ->
    Exec(scmi_env:extend([], [], BaseEnv), Ok, Ng).

apply_procn(#ln{params=Parameters, body=Exec, env=BaseEnv}, Args, Ok, Ng) ->
    Exec(scmi_env:extend(Parameters, Args, BaseEnv), Ok, Ng).

apply_procv(#lv{param=Parameter, body=Exec, env=BaseEnv}, Args, Ok, Ng) ->
    Exec(scmi_env:extend([Parameter], [Args], BaseEnv), Ok, Ng).

apply_procnv(#lnv{n=N, params=Parameters, body=Exec, env=BaseEnv}, Args, Ok, Ng) ->
    ArgsN = splitnv_arguments(N, Args),
    Exec(scmi_env:extend(Parameters, ArgsN, BaseEnv), Ok, Ng).

include_pp(#string{val=Val}) when Val /= {} ->
    case filename:split(tuple_to_list(Val)) of
        [[$$|Start]|Rest]=All ->
            case os:getenv(Start) of
                false ->
                    filename:join(All);
                EnvStart ->
                    NewStart = filename:join(filename:split(EnvStart)),
                    filename:join([NewStart|Rest])
            end;
        All ->
            filename:join(All)
    end;
include_pp(Exp) ->
    erlang:error(badarg, [Exp]).

includer(Name, Ana) ->
    case filelib:is_regular(Name) of
        true ->
            includer_analyze(Name, Ana);
        false ->
            case filename:pathtype(Name) of
                absolute ->
                    erlang:error(badarg, [Name, Ana]);
                _ ->
                    includer_path(Name, Ana)
            end
    end.

includer_path(Name, #ana{file=undefined, path=Path}=Ana) ->
    includer_path(Path, Name, Ana);
includer_path(Name, #ana{file=File, path=Path}=Ana) ->
    DirName = filename:dirname(File),
    includer_path([DirName|Path], Name, Ana).

includer_path([], Name, Ana) ->
    erlang:error(badarg, [[], Name, Ana]);
includer_path([H|T], Name, Ana) ->
    Path = filename:join(H, Name),
    case filelib:is_regular(Path) of
        true ->
            includer_analyze(Path, Ana);
        false ->
            includer_path(T, Name, Ana)
    end.

includer_lib(Name, Ana) ->
    case filename:pathtype(Name) of
        absolute ->
            erlang:error(badarg, [Name, Ana]);
        _ ->
            [AppName|Rest] = filename:split(Name),
            case code:lib_dir(list_to_atom(AppName)) of
                {error, bad_name} ->
                    erlang:error(badarg, [Name, Ana]);
                DirName ->
                    Path = filename:join(DirName, Rest),
                    case filelib:is_regular(Path) of
                        true ->
                            includer_analyze(Path, Ana);
                        false ->
                            erlang:error(badarg, [Name, Ana])
                    end
            end
    end.

includer_analyze(Name, Ana) ->
    case scmd_parse:file(Name) of
        {ok, Exp} ->
            analyze(Exp, Ana#ana{file=Name});
        {error, Reason} ->
            erlang:error(Reason, [Name, Ana])
    end.
