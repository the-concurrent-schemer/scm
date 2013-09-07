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

%% SCMI Exports
-export(['$scmi_exports'/0]).

%% External exports
-export([analyze_quote/2
         , analyze_lambda/2
         , analyze_sequence/2
         , analyze_application/2, analyze_proc_application/3, apply/5
         , analyze_if/2
         , analyze_assignment/2
         , analyze_include/2
         , analyze_include_ci/2
         , analyze_include_lib/2
         , analyze_include_lib_ci/2
        ]).

%% Internal imports
-import(scmi_analyze, [analyze/2, validate_variables/1, validate_variable/1, splitnv_arguments/2]).
-import(scmi_analyze_derived, [scan_out_internal_definitions/2]).

-include("scmi_analyze.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% SCMI Exports
%%%----------------------------------------------------------------------

-spec '$scmi_exports'() -> [{scm_symbol(), scmi_expander()}].
'$scmi_exports'() ->
    [{'quote', #expander{val=fun ?MODULE:'analyze_quote'/2}}
     , {'lambda', #expander{val=fun ?MODULE:'analyze_lambda'/2}}
     , {'if', #expander{val=fun ?MODULE:'analyze_if'/2}}
     , {'set!', #expander{val=fun ?MODULE:'analyze_assignment'/2}}
     , {'include', #expander{val=fun ?MODULE:'analyze_include'/2}}
     , {'include-ci', #expander{val=fun ?MODULE:'analyze_include_ci'/2}}
     , {'include-lib', #expander{val=fun ?MODULE:'analyze_include_lib'/2}}
     , {'include-lib-ci', #expander{val=fun ?MODULE:'analyze_include_lib_ci'/2}}
    ].

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec analyze_quote(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_quote([Exp], _SEnv) ->
    fun(_Env, Ok, Ng) -> Ok(Exp, Ng) end.

-spec analyze_lambda(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_lambda([[]|Body], SEnv) ->
    Exec = analyze_sequence(scan_out_internal_definitions(Body, SEnv), SEnv),
    Src = fun() -> Body end,
    fun(Env, Ok, Ng) -> Ok(#lip0{val=#l0{body=Exec, env=Env, src=Src}}, Ng) end;
analyze_lambda([Variable|Body], SEnv) when not is_list(Variable) ->
    validate_variable(Variable), % validate variable
    Exec = analyze_sequence(scan_out_internal_definitions(Body, SEnv), SEnv),
    Src = fun() -> Body end,
    fun(Env, Ok, Ng) -> Ok(#lipv{val=#lv{param=Variable, body=Exec, env=Env, src=Src}}, Ng) end;
analyze_lambda([[Variables|Variable]=Vs|Body], SEnv) when not is_list(Variable) ->
    validate_variables(Vs), % validate variables
    AllVariables = Variables ++ [Variable],
    Exec = analyze_sequence(scan_out_internal_definitions(Body, SEnv), SEnv),
    Src = fun() -> Body end,
    fun(Env, Ok, Ng) -> Ok(#lipnv{val=#lnv{n=length(Variables), params=AllVariables, body=Exec, env=Env, src=Src}}, Ng) end;
analyze_lambda([Variables|Body], SEnv) when is_list(Variables) ->
    validate_variables(Variables), % validate variables
    Exec = analyze_sequence(scan_out_internal_definitions(Body, SEnv), SEnv),
    Src = fun() -> Body end,
    fun(Env, Ok, Ng) -> Ok(#lipn{val=#ln{params=Variables, body=Exec, env=Env, src=Src}}, Ng) end.

-spec analyze_sequence(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_sequence(Exps, SEnv) ->
    sequentially([ analyze(Exp, SEnv) || Exp <- Exps ]).

-spec analyze_application(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_application([Operator|Operands], SEnv) when is_list(Operands) ->
    case analyze(Operator, SEnv) of
        #expander{val=Fun} ->
            Args = [ analyze(Operand, SEnv) || Operand <- Operands ],
            Fun(Args, SEnv);
        FExec ->
            AExecs = [ analyze(Operand, SEnv) || Operand <- Operands ],
            fun(Env, Ok, Ng) ->
                    FExec(Env,
                          fun(Proc, Ng1) ->
                                  get_args(AExecs, Env,
                                           fun(Args, Ng2) -> apply(Proc, Args, Env, Ok, Ng2) end,
                                           Ng1)
                          end,
                          Ng)
            end
    end;
analyze_application(Exp, SEnv) ->
    erlang:error(badarg, [Exp, SEnv]).

-spec analyze_proc_application(scmi_proc(), scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_proc_application(Proc, Operands, SEnv) when is_list(Operands) ->
    AExecs = [ analyze(Operand, SEnv) || Operand <- Operands ],
    fun(Env, Ok, Ng) ->
            get_args(AExecs, Env,
                     fun(Args, Ng1) -> apply(Proc, Args, Env, Ok, Ng1) end,
                     Ng)
    end;
analyze_proc_application(Proc, Operands, SEnv) ->
    erlang:error(badarg, [Proc, Operands, SEnv]).

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

-spec analyze_if(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_if([Test, Consequent, Alternate], SEnv) ->
    TExec = analyze(Test, SEnv),
    CExec = analyze(Consequent, SEnv),
    AExec = analyze(Alternate, SEnv),
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
analyze_if([Test, Consequent], SEnv) ->
    analyze_if([Test, Consequent, ?FALSE], SEnv).

-spec analyze_assignment(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_assignment([Variable, Exp], SEnv) ->
    validate_variable(Variable), % validate variable
    Exec = analyze(Exp, SEnv),
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

-spec analyze_include(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_include(Ss, SEnv) ->
    sequentially([ includer(include_pp(S), SEnv) || S <- Ss ]).

-spec analyze_include_ci(scmi_exp(), scmi_senv()) -> no_return().
analyze_include_ci(Exp, SEnv) ->
    erlang:error(unsupported, [Exp, SEnv]).

-spec analyze_include_lib(scmi_exp(), scmi_senv()) -> scmi_dexec().
analyze_include_lib(Ss, SEnv) ->
    sequentially([ includer_lib(include_pp(S), SEnv) || S <- Ss ]).

-spec analyze_include_lib_ci(scmi_exp(), scmi_senv()) -> no_return().
analyze_include_lib_ci(Exp, SEnv) ->
    erlang:error(unsupported, [Exp, SEnv]).

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

includer(Name, SEnv) ->
    case filelib:is_regular(Name) of
        true ->
            includer_analyze(Name, SEnv);
        false ->
            case filename:pathtype(Name) of
                absolute ->
                    erlang:error(badarg, [Name, SEnv]);
                _ ->
                    includer_path(Name, SEnv)
            end
    end.

includer_path(Name, #senv{file=undefined, path=Path}=SEnv) ->
    includer_path(Path, Name, SEnv);
includer_path(Name, #senv{file=File, path=Path}=SEnv) ->
    DirName = filename:dirname(File),
    includer_path([DirName|Path], Name, SEnv).

includer_path([], Name, SEnv) ->
    erlang:error(badarg, [[], Name, SEnv]);
includer_path([H|T], Name, SEnv) ->
    Path = filename:join(H, Name),
    case filelib:is_regular(Path) of
        true ->
            includer_analyze(Path, SEnv);
        false ->
            includer_path(T, Name, SEnv)
    end.

includer_lib(Name, SEnv) ->
    case filename:pathtype(Name) of
        absolute ->
            erlang:error(badarg, [Name, SEnv]);
        _ ->
            [AppName|Rest] = filename:split(Name),
            case code:lib_dir(list_to_atom(AppName)) of
                {error, bad_name} ->
                    erlang:error(badarg, [Name, SEnv]);
                DirName ->
                    Path = filename:join(DirName, Rest),
                    case filelib:is_regular(Path) of
                        true ->
                            includer_analyze(Path, SEnv);
                        false ->
                            erlang:error(badarg, [Name, SEnv])
                    end
            end
    end.

includer_analyze(Name, SEnv) ->
    case scmd_parse:file(Name) of
        {ok, Exp} ->
            analyze(Exp, SEnv#senv{file=Name});
        {error, Reason} ->
            erlang:error(Reason, [Name, SEnv])
    end.
