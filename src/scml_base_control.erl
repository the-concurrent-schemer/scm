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

%%% @doc Scheme base library for control features
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_base_control).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['procedure?'/1
         , 'apply'/4
         , 'map'/4
         , 'string-map'/4
         , 'vector-map'/4
         , 'for-each'/4
         , 'string-for-each'/4
         , 'vector-for-each'/4
         , 'call/cc'/4
         , 'values'/4
         , 'call-with-values'/5
         , 'dynamic-wind'/6
        ]).

-import(scmi_analyze_primitive, [apply/5]).
-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

-type winder()  :: {Env::scmi_denv(), Before::scm_thunk(), After::scm_thunk()}.
-type winders() :: [winder()] | undefined.

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'procedure?', #nipn{val=fun 'procedure?'/1}}
     , {'apply', #xnipv{val=fun 'apply'/4}}
     , {'map', #xnipv{val=fun 'map'/4}}
     , {'string-map', #xnipv{val=fun 'string-map'/4}}
     , {'vector-map', #xnipv{val=fun 'vector-map'/4}}
     , {'for-each', #xnipv{val=fun 'for-each'/4}}
     , {'string-for-each', #xnipv{val=fun 'string-for-each'/4}}
     , {'vector-for-each', #xnipv{val=fun 'vector-for-each'/4}}
     , {'call/cc', #xnipn{val=fun 'call/cc'/4}}
     , {'call-with-current-continuation', #xnipn{val=fun 'call/cc'/4}}
     , {'values', #xnipv{val=fun 'values'/4}}
     , {'call-with-values', #xnipn{val=fun 'call-with-values'/5}}
     , {'dynamic-wind', #xnipn{val=fun 'dynamic-wind'/6}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns #t if obj is a procedure, otherwise returns #f.
-spec 'procedure?'(scm_obj()) -> scm_boolean().
'procedure?'(#nip0{}) ->
    ?TRUE;
'procedure?'(#nipn{}) ->
    ?TRUE;
'procedure?'(#nipv{}) ->
    ?TRUE;
'procedure?'(#nipnv{}) ->
    ?TRUE;
'procedure?'(#xnip0{}) ->
    ?TRUE;
'procedure?'(#xnipn{}) ->
    ?TRUE;
'procedure?'(#xnipv{}) ->
    ?TRUE;
'procedure?'(#xnipnv{}) ->
    ?TRUE;
'procedure?'(#lip0{}) ->
    ?TRUE;
'procedure?'(#lipn{}) ->
    ?TRUE;
'procedure?'(#lipv{}) ->
    ?TRUE;
'procedure?'(#lipnv{}) ->
    ?TRUE;
'procedure?'(_) ->
    ?FALSE.

%% @doc Calls +Proc+ with the elements of the list +(append (list arg1
%% ...) args)+ as the actual arguments.
-spec 'apply'([scm_any(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> scm_any().
'apply'([Proc|[Args|Arg]], Env, Ok, Ng) when is_list(Args), not is_list(Arg) ->
    apply(Proc, Args ++ [Arg], Env, Ok, Ng);
'apply'([Proc|Args], Env, Ok, Ng) when is_list(Args) ->
    apply(Proc, Args, Env, Ok, Ng).

%% @doc Applies proc element-wise to the elements of the lists and
%% returns a list of the results, in order.  If more than one list is
%% given and not all lists have the same length, map terminates when
%% the shortest list runs out.  The dynamic order in which proc is
%% applied to the elements of the lists is unspecified.
-spec 'map'([scm_any(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> [scm_any()].
'map'([Proc|Args], Env, Ok, Ng) ->
    do_map(Proc, Args, Env, Ok, Ng).

%% @equiv 'map'([Proc|Args])
-spec 'string-map'([scm_any(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> scm_string().
'string-map'([Proc|Args], Env, Ok, Ng) ->
    do_smap(Proc, Args, Env, Ok, Ng).

%% @equiv 'map'([Proc|Args])
-spec 'vector-map'([scm_any(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> scm_vector().
'vector-map'([Proc|Args], Env, Ok, Ng) ->
    do_vmap(Proc, Args, Env, Ok, Ng).

%% @doc Applies proc element-wise to the elements of the lists and
%% returns #f.  Calls proc for its side effects rather than for its
%% values.  Unlike map, for-each is guaranteed to call proc on the
%% elements of the lists in order from the first element(s) to the
%% last. If more than one list is given and not all lists have the
%% same length, for-each terminates when the shortest list runs out.
-spec 'for-each'([scm_any(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> scm_false().
'for-each'([Proc|Args], Env, Ok, Ng) ->
    do_foreach(Proc, Args, Env, Ok, Ng).

%% @equiv 'for-each'([Proc|Args])
-spec 'string-for-each'([scm_any(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> scm_false().
'string-for-each'([Proc|Args], Env, Ok, Ng) ->
    do_sforeach(Proc, Args, Env, Ok, Ng).

%% @equiv 'for-each'([Proc|Args])
-spec 'vector-for-each'([scm_any(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> scm_false().
'vector-for-each'([Proc|Args], Env, Ok, Ng) ->
    do_vforeach(Proc, Args, Env, Ok, Ng).

%% @doc Packages the current continuation as an "escape procedure" and
%% passes it as an argument to +Proc+. The escape procedure is a
%% Scheme procedure that, if it is later called, will abandon whatever
%% continuation is in effect at that later time and will instead use
%% the continuation that was in effect when the escape procedure was
%% created. Calling the escape procedure will cause the invocation of
%% before and after thunks installed using +dynamic-wind+.
%% @equiv 'call-with-current-continuation(Proc)'
-spec 'call/cc'(scm_proc(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_any().
'call/cc'(Proc, Env, Ok, Ng) ->
    Ws = get_winders(),
    Escape = fun(Exp, _Env, _Ok, _Ng) ->
                     do_wind(Ws, Exp, Env, Ok, Ng)
             end,
    apply(Proc, [#xnipn{val=Escape}], Env, Ok, Ng).

%% @doc Delivers all of its arguments to its continuation.
-spec 'values'([scm_any(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> scm_any().
'values'(Args, _Env, Ok, Ng) ->
    Ok(Args, Ng).

%% @doc Calls its producer argument with no values and a continuation
%% that, when passed some values, calls the consumer procedure with
%% those values as arguments. The continuation for the call to
%% consumer is the continuation of the call to +call-with-values+.
-spec 'call-with-values'(scm_thunk(), scm_proc(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_any().
'call-with-values'(Producer, Consumer, Env, Ok, Ng) ->
    POk = fun(Val, _Ng) ->
                  apply(Consumer, Val, Env, Ok, Ng)
          end,
    apply(Producer, [], Env, POk, Ng).

%% @doc Calls +Thunk+ without arguments, returning the result(s) of
%% this call. +Before+ and +After+ are called, also without arguments,
%% as required.  Note that, in the absence of calls to continuations
%% captured using +call/cc+, the three arguments are called once each,
%% in order.  +Before+ is called whenever execution enters the dynamic
%% extent of the call to +Thunk+ and +After+ is called whenever it
%% exits that dynamic extent.  The +Before+ and +After+ thunks are
%% called in the same dynamic environment as the call to
%% +dynamic-wind+.
%%
%% No special handling is given for escapes that might occur inside
%% the +Before+ and +After+ thunks. It protects the +Thunk+ by its
%% continuation that enforces the following three rules.
%%
%% 1. Its normal continuation is for +Before+ to be called before
%%    +Thunk+, which is called before +After+, and finally to return
%%    the value of the evaluation of +Thunk+ as the value of the
%%    entire dynamic-wind expression.
%% 2. If an escape is made out of the +Thunk+, +dynamic-wind+
%%    guarantees that the +After+ will be called before the escape
%%    occurs.
%% 3. If an escape is made into the +Thunk+, it guarantees that the
%%    +Before+ will be called before control is returned to the place
%%    of initial escape in the +Thunk+, and finally the +After+ is
%%    called.
-spec 'dynamic-wind'(scm_thunk(), scm_thunk(), scm_thunk(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_any().
'dynamic-wind'(Before, Thunk, After, Env, Ok, Ng) ->
    %% before
    C = fun(_, Env1, Ok1, Ng1) ->
                W = {Env, Before, After},
                push_winder(W),
                %% thunk
                C1 = fun(Val, Env2, Ok2, Ng2) ->
                             W = pop_winder(),
                             %% after
                             C2 = fun(_, Env3, Ok3, Ng3) ->
                                          'values'(Val, Env3, Ok3, Ng3)
                                  end,
                             'call-with-values'(After, #xnipv{val=C2}, Env2, Ok2, Ng2)
                     end,
                'call-with-values'(Thunk, #xnipv{val=C1}, Env1, Ok1, Ng1)
        end,
    'call-with-values'(Before, #xnipv{val=C}, Env, Ok, Ng).

%%%===================================================================
%%% internal helpers
%%%===================================================================

%% iter helper
do_iterN(ArgsL) ->
    Fun = fun([Arg|Args], {Acc1, Acc2}) ->
                  {[Arg|Acc1], [Args|Acc2]};
             (_, _) ->
                  done
          end,
    case lists:foldl(Fun, {[], []}, ArgsL) of
        {Args, NewArgsL} ->
            {lists:reverse(Args), lists:reverse(NewArgsL)};
        Else ->
            Else
    end.

%% map
do_map(Proc, [Args], Env, Ok, Ng) ->
    do_map1(Proc, Args, [], Env, Ok, Ng);
do_map(Proc, ArgsL, Env, Ok, Ng) ->
    do_mapN(Proc, ArgsL, [], Env, Ok, Ng).

do_map1(_Proc, [], Acc, _Env, Ok, Ng) ->
    Ok(lists:reverse(Acc), Ng);
do_map1(Proc, [Arg|Args], Acc, Env, Ok, Ng) ->
    Ok1 = fun(Val, Ng1) ->
                  do_map1(Proc, Args, [Val|Acc], Env, Ok, Ng1)
          end,
    apply(Proc, [Arg], Env, Ok1, Ng).

do_mapN(_Proc, [], Acc, _Env, Ok, Ng) ->
    Ok(lists:reverse(Acc), Ng);
do_mapN(Proc, ArgsL, Acc, Env, Ok, Ng) ->
    case do_iterN(ArgsL) of
        done ->
            Ok(lists:reverse(Acc), Ng);
        {Args, NewArgsL} ->
            Ok1 = fun(Val, Ng1) ->
                          do_mapN(Proc, NewArgsL, [Val|Acc], Env, Ok, Ng1)
                  end,
            apply(Proc, Args, Env, Ok1, Ng)
    end.

%% string-map
%% NOTE: structure identical to vector-map
do_smap(Proc, [Str], Env, Ok, Ng) ->
    do_smap1(Proc, Str, scml_base_string:'string-length'(Str)-1, [], Env, Ok, Ng);
do_smap(Proc, Strs, Env, Ok, Ng) ->
    do_smapN(Proc, Strs, Env, Ok, Ng).

do_smap1(_Proc, _Str, -1, Acc, _Env, Ok, Ng) ->
    Ok(scml_base_string:'list->string'(Acc), Ng);
do_smap1(Proc, Str, K, Acc, Env, Ok, Ng) ->
    Arg = scml_base_string:'string-ref'(Str, K),
    Ok1 = fun(Val, Ng1) ->
                  do_smap1(Proc, Str, K-1, [Val|Acc], Env, Ok, Ng1)
          end,
    apply(Proc, [Arg], Env, Ok1, Ng).

do_smapN(Proc, Strs, Env, Ok, Ng) ->
    ArgsL = [ scml_base_string:'string->list'(Str) || Str <- Strs ],
    L = do_mapN(Proc, ArgsL, [], Env, Ok, Ng),
    scml_base_string:'list->string'(L).

%% vector-map
%% NOTE: structure identical to string-map
do_vmap(Proc, [Vec], Env, Ok, Ng) ->
    do_vmap1(Proc, Vec, scml_base_vector:'vector-length'(Vec)-1, [], Env, Ok, Ng);
do_vmap(Proc, Vecs, Env, Ok, Ng) ->
    do_vmapN(Proc, Vecs, Env, Ok, Ng).

do_vmap1(_Proc, _Vec, -1, Acc, _Env, Ok, Ng) ->
    Ok(scml_base_vector:'list->vector'(Acc), Ng);
do_vmap1(Proc, Vec, K, Acc, Env, Ok, Ng) ->
    Arg = scml_base_vector:'vector-ref'(Vec, K),
    Ok1 = fun(Val, Ng1) ->
                  do_vmap1(Proc, Vec, K-1, [Val|Acc], Env, Ok, Ng1)
          end,
    apply(Proc, [Arg], Env, Ok1, Ng).

do_vmapN(Proc, Vecs, Env, Ok, Ng) ->
    ArgsL = [ scml_base_vector:'vector->list'(Vec) || Vec <- Vecs ],
    L = do_mapN(Proc, ArgsL, [], Env, Ok, Ng),
    scml_base_vector:'list->vector'(L).

%% for-each
do_foreach(Proc, [Args], Env, Ok, Ng) ->
    do_foreach1(Proc, Args, Env, Ok, Ng);
do_foreach(Proc, ArgsL, Env, Ok, Ng) ->
    do_foreachN(Proc, ArgsL, Env, Ok, Ng).

do_foreach1(_Proc, [], _Env, Ok, Ng) ->
    Ok(?FALSE, Ng);
do_foreach1(Proc, [Arg|Args], Env, Ok, Ng) ->
    Ok1 = fun(_Val, Ng1) ->
                  do_foreach1(Proc, Args, Env, Ok, Ng1)
          end,
    apply(Proc, [Arg], Env, Ok1, Ng).

do_foreachN(_Proc, [], _Env, Ok, Ng) ->
    Ok(?FALSE, Ng);
do_foreachN(Proc, ArgsL, Env, Ok, Ng) ->
    case do_iterN(ArgsL) of
        done ->
            Ok(?FALSE, Ng);
        {Args, NewArgsL} ->
            Ok1 = fun(_Val, Ng1) ->
                          do_foreachN(Proc, NewArgsL, Env, Ok, Ng1)
                  end,
            apply(Proc, Args, Env, Ok1, Ng)
    end.

%% string-for-each
%% NOTE: structure identical to vector-for-each
do_sforeach(Proc, [Str], Env, Ok, Ng) ->
    do_sforeach1(Proc, Str, 0, scml_base_string:'string-length'(Str), Env, Ok, Ng);
do_sforeach(Proc, Strs, Env, Ok, Ng) ->
    do_sforeachN(Proc, Strs, Env, Ok, Ng).

do_sforeach1(_Proc, _Str, _N, _N, _Env, Ok, Ng) ->
    Ok(?FALSE, Ng);
do_sforeach1(Proc, Str, K, N, Env, Ok, Ng) ->
    Arg = scml_base_string:'string-ref'(Str, K),
    Ok1 = fun(_Val, Ng1) ->
                  do_sforeach1(Proc, Str, K+1, N, Env, Ok, Ng1)
          end,
    apply(Proc, [Arg], Env, Ok1, Ng).

do_sforeachN(Proc, Strs, Env, Ok, Ng) ->
    ArgsL = [ scml_base_string:'string->list'(Str) || Str <- Strs ],
    do_foreachN(Proc, ArgsL, Env, Ok, Ng).

%% vector-for-each
%% NOTE: structure identical to string-for-each
do_vforeach(Proc, [Str], Env, Ok, Ng) ->
    do_vforeach1(Proc, Str, 0, scml_base_vector:'vector-length'(Str), Env, Ok, Ng);
do_vforeach(Proc, Strs, Env, Ok, Ng) ->
    do_vforeachN(Proc, Strs, Env, Ok, Ng).

do_vforeach1(_Proc, _Str, _N, _N, _Env, Ok, Ng) ->
    Ok(?FALSE, Ng);
do_vforeach1(Proc, Str, K, N, Env, Ok, Ng) ->
    Arg = scml_base_vector:'vector-ref'(Str, K),
    Ok1 = fun(_Val, Ng1) ->
                  do_vforeach1(Proc, Str, K+1, N, Env, Ok, Ng1)
          end,
    apply(Proc, [Arg], Env, Ok1, Ng).

do_vforeachN(Proc, Strs, Env, Ok, Ng) ->
    ArgsL = [ scml_base_vector:'vector->list'(Str) || Str <- Strs ],
    do_foreachN(Proc, ArgsL, Env, Ok, Ng).

-spec do_wind(winders(), scm_any(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_any().
do_wind(New, Reply, Env, Ok, Ng) ->
    case get_winders() of
        New ->
            %% return reply to continuation
            As = [],
            Bs = [];
        Old ->
            put_winders(New),
            %% 1. invoke after thunk for each old winder that is not a
            %%    member of new winders
            %% 2. invoke before thunk for each new winder that is not a
            %%    not a member of old winders
            %% 3. return reply to continuation
            if Old == undefined ->
                    As = [],
                    Bs = New;
               New == undefined ->
                    As = Old,
                    Bs = [];
               true ->
                    As = Old -- New,
                    Bs = New -- Old
            end
    end,
    do_wind(As,Bs, Reply, Env, Ok, Ng).

-spec do_wind(winders(), winders(), scm_any(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_any().
do_wind([], [], Reply, Env, Ok, Ng) ->
    %% return reply to continuation
    'values'(Reply, Env, Ok, Ng);
do_wind([], [{BEnv,B,_}|Bs], Reply, Env, Ok, Ng) ->
    BOk = fun(_, _BNg) ->
                  %% @TODO BNg ignored - is it OK?
                  do_wind([], Bs, Reply, Env, Ok, Ng)
          end,
    apply(B, [], BEnv, BOk, Ng);
do_wind([{AEnv,_,A}|As], Bs, Reply, Env, Ok, Ng) ->
    AOk = fun(_, _ANg) ->
                  %% @TODO ANg ignored - is it OK?
                  do_wind(As, Bs, Reply, Env, Ok, Ng)
          end,
    apply(A, [], AEnv, AOk, Ng).

-spec get_winders() -> winders() | undefined.
get_winders() ->
    get(?SCMLDYNWINDERS).

-spec put_winders(winders()) -> true.
put_winders(undefined) ->
    erase(?SCMLDYNWINDERS),
    true;
put_winders(Ws) ->
    put(?SCMLDYNWINDERS, Ws),
    true.

-spec push_winder(winder()) -> true.
push_winder(W) ->
    case get_winders() of
        undefined ->
            put(?SCMLDYNWINDERS, [W]),
            true;
        Ws ->
            put(?SCMLDYNWINDERS, [W|Ws]),
            true
    end.

-spec pop_winder() -> winder().
pop_winder() ->
    case get_winders() of
        [W] ->
            erase(?SCMLDYNWINDERS),
            W;
        [W|Ws] ->
            put(?SCMLDYNWINDERS, Ws),
            W
    end.
