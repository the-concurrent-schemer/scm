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

%%% @doc Scheme base library for exceptions
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_base_exception).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['with-exception-handler'/5
         , 'raise'/4
         , 'raise-continuable'/4
         , 'error'/4
         , 'error-object?'/1
         , 'error-object-message'/1
         , 'error-object-irritants'/1
         , 'read-error?'/1
         , 'file-error?'/1
        ]).

-import(scmi_analyze_primitive, [apply/5]).
-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{'with-exception-handler', #xnipn{val=fun ?MODULE:'with-exception-handler'/5}}
     , {'raise', #xnipn{val=fun ?MODULE:'raise'/4}}
     , {'raise-continuable', #xnipn{val=fun ?MODULE:'raise-continuable'/4}}
     , {'error', #xnipv{val=fun ?MODULE:'error'/4}}
     , {'error-object?', #nipn{val=fun ?MODULE:'error-object?'/1}}
     , {'error-object-message', #nipn{val=fun ?MODULE:'error-object-message'/1}}
     , {'error-object-irritants', #nipn{val=fun ?MODULE:'error-object-irritants'/1}}
     , {'read-error?', #nipn{val=fun ?MODULE:'read-error?'/1}}
     , {'file-error?', #nipn{val=fun ?MODULE:'file-error?'/1}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the results of invoking +Thunk+. +Handler+ is
%% installed as the current exception handler in the dynamic
%% environment used for the invocation of +Thunk+.  It is an error if
%% +Handler+ does not accept one argument.  It is also an error if
%% +Thunk+ does not accept zero arguments.
-spec 'with-exception-handler'(scm_proc(), scm_thunk(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_obj().
'with-exception-handler'(Handler, Thunk, Env, Ok, Ng) ->
    Ng1 = fun(#cexception{val=[#signal{obj=Obj1, ok=OkC, ng=NgC}|_]}) ->
                  Ok1 = fun(Obj2, _Ng2) ->
                                OkC(Obj2, NgC)
                        end,
                  apply(Handler, [Obj1], Env, Ok1, Ng);
             (#exception{val=[#signal{obj=Obj1}|_]=Signals}=Error) ->
                  Ok1 = fun(Obj2, Ng2) ->
                                Signal = #signal{obj=Obj2, env=Env, ok=Ok, ng=Ng},
                                Ng2(Error#exception{val=[Signal|Signals]})
                        end,
                  apply(Handler, [Obj1], Env, Ok1, Ng)
          end,
    apply(Thunk, [], Env, Ok, Ng1).

%% @doc Raises an exception by invoking the current exception handler
%% on +Obj+.  The handler is called with the same dynamic environment
%% as that of the call to +raise+, except that the current exception
%% handler is the one that was in place when the handler being called
%% was installed.  If the handler returns, a secondary exception is
%% raised in the same dynamic environment as the handler.
-spec 'raise'(scm_obj(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_obj().
'raise'(Obj, Env, Ok, Ng) ->
    Signal = #signal{obj=Obj, env=Env, ok=Ok, ng=Ng},
    Ng(#exception{val=[Signal]}).

%% @doc Raises an exception by invoking the current exception handler
%% on +Obj+.  The handler is called with the same dynamic environment
%% as the call to +raise-continuable+, except that: (1) the current
%% exception handler is the one that was in place when the handler
%% being called was installed, and (2) if the handler being called
%% returns, then it will again become the current exception handler.
%% If the handler returns, the values it returns become the values
%% returned by the call to +raise-continuable+.
-spec 'raise-continuable'(scm_obj(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_obj().
'raise-continuable'(Obj, Env, Ok, Ng) ->
    Signal = #signal{obj=Obj, env=Env, ok=Ok, ng=Ng},
    Ng(#cexception{val=[Signal]}).

%% @doc Raises an exception as if by calling +raise+ on a newly
%% allocated implementation-defined object which encapsulates the
%% information provided by +Message+, as well as any objects, known as
%% the +Irritants+. The procedure +error-object?+ must return #t on
%% such objects.
-spec 'error'([scm_obj(),...], scmi_denv(), scmi_dok(), scmi_dng()) -> scm_obj().
'error'([_Message|Irritants]=Obj, Env, Ok, Ng) when is_list(Irritants) ->
    'raise'(#error_user{val=Obj}, Env, Ok, Ng).

%% @doc Returns #t if the given object is an object created by
%% +error+, raised by the +read+ procedure, or raised by the inability
%% to open an input or output port on a file.
-spec 'error-object?'(scm_obj()) -> scm_boolean().
'error-object?'(#error_file{}) ->
    ?TRUE;
'error-object?'(#error_read{}) ->
    ?TRUE;
'error-object?'(#error_user{}) ->
    ?TRUE;
'error-object?'(_) ->
    ?FALSE.

%% @doc Returns the message encapsulated by the +error-object+.
-spec 'error-object-message'(scm_error()) -> scm_obj().
'error-object-message'(#error_file{val=[Message|_Irritants]}) ->
    Message;
'error-object-message'(#error_read{val=[Message|_Irritants]}) ->
    Message;
'error-object-message'(#error_user{val=[Message|_Irritants]}) ->
    Message.

%% @doc Returns the list of the irritants encapsulated by the
%% +error-object+.
-spec 'error-object-irritants'(scm_error()) -> [scm_obj()].
'error-object-irritants'(#error_file{val=[_Message|Irritants]}) ->
    Irritants;
'error-object-irritants'(#error_read{val=[_Message|Irritants]}) ->
    Irritants;
'error-object-irritants'(#error_user{val=[_Message|Irritants]}) ->
    Irritants.

%% @doc Returns #t if the given object is an object raised by the
%% +read+ procedure.
-spec 'read-error?'(scm_obj()) -> scm_boolean().
'read-error?'(#error_read{}) ->
    ?TRUE;
'read-error?'(_) ->
    ?FALSE.

%% @doc Returns #t if the given object is an object raised by the
%% inability to open an input or output port on a file.
-spec 'file-error?'(scm_obj()) -> scm_boolean().
'file-error?'(#error_file{}) ->
    ?TRUE;
'file-error?'(_) ->
    ?FALSE.

%%%===================================================================
%%% internal helpers
%%%===================================================================
