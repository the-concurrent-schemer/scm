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

-module(scmi_env).

-include("scmd_type.hrl").
-include("scmi.hrl").

%% External exports
-export([notify_when_destroyed/2
         , notify_when_destroyed/3
         , is_resource/1
         , the_empty/0
         , extend/3
         , make_immutable/1
         , is_immutable/1
         , lookup_variable/2
         , safe_lookup_variable/2
         , set_variable/3
         , define_variable/3
        ]).

-on_load(init/0).

%% External types
-export_type([var/0
              , val/0
              , env/0
             ]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%% binding
-type var()      :: scm_symbol().
-type val()      :: scm_obj().

%% environment
-type resource() :: binary().
-opaque env()    :: {reference(), resource()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec init() -> ok | {error, {Reason::bad_lib|load|load_failed|old_code|reload|upgrade, Text::string()}}.
init() ->
    Path =
        case code:priv_dir(scm) of
            {error, bad_name} ->
                "../../scm/priv/lib";
            Dir ->
                filename:join([Dir, "lib"])
        end,
    erlang:load_nif(filename:join(Path, "scmi_env"), 0).

-spec notify_when_destroyed(term(), env()) -> true.
notify_when_destroyed(Msg, Env) ->
    notify_when_destroyed(erlang:self(), Msg, Env).

-spec notify_when_destroyed(pid(), term(), env()) -> true.
notify_when_destroyed(Pid, Msg, {Ref, Resource}) when is_reference(Ref) ->
    '$notify_when_destroyed'(Pid, Msg, Resource).

-spec is_resource(term()) -> boolean().
is_resource({Ref, Term}) when is_reference(Ref) ->
    '$is_resource'(Term);
is_resource(_Term) ->
    false.

-spec the_empty() -> env().
the_empty() ->
    make_env('$the_empty'()).

-spec extend([var()], [val()], env()) -> val().
extend(Vars, Vals, {Ref, BaseResource}) when is_reference(Ref) ->
    make_env('$extend'(Vars, Vals, BaseResource)).

-spec make_immutable(env()) -> true.
make_immutable({Ref, Resource}) when is_reference(Ref) ->
    '$make_immutable'(Resource).

-spec is_immutable(env()) -> boolean().
is_immutable({Ref, Resource}) when is_reference(Ref) ->
    '$is_immutable'(Resource).

-spec lookup_variable(var(), env()) -> val().
lookup_variable(Var, Env) ->
    case safe_lookup_variable(Var, Env) of
        ?UNASSIGNED ->
            erlang:error(badarg, [Var, Env]);
        Val ->
            Val
    end.

-spec safe_lookup_variable(var(), env()) -> val().
safe_lookup_variable(Var, {Ref, Resource}) when is_reference(Ref) ->
    '$safe_lookup_variable'(Var, Resource).

-spec set_variable(var(), val(), env()) -> true.
set_variable(Var, Val, {Ref, Resource}) when is_reference(Ref) ->
    '$set_variable'(Var, Val, Resource).

-spec define_variable(var(), val(), env()) -> true.
define_variable(Var, Val, {Ref, Resource}) when is_reference(Ref) ->
    '$define_variable'(Var, Val, Resource).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec make_env(resource()) -> env().
make_env(Resource) ->
    {make_ref(), Resource}.

-spec '$notify_when_destroyed'(pid(), term(), resource()) -> true.
'$notify_when_destroyed'(Pid, Msg, Resource) ->
    erlang:nif_error(badarg, [Pid, Msg, Resource]).

-spec '$is_resource'(term()) -> boolean().
'$is_resource'(Term) ->
    erlang:nif_error(badarg, [Term]).

-spec '$the_empty'() -> resource().
'$the_empty'() ->
    erlang:nif_error(badarg, []).

-spec '$extend'([var()], [val()], resource()) -> val().
'$extend'(Vars, Vals, BaseResource) ->
    erlang:nif_error(badarg, [Vars, Vals, BaseResource]).

-spec '$make_immutable'(resource()) -> true.
'$make_immutable'(Resource) ->
    erlang:nif_error(badarg, [Resource]).

-spec '$is_immutable'(resource()) -> boolean().
'$is_immutable'(Resource) ->
    erlang:nif_error(badarg, [Resource]).

-spec '$safe_lookup_variable'(var(), resource()) -> val().
'$safe_lookup_variable'(Var, Resource) ->
    erlang:nif_error(badarg, [Var, Resource]).

-spec '$set_variable'(var(), val(), resource()) -> true.
'$set_variable'(Var, Val, Resource) ->
    erlang:nif_error(badarg, [Var, Val, Resource]).

-spec '$define_variable'(var(), val(), resource()) -> true.
'$define_variable'(Var, Val, Resource) ->
    erlang:nif_error(badarg, [Var, Val, Resource]).
