%%% The MIT License
%%%
%%% Copyright (C) 2013-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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

%%% @doc Scheme interpreter i/o device resource and client
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scmi_iodev).

%% External exports
-export([%% resource
         notify_when_destroyed/2
         , notify_when_destroyed/3
         , is_resource/1
         , new/2
         %% client
         , is_resource_alive/1, is_resource_alive/2
         , open/2
         , close/1, close/2
         , is_ready/1
         , read/2
         , read_all/1
         , read_line/1
         , peek/2
         , write/2
         , flush/1
        ]).

-on_load(init/0).

%% External types
-export_type([iodev/0, mode_file/0, mode_ram/0, mode_std/0]).

-include("scmi.hrl").

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

%% resource
-type resource()    :: binary().
-opaque iodev()     :: {pid(), module(), resource()}.

-type filename()    :: scmi_iodev_server_file:filename().
-type mode_file()   :: scmi_iodev_server_file:mode().
-type mode_ram()    :: scmi_iodev_server_ram:mode().
-type mode_std()    :: scmi_iodev_server_std:mode().
-type mode_stderr() :: scmi_iodev_server_std:mode_error().

%%%----------------------------------------------------------------------
%%% API - resource
%%%----------------------------------------------------------------------

-spec init() -> ok | {error, {Reason::bad_lib|load|load_failed|old_code|reload|upgrade, Text::string()}}.
init() ->
    Path = scm_app:priv_libdir(),
    erlang:load_nif(filename:join(Path, "scmi_iodev"), 0).

-spec notify_when_destroyed(term(), iodev()) -> true.
notify_when_destroyed(Msg, Iodev) ->
    notify_when_destroyed(erlang:self(), Msg, Iodev).

-spec notify_when_destroyed(pid(), term(), iodev()) -> true.
notify_when_destroyed(Pid, Msg, {_Pid, _Mod, Resource}) when is_pid(Pid), is_pid(_Pid), is_atom(_Mod) ->
    '$notify_when_destroyed'(Pid, Msg, Resource).

-spec is_resource(term()) -> boolean().
is_resource({Pid, Mod, Term}) when is_pid(Pid), is_atom(Mod) ->
    '$is_resource'(Term);
is_resource(_Term) ->
    false.

-spec new(pid(), module()) -> iodev().
new(Pid, Mod) ->
    make_iodev(Pid, Mod, '$new'()).

%%%----------------------------------------------------------------------
%%% API - client
%%%----------------------------------------------------------------------

-spec is_resource_alive(term()) -> boolean().
is_resource_alive({Pid, _Mod, _}=Term) when is_pid(Pid) ->
    case is_resource(Term) of
        true ->
            is_process_alive(Pid);
        false ->
            false
    end;
is_resource_alive(_) ->
    false.

-spec is_resource_alive(term(), read | write | binary) -> boolean().
is_resource_alive({Pid, Mod, _}=Term, Mode) when is_pid(Pid) ->
    case is_resource_alive(Term) of
        true ->
            Mod:getopts(Pid, Mode);
        false ->
            false
    end;
is_resource_alive(_, _) ->
    false.

-spec open(standard_io, [mode_std()]) -> iodev();
          (standard_error, [mode_stderr()]) -> iodev();
          (iodata(), [mode_ram()]) -> iodev();
          (filename(), [mode_file()]) -> iodev().
open(Term, Modes) ->
    Mod = case Term of
              standard_io ->
                  scmi_iodev_server_std;
              standard_error ->
                  scmi_iodev_server_std;
              _ ->
                  Ram = proplists:get_bool(ram, Modes),
                  if Ram ->
                          scmi_iodev_server_ram;
                     true ->
                          scmi_iodev_server_file
                  end
          end,
    case Mod:start(Term, Modes) of
        {ok, Pid} ->
            IODev = new(Pid, Mod),
            true = notify_when_destroyed(Pid, close, IODev),
            IODev;
        ignore ->
            erlang:error(badarg, [Term, Modes]);
        {error, Err} ->
            erlang:error(Err, [Term, Modes])
    end.

-spec close(iodev()) -> boolean().
close({Pid, Mod, _}=IODev) when is_pid(Pid) ->
    case is_resource_alive(IODev) of
        true ->
            Mod:close(Pid);
        false ->
            false
    end;
close(IODev) ->
    erlang:error(badarg, [IODev]).

-spec close(iodev(), read | write) -> boolean().
close({Pid, Mod, _}=IODev, Mode) when is_pid(Pid) ->
    case is_resource_alive(IODev) of
        true ->
            Mod:close(Pid, Mode);
        false ->
            false
    end;
close(IODev, Mode) ->
    erlang:error(badarg, [IODev, Mode]).

-spec is_ready(iodev()) -> boolean().
is_ready({Pid, Mod, _}) when is_pid(Pid) ->
    Mod:is_ready(Pid);
is_ready(IODev) ->
    erlang:error(badarg, [IODev]).

-spec read(iodev(), non_neg_integer()) -> string() | binary() | eof | {error, Reason::term()}.
read({Pid, Mod, _}, K) when is_pid(Pid) ->
    Mod:read(Pid, K);
read(IODev, K) ->
    erlang:error(badarg, [IODev, K]).

-spec read_all(iodev()) -> string() | binary() | eof | {error, Reason::term()}.
read_all({Pid, Mod, _}) when is_pid(Pid) ->
    Mod:read_all(Pid);
read_all(IODev) ->
    erlang:error(badarg, [IODev]).

-spec read_line(iodev()) -> string() | binary() | eof | {error, Reason::term()}.
read_line({Pid, Mod, _}) when is_pid(Pid) ->
    Mod:read_line(Pid);
read_line(IODev) ->
    erlang:error(badarg, [IODev]).

-spec peek(iodev(), non_neg_integer()) -> string() | binary() | eof | {error, Reason::term()}.
peek({Pid, Mod, _}, K) when is_pid(Pid) ->
    Mod:peek(Pid, K);
peek(IODev, K) ->
    erlang:error(badarg, [IODev, K]).

-spec write(iodev(), string() | binary()) -> ok | {error, Reason::term()}.
write({Pid, Mod, _}, Data) when is_pid(Pid) ->
    Mod:write(Pid, Data);
write(IODev, Data) ->
    erlang:error(badarg, [IODev, Data]).

-spec flush(iodev()) -> ok | {error, Reason::term()}.
flush({Pid, Mod, _}) when is_pid(Pid) ->
    Mod:flush(Pid);
flush(IODev) ->
    erlang:error(badarg, [IODev]).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec make_iodev(pid(), module(), resource()) -> iodev().
make_iodev(Pid, Mod, Resource) ->
    {Pid, Mod, Resource}.

-spec '$notify_when_destroyed'(pid(), term(), resource()) -> true.
'$notify_when_destroyed'(Pid, Msg, Resource) ->
    erlang:nif_error(badarg, [Pid, Msg, Resource]).

-spec '$is_resource'(term()) -> boolean().
'$is_resource'(Term) ->
    erlang:nif_error(badarg, [Term]).

-spec '$new'() -> resource().
'$new'() ->
    erlang:nif_error(badarg, []).
