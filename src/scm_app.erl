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

%%% @doc Scheme application
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scm_app).

-behaviour(application).

%% External exports
-export([start/2, stop/1, priv_libdir/0, priv_scmdir/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    case scm_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

priv_libdir() ->
    case code:priv_dir(scm) of
        {error, bad_name} ->
            Fun = fun (Dir, Acc) ->
                          case filelib:wildcard(filename:join([Dir, "priv/lib/scmi_env.*"])) of
                              [] ->
                                  Acc;
                              [H] ->
                                  filename:dirname(H)
                          end
                  end,
            lists:foldl(Fun, "priv/lib", ["../../scm", "../scm", "../", "./"]);
        Dir ->
            filename:join([Dir, "lib"])
    end.

priv_scmdir() ->
    case code:priv_dir(scm) of
        {error, bad_name} ->
            Fun = fun (Dir, Acc) ->
                          case filelib:wildcard(filename:join([Dir, "priv/scm/*.scm"])) of
                              [] ->
                                  Acc;
                              [H|_] ->
                                  filename:dirname(H)
                          end
                  end,
            lists:foldl(Fun, "priv/scm", ["../../scm", "../scm", "../", "./"]);
        Dir ->
            filename:join([Dir, "scm"])
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
