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

%%% @doc Scheme base library for system interface
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_base_system).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export([features/0
        ]).

-include("scml.hrl").

%%%===================================================================
%%% Types/Specs/Records
%%%===================================================================

%%%===================================================================
%%% SCML Exports
%%%===================================================================

-spec '$scml_exports'() -> [{scm_symbol(), scmi_nip()}].
'$scml_exports'() ->
    [{features, #nip0{val=fun features/0}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec features() -> [scm_symbol()].
features() ->
    ['r7rs'
     %% , 'exact-closed'  @TODO v0.7.0
     %% , 'exact-complex' @TODO v0.7.0
     %% , 'ieee-float'    @TODO v0.7.0
     , 'full-unicode'
     %% , 'ratios'        @TODO v0.7.0
    ]
        ++ [ 'posix' || has_posix() ]
        ++ [ 'windows' || has_windows() ]
        ++ system_info()
        ++ [ 'big-endian' ]
        ++ [ ?SCM, ?SCMVSN ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

has_posix() ->
    case os:type() of
        {unix, _} ->
            true;
        _ ->
            false
    end.

has_windows() ->
    case os:type() of
        {win32, _} ->
            true;
        _ ->
            false
    end.

system_info() ->
    {OSFamily, OSName} = os:type(),
    OTPRel = erlang:system_info(otp_release),
    SysArch = erlang:system_info(system_architecture),
    WordSize = integer_to_list(erlang:system_info({wordsize, external}) * 8),
    Strs = [OTPRel, SysArch, WordSize] ++ string:tokens(SysArch, "-"),
    lists:sort([OSFamily, OSName] ++ [ list_to_atom(Str) || Str <- Strs ]).
