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

-module(scml_base_io).

%% Imports
-export([imports/0]).

%% API
-export(['call-with-port'/2
         , 'input-port?'/1
         , 'output-port?'/1
         , 'textual-port?'/1
         , 'binary-port?'/1
         , 'port?'/1
         , 'input-port-open?'/1
         , 'output-port-open?'/1
         , 'current-input-port'/0
         , 'current-output-port'/0
         , 'current-error-port'/0
         , 'close-port'/1
         , 'close-input-port'/1
         , 'close-output-port'/1
         , 'open-input-string'/1
         , 'open-output-string'/0
         , 'get-output-string'/1
         , 'open-input-bytevector'/1
         , 'open-output-bytevector'/0
         , 'get-output-bytevector'/1
         , 'read-char'/0
         , 'read-char'/1
         , 'peek-char'/0
         , 'peek-char'/1
         , 'read-line'/0
         , 'read-line'/1
         , 'eof-object?'/1
         , 'eof-object'/0
         , 'char-ready?'/0
         , 'char-ready?'/1
         , 'read-string'/1
         , 'read-string'/2
         , 'read-u8'/0
         , 'read-u8'/1
         , 'peek-u8'/0
         , 'peek-u8'/1
         , 'u8-ready?'/0
         , 'u8-ready?'/1
         , 'read-bytevector'/1
         , 'read-bytevector'/2
         , 'read-bytevector!'/1
         , 'read-bytevector!'/2
         , 'read-bytevector!'/3
         , 'read-bytevector!'/4
         , 'newline'/0
         , 'newline'/1
         , 'write-char'/1
         , 'write-char'/2
         , 'write-string'/1
         , 'write-string'/2
         , 'write-string'/3
         , 'write-string'/4
         , 'write-u8'/1
         , 'write-u8'/2
         , 'write-bytevector'/1
         , 'write-bytevector'/2
         , 'write-bytevector'/3
         , 'write-bytevector'/4
         , 'flush-output-port'/0
         , 'flush-output-port'/1
        ]).

-include("scmi.hrl").

%%%===================================================================
%%% Imports
%%%===================================================================

-spec imports() -> [{scm_symbol(), scmi_nip()}].
imports() ->
    [{'call-with-port', #nipn{val=fun 'call-with-port'/2}}
     , {'input-port?', #nipn{val=fun 'input-port?'/1}}
     , {'output-port?', #nipn{val=fun 'output-port?'/1}}
     , {'textual-port?', #nipn{val=fun 'textual-port?'/1}}
     , {'binary-port?', #nipn{val=fun 'binary-port?'/1}}
     , {'port?', #nipn{val=fun 'port?'/1}}
     , {'input-port-open?', #nipn{val=fun 'input-port-open?'/1}}
     , {'output-port-open?', #nipn{val=fun 'output-port-open?'/1}}
     , {'current-input-port', #nip0{val=fun 'current-input-port'/0}}
     , {'current-output-port', #nip0{val=fun 'current-output-port'/0}}
     , {'current-error-port', #nip0{val=fun 'current-error-port'/0}}
     , {'close-port', #nipn{val=fun 'close-port'/1}}
     , {'close-input-port', #nipn{val=fun 'close-input-port'/1}}
     , {'close-output-port', #nipn{val=fun 'close-output-port'/1}}
     , {'open-input-string', #nipn{val=fun 'open-input-string'/1}}
     , {'open-output-string', #nip0{val=fun 'open-output-string'/0}}
     , {'get-output-string', #nipn{val=fun 'get-output-string'/1}}
     , {'open-input-bytevector', #nipn{val=fun 'open-input-bytevector'/1}}
     , {'open-output-bytevector', #nip0{val=fun 'open-output-bytevector'/0}}
     , {'get-output-bytevector', #nipn{val=fun 'get-output-bytevector'/1}}
     , {'read-char', #nipn{val=[fun 'read-char'/0, fun 'read-char'/1]}}
     , {'peek-char', #nipn{val=[fun 'peek-char'/0, fun 'peek-char'/1]}}
     , {'read-line', #nipn{val=[fun 'read-line'/0, fun 'read-line'/1]}}
     , {'eof-object?', #nipn{val=fun 'eof-object?'/1}}
     , {'eof-object', #nip0{val=fun 'eof-object'/0}}
     , {'char-ready?', #nipn{val=[fun 'char-ready?'/0, fun 'char-ready?'/1]}}
     , {'read-string', #nipn{val=[fun 'read-string'/1, fun 'read-string'/2]}}
     , {'read-u8', #nipn{val=[fun 'read-u8'/0, fun 'read-u8'/1]}}
     , {'peek-u8', #nipn{val=[fun 'peek-u8'/0, fun 'peek-u8'/1]}}
     , {'u8-ready?', #nipn{val=[fun 'u8-ready?'/0, fun 'u8-ready?'/1]}}
     , {'read-bytevector', #nipn{val=[fun 'read-bytevector'/1, fun 'read-bytevector'/2]}}
     , {'read-bytevector!', #nipn{val=[fun 'read-bytevector!'/1, fun 'read-bytevector!'/2, fun 'read-bytevector!'/3, fun 'read-bytevector!'/4]}}
     , {'newline', #nipn{val=[fun 'newline'/0, fun 'newline'/1]}}

     , {'write-char', #nipn{val=[fun 'write-char'/1, fun 'write-char'/2]}}

     , {'write-string', #nipn{val=[fun 'write-string'/1, fun 'write-string'/2, fun 'write-string'/3, fun 'write-string'/4]}}
     , {'write-u8', #nipn{val=[fun 'write-u8'/1, fun 'write-u8'/2]}}
     , {'write-bytevector', #nipn{val=[fun 'write-bytevector'/1, fun 'write-bytevector'/2, fun 'write-bytevector'/3, fun 'write-bytevector'/4]}}
     , {'flush-output-port', #nipn{val=[fun 'flush-output-port'/0, fun 'flush-output-port'/1]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

-spec 'call-with-port'(scm_port(), scm_proc()) -> scm_obj().
'call-with-port'(Port, Proc) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port, Proc]).

-spec 'input-port?'(scm_port()) -> scm_boolean().
'input-port?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'output-port?'(scm_port()) -> scm_boolean().
'output-port?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'textual-port?'(scm_port()) -> scm_boolean().
'textual-port?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'binary-port?'(scm_port()) -> scm_boolean().
'binary-port?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'port?'(scm_port()) -> scm_boolean().
'port?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'input-port-open?'(scm_port()) -> scm_boolean().
'input-port-open?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'output-port-open?'(scm_port()) -> scm_boolean().
'output-port-open?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'current-input-port'() -> scm_port().
'current-input-port'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'current-output-port'() -> scm_port().
'current-output-port'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'current-error-port'() -> scm_port().
'current-error-port'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'close-port'(scm_port()) -> scm_false().
'close-port'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'close-input-port'(scm_port()) -> scm_false().
'close-input-port'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'close-output-port'(scm_port()) -> scm_false().
'close-output-port'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'open-input-string'(scm_string()) -> scm_port().
'open-input-string'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'open-output-string'() -> scm_port().
'open-output-string'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'get-output-string'(scm_port()) -> scm_string().
'get-output-string'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'open-input-bytevector'(scm_bytevector()) -> scm_port().
'open-input-bytevector'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'open-output-bytevector'() -> scm_port().
'open-output-bytevector'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'get-output-bytevector'(scm_port()) -> scm_bytevector().
'get-output-bytevector'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'read-char'() -> scm_char() | scm_eof().
'read-char'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'read-char'(scm_port()) -> scm_char() | scm_eof().
'read-char'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'peek-char'() -> scm_char() | scm_eof().
'peek-char'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'peek-char'(scm_port()) -> scm_char() | scm_eof().
'peek-char'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'read-line'() -> scm_string() | scm_eof().
'read-line'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'read-line'(scm_port()) -> scm_string() | scm_eof().
'read-line'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'eof-object?'(scm_obj()) -> scm_boolean().
'eof-object?'(Obj) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Obj]).

-spec 'eof-object'() -> scm_eof().
'eof-object'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'char-ready?'() -> scm_boolean().
'char-ready?'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'char-ready?'(scm_port()) -> scm_boolean().
'char-ready?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'read-string'(scm_k()) -> scm_string() | scm_eof().
'read-string'(K) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K]).

-spec 'read-string'(scm_k(), scm_port()) -> scm_string() | scm_eof().
'read-string'(K, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K, Port]).

-spec 'read-u8'() -> scm_byte() | scm_eof().
'read-u8'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'read-u8'(scm_port()) -> scm_byte() | scm_eof().
'read-u8'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'peek-u8'() -> scm_byte() | scm_eof().
'peek-u8'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'peek-u8'(scm_port()) -> scm_byte() | scm_eof().
'peek-u8'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'u8-ready?'() -> scm_boolean().
'u8-ready?'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'u8-ready?'(scm_port()) -> scm_boolean().
'u8-ready?'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'read-bytevector'(scm_k()) -> scm_bytevector() | scm_eof().
'read-bytevector'(K) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K]).

-spec 'read-bytevector'(scm_k(), scm_port()) -> scm_bytevector() | scm_eof().
'read-bytevector'(K, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [K, Port]).

-spec 'read-bytevector!'(scm_bytevector()) -> scm_k() | scm_eof().
'read-bytevector!'(V) ->
    erlang:error(unsupported, [V]).

-spec 'read-bytevector!'(scm_bytevector(), scm_port()) -> scm_k() | scm_eof().
'read-bytevector!'(V, Port) ->
    erlang:error(unsupported, [V, Port]).

-spec 'read-bytevector!'(scm_bytevector(), scm_port(), scm_start()) -> scm_k() | scm_eof().
'read-bytevector!'(V, Port, Start) ->
    erlang:error(unsupported, [V, Port, Start]).

-spec 'read-bytevector!'(scm_bytevector(), scm_port(), scm_start(), scm_end()) -> scm_k() | scm_eof().
'read-bytevector!'(V, Port, Start, End) ->
    erlang:error(unsupported, [V, Port, Start, End]).

-spec 'newline'() -> scm_false().
'newline'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'newline'(scm_port()) -> scm_false().
'newline'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

-spec 'write-char'(scm_char()) -> scm_false().
'write-char'(C) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [C]).

-spec 'write-char'(scm_char(), scm_port()) -> scm_false().
'write-char'(C, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [C, Port]).

-spec 'write-string'(scm_string()) -> scm_false().
'write-string'(S) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S]).

-spec 'write-string'(scm_string(), scm_port()) -> scm_false().
'write-string'(S, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Port]).

-spec 'write-string'(scm_string(), scm_port(), scm_start()) -> scm_false().
'write-string'(S, Port, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Port, Start]).

-spec 'write-string'(scm_string(), scm_port(), scm_start(), scm_end()) -> scm_false().
'write-string'(S, Port, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [S, Port, Start, End]).

-spec 'write-u8'(scm_byte()) -> scm_false().
'write-u8'(B) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [B]).

-spec 'write-u8'(scm_byte(), scm_port()) -> scm_false().
'write-u8'(B, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [B, Port]).

-spec 'write-bytevector'(scm_bytevector()) -> scm_false().
'write-bytevector'(V) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V]).

-spec 'write-bytevector'(scm_bytevector(), scm_port()) -> scm_false().
'write-bytevector'(V, Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Port]).

-spec 'write-bytevector'(scm_bytevector(), scm_port(), scm_start()) -> scm_false().
'write-bytevector'(V, Port, Start) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Port, Start]).

-spec 'write-bytevector'(scm_bytevector(), scm_port(), scm_start(), scm_end()) -> scm_false().
'write-bytevector'(V, Port, Start, End) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [V, Port, Start, End]).

-spec 'flush-output-port'() -> scm_false().
'flush-output-port'() ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, []).

-spec 'flush-output-port'(scm_port()) -> scm_false().
'flush-output-port'(Port) ->
    %% @TODO
    erlang:error({roadmap,'v0.4.0'}, [Port]).

%%%===================================================================
%%% internal helpers
%%%===================================================================
