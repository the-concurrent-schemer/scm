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

%%% @doc Scheme base library for input and output
%%% @author Joseph Wayne Norton <norton@alum.mit.edu>

-module(scml_base_io).

%% SCML Exports
-export(['$scml_exports'/0]).

%% API
-export(['call-with-port'/5
         , 'input-port?'/1
         , 'output-port?'/1
         , 'textual-port?'/1
         , 'binary-port?'/1
         , 'port?'/1
         , 'input-port-open?'/1
         , 'output-port-open?'/1
         , 'current-input-port'/0, 'current-input-port'/1, 'current-input-port'/2
         , 'current-output-port'/0, 'current-output-port'/1, 'current-output-port'/2
         , 'current-error-port'/0, 'current-error-port'/1, 'current-error-port'/2
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
    [{'call-with-port', #xnipn{val=fun ?MODULE:'call-with-port'/5}}
     , {'input-port?', #nipn{val=fun ?MODULE:'input-port?'/1}}
     , {'output-port?', #nipn{val=fun ?MODULE:'output-port?'/1}}
     , {'textual-port?', #nipn{val=fun ?MODULE:'textual-port?'/1}}
     , {'binary-port?', #nipn{val=fun ?MODULE:'binary-port?'/1}}
     , {'port?', #nipn{val=fun ?MODULE:'port?'/1}}
     , {'input-port-open?', #nipn{val=fun ?MODULE:'input-port-open?'/1}}
     , {'output-port-open?', #nipn{val=fun ?MODULE:'output-port-open?'/1}}
     , {'current-input-port', #nipn{val=[fun ?MODULE:'current-input-port'/0, fun ?MODULE:'current-input-port'/1, fun ?MODULE:'current-input-port'/2]}}
     , {'current-output-port', #nipn{val=[fun ?MODULE:'current-output-port'/0, fun ?MODULE:'current-output-port'/1, fun ?MODULE:'current-output-port'/2]}}
     , {'current-error-port', #nipn{val=[fun ?MODULE:'current-error-port'/0, fun ?MODULE:'current-error-port'/1, fun ?MODULE:'current-error-port'/2]}}
     , {'close-port', #nipn{val=fun ?MODULE:'close-port'/1}}
     , {'close-input-port', #nipn{val=fun ?MODULE:'close-input-port'/1}}
     , {'close-output-port', #nipn{val=fun ?MODULE:'close-output-port'/1}}
     , {'open-input-string', #nipn{val=fun ?MODULE:'open-input-string'/1}}
     , {'open-output-string', #nip0{val=fun ?MODULE:'open-output-string'/0}}
     , {'get-output-string', #nipn{val=fun ?MODULE:'get-output-string'/1}}
     , {'open-input-bytevector', #nipn{val=fun ?MODULE:'open-input-bytevector'/1}}
     , {'open-output-bytevector', #nip0{val=fun ?MODULE:'open-output-bytevector'/0}}
     , {'get-output-bytevector', #nipn{val=fun ?MODULE:'get-output-bytevector'/1}}
     , {'read-char', #nipn{val=[fun ?MODULE:'read-char'/0, fun ?MODULE:'read-char'/1]}}
     , {'peek-char', #nipn{val=[fun ?MODULE:'peek-char'/0, fun ?MODULE:'peek-char'/1]}}
     , {'read-line', #nipn{val=[fun ?MODULE:'read-line'/0, fun ?MODULE:'read-line'/1]}}
     , {'eof-object?', #nipn{val=fun ?MODULE:'eof-object?'/1}}
     , {'eof-object', #nip0{val=fun ?MODULE:'eof-object'/0}}
     , {'char-ready?', #nipn{val=[fun ?MODULE:'char-ready?'/0, fun ?MODULE:'char-ready?'/1]}}
     , {'read-string', #nipn{val=[fun ?MODULE:'read-string'/1, fun ?MODULE:'read-string'/2]}}
     , {'read-u8', #nipn{val=[fun ?MODULE:'read-u8'/0, fun ?MODULE:'read-u8'/1]}}
     , {'peek-u8', #nipn{val=[fun ?MODULE:'peek-u8'/0, fun ?MODULE:'peek-u8'/1]}}
     , {'u8-ready?', #nipn{val=[fun ?MODULE:'u8-ready?'/0, fun ?MODULE:'u8-ready?'/1]}}
     , {'read-bytevector', #nipn{val=[fun ?MODULE:'read-bytevector'/1, fun ?MODULE:'read-bytevector'/2]}}
     , {'read-bytevector!', #nipn{val=[fun ?MODULE:'read-bytevector!'/1, fun ?MODULE:'read-bytevector!'/2, fun ?MODULE:'read-bytevector!'/3, fun ?MODULE:'read-bytevector!'/4]}}
     , {'newline', #nipn{val=[fun ?MODULE:'newline'/0, fun ?MODULE:'newline'/1]}}

     , {'write-char', #nipn{val=[fun ?MODULE:'write-char'/1, fun ?MODULE:'write-char'/2]}}

     , {'write-string', #nipn{val=[fun ?MODULE:'write-string'/1, fun ?MODULE:'write-string'/2, fun ?MODULE:'write-string'/3, fun ?MODULE:'write-string'/4]}}
     , {'write-u8', #nipn{val=[fun ?MODULE:'write-u8'/1, fun ?MODULE:'write-u8'/2]}}
     , {'write-bytevector', #nipn{val=[fun ?MODULE:'write-bytevector'/1, fun ?MODULE:'write-bytevector'/2, fun ?MODULE:'write-bytevector'/3, fun ?MODULE:'write-bytevector'/4]}}
     , {'flush-output-port', #nipn{val=[fun ?MODULE:'flush-output-port'/0, fun ?MODULE:'flush-output-port'/1]}}
    ].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Calls proc with port as an argument.  If proc returns, then
%% the port is closed and the values yielded by the proc are returned.
%% Otherwise, the port is automatically closed when there are no
%% longer any references to the port.
-spec 'call-with-port'(scm_port(), scm_proc(), scmi_denv(), scmi_dok(), scmi_dng()) -> scm_obj().
'call-with-port'(Port, Proc, Env, Ok, Ng) ->
    POk = fun(Val, PNg) ->
                  _ = 'close-port'(Port),
                  Ok(Val, PNg)
          end,
    apply(Proc, [Port], Env, POk, Ng).

%% @doc Returns #t if obj is an input port, otherwise returns #f.
-spec 'input-port?'(scm_obj()) -> scm_boolean().
'input-port?'(Obj) ->
    case scmi_iodev:is_resource_alive(Obj, read) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%% @doc Returns #t if obj is an output port, otherwise returns #f.
-spec 'output-port?'(scm_obj()) -> scm_boolean().
'output-port?'(Obj) ->
    case scmi_iodev:is_resource_alive(Obj, write) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%% @doc Returns #t if obj is a textual port, otherwise returns #f.
-spec 'textual-port?'(scm_obj()) -> scm_boolean().
'textual-port?'(Obj) ->
    case scmi_iodev:is_resource_alive(Obj, binary) of
        true ->
            ?FALSE;
        false ->
            ?TRUE
    end.

%% @doc Returns #t if obj is a binary port, otherwise returns #f.
-spec 'binary-port?'(scm_obj()) -> scm_boolean().
'binary-port?'(Obj) ->
    case scmi_iodev:is_resource_alive(Obj, binary) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%% @doc Returns #t if obj is a port, otherwise returns #f.
-spec 'port?'(scm_obj()) -> scm_boolean().
'port?'(Obj) ->
    case scmi_iodev:is_resource(Obj) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%% @doc Returns #t if port is still open and capable of performing
%% input, otherwise returns #t.
-spec 'input-port-open?'(scm_port()) -> scm_boolean().
'input-port-open?'(Port) ->
    case scmi_iodev:is_resource_alive(Port, read) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%% @doc Returns #t if port is still open and capable of performing
%% output, otherwise returns #t.
-spec 'output-port-open?'(scm_port()) -> scm_boolean().
'output-port-open?'(Port) ->
    case scmi_iodev:is_resource_alive(Port, write) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%% @doc Returns the current input port.  The initial value is the
%% default input port.  The default input port is a textual port.  The
%% +current-input-port+ procedures behave like a parameter object,
%% which can be overridden with +parameterize+.
-spec 'current-input-port'() -> scm_port().
'current-input-port'() ->
    get_port(?SCMLCURINPORT, fun() -> scmi_iodev:open(standard_io, [read]) end).

%% @doc Returns the converter procedure for the current input port.
-spec 'current-input-port'(?SCMIPARAMCVT) -> scm_proc().
'current-input-port'(?SCMIPARAMCVT) ->
    #nipn{val=fun(X) -> X end}.

%% @doc Saves the given port as the current input port.  Returns #f.
-spec 'current-input-port'(?SCMIPARAMSET, scm_port()) -> scm_false().
'current-input-port'(?SCMIPARAMSET, Port) ->
    put_port(?SCMLCURINPORT, Port),
    ?FALSE.

%% @doc Returns the current output port.  The initial value is the
%% default output port.  The default output port is a textual port.
%% The +current-output-port+ procedures behave like a parameter
%% object, which can be overridden with +parameterize+.
-spec 'current-output-port'() -> scm_port().
'current-output-port'() ->
    get_port(?SCMLCUROUTPORT, fun() -> scmi_iodev:open(standard_io, [write]) end).

%% @doc Returns the converter procedure for the current output port.
-spec 'current-output-port'(?SCMIPARAMCVT) -> scm_proc().
'current-output-port'(?SCMIPARAMCVT) ->
    #nipn{val=fun(X) -> X end}.

%% @doc Saves the given port as the current output port.  Returns #f.
-spec 'current-output-port'(?SCMIPARAMSET, scm_port()) -> scm_false().
'current-output-port'(?SCMIPARAMSET, Port) ->
    put_port(?SCMLCUROUTPORT, Port),
    ?FALSE.

%% @doc Returns the current error port.  The initial value is the
%% default error port.  The default error port is a textual port.  The
%% +current-error-port+ procedures behave like a parameter object,
%% which can be overridden with +parameterize+.
-spec 'current-error-port'() -> scm_port().
'current-error-port'() ->
    get_port(?SCMLCURERRPORT, fun() -> scmi_iodev:open(standard_error, [write]) end).

%% @doc Returns the converter procedure for the current error port.
-spec 'current-error-port'(?SCMIPARAMCVT) -> scm_proc().
'current-error-port'(?SCMIPARAMCVT) ->
    #nipn{val=fun(X) -> X end}.

%% @doc Saves the given port as the current error port.  Returns #f.
-spec 'current-error-port'(?SCMIPARAMSET, scm_port()) -> scm_false().
'current-error-port'(?SCMIPARAMSET, Port) ->
    put_port(?SCMLCURERRPORT, Port),
    ?FALSE.

%% @doc Close the resource associated with port, rendering the port
%% incapable of delivering or accepting data.  This procedure has no
%% effect if the port has already been closed.
-spec 'close-port'(scm_port()) -> scm_false().
'close-port'(Port) ->
    _ = scmi_iodev:close(Port),
    ?FALSE.

%% @doc Close the resource associated with port, rendering the input
%% port incapable of delivering data.  This procedure has no effect if
%% the input port has already been closed.  It is an error to apply
%% this procedure to a port which is not an input port.
-spec 'close-input-port'(scm_port()) -> scm_false().
'close-input-port'(Port) ->
    _ = scmi_iodev:close(Port, read),
    ?FALSE.

%% @doc Close the resource associated with port, rendering the input
%% port incapable of delivering data.  This procedure has no effect if
%% the input port has already been closed.  It is an error to apply
%% this procedure to a port which is not an input port.
-spec 'close-output-port'(scm_port()) -> scm_false().
'close-output-port'(Port) ->
    _ = scmi_iodev:close(Port, write),
    ?FALSE.

%% @doc Returns a textual input port that delivers characters from the
%% given string.
-spec 'open-input-string'(scm_string()) -> scm_port().
'open-input-string'(#string{val=S}) ->
    scmi_iodev:open(tuple_to_list(S), [ram, read]).

%% @doc Returns a textual output port that will accumulate characters
%% for retrieval by +get-output-string+.
-spec 'open-output-string'() -> scm_port().
'open-output-string'() ->
    scmi_iodev:open([], [ram, read, write]).

%% @doc Returns a string consisting of the characters that have been
%% output to the port so far in the order they were output.  It is an
%% error if port is not a textual output port.
-spec 'get-output-string'(scm_port()) -> scm_string().
'get-output-string'(Port) ->
    case scmi_iodev:read_all(Port) of
        Data when is_list(Data) ->
            #string{val=list_to_tuple(Data)};
        eof ->
            #string{val={}};
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @doc Returns a binary input port that delivers bytes from the given
%% bytevector.
-spec 'open-input-bytevector'(scm_bytevector()) -> scm_port().
'open-input-bytevector'(#bytevector{val=V}) ->
    scmi_iodev:open(V, [ram, read, binary]).

%% @doc Returns a binary output port that will accumulate bytes for
%% retrieval by +get-output-bytevector+.
-spec 'open-output-bytevector'() -> scm_port().
'open-output-bytevector'() ->
    scmi_iodev:open(<<>>, [ram, read, binary, write]).

%% @doc Returns a bytevector consisting of the bytes that have been
%% output to the port so far in the order they were output.  It is an
%% error if port is not a binary output port.
-spec 'get-output-bytevector'(scm_port()) -> scm_bytevector().
'get-output-bytevector'(Port) ->
    case scmi_iodev:read_all(Port) of
        Data when is_binary(Data) ->
            #bytevector{val=Data};
        eof ->
            ?EOF;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'read-char'('current-input-port'())
-spec 'read-char'() -> scm_char() | scm_eof().
'read-char'() ->
    'read-char'('current-input-port'()).

%% @doc Returns the next character available from the textual input
%% port, updating the port to point to the following character.  If no
%% more characters are available, an end-of-file object is returned.
-spec 'read-char'(scm_port()) -> scm_char() | scm_eof().
'read-char'(Port) ->
    case scmi_iodev:read(Port, 1) of
        [Data] when is_integer(Data) ->
            #character{val=Data};
        eof ->
            ?EOF;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'peek-char'('current-input-port'())
-spec 'peek-char'() -> scm_char() | scm_eof().
'peek-char'() ->
    'peek-char'('current-input-port'()).

%% @doc Returns the next character available from the textual input
%% port, but without updating the port to point to the following
%% character.  If no more characters are available, an end-of-file
%% object is returned.
-spec 'peek-char'(scm_port()) -> scm_char() | scm_eof().
'peek-char'(Port) ->
    case scmi_iodev:peek(Port, 1) of
        [Data] when is_integer(Data) ->
            #character{val=Data};
        eof ->
            ?EOF;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'read-line'('current-input-port'())
-spec 'read-line'() -> scm_string() | scm_eof().
'read-line'() ->
    'read-line'('current-input-port'()).

%% @doc Returns the next line of text available from the textual input
%% port, updating the port to point to the following character.  If an
%% end of line is read, a string containing all of the text up to (but
%% not including) the end of file is returned, and the port is updated
%% to point just past the end of line.  If an end of file is
%% encountered before any end of line is read, but some characters
%% have been read, a string containing those characters is returned.
%% If an end of file is encountered before any characters are read, an
%% end-of-file object is returned.  For the purpose of this procedure,
%% an end of line consists of either a linefeed character, a carriage
%% return character, or a sequence of a carriage return character
%% followed by a linefeed character.
-spec 'read-line'(scm_port()) -> scm_string() | scm_eof().
'read-line'(Port) ->
    case scmi_iodev:read_line(Port) of
        Data when is_list(Data) ->
            #string{val=list_to_tuple(Data)};
        eof ->
            ?EOF;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @doc Returns #t if obj is an end-of-file object, otherwise returns
%% #f.
-spec 'eof-object?'(scm_obj()) -> scm_boolean().
'eof-object?'(?EOF) ->
    ?TRUE;
'eof-object?'(_Obj) ->
    ?FALSE.

%% @doc Returns an end-of-file object.
-spec 'eof-object'() -> scm_eof().
'eof-object'() ->
    ?EOF.

%% @equiv 'char-ready?'('current-input-port'())
-spec 'char-ready?'() -> scm_boolean().
'char-ready?'() ->
    'char-ready?'('current-input-port'()).

%% @doc Returns #t if a character is ready on the textual input port
%% or if port is at the end of the file, otherwise returns #f.
-spec 'char-ready?'(scm_port()) -> scm_boolean().
'char-ready?'(Port) ->
    case scmi_iodev:is_ready(Port) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%% @equiv 'read-string'(K, 'current-input-port'())
-spec 'read-string'(scm_k()) -> scm_string() | scm_eof().
'read-string'(K) ->
    'read-string'(K, 'current-input-port'()).

%% @doc Reads the next k characters, or as many as are available
%% before the end of file, from the textual input port into a string
%% in left-to-right order and returns the string.  If no characters
%% are available before the end of file, an end-of-file object is
%% returned.
-spec 'read-string'(scm_k(), scm_port()) -> scm_string() | scm_eof().
'read-string'(K, Port) ->
    case scmi_iodev:read(Port, K) of
        Data when is_list(Data) ->
            #string{val=list_to_tuple(Data)};
        eof ->
            ?EOF;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'read-u8'('current-input-port'())
-spec 'read-u8'() -> scm_byte() | scm_eof().
'read-u8'() ->
    'read-u8'('current-input-port'()).

%% @doc Returns the next byte available from the binary input port,
%% updating the port to point to the following byte.  If no more bytes
%% are available, an end-of-file object is returned.
-spec 'read-u8'(scm_port()) -> scm_byte() | scm_eof().
'read-u8'(Port) ->
    case scmi_iodev:read(Port, 1) of
        <<Data:8>> ->
            Data;
        eof ->
            ?EOF;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'peek-u8'('current-input-port'())
-spec 'peek-u8'() -> scm_byte() | scm_eof().
'peek-u8'() ->
    'peek-u8'('current-input-port'()).

%% @doc Returns the next byte available from the binary input port,
%% but without updating the port to point to the following byte.  If
%% not more bytes are available, an end-of-file object is returned.
-spec 'peek-u8'(scm_port()) -> scm_byte() | scm_eof().
'peek-u8'(Port) ->
    case scmi_iodev:peek(Port, 1) of
        <<Data:8>> ->
            Data;
        eof ->
            ?EOF;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'u8-ready?'('current-input-port'())
-spec 'u8-ready?'() -> scm_boolean().
'u8-ready?'() ->
    'u8-ready?'('current-input-port'()).

%% @doc Returns #t if a byte is ready on the binary input port or if
%% port is at the end of the file, otherwise returns #f.
-spec 'u8-ready?'(scm_port()) -> scm_boolean().
'u8-ready?'(Port) ->
    case scmi_iodev:is_ready(Port) of
        true ->
            ?TRUE;
        false ->
            ?FALSE
    end.

%% @equiv 'read-bytevector'(K, 'current-input-port'())
-spec 'read-bytevector'(scm_k()) -> scm_bytevector() | scm_eof().
'read-bytevector'(K) ->
    'read-bytevector'(K, 'current-input-port'()).

%% @doc Reads the next k bytes, or as many as are available before the
%% end of file, from the binary input port into a bytevector in
%% left-to-right order and returns the bytevector.  If no bytes are
%% available before the end of file, an end-of-file object is
%% returned.
-spec 'read-bytevector'(scm_k(), scm_port()) -> scm_bytevector() | scm_eof().
'read-bytevector'(K, Port) ->
    case scmi_iodev:read(Port, K) of
        Data when is_binary(Data) ->
            #bytevector{val=Data};
        eof ->
            ?EOF;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @doc _unsupported_
-spec 'read-bytevector!'(scm_bytevector()) -> scm_k() | scm_eof().
'read-bytevector!'(V) ->
    erlang:error(unsupported, [V]).

%% @doc _unsupported_
-spec 'read-bytevector!'(scm_bytevector(), scm_port()) -> scm_k() | scm_eof().
'read-bytevector!'(V, Port) ->
    erlang:error(unsupported, [V, Port]).

%% @doc _unsupported_
-spec 'read-bytevector!'(scm_bytevector(), scm_port(), scm_start()) -> scm_k() | scm_eof().
'read-bytevector!'(V, Port, Start) ->
    erlang:error(unsupported, [V, Port, Start]).

%% @doc _unsupported_
-spec 'read-bytevector!'(scm_bytevector(), scm_port(), scm_start(), scm_end()) -> scm_k() | scm_eof().
'read-bytevector!'(V, Port, Start, End) ->
    erlang:error(unsupported, [V, Port, Start, End]).

%% @equiv 'newline'('current-output-port'())
-spec 'newline'() -> scm_false().
'newline'() ->
    'newline'('current-output-port'()).

%% @doc Writes an end of line to the textual output port.
-spec 'newline'(scm_port()) -> scm_false().
'newline'(Port) ->
    case scmi_iodev:write(Port, [$\n]) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'write-char'(C, 'current-output-port'())
-spec 'write-char'(scm_char()) -> scm_false().
'write-char'(C) ->
    'write-char'(C, 'current-output-port'()).

%% @doc Writes the character char to the given textual output port.
-spec 'write-char'(scm_char(), scm_port()) -> scm_false().
'write-char'(#character{val=C}, Port) ->
    case scmi_iodev:write(Port, [C]) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'write-string'(S, 'current-output-port'())
-spec 'write-string'(scm_string()) -> scm_false().
'write-string'(S) ->
    'write-string'(S, 'current-output-port'()).

%% @equiv 'write-string'(S, 'current-output-port'(), 0)
-spec 'write-string'(scm_string(), scm_port()) -> scm_false().
'write-string'(#string{val=S}, Port) ->
    case scmi_iodev:write(Port, tuple_to_list(S)) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'write-string'(S, 'current-output-port'(), Start, 'string-length'(S))
-spec 'write-string'(scm_string(), scm_port(), scm_start()) -> scm_false().
'write-string'(#string{val=S}, Port, Start) ->
    case scmi_iodev:write(Port, scml:list_part(tuple_to_list(S), Start)) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @doc Writes the characters of string from start to end in
%% left-right order to the textual output port.
-spec 'write-string'(scm_string(), scm_port(), scm_start(), scm_end()) -> scm_false().
'write-string'(S, Port, Start, End) ->
    case scmi_iodev:write(Port, scml:list_part(tuple_to_list(S), Start, End)) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'write-u8'(B, 'current-output-port'())
-spec 'write-u8'(scm_byte()) -> scm_false().
'write-u8'(B) ->
    'write-u8'(B, 'current-output-port'()).

%% @doc Writes the byte to the given binary output port.
-spec 'write-u8'(scm_byte(), scm_port()) -> scm_false().
'write-u8'(B, Port) ->
    case scmi_iodev:write(Port, [B]) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'write-bytevector'(V, 'current-output-port'())
-spec 'write-bytevector'(scm_bytevector()) -> scm_false().
'write-bytevector'(V) ->
    'write-bytevector'(V, 'current-output-port'()).

%% @equiv 'write-bytevector'(V, 'current-output-port'(), 0, 'bytevector-length'(V))
-spec 'write-bytevector'(scm_bytevector(), scm_port()) -> scm_false().
'write-bytevector'(#bytevector{val=V}, Port) ->
    case scmi_iodev:write(Port, V) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'write-bytevector'(V, 'current-output-port'(), Start, 'bytevector-length'(V))
-spec 'write-bytevector'(scm_bytevector(), scm_port(), scm_start()) -> scm_false().
'write-bytevector'(#bytevector{val=V}, Port, Start) ->
    case scmi_iodev:write(Port, scml:binary_part(V, Start)) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @doc Writes the bytes of bytevector from start to end in
%% left-to-right order to the binary output port.
-spec 'write-bytevector'(scm_bytevector(), scm_port(), scm_start(), scm_end()) -> scm_false().
'write-bytevector'(#bytevector{val=V}, Port, Start, End) ->
    case scmi_iodev:write(Port, scml:binary_part(V, Start, End)) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%% @equiv 'flush-output-port'('current-output-port'())
-spec 'flush-output-port'() -> scm_false().
'flush-output-port'() ->
    'flush-output-port'('current-output-port'()).

%% @doc Flushes any buffered output from the buffer of output port to
%% the underlying file or device.
-spec 'flush-output-port'(scm_port()) -> scm_false().
'flush-output-port'(Port) ->
    case scmi_iodev:flush(Port) of
        ok ->
            ?FALSE;
        {error, Reason} ->
            erlang:error(Reason, [Port])
    end.

%%%===================================================================
%%% internal helpers
%%%===================================================================

get_port(Key, Gen) ->
    case get(Key) of
        undefined ->
            Port = Gen(),
            put(Key, Port),
            Port;
        Port ->
            Port
    end.

put_port(Key, Port) ->
    put(Key, Port).
