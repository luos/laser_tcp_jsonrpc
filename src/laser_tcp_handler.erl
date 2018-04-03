-module(laser_tcp_handler).

-compile({no_auto_import,[error/2]}).

-export([
    loop/1,
    init/2,
    send_error/3
]).

-record(state, {
    socket,
    receiver :: pid(),
    parse_state :: laser_jsonrpc_parser:parse_state()
}).

send_error(Connection, ErrorCode, Message)->
    Connection ! {send_error, ErrorCode, Message}.

init(LS, ReceiverFn) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            lager:info("Accepted listen sock connection"),
            {ok, ReceiverPid} = ReceiverFn(self()),
            lager:info("Bleh ~p",[ReceiverPid]),
            loop(#state{
                socket = S,
                receiver = ReceiverPid,
                parse_state = laser_jsonrpc_parser:empty_parse_state()
            }),
            init(LS, ReceiverFn);
        Other ->
            io:format("accept returned ~p - goodbye!~n",[Other]),
            ok
    end.

loop(State = #state{socket = Socket,
                    receiver = Receiver,
                    parse_state = ParseState}) ->
    inet:setopts(Socket,[{active,once}]),
    receive
        {tcp,Socket,Data} ->
            lager:info("~p: Got data: ~s ~p ~p",[Socket, binary_to_list(Data), State, Receiver]),
            case laser_jsonrpc_parser:process(Data, ParseState) of 
                {Messages, ParseState2} -> 
                    process_messages(Socket, Receiver, Messages),
                    loop(State#state{
                        parse_state = ParseState2
                    })     
            end;
        {tcp_closed,Socket} ->
            io:format("Socket ~w closed [~w]~n",[Socket,self()]),
            ok;
        {send_error, ErrorCode, Message} -> 
            Msg = error(0, [{<<"message">>, Message}, {<<"code">>, ErrorCode}]),
            send_message(Socket, Msg),
            loop(State);
        Unknown -> lager:info("Unknown message ~p",[Unknown])
    end.

process_messages(_, _, []) -> ok;

process_messages(Socket, Receiver, [Msg | Messages]) -> 
        Answer = process(Receiver, Msg),
        case Answer of 
            noreply -> 
                ok;
            {reply, Reply} -> send_message(Socket, Reply)
        end,
        process_messages(Socket, Receiver, Messages).


process(_Receiver, {invalid, <<>>}) -> 
  noreply;

process(_Receiver, {invalid, Msg}) -> 
    {reply, 
        error(undefined, <<"unexpected message: ", Msg/binary>>)
    };

process(Receiver, Data) -> 
    try jsx:decode(Data) of
        Decoded -> 
            lager:info("Got data: ~p sendng to wp: ~p",[Decoded, Receiver]),
            Method = proplists:get_value(<<"method">>, Decoded),
            Params = proplists:get_value(<<"params">>, Decoded),
            Id = proplists:get_value(<<"id">>, Decoded),
            Response = case call(Receiver, Method, Params) of 
                {ok, Result} -> result(Id, Result);
                unknown_message -> error(Id, <<"invalid_message">>)
            end,
            lager:info("Got response: ~p",[Response]),
            {reply, 
                Response
            }
    catch
        error:badarg ->
            lager:error("Couldn't decode json: ~p",[Data]),
            noreply
    end.

result(Id, Result) -> [{<<"result">>, Result}, {<<"id">>, Id}, {<<"jsonrpc">>, <<"2.0">>}].
error(Id, Result) -> [{<<"error">>, Result}, {<<"id">>, Id}, {<<"jsonrpc">>, <<"2.0">>}].

send_message(Socket, Message) ->
    Encoded = jsx:encode(Message),
    lager:info("Sending message...~p",[Encoded]),
    ContentLength = io_lib:format("Content-Length: ~w\r\n\r\n", [size(Encoded)]),
    gen_tcp:send(Socket, ContentLength),
    gen_tcp:send(Socket, Encoded).



call(Receiver, Method, Params) ->
  gen_server:call(Receiver, {method_call, {Method, Params}}).