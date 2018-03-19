-module(laser_tcp_handler).

-compile({no_auto_import,[error/2]}).

-export([
    loop/1,
    init/2,
    send_error/3
]).

-record(state, {
    socket,
    workspace
}).

send_error(Connection, ErrorCode, Message)->
    Connection ! {send_error, ErrorCode, Message}.

init(LS, ReceiverFn) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            lager:info("Accepted listen sock connection"),
            {ok, WorkspacePid} = ReceiverFn(self()),
            loop(#state{
                socket = S,
                workspace = WorkspacePid
            }),
            init(LS, ReceiverFn);
        Other ->
            io:format("accept returned ~p - goodbye!~n",[Other]),
            ok
    end.

loop(State = #state{socket = Socket,workspace = Workspace}) ->
    inet:setopts(Socket,[{active,once}]),
    receive
        {tcp,Socket,Data} ->
            lager:info("~p: Got data: ~s ~p ~p",[Socket, binary_to_list(Data), State, Workspace]),
            Answer = process(Workspace, Data),
            case Answer of 
                noreply -> 
                    lager:info("Ignored  msg"),
                    ok;
                {reply, Reply} -> send_message(Socket, Reply);
                unknown_message -> send_message(Socket, <<"invalid message">>)
            end,
            loop(State);
        {tcp_closed,Socket} ->
            io:format("Socket ~w closed [~w]~n",[Socket,self()]),
            ok;
        {send_error, ErrorCode, Message} -> 
            Msg = error(0, [{<<"message">>, Message}, {<<"code">>, ErrorCode}]),
            send_message(Socket, Msg),
            loop(State);
        Unknown -> lager:info("Unknown message ~p",[Unknown])
    end.

process(_Ws, <<"Content-Length:", _/binary>>) -> noreply;

process(Workspace, Data) -> 
        try jsx:decode(Data) of
            Decoded -> 
                lager:info("Got data: ~p sendng to wp: ~p",[Decoded, Workspace]),
                Method = proplists:get_value(<<"method">>, Decoded),
                Params = proplists:get_value(<<"params">>, Decoded),
                Id = proplists:get_value(<<"id">>, Decoded),
                Response = case laser_workspace:call(Workspace, Method, Params) of 
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
    lager:info("Sending error...~p",[Encoded]),
    ContentLength = io_lib:format("Content-Length: ~w\r\n\r\n", [size(Encoded)]),
    gen_tcp:send(Socket, ContentLength),
    gen_tcp:send(Socket, Encoded).