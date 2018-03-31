-module(laser_tcp_server).
-behavior(gen_server).

-export([
  init/1,
  start_link/2,
  handle_call/3,
  handle_cast/2]).

-record(state, {
    port
}).

start_link(Port, ReceiverCreatorFn) ->
  gen_server:start_link(?MODULE, [Port, ReceiverCreatorFn], []).

init([LPort, ReceiverCreatorFn]) ->
  Num = 10,
  lager:info("Start listening... ~p", [LPort]),
  case gen_tcp:listen(LPort, [{active, false}, {packet, 0}, binary, {reuseaddr, true}]) of
    {ok, ListenSock} ->
      {ok, LPort} = inet:port(ListenSock),
      lager:info("~p: Bound to port ~p", [ListenSock, LPort]),
      start_servers(Num, ListenSock, ReceiverCreatorFn),
      {ok, #state{
          port = LPort
      }};
    {error, Reason} ->
      lager:info("Listen error: ~p", [Reason]),
      {error, Reason}
  end.

start_servers(0, _, _ReceiverCreatorFn) ->
  ok;
start_servers(Num, LS, ReceiverCreatorFn) ->
  spawn(laser_tcp_handler, init, [LS, ReceiverCreatorFn]),
  start_servers(Num - 1, LS, ReceiverCreatorFn).

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).