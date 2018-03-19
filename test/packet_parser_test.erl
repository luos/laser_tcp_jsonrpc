-module(packet_parser_test).
-include_lib("eunit/include/eunit.hrl").

get_num(<<N:8, Rest/binary>>, Acc) when N >= $0, N =< $9 ->
  get_num(Rest, [N | Acc]);

get_num(Rest, Acc) ->
  {list_to_integer(lists:reverse(Acc)), Rest}.


-type message() :: [
  {headers, [term()]}
].
-type packet_state() :: term().
-spec process_packet(Packet :: binary(), PacketState :: packet_state()) -> {
  Messages :: [message()], packet_state()
}.
process_packet(<<"Content-Length: ", RestWithNum/binary>>, {0, <<>>}) ->
  {Num, Rest} = get_num(RestWithNum, []),
  OnlyData = remove_newlines(Rest),
  process_packet(OnlyData, {Num, <<>>});

process_packet(<<>>, {LeftBytes, ParsedData}) -> {LeftBytes, ParsedData, <<>>};

process_packet(Rest, {LeftBytes, ParsedData})
  when size(Rest) < LeftBytes, is_binary(Rest) ->
  RestLen = size(Rest),
  {LeftBytes - RestLen, erlang:iolist_to_binary([ParsedData, Rest]), <<>>};

process_packet(Rest, {LeftBytes, ParsedData}) when is_binary(Rest) ->
  <<ThisPacketData:LeftBytes/binary, Rest2/binary>> = Rest,
  {0, erlang:iolist_to_binary([ThisPacketData, ParsedData]), Rest2};

process_packet(NewData, {LeftBytes, ParsedData, LeftOver}) ->
  AllData = remove_newlines(erlang:iolist_to_binary([LeftOver, NewData])),
  ?debugFmt("Trying to parse ~p", [AllData]),
  process_packet(AllData, {LeftBytes, ParsedData}).

remove_newlines(<<$\r, $\n, Rest/binary>>) ->
  remove_newlines(Rest);

remove_newlines(<<Rest/binary>>) ->
  Rest.

empty_parse_state() -> {0, <<>>}.



given_test_() ->
  {setup,
    fun() -> ok end,
    fun(_) -> ok end,
    [
      fun given_packet_length_with_content_length_parses_it/0,
      fun given_packet_length_and_content_with_content_length_parses_it/0,
      fun given_longer_packet_than_content_length_returns_leftover_data/0,
      fun given_previous_has_leftover_new_packet_can_be_parsed/0
    ]
  }.

given_packet_length_with_content_length_parses_it() ->
  Packet = <<"Content-Length: 123", $\r, $\n>>,
  Parsed = process_packet(Packet, empty_parse_state()),
  ?assertEqual({123, <<"">>, <<>>}, Parsed).

given_packet_length_and_content_with_content_length_parses_it() ->
  Packet = <<"Content-Length: 123", $\r, $\n, $\r, $\n, "abcdef">>,
  Parsed = process(Packet),
  ?assertEqual({123 - 6, <<"abcdef">>, <<>>}, Parsed).

given_longer_packet_than_content_length_returns_leftover_data() ->
  Packet = <<"Content-Length: 3", $\r, $\n, $\r, $\n, "abc", $\r, $\n, "Content-Length: ...">>,
  Parsed = process(Packet),
  ?assertEqual({0, <<"abc">>, <<$\r, $\n, "Content-Length: ...">>}, Parsed).

given_previous_has_leftover_new_packet_can_be_parsed() ->
  PrevPacket = <<
    "Content-Length: 5", $\r, $\n, $\r, $\n,
    "abcde", $\r, $\n, $\r, $\n,
    "Content-Length: 3", $\r, $\n, $\r, $\n
  >>,
  NextPacket = <<
    "123"
  >>,
  LeftOver = <<$\r, $\n, $\r, $\n, "Content-Length: 3", $\r, $\n, $\r, $\n>>,
  ?assertEqual({0, <<"abcde">>, LeftOver}, process(PrevPacket)),
  {0, <<"123">>, <<>>} = process_packet(NextPacket, {0, <<"">>, LeftOver}),
  ok.

process(Packet) ->
  {BytesLeft, Data, LeftOver} = process_packet(Packet, empty_parse_state()),
  {BytesLeft, Data, LeftOver}.