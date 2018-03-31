-module(packet_parser_test).
-include_lib("eunit/include/eunit.hrl").

packet_parser_test_() ->
  {setup,
    fun() -> ok end,
    fun(_) -> ok end,
    [
      {"given_packet_with_content_length_parses_it",
      fun given_packet_with_content_length_parses_it/0},
      {"given_packet_length_and_content_with_content_length_parses_it",
      fun given_packet_length_and_content_with_content_length_parses_it/0},
      {"given_longer_packet_than_content_length_returns_leftover_data",
      fun given_longer_packet_than_content_length_returns_leftover_data/0},
      {"given_previous_has_leftover_new_packet_can_be_parsed",
      fun given_previous_has_leftover_new_packet_can_be_parsed/0},
      {"given_multiple_messages_in_one_packet_returns_them_in_order", 
      fun given_multiple_messages_in_one_packet_returns_them_in_order/0},
      {"given_invalid_data_returns_message_invalid", 
      fun given_invalid_data_returns_message_invalid/0}
    ]
  }.

given_packet_with_content_length_parses_it() ->
  Packet = <<"Content-Length: 123", $\r, $\n>>,
  Parsed = process_packet(Packet, empty_state()),
  ?assertEqual({[], {123, <<>>}}, Parsed).

given_packet_length_and_content_with_content_length_parses_it() ->
  Packet = <<"Content-Length: 123", $\r, $\n, $\r, $\n, "abcdef">>,
  Parsed = process(Packet),
  ?assertEqual({[], {123, <<"abcdef">>}}, Parsed).

given_longer_packet_than_content_length_returns_leftover_data() ->
  Packet = <<"Content-Length: 3", $\r, $\n, $\r, $\n, "abc", $\r, $\n, "Content-Length: 1">>,
  Parsed = process(Packet),
  ?assertEqual({
    [<<"abc">>], {1, <<"">>}
  }, Parsed).

given_previous_has_leftover_new_packet_can_be_parsed() ->
  PrevPacket = <<
    "Content-Length: 5", $\r, $\n, $\r, $\n,
    "abcde", $\r, $\n, $\r, $\n,
    "Content-Length: 3", $\r, $\n, $\r, $\n
  >>,
  NextPacket = <<
    "123"
  >>,
  ?assertEqual({[<<"abcde">>],{3, <<"">>}}, process(PrevPacket)),
  {[<<"123">>], {0, <<>>}} = process_packet(NextPacket, {3, <<"">>}),
  ok.

given_multiple_messages_in_one_packet_returns_them_in_order() -> 
  Packet = <<
  "Content-Length: 5", $\r, $\n, $\r, $\n,
  "abcde", $\r, $\n, $\r, $\n,
  "Content-Length: 3", $\r, $\n, $\r, $\n,
  "abc", $\r, $\n, $\r, $\n
  >>,
  ?assertEqual({[<<"abcde">>, <<"abc">>], {0, <<>>}}, process(Packet)).

given_invalid_data_returns_message_invalid() -> 
  Packet = <<"blah",$\r, $\n,"bleh">>,
  Res = process(Packet),
  ?assertEqual({[{invalid, <<"blah">>},{invalid, <<"bleh">>}], {0, <<>>}}, Res),
  ok.


process(Packet) ->
  {Messages, State} = 
              process_packet(Packet, empty_state()),
  {Messages, State}.

process_packet(Packet, State) -> laser_jsonrpc_parser:process(Packet, State).

empty_state() -> 
    laser_jsonrpc_parser:empty_parse_state().