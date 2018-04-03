-module(laser_jsonrpc_parser).

-export([
    process/2,
    empty_parse_state/0
]).

-export_type([
  parse_state/0,
  parsed_message/0
]).

-type parse_state() :: {integer(), binary()}.
-type parsed_message() :: {invalid, binary()} | binary().

-spec process(binary(), parse_state()) 
                  -> {[parsed_message()], parse_state()}.
process(NewData, {MsgSize, Buffer}) -> 
    case process_packet([], MsgSize, <<Buffer/binary, NewData/binary>>) of 
        {Msgs, State} -> {lists:reverse(Msgs), State}
    end.

process_packet(ParsedMessages, 0, <<"Content-Length: ", RestWithNum/binary>>) ->
  {Num, Rest} = get_num(RestWithNum, []),
  OnlyData = remove_newlines(Rest),
  process_packet(ParsedMessages, Num, OnlyData);

process_packet(ParsedMessages, 0, <<>>) -> 
  {ParsedMessages, {0, <<>>}};

process_packet(ParsedMessages, 0, SomeData) -> 
  {Start, Rest} = take_until(SomeData, <<$\r,$\n>>),
  Msgs = [{invalid, Start}| ParsedMessages],
  process_packet(Msgs, 0, Rest);

process_packet(ParsedMessages, LeftBytes, <<>>) -> {ParsedMessages, {LeftBytes, <<>>}};

process_packet(ParsedMessages, MsgSize, Rest)
  when size(Rest) < MsgSize, is_binary(Rest) ->
  {ParsedMessages, {MsgSize, Rest}}; 

process_packet(ParsedMessages, MsgSize, Rest) 
    when is_binary(Rest), size(Rest) >= MsgSize ->
  <<ThisPacketData:MsgSize/binary, Rest2/binary>> = Rest,
  Msgs = [ThisPacketData | ParsedMessages],
  case remove_newlines(Rest2) of 
      Rest3 when size(Rest3) > 0 -> 
          process_packet(Msgs, 0, Rest3);
      _ -> {Msgs, {0, <<>>}}
  end.

-spec empty_parse_state() -> parse_state().
empty_parse_state() -> {0, <<>>}.

remove_newlines(<<$\r, $\n, Rest/binary>>) ->
  remove_newlines(Rest);

remove_newlines(<<Rest/binary>>) ->
  Rest.

get_num(<<N:8, Rest/binary>>, Acc) when N >= $0, N =< $9 ->
  get_num(Rest, [N | Acc]);

get_num(Rest, Acc) ->
  {list_to_integer(lists:reverse(Acc)), Rest}.

take_until(Binary, Match) when is_binary(Binary), is_binary(Match) -> 
  take_until(Binary, Match, 0).

take_until(Bin, Match, Beginning) -> 
  MatchSize = size(Match),
  case Bin of 
    <<Start:Beginning/binary, Match:MatchSize/binary, Rest/binary>> -> 
      {Start, Rest};
      <<_Start:Beginning/binary, _Rest/binary>> ->
        take_until(Bin, Match, Beginning + 1);
      Bin -> 
        {Bin, <<>>}
  end.



