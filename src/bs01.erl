-module(bs01).
-export([first_word/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

first_word(Bin) ->
    first_word(Bin, <<>>).

first_word(<<" ", Rest/binary>>, <<>>) ->
    first_word(Rest, <<>>);
first_word(<<" ", _Rest/binary>>, Acc) ->
    Acc;
first_word(<<A, Rest/binary>>, Acc) ->
    first_word(Rest, <<Acc/binary, A>>);
first_word(<<>>, Acc) ->
    Acc.

-ifdef(TEST).

first_word_test_() -> [
    ?_assertEqual(first_word(<<"Some Words">>), <<"Some">>),
    ?_assertEqual(first_word(<<" Some Words">>), <<"Some">>)
].

-endif.