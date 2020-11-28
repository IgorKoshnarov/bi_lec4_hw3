-module(bs02).
-export([words/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

words(Bin) ->
    reverse(words(Bin, <<>>, [])).

words(<<" ", Rest/binary>>, <<>>, List) ->
    words(Rest, <<>>, List);
words(<<" ", Rest/binary>>, Word, List) ->
    words(Rest, <<>>, [Word | List]);
words(<<A/utf8, Rest/binary>>, Word, List) ->
    words(Rest, <<Word/binary, A/utf8>>, List);
words(<<>>, Word, List) ->
    [Word | List].

reverse(L) ->
    reverse(L, []).

reverse([H | T], L) ->
    reverse(T, [H | L]);
reverse([], L) ->
    L.

-ifdef(TEST).

words_test_() -> [
    ?_assertEqual(words(<<"Text with four words">>), 
    [<<"Text">>, <<"with">>, <<"four">>, <<"words">>]),
    ?_assertEqual(words(<<" Text with other four words">>), 
    [<<"Text">>, <<"with">>, <<"other">>, <<"four">>, <<"words">>])
].

-endif.