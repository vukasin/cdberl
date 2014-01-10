-module(cdberl).

-export([writer/1,
         reader/1,
         add/3,
         finish/1,
         seek/2,
         close_reader/1,
         next/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

reader(Path) ->
    cdberl_nif:new(Path).

seek(Ref, Key) ->
    cdberl_nif:seek(Ref, Key).

next(Ref) ->
    cdberl_nif:next(Ref).

close_reader(Ref) ->
    cdberl_nif:close(Ref).

writer(Path) ->
    cdberlwriter_nif:new(Path).

add(Ref, Key, Value) ->
    cdberlwriter_nif:add(Ref, Key, Value).

finish(Ref) ->
    cdberlwriter_nif:finish(Ref).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    Path = <<"test.cdb">>,
    {ok, Ref} = writer(Path),
    ?assertEqual(ok, add(Ref, <<"greeting">>, <<"hello">>)),
    ?assertEqual(ok, add(Ref, <<"greeting">>, <<"world">>)),
    ?assertEqual(ok, finish(Ref)), 
    {ok, Ref2} = reader(Path),
    ?assertEqual(ok, seek(Ref2, <<"greeting">>)),
    ?assertEqual({ok, <<"hello">>}, next(Ref2)),
    ?assertEqual({ok, <<"world">>}, next(Ref2)),
    ?assertEqual(eof, next(Ref2)),
    ?assertEqual(ok, seek(Ref2, <<"greetingZ">>)),
    ?assertEqual(eof, next(Ref2)),
    ?assertEqual(ok, seek(Ref2, <<"greeting">>)),
    ?assertEqual({ok, <<"hello">>}, next(Ref2)),
    ?assertEqual({ok, <<"world">>}, next(Ref2)),
    ?assertEqual(eof, next(Ref2)),
    ok.
-endif.
