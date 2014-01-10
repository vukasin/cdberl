-module(cdberlwriter_nif).

-export([new/1,
         add/3,
         finish/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new(Path) ->
    new(Path, <<Path/binary, <<".XXXXXX">>/binary>>).

new(_Path, _TmpPath) ->
    ?nif_stub.

add(_Ref, _Key, _Value) ->
    ?nif_stub.

finish(_Ref) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(<<"test.cdb">>),
    ?assertEqual(ok, add(Ref, <<"greeting">>, <<"hello">>)),
    ?assertEqual(ok, add(Ref, <<"greeting">>, <<"world">>)),
    ?assertEqual(ok, finish(Ref)), 
    ok.
-endif.
