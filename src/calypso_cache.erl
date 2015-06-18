-module(calypso_cache).
-author("begemot").

%% API
-export([
  start/2, stop/1,
  get/3, get/2, get/4,
  clear/2, put/4
]).

start(Name, Tick) ->
  { ok, _ } = calypso_cache_sup:add_child(Name, Tick).

stop(Name) ->
  ok = calypso_cache_sup:delete_child(Name).

get(Name, Key) ->
  case ets:lookup(Name, Key) of
    [{ _, Value, _ }] -> { ok, Value };
    _ -> undefined
  end.

get(Name, Key, Default) ->
  case get(Name, Key) of
    { ok, Value } -> Value;
    undefined ->
      if
        is_function(Default,0) -> Default();
        true -> Default
      end
  end.

get(Name, Key, Default, TTL) ->
  Value = get(Name, Key, Default),
  put(Name, Key, Value, TTL),
  Value.

clear(Name, Key) ->
  ets:delete(Name, Key).

put(Name, Key, Value, TTL) ->
  ets:insert(Name, { Key, Value, calypso_time:now() + TTL}),
  ok.

