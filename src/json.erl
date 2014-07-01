%% The MIT License

%% Copyright (c) 2014 alisdair sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(json).
-compile({no_auto_import,[get/1, apply/3]}).

-export([from_binary/1, to_binary/1]).
-export([get/2, add/3, remove/2, replace/3, copy/3, move/3, test/3, apply/3]).
-export([get/1, add/2, remove/1, replace/2, copy/2, move/2, test/2, apply/2]).
-export([patch/2, fold/2, keys/2]).
-export([init/1, handle_event/2]).


-type path() :: binary() | [atom() | integer() | binary()].
-type json() :: #{binary() => json()}
  | [json()]
  | integer()
  | float()
  | binary()
  | true
  | false
  | null.


-spec from_binary(JSON::binary()) -> json().

from_binary(JSON) ->
  try (jsx:decoder(?MODULE, [], []))(JSON)
  catch error:_ -> erlang:error(badarg)
  end.


-spec to_binary(JSON::json()) -> binary().

to_binary(JSON) ->
  try jsx:encode(JSON)
  catch error:_ -> erlang:error(badarg)
  end.


-spec get(Path::path(), JSON::json()) -> json().
-spec get(Path::path()) -> fun((JSON::json()) -> json()).

get(Path, JSON) ->
  try get0(maybe_decode(Path), JSON)
  catch error:_ -> erlang:error(badarg)
  end.

get(Path) -> fun(JSON) -> get(Path, JSON) end.


-spec add(Path::path(), Value::json(), JSON::json()) -> json().
-spec add(Path::path(), Value::json()) -> fun((JSON::json()) -> json()).

add(Path, Value, JSON) ->
  try add0(maybe_decode(Path), Value, JSON)
  catch error:_ -> erlang:error(badarg)
  end.

add(Path, Value) -> fun(JSON) -> add(Path, Value, JSON) end.


-spec remove(Path::path(), JSON::json()) -> json().
-spec remove(Path::path()) -> fun((JSON::json()) -> json()).

remove(Path, JSON) ->
  try remove0(maybe_decode(Path), JSON)
  catch error:_ -> erlang:error(badarg)
  end.

remove(Path) -> fun(JSON) -> remove(Path, JSON) end.


-spec replace(Path::path(), Value::json(), JSON::json()) -> json().
-spec replace(Path::path(), Value::json()) -> fun((JSON::json()) -> json()).

replace(Path, Value, JSON) ->
  try replace0(maybe_decode(Path), Value, JSON)
  catch error:_ -> erlang:error(badarg)
  end.

replace(Path, Value) -> fun(JSON) -> replace(Path, Value, JSON) end.


-spec copy(From::path(), To::path(), JSON::json()) -> json().
-spec copy(From::path(), To::path()) -> fun((JSON::json()) -> json()).

copy(From, To, JSON) ->
  try add(To, get(From, JSON), JSON)
  catch error:_ -> erlang:error(badarg)
  end.

copy(From, To) -> fun(JSON) -> copy(From, To, JSON) end.


-spec move(From::path(), To::path(), JSON::json()) -> json().
-spec move(From::path(), To::path()) -> fun((JSON::json()) -> json()).

move(From, To, JSON) ->
  try move0(maybe_decode(From), maybe_decode(To), JSON)
  catch error:_ -> erlang:error(badarg)
  end.

move(From, To) -> fun(JSON) -> move(From, To, JSON) end.


-spec test(Path::path(), Value::json(), JSON::json()) -> json().
-spec test(Path::path(), Value::json()) -> fun((JSON::json()) -> json()).

test(Path, Value, JSON) ->
  try Get = get(Path, JSON), Get = Value, JSON
  catch error:_ -> erlang:error(badarg)
  end.

test(Path, Value) -> fun(JSON) -> test(Path, Value, JSON) end.


-spec apply(Path::path(), Fun::function(), JSON::json()) -> json().
-spec apply(Path::path(), Fun::function()) -> fun((JSON::json()) -> json()).

apply(Path, Fun, JSON) ->
  try replace(Path, Fun(get(Path, JSON)), JSON)
  catch error:_ -> erlang:error(badarg)
  end.

apply(Path, Fun) -> fun(JSON) -> apply(Path, Fun, JSON) end.


-spec patch(Ops::[map()], JSON::json()) -> json().

patch(Ops, JSON) -> patch0(Ops, JSON).


-spec fold([function()], JSON::json()) -> json().

fold(Funs, JSON) -> lists:foldl(fun(Fun, IR) -> Fun(IR) end, JSON, Funs).


-spec keys(Path::path(), JSON::json()) -> [binary()].

keys(Path, JSON) ->
  try maps:keys(get(Path, JSON))
  catch error:_ -> erlang:error(badarg)
  end.


% internal functions

maybe_decode(Path) when is_list(Path) -> Path;
maybe_decode(Path) when is_binary(Path) -> jsonpointer:decode(Path).


get0([], JSON) -> JSON;
get0([Ref|Rest], JSON)
when is_binary(Ref), is_map(JSON) ->
  get0(Rest, maps:get(Ref, JSON));
get0([Ref|Rest], JSON)
when is_atom(Ref), is_map(JSON) ->
  get0(Rest, maps:get(atom_to_binary(Ref, utf8), JSON));
get0([Ref|Rest], JSON)
when is_integer(Ref), is_list(JSON) ->
  % jsonpointer arrays are zero indexed, erlang lists are indexed from 1
  get0(Rest, lists:nth(Ref + 1, JSON));
get0([Ref|Rest], JSON)
when is_binary(Ref), is_list(JSON) ->
  get0([jsonpointer:ref_to_int(Ref)] ++ Rest, JSON).


add0([], Value, _JSON) -> Value;
add0([Ref], Value, JSON)
when is_binary(Ref), is_map(JSON) ->
  maps:put(Ref, Value, JSON);
add0([Ref], Value, JSON)
when is_integer(Ref), is_list(JSON) ->
  {A, B} = lists:split(Ref, JSON),
  A ++ [Value] ++ B;
add0([<<"-">>], Value, JSON)
when is_list(JSON) ->
  JSON ++ [Value];
add0([Ref|Rest], Value, JSON)
when is_binary(Ref), is_map(JSON) ->
  maps:update(Ref, add0(Rest, Value, get([Ref], JSON)), JSON);
add0([Ref|Rest], Value, JSON)
when is_integer(Ref), is_list(JSON) ->
  {A, [B|C]} = lists:split(Ref, JSON),
  A ++ [add0(Rest, Value, B)] ++ C;
add0([Ref|Rest], Value, JSON)
when is_atom(Ref), is_map(JSON) ->
  add0([atom_to_binary(Ref, utf8)] ++ Rest, Value, JSON);
add0([Ref|Rest], Value, JSON)
when is_binary(Ref), is_list(JSON) ->
  add0([jsonpointer:ref_to_int(Ref)] ++ Rest, Value, JSON).


remove0([], _JSON) -> erlang:error(badarg);
remove0([Ref], JSON)
when is_binary(Ref), is_map(JSON) ->
  case maps:is_key(Ref, JSON) of
    true -> maps:remove(Ref, JSON);
    false -> erlang:error(badarg)
  end;
remove0([Ref], JSON)
when is_integer(Ref), is_list(JSON) ->
  {A, [_|B]} = lists:split(Ref, JSON),
  A ++ B;
remove0([Ref|Rest], JSON)
when is_binary(Ref), is_map(JSON) ->
  maps:update(Ref, remove0(Rest, get([Ref], JSON)), JSON);
remove0([Ref|Rest], JSON)
when is_integer(Ref), is_list(JSON) ->
  {A, [B|C]} = lists:split(Ref, JSON),
  A ++ [remove0(Rest, B)] ++ C;
remove0([Ref|Rest], JSON)
when is_atom(Ref), is_map(JSON) ->
  remove0([atom_to_binary(Ref, utf8)] ++ Rest, JSON);
remove0([Ref|Rest], JSON)
when is_binary(Ref), is_list(JSON) ->
  remove0([jsonpointer:ref_to_int(Ref)] ++ Rest, JSON).


replace0([], Value, _JSON) -> Value;
replace0(Path, Value, JSON) -> add(Path, Value, remove(Path, JSON)).


move0([], To, JSON) when is_map(JSON) -> add(To, JSON, #{});
move0([], To, JSON) when is_list(JSON) -> add(To, JSON, []);
move0(From, To, JSON) ->
  Value = get(From, JSON),
  add(To, Value, remove(From, JSON)).


patch0(Ops, JSON) ->
  fold([ case maps:get(<<"op">>, Op) of
    <<"add">> ->
      Path = maps:get(<<"path">>, Op),
      Value = maps:get(<<"value">>, Op),
      json:add(Path, Value);
    <<"remove">> ->
      Path = maps:get(<<"path">>, Op),
      json:remove(Path);
    <<"replace">> ->
      Path = maps:get(<<"path">>, Op),
      Value = maps:get(<<"value">>, Op),
      json:replace(Path, Value);
    <<"copy">> ->
      Path = maps:get(<<"path">>, Op),
      From = maps:get(<<"from">>, Op),
      json:copy(From, Path);
    <<"move">> ->
      Path = maps:get(<<"path">>, Op),
      From = maps:get(<<"from">>, Op),
      json:move(From, Path);
    <<"test">> ->
      Path = maps:get(<<"path">>, Op),
      Value = maps:get(<<"value">>, Op),
      json:test(Path, Value)
  end || Op <- Ops ], JSON).


% replace the decode backend of jsx with one that produces maps

-type state() :: [any()].
-spec init([]) -> state().

init([]) -> [].


-spec handle_event(Event::any(), State::state()) -> state().

handle_event(end_json, State) -> State;
handle_event(start_object, State) -> start_object(State);
handle_event(end_object, State) -> finish(State);
handle_event(start_array, State) -> start_array(State);
handle_event(end_array, State) -> finish(State);
handle_event({key, Key}, State) -> insert(Key, State);
handle_event({_, Event}, State) -> insert(Event, State).

%% internal state is a stack of in progress objects/arrays
%%  `[Current, Parent, Grandparent,...OriginalAncestor]`
%% an object has the representation on the stack of
%%  `{object, #{NthKey => NthValue, NMinus1Key => NthMinus1Value,...FirstKey => FirstValue}}`
%% of if there's a key with a yet to be matched value
%%  `{object, Key, #{NthKey => NthValue},...}}`
%% an array looks like
%%  `{array, [NthValue, NthMinus1Value,...FirstValue]}`

%% allocate a new object on top of the stack
start_object(Stack) -> [{object, #{}}] ++ Stack.

%% allocate a new array on top of the stack
start_array(Stack) -> [{array, []}] ++ Stack.

%% finish an object or array and insert it into the parent object if it exists or
%%  return it if it is the root object
finish([{object, EmptyMap}])
when is_map(EmptyMap), map_size(EmptyMap) < 1 ->
  #{};
finish([{object, EmptyMap}|Rest])
when is_map(EmptyMap), map_size(EmptyMap) < 1 ->
  insert(#{}, Rest);
finish([{object, Pairs}]) -> Pairs;
finish([{object, Pairs}|Rest]) -> insert(Pairs, Rest);
finish([{array, Values}]) -> lists:reverse(Values);
finish([{array, Values}|Rest]) -> insert(lists:reverse(Values), Rest);
finish(_) -> erlang:error(badarg).

%% insert a value when there's no parent object or array
insert(Value, []) -> Value;
%% insert a key or value into an object or array, autodetects the 'right' thing
insert(Key, [{object, Pairs}|Rest]) -> [{object, Key, Pairs}] ++ Rest;
insert(Value, [{object, Key, Pairs}|Rest]) -> [{object, maps:put(Key, Value, Pairs)}] ++ Rest;
insert(Value, [{array, Values}|Rest]) -> [{array, [Value] ++ Values}] ++ Rest;
insert(_, _) -> erlang:error(badarg).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


encode_test_() -> [{"empty object", ?_assertEqual(<<"{}">>, to_binary(#{}))}].

decode_test_() ->
  [
    {"empty object", ?_assertEqual(#{}, from_binary(<<"{}">>))},
    {"simple object", ?_assertEqual(
      #{<<"key">> => <<"value">>},
      from_binary(<<"{\"key\": \"value\"}">>)
    )},
    {"nested object", ?_assertEqual(
      #{<<"key">> => #{<<"key">> => <<"value">>}},
      from_binary(<<"{\"key\": {\"key\": \"value\"}}">>)
    )},
    {"complex object", ?_assertEqual(
      #{<<"key">> => [
          #{<<"key">> => <<"value">>},
          #{<<"key">> => []},
          #{<<"key">> => 1.0},
          true,
          false,
          null
        ],
        <<"another key">> => #{}
      },
      from_binary(<<"{\"key\": [
            {\"key\": \"value\"},
            {\"key\": []},
            {\"key\": 1.0},
            true,
            false,
            null
        ], \"another key\": {}
      }">>)
    )},
    {"empty list", ?_assertEqual([], from_binary(<<"[]">>))},
    {"raw value", ?_assertEqual(1.0, from_binary(<<"1.0">>))}
  ].


get_test_() ->
  JSON = #{
    <<"a">> => 1,
    <<"b">> => #{<<"c">> => 2},
    <<"d">> => #{
      <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
    }
  },
  [
    ?_assertEqual(JSON, get(<<>>, JSON)),
    ?_assertEqual(JSON, get([], JSON)),
    ?_assertEqual(1, get(<<"/a">>, JSON)),
    ?_assertEqual(1, get([<<"a">>], JSON)),
    ?_assertEqual(1, get([a], JSON)),
    ?_assertEqual(2, get(<<"/b/c">>, JSON)),
    ?_assertEqual(2, get([<<"b">>, <<"c">>], JSON)),
    ?_assertEqual(2, get([b, c], JSON)),
    ?_assertEqual(3, get(<<"/d/e/0/a">>, JSON)),
    ?_assertEqual(3, get([<<"d">>, <<"e">>, <<"0">>, <<"a">>], JSON)),
    ?_assertEqual(3, get([d, e, 0, a], JSON)),
    ?_assertEqual(4, get(<<"/d/e/1/b">>, JSON)),
    ?_assertEqual(4, get([<<"d">>, <<"e">>, <<"1">>, <<"b">>], JSON)),
    ?_assertEqual(4, get([d, e, 1, b], JSON)),
    ?_assertEqual(5, get(<<"/d/e/2/c">>, JSON)),
    ?_assertEqual(5, get([<<"d">>, <<"e">>, <<"2">>, <<"c">>], JSON)),
    ?_assertEqual(5, get([d, e, 2 ,c], JSON)),
    ?_assertError(badarg, get(<<"/e">>, JSON)),
    ?_assertError(badarg, get(<<"a">>, JSON)),
    ?_assertError(badarg, get(<<"a/">>, JSON)),
    ?_assertError(badarg, get(a, JSON)),
    ?_assertError(badarg, get(1, JSON))
  ].


add_test_() ->
  [
    ?_assertEqual(<<"foo">>, add(<<>>, <<"foo">>, #{})),
    ?_assertEqual(<<"foo">>, add([], <<"foo">>, #{})),
    ?_assertEqual(<<"foo">>, add(<<>>, <<"foo">>, #{<<"bar">> => <<"baz">>})),
    ?_assertEqual(<<"foo">>, add([], <<"foo">>, #{<<"bar">> => <<"baz">>})),
    ?_assertEqual(
      #{<<"foo">> => <<"baz">>},
      add(<<"/foo">>, <<"baz">>, #{<<"foo">> => <<"bar">>})
    ),
    ?_assertEqual(
      #{<<"foo">> => <<"baz">>},
      add([<<"foo">>], <<"baz">>, #{<<"foo">> => <<"bar">>})
    ),
    ?_assertEqual(
      #{<<"foo">> => <<"baz">>},
      add([foo], <<"baz">>, #{<<"foo">> => <<"bar">>})
    ),
    ?_assertEqual(
      #{<<"baz">> => <<"qux">>, <<"foo">> => <<"bar">>},
      add(<<"/baz">>, <<"qux">>, #{<<"foo">> => <<"bar">>})
    ),
    ?_assertEqual(
      #{<<"foo">> => [<<"bar">>, <<"qux">>, <<"baz">>]},
      add(<<"/foo/1">>, <<"qux">>, #{<<"foo">> => [<<"bar">>, <<"baz">>]})
    ),
    ?_assertEqual(
      #{<<"foo">> => [<<"bar">>, <<"qux">>, <<"baz">>]},
      add([<<"foo">>, 1], <<"qux">>, #{<<"foo">> => [<<"bar">>, <<"baz">>]})
    ),
    ?_assertEqual(
      #{<<"foo">> => [#{<<"bar">> => 1}]},
      add(<<"/foo/0/bar">>, 1, #{<<"foo">> => [#{<<"bar">> => 0}]})
    ),
    ?_assertEqual(
      #{<<"foo">> => [#{<<"bar">> => 1}]},
      add([<<"foo">>, 0, <<"bar">>], 1, #{<<"foo">> => [#{<<"bar">> => 0}]})
    ),
    ?_assertEqual(
      #{
        <<"foo">> => <<"bar">>,
        <<"child">> => #{<<"grandchild">> => #{}}
      },
      add(<<"/child">>, #{<<"grandchild">> => #{}}, #{<<"foo">> => <<"bar">>})
    ),
    ?_assertEqual(
      #{<<"foo">> => [<<"bar">>, [<<"abc">>, <<"def">>]]},
      add(<<"/foo/-">>, [<<"abc">>, <<"def">>], #{<<"foo">> => [<<"bar">>]})
    ),
    ?_assertError(
      badarg,
      add(<<"/baz/bat">>, <<"qux">>, #{<<"foo">> => <<"bar">>})
    )
  ].


remove_test_() ->
  JSON = #{
    <<"a">> => 1,
    <<"b">> => #{<<"c">> => 2},
    <<"d">> => #{
      <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
    }
  },
  [
    ?_assertError(badarg, remove(<<>>, JSON)),
    ?_assertError(badarg, remove([], JSON)),
    ?_assertEqual(
      #{
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove(<<"/a">>, JSON)
    ),
    ?_assertEqual(
      #{
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove([<<"a">>], JSON)
    ),
    ?_assertEqual(
      #{
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove([a], JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove(<<"/b/c">>, JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove([<<"b">>, <<"c">>], JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove([b, c], JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove(<<"/d/e/0/a">>, JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove([<<"d">>, <<"e">>, <<"0">>, <<"a">>], JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      remove([d, e, 0, a], JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{}, #{<<"c">> => 5}]
        }
      },
      remove(<<"/d/e/1/b">>, JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{}, #{<<"c">> => 5}]
        }
      },
      remove([<<"d">>, <<"e">>, <<"1">>, <<"b">>], JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{}, #{<<"c">> => 5}]
        }
      },
      remove([d, e, 1, b], JSON)
    ),
    ?_assertError(badarg, remove(<<"/e">>, JSON)),
    ?_assertError(badarg, remove(<<"a">>, JSON)),
    ?_assertError(badarg, remove(<<"a/">>, JSON)),
    ?_assertError(badarg, remove(a, JSON)),
    ?_assertError(badarg, remove(1, JSON))
  ].


replace_test_() ->
  JSON = #{
    <<"a">> => 1,
    <<"b">> => #{<<"c">> => 2},
    <<"d">> => #{
      <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
    }
  },
  [
    ?_assertEqual(#{}, replace(<<>>, #{}, JSON)),
    ?_assertEqual(#{}, replace([], #{}, JSON)),
    ?_assertEqual(
      #{
        <<"a">> => 2,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      replace(<<"/a">>, 2, JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 3},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      replace(<<"/b/c">>, 3, JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 4}, #{<<"b">> => 4}, #{<<"c">> => 5}]
        }
      },
      replace(<<"/d/e/0/a">>, 4, JSON)
    ),
    ?_assertEqual(
      #{
        <<"a">> => 1,
        <<"b">> => #{<<"c">> => 2},
        <<"d">> => #{
          <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 5}, #{<<"c">> => 5}]
        }
      },
      replace(<<"/d/e/1/b">>, 5, JSON)
    ),
    ?_assertError(badarg, replace(<<"/e">>, #{}, JSON)),
    ?_assertError(badarg, replace(<<"a">>, #{}, JSON)),
    ?_assertError(badarg, replace(<<"a/">>, #{}, JSON)),
    ?_assertError(badarg, replace(a, #{}, JSON)),
    ?_assertError(badarg, replace(1, #{}, JSON))
  ].


copy_test_() ->
  JSON = #{<<"foo">> => <<"bar">>},
  [
    ?_assertEqual(JSON, copy([foo], [foo], JSON)),
    ?_assertEqual(#{<<"foo">> => JSON}, copy([], [foo], JSON)),
    ?_assertEqual([[1], 1], copy([], [0], [1])),
    ?_assertEqual(#{<<"foo">> => <<"bar">>, <<"qux">> => <<"bar">>}, copy([foo], [qux], JSON)),
    ?_assertError(badarg, copy([qux], [foo], JSON))
  ].


move_test_() ->
  JSON = #{<<"foo">> => <<"bar">>},
  [
    ?_assertEqual(JSON, move([foo], [foo], JSON)),
    ?_assertEqual(#{<<"foo">> => JSON}, move([], [foo], JSON)),
    ?_assertEqual([[1]], move([], [0], [1])),
    ?_assertEqual(#{<<"qux">> => <<"bar">>}, move([foo], [qux], JSON)),
    ?_assertError(badarg, move([qux], [foo], JSON))
  ].


test_test_() ->
  JSON = #{
    <<"a">> => 1,
    <<"b">> => #{<<"c">> => 2},
    <<"d">> => #{
      <<"e">> => [#{<<"a">> => 3}, #{<<"b">> => 4}]
    }
  },
  [
    ?_assertEqual(JSON, test(<<"/a">>, 1, JSON)),
    ?_assertEqual(JSON, test(<<"/b/c">>, 2, JSON)),
    ?_assertEqual(JSON, test(<<"/d/e/0/a">>, 3, JSON)),
    ?_assertEqual(JSON, test(<<"/d/e/1/b">>, 4, JSON)),
    ?_assertError(badarg, test(<<"/e">>, false, JSON))
  ].


apply_test_() ->
  [
    ?_assertEqual(#{<<"key">> => false}, apply(<<"/key">>, fun(true) -> false end, #{<<"key">> => true})),
    ?_assertError(badarg, apply(<<"/nokey">>, fun(_) -> ok end, #{})),
    ?_assertError(badarg, apply(<<"/key">>, fun(_) -> erlang:error(noerror) end, #{<<"key">> => true}))
  ].


patch_test_() ->
  JSON = #{<<"foo">> => <<"bar">>},
  [
    ?_assertEqual(
      #{<<"foo">> => <<"baz">>},
      patch([
          #{<<"op">> => <<"test">>, <<"path">> => <<"/foo">>, <<"value">> => <<"bar">>},
          #{<<"op">> => <<"copy">>, <<"from">> => <<"/foo">>, <<"path">> => <<"/qux">>},          
          #{<<"op">> => <<"test">>, <<"path">> => <<"/qux">>, <<"value">> => <<"bar">>},
          #{<<"op">> => <<"replace">>, <<"path">> => <<"/qux">>, <<"value">> => <<"baz">>},
          #{<<"op">> => <<"remove">>, <<"path">> => <<"/foo">>},
          #{<<"op">> => <<"move">>, <<"from">> => <<"/qux">>, <<"path">> => <<"/foo">>},
          #{<<"op">> => <<"test">>, <<"path">> => <<"/foo">>, <<"value">> => <<"baz">>}
        ], JSON
      )
    )
  ].


fold_test_() ->
  JSON = #{<<"foo">> => <<"bar">>},
  [
    ?_assertEqual(
      #{<<"foo">> => <<"baz">>},
      fold([
          test(<<"/foo">>, <<"bar">>),
          copy(<<"/foo">>, <<"/qux">>),
          test(<<"/qux">>, <<"bar">>),
          replace(<<"/qux">>, <<"baz">>),
          remove(<<"/foo">>),
          move(<<"/qux">>, <<"/foo">>),
          test(<<"/foo">>, <<"baz">>)
        ], JSON
      )
    )
  ].


keys_test_() ->
  [
    ?_assertEqual([<<"bar">>, <<"foo">>], keys(<<>>, #{<<"foo">> => 1, <<"bar">> => 1})),
    ?_assertEqual([<<"bar">>, <<"baz">>], keys(
      <<"/foo">>,
      #{<<"foo">> => #{<<"bar">> => true, <<"baz">> => true}}
    )),
    ?_assertError(badarg, keys(<<>>, [1,2,3]))
  ].


-endif.