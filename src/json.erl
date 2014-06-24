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

-export([from_binary/1, to_binary/1]).
-export([get/2, add/3]).
-export([init/1, handle_event/2]).


-type path() :: binary() | [atom() | integer() | binary()].
-type json() :: #{integer() | atom() | binary() => json()}
  | [json()]
  | integer()
  | float()
  | binary()
  | true
  | false
  | null.


-spec from_binary(binary()) -> json().

from_binary(JSON) ->
  try (jsx:decoder(?MODULE, [], []))(JSON)
  catch error:_ -> erlang:error(badarg)
  end.

-spec to_binary(json()) -> binary().

to_binary(JSON) ->
  try jsx:encode(JSON)
  catch error:_ -> erlang:error(badarg)
  end.

-spec get(path(), json()) -> json().

get(Path, JSON) ->
  try get0(maybe_decode(Path), JSON)
  catch error:_ -> erlang:error(badarg)
  end.

-spec add(path(), json(), json()) -> json().

add(Path, JSON, Value) ->
  try add0(maybe_decode(Path), JSON, Value)
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

add0([], _JSON, Value) -> Value;
add0([Ref], JSON, Value)
when is_binary(Ref), is_map(JSON) ->
  maps:put(Ref, Value, JSON);
add0([Ref], JSON, Value)
when is_integer(Ref), is_list(JSON) ->
  {A, B} = lists:split(Ref, JSON),
  A ++ [Value] ++ B;
add0([<<"-">>], JSON, Value)
when is_list(JSON) ->
  JSON ++ [Value];
add0([Ref|Rest], JSON, Value)
when is_binary(Ref), is_map(JSON) ->
  maps:update(Ref, add0(Rest, get([Ref], JSON), Value), JSON);
add0([Ref|Rest], JSON, Value)
when is_integer(Ref), is_list(JSON) ->
  {A, [B|C]} = lists:split(Ref, JSON),
  A ++ [add0(Rest, B, Value)] ++ C;
add0([Ref|Rest], JSON, Value)
when is_atom(Ref), is_map(JSON) ->
  add0([atom_to_binary(Ref, utf8)] ++ Rest, JSON, Value);
add0([Ref|Rest], JSON, Value)
when is_binary(Ref), is_list(JSON) ->
  add0([jsonpointer:ref_to_int(Ref)] ++ Rest, JSON, Value).


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
    ?_assertEqual(<<"foo">>, add(<<>>, #{}, <<"foo">>)),
    ?_assertEqual(<<"foo">>, add([], #{}, <<"foo">>)),
    ?_assertEqual(<<"foo">>, add(<<>>, #{<<"bar">> => <<"baz">>}, <<"foo">>)),
    ?_assertEqual(<<"foo">>, add([], #{<<"bar">> => <<"baz">>}, <<"foo">>)),
    ?_assertEqual(
      #{<<"foo">> => <<"baz">>},
      add(<<"/foo">>, #{<<"foo">> => <<"bar">>}, <<"baz">>)
    ),
    ?_assertEqual(
      #{<<"foo">> => <<"baz">>},
      add([<<"foo">>], #{<<"foo">> => <<"bar">>}, <<"baz">>)
    ),
    ?_assertEqual(
      #{<<"foo">> => <<"baz">>},
      add([foo], #{<<"foo">> => <<"bar">>}, <<"baz">>)
    ),
    ?_assertEqual(
      #{<<"baz">> => <<"qux">>, <<"foo">> => <<"bar">>},
      add(<<"/baz">>, #{<<"foo">> => <<"bar">>}, <<"qux">>)
    ),
    ?_assertEqual(
      #{<<"foo">> => [<<"bar">>, <<"qux">>, <<"baz">>]},
      add(<<"/foo/1">>, #{<<"foo">> => [<<"bar">>, <<"baz">>]}, <<"qux">>)
    ),
    ?_assertEqual(
      #{<<"foo">> => [<<"bar">>, <<"qux">>, <<"baz">>]},
      add([<<"foo">>, 1], #{<<"foo">> => [<<"bar">>, <<"baz">>]}, <<"qux">>)
    ),
    ?_assertEqual(
      #{<<"foo">> => [#{<<"bar">> => 1}]},
      add(<<"/foo/0/bar">>, #{<<"foo">> => [#{<<"bar">> => 0}]}, 1)
    ),
    ?_assertEqual(
      #{<<"foo">> => [#{<<"bar">> => 1}]},
      add([<<"foo">>, 0, <<"bar">>], #{<<"foo">> => [#{<<"bar">> => 0}]}, 1)
    ),
    ?_assertEqual(
      #{
        <<"foo">> => <<"bar">>,
        <<"child">> => #{<<"grandchild">> => #{}}
      },
      add(<<"/child">>, #{<<"foo">> => <<"bar">>}, #{<<"grandchild">> => #{}})
    ),
    ?_assertEqual(
      #{<<"foo">> => [<<"bar">>, [<<"abc">>, <<"def">>]]},
      add(<<"/foo/-">>, #{<<"foo">> => [<<"bar">>]}, [<<"abc">>, <<"def">>])
    ),
    ?_assertError(
      badarg,
      add(<<"/baz/bat">>, #{<<"foo">> => <<"bar">>}, <<"qux">>)
    )
  ].


-endif.