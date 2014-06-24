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
-export([init/1, handle_event/2]).


from_binary(JSON)
when is_binary(JSON) ->
  (jsx:decoder(?MODULE, [], []))(JSON).


to_binary(JSON) ->
  try jsx:encode(JSON)
  catch error:_ -> erlang:error(badarg)
  end.


% replace the decode backend of jsx with our own that produces maps

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


basic_decode_test_() ->
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


-endif.