# json #

a high level library for working with json in erlang (17.0+)

**json** is built via [rebar][rebar], and continuous integration testing provided by [travis-ci][travis]

current status: [![Build Status](https://travis-ci.org/talentdeficit/json.svg?branch=master)](https://travis-ci.org/talentdeficit/json)

**json** is released under the terms of the [MIT][MIT] license

copyright 2014 alisdair sullivan

## notes ##

erlang 17.0+ is required to use this library due to it's reliance on the new map implementation

## dependencies ##

[jsx][jsx] is used when converting between erlang binary() and json() types

[jsonpointer][jsonpointer] is used for handling json paths in accordance with [rfc6901][rfc6901].

## index ##

* [quickstart](#quickstart)
* [description](#description)
* [data types](#data-types)
  - [`path()`](#path-type)
  - [`json()`](#json-type)
* [exports](#exports)
  - [`from_binary/1 and to_binary/1`](#from_binary1-and-to_binary1)
  - [`get/1,2`](#get12)
  - [`add/2,3`](#add23)
  - [`remove/1,2`](#remove12)
  - [`replace/2,3`](#replace23)
  - [`copy/2,3`](#copy23)
  - [`move/2,3`](#move23)
  - [`test/2,3`](#test23)
  - [`apply/2,3`](#apply23)
  - [`patch/2`](#patch2)
  - [`fold/2`](#fold2)
  - [`keys/2`](#keys2)
* [acknowledgements](#acknowledgements)

## quickstart ##

#### to build library and run tests  ####

```bash
$ rebar get-deps
$ rebar compile
$ rebar eunit
```

#### conversion between json and utf8 binaries####

convert between json() and binary() erlang types.

`ExampleJSON` will be used as an example JSON input for the rest of the quickstart.

```erlang
1> ExampleJSON = json:from_binary(<<"{\"library\": \"json\", \"awesome\": true,
1> \"list\": [{\"a\": 1}, {\"b\": 2}, {\"c\": 3} ]}">>).
#{<<"awesome">> => true,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}
```

```erlang
1> json:to_binary(ExampleJSON).
<<"{\"awesome\":true,\"library\":\"json\",\"list\":[{\"a\":1},{\"b\":2},{\"c\":3}]}">>
```

#### get json at supplied json path ####

```erlang
1> json:get(<<"/list/0/a">>, ExampleJSON).
1
2> json:get([list, 0, a], ExampleJSON).
1
3> GetJSONFun = json:get([list, 0, a]).
#Fun<json.0.18938731>
4> GetJSONFun(ExampleJSON).
1
```

#### add json to supplied json path ####

```erlang
1> json:add([addition], <<"1+1">>, ExampleJSON).
#{<<"addition">> => <<"1+1">>,
  <<"awesome">> => true,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}
2> json:add([recursion], ExampleJSON, ExampleJSON).
#{<<"awesome">> => true,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"recursion">> => #{<<"awesome">> => true,
    <<"library">> => <<"json">>,
    <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}}
3> json:add([map], #{<<"test2">> => 50}, ExampleJSON).                                  
#{<<"awesome">> => true,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"map">> => #{<<"test2">> => 50}}
4> json:add(<<"/listtest">>, [50, <<"listadd">>, testatom], ExampleJSON).
#{<<"awesome">> => true,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"listtest">> => [50,<<"listadd">>,testatom]}
```

#### remove json at supplied json path ####

```erlang
1> json:remove([list, 2], ExampleJSON).
#{<<"awesome">> => true,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2}]}
```

#### replace json at supplied json path with new json####

```erlang
1> json:replace([awesome], <<"json in erlang!">> ,ExampleJSON).                                                
#{<<"awesome">> => <<"json in erlang!">>,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}

```

#### copy json from one json path to another ####

```erlang
1> json:copy([list],[copiedlist],ExampleJSON).
#{<<"awesome">> => true,
  <<"copiedlist">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}], 
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}
```

#### move json from one json path to another ####

```erlang
1> json:move([library], [newlibrary], ExampleJSON).
#{<<"awesome">> => true,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"newlibrary">> => <<"json">>}
```

#### patch json using a list of ops ####

```erlang
ExampleJSON = json:from_binary(<<"{\"library\": \"json\", \"awesome\": true, \"list\": [{\"a\": 1}, {\"b\": 2}, {\"c\": 3} ]}">>).
#{<<"awesome">> => true,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}
2> PatchEx = #{<<"op">> => <<"add">>, <<"path">> => [patchtest], <<"value">> => true}.
#{<<"op">> => <<"add">>,<<"path">> => [patchtest],<<"value">> => true}
3> json:patch([PatchEx], ExampleJSON).
#{<<"awesome">> => true,
  <<"library">> => <<"json">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"patchtest">> => true}
```

#### fold function over json at supplied json path ####

```erlang
1> JSON = #{<<"foo">> => <<"bar">>}.
#{<<"foo">> => <<"bar">>}
2> json:fold([json:test(<<"/foo">>, <<"bar">>),
2>   json:copy(<<"/foo">>, <<"/qux">>),
2>   json:test(<<"/qux">>, <<"bar">>),
2>   json:replace(<<"/qux">>, <<"baz">>),
2>   json:remove(<<"/foo">>),
2>   json:move(<<"/qux">>, <<"/foo">>),
2>   json:test(<<"/foo">>, <<"baz">>)
2> ], JSON).
#{<<"foo">> => <<"baz">>}
```

#### return json keys at supplied json path ####

```erlang
2> json:keys([], ExampleJSON).                   
[<<"awesome">>,<<"library">>,<<"list">>]
```

## description ##

a high level library for erlang 17.0+. leverages the new maps to support the [json][json] spec

## data types ##

#### `path() type` ####

```erlang
path() :: binary() | [atom() | integer() | binary()]
```

#### `json() type` ####

```erlang
json() :: #{binary() => json()}
  | [json()]
  | integer()
  | float()
  | binary()
  | true
  | false
  | null
```

## exports ##

### from_binary/1 and to_binary/1 ###

```erlang
from_binary(JSON) -> json()

JSON = binary()
```
uses [jsx][jsx]'s [`decoder/3`][jsxencdeenc] to convert a `binary()` to `json()`

```erlang
to_binary(JSON) -> binary()

JSON = json()
```
uses [jsx][jsx]'s [`encode`][jsxencdeenc] to convert `json()` to `binary()`

### get/1,2 ###

```erlang
get(Path, JSON) -> json()
get(Path) -> fun((JSON) -> json())

Path = path()
JSON = json()
```

get `json()` at supplied *path* in *JSON*, or return an anonymous function to be applied to a given *JSON* value

### add/2,3 ###

```erlang
add(Path, Value, JSON) -> json()
add(Path, Value) -> fun((JSON) -> json())

Path = path()
Value = json()
JSON = json()
```

add *Value* at supplied *Path* in *JSON*, or return an anonymous function to do the same for a supplied *JSON* value

### remove/1,2 ###
```erlang
remove(Path, JSON) -> json()
remove(Path) -> fun((JSON) -> json())

Path = path()
JSON = json()
```

remove json() at *Path* in *JSON*, or return an anonymous function to do the same for a supplied *JSON* value

### replace/2,3 ###

```erlang
replace(Path, Value, JSON) -> json()
replace(Path, Value) -> fun((JSON) -> json())

Path = path()
Value = json()
JSON = json()
```
replace *Path* in *JSON* with *Value*, or return an anonymous function to do the same for a supplied *JSON* value

### copy/2,3 ###

```erlang
copy(From, To, JSON) -> json()
copy(From, To) -> fun((JSON) -> json())

From = path()
To = path()
JSON = json()
```

copy a json() term *From* a path *To* another path in *JSON*, or return an anonymous function to do the same for a supplied *JSON* value

### move/2,3 ###

```erlang
move(From, To, JSON) -> json()
move(From, To) -> fun((JSON) -> json())

From = path()
To = path()
JSON = json()
```
Move a json() term *From* a path *To* another path in *JSON*, or return an anonymous function to do the same for a supplied *JSON*.

### test/2,3 ###
```erlang
test(Path, Value, JSON) -> json()
test(Path, Value) -> fun((JSON) -> json())

Path = path()
Value = json()
JSON = json()
```
test the existence of *Value* at a given *Path* in *JSON*, or return an anonymous function to do the same for a supplied *JSON* value

### apply/2,3 ###

```erlang
apply(Path, Fun, JSON) -> json()
apply(Path, Fun)  -> fun((JSON) -> json())
```
apply function *Fun* at a given *Path* in *JSON*, or return an anonymous function to do the same for a supplied *JSON* value

### patch/2 ###

```erlang
patch(Ops, JSON) -> json()

Ops = [#{<<"op">> => Op, <<"path">> => Path, <<"arg">> => Arg}]
JSON = json()
Op = <<"add">>      % Arg = <<"value">>
   | <<"remove">>   %     = none
   | <<"replace">>  %     = <<"value">>
   | <<"copy">>     %     = <<"from">>
   | <<"move">>     %     = <<"from">>
   | <<"test">>     %     = <<"value">>
Path = path()
Arg = <<"value">> | <<"from">>
```

patches *JSON* using the methods supplied by the maps in *Ops*.


### fold/2 ###

```erlang
fold(Funs, JSON) -> json().

Funs = [function()]
JSON = json()

```
fold a list of functions, *Funs*, over *JSON*

### keys/2 ###

```erlang
keys(Path, JSON) -> [binary()]
```
return a list of binary() keys at *Path* in *JSON*


## acknowledgements ##

[bobthenameless][bobthenameless] for writing the documentation


[rebar]: https://github.com/rebar/rebar
[travis]: https://travis-ci.org
[jsx]: https://github.com/talentdeficit/jsx
[jsonpointer]: https://github.com/talentdeficit/jsonpointer
[erlenv]: https://github.com/talentdeficit/erlenv
[rbenv]: https://github.com/sstephenson/rbenv
[rfc6901]: http://tools.ietf.org/html/rfc6901
[MIT]: http://www.opensource.org/licenses/mit-license.html
[json]: http://json.org
[jsxencdeenc]: https://github.com/talentdeficit/jsx#encoder3-decoder3--parser3
[bobthenameless]: https://github.com/bobthenameless