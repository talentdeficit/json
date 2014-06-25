# json #

a high level library for erlang (17.0+)

**json** is built via [rebar][rebar], and continuous integration testing provided by [travis-ci][travis]


current status: [![Build Status](https://travis-ci.org/talentdeficit/json.svg?branch=master)](https://travis-ci.org/talentdeficit/json)

**json** is released under the terms of the [MIT][MIT] license

copyright 2010-2014 alisdair sullivan

## notes ##

Erlang 17.0+ is required to use this library due to the new map implementation.

If you are developing with multiple Erlang releases, take a look at [erlenv][erlenv]. It is an erlang release management tool forked from [rbenv][rbenv[ and can be used to easily manage multiple releases of Erlang/OTP.


## dependencies ##

[jsx][jsx] is used when converting between Erlang binary() and json() types (? verify this is true later)

[jsonpointer][jsonpointer] is used for handling json paths in accordance with [RFC6901][rfc6901].

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
  - [`test/1,2`](#test12)
  - [`fold/2`](#fold2)
  - [`keys/2`](#keys2)
* [callback exports](#callback-exports)
  - [`Module:init/1`](#moduleinit1)
  - [`Module:handle_event/2`](#modulehandle_event2)
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
1> ExampleJSON = json:from_binary(<<"{\"library\": \"jsx\", \"awesome\": true, \"list\": [{\"a\": 1}, {\"b\": 2}, {\"c\": 3} ]}">>).
#{<<"awesome">> => true,
  <<"library">> => <<"jsx">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}
  
```

```erlang
2> json:to_binary(ExampleJSON).
<<"{\"awesome\":true,\"library\":\"jsx\",\"list\":[{\"a\":1},{\"b\":2},{\"c\":3}]}">>
```


#### get json at supplied json path ####


```erlang
%% get/1 returns a function accepting json(), get/2 returns the json() object at the given path.
%% other functions with differing arity perform similarly.

2> GetJSONFun = json:get([list, 0, a]). % supplying a list as a path
#Fun<json.0.18938731>
3> GetJSONFun(ExampleJSON).
1
4> json:get(<<"/list/0/a">>, ExampleJSON). % can also give path as a binary JSON pointer
1
5> json:get([awesome], ExampleJSON).      
true
```

#### add json to supplied json path ####

```erlang
2> json:add([addition], <<"1+1" >>, ExampleJSON).
#{<<"addition">> => <<"1+1">>,
  <<"awesome">> => true,
  <<"library">> => <<"jsx">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}
  
3> json:add([recursion], ExampleJSON, ExampleJSON).   #{<<"awesome">> => true,
  <<"library">> => <<"jsx">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"recursion">> => #{<<"awesome">> => true,
    <<"library">> => <<"jsx">>,
    <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}}

4> json:add([map], #{<<"test2">> => 50}, ExampleJSON).                                  
#{<<"awesome">> => true,
  <<"library">> => <<"jsx">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"map">> => #{<<"test2">> => 50}}

5> json:add(<<"/listtest">>, [50, <<"listadd">>, testatom], ExampleJSON).
#{<<"awesome">> => true,
  <<"library">> => <<"jsx">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"listtest">> => [50,<<"listadd">>,testatom]}
```

#### remove json at supplied json path ####

```erlang
2> json:remove([list, 2], ExampleJSON).
#{<<"awesome">> => true,
  <<"library">> => <<"jsx">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2}]}

```

#### replace json at supplied json path with new json####

```erlang
2> json:replace([awesome], <<"json in erlang!">> ,ExampleJSON).                                                      
#{<<"awesome">> => <<"json in erlang!">>,
  <<"library">> => <<"jsx">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}

```

#### copy json from one json path to another ####

```erlang
json:copy([list],[copiedlist],ExampleJSON).
#{<<"awesome">> => true,
  <<"copiedlist">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}], 
  <<"library">> => <<"jsx">>,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}]}

```
#### move json from one json path to another ####

```erlang
2> json:move([library], [newlibrary], ExampleJSON).
#{<<"awesome">> => true,
  <<"list">> => [#{<<"a">> => 1},#{<<"b">> => 2},#{<<"c">> => 3}],
  <<"newlibrary">> => <<"jsx">>}

```



#### fold function over json at supplied json path ####

```erlang
2> json:fold([json:get([]), json:get([list]), json:remove([0]), json:replace([1, c], <<"end of fold">> )], ExampleJSON).
[#{<<"b">> => 2},#{<<"c">> => <<"end of fold">>}]
3> json:fold([json:get([]), json:get([list]), json:remove([0]), json:replace([1, c], <<"123456789">> ), json:get([1, c]), fun binary:bin_to_list/1, fun string:len/1], ExampleJSON).
9


```
#### return json keys at supplied json path ####

```erlang
2> json:keys([], ExampleJSON).                   
[<<"awesome">>,<<"library">>,<<"list">>]

```

## description ##

A high level library for Erlang 17.0+. Leverages the new maps to support the [json][json] spec.

More info to come.

## data types ##

#### `path() type` ####
path() :: binary() | [atom() | integer() | binary()]

#### `json() type` ####
json() :: #{integer() | atom() | binary() => json()}
  | [json()]
  | integer()
  | float()
  | binary()
  | true
  | false
  | null

## exports ##

### from_binary/1 and to_binary/1 ###
```erlang
from_binary/1
to_binary/1
```

### get/1,2 ###
```erlang
get/2
get/1
```

### add/2,3 ###
```erlang
add/3
add/2
```

### remove/1,2 ###
```erlang
remove/2
remove/1
```

### replace/2,3 ###
```erlang
replace/3
replace/2
```

### copy/2,3 ###
```erlang
copy/3
copy/2
```

### move/2,3 ###
```erlang
move/3
move/2
```

### test/1,2 ###
```erlang
test/2
test/1
```

### fold/2 ###
```erlang
fold/2
```

### keys/2 ###
```erlang
keys/2
```
## callback exports ##
the following should be exported from a json callback module

#### Module:init/1 ####

init/1

#### Module:handle_event/2 ####

handle_event/2



## acknowledgements ##
tdeficit for originally doing all the hard work
bobthenameless for helping with documentation


[rebar]: https://github.com/rebar/rebar
[travis]: https://travis-ci.org
[jsx]: https://github.com/talentdeficit/jsx
[jsonpointer]: https://github.com/talentdeficit/jsonpointer
[erlenv]: https://github.com/talentdeficit/erlenv
[rbenv]: https://github.com/sstephenson/rbenv
[rfc6901]: http://tools.ietf.org/html/rfc6901
[MIT]: http://www.opensource.org/licenses/mit-license.html
[json]: http://json.org