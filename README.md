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

[jsonpointer][jsonpointer] is used for handling json pointer syntax in accordance with [RFC6901][rfc6901].

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
uses jsx

```erlang
to_binary/1
```

```erlang
from_binary/1
```

#### get json at supplied json path ####


```erlang
get/1
get/2
```

#### add json to supplied json path ####

```erlang
add/2
add/3
```

#### remove json at supplied json path ####

```erlang
remove/1
remove/2
```

#### replace json at supplied json path with new json####

```erlang
replace/2
replace/3

```

#### copy json from one json path to another ####

```erlang
copy/2
copy/3

```
#### move json from one json path to another ####

```erlang
move/2
move/3

```

#### test json existance at supplied json path ####

```erlang
test/1
test/2

```

#### fold function over json at supplied json path ####

```erlang
fold/2


```
#### return json keys at supplied json path ####

```erlang
keys/2

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