# json #

a high level library for erlang (17.0+)

**json** is built via [rebar][rebar], and continuous integration testing provided by [travis-ci][travis]

[jsx][jsx] is used when converting between Erlang binary() and json() types (? verify this is true later)

[jsonpointer][jsonpointer] is used for handling json pointer syntax in accordance with [RFC6901][rfc6901].

current status: [![Build Status](https://travis-ci.org/talentdeficit/json.svg?branch=master)](https://travis-ci.org/talentdeficit/json)

**json** is released under the terms of the [MIT][MIT] license

copyright 2010-2014 alisdair sullivan



## index ##

* [quickstart](#quickstart)
* [description](#description)
* [data types](#data-types)
* [exports](#exports)
* [callback exports](#callback_exports)
* [acknowledgements](#acknowledgements)

## quickstart ##

```bash
$ rebar get-deps
$ rebar compile
$ rebar eunit
```


## description ##

A high level library for Erlang 17.0+. Leverages the new maps to support the [json][json] spec.

More info to come.

## data types ##
path() :: binary() | [atom() | integer() | binary()]

json() :: #{integer() | atom() | binary() => json()}
  | [json()]
  | integer()
  | float()
  | binary()
  | true
  | false
  | null

## exports ##

### from_binary ###

* from_binary/1
* to_binary/1

### get ###

* get/2

* get/1

### add ###

* add/3
* add/2

### remove ###

* remove/2
* remove/1

### replace ###

* replace/3
* replace/2

### copy ###
* copy/3
* copy/2

### move ###
* move/3
* move/2

### test ###
* test/2
* test/1

### fold ###

* fold/2

### keys ###
* keys/2

## otp callback exports ##
* init/1
* handle_event/2

## acknowledgements ##
tdeficit for originally doing all the hard work
bobthenameless for helping with documentation


[rebar]: https://github.com/rebar/rebar
[travis]: https://travis-ci.org
[jsx]: https://github.com/talentdeficit/jsx
[jsonpointer]: https://github.com/talentdeficit/jsonpointer
[rfc6901]: http://tools.ietf.org/html/rfc6901
[MIT]:
[json]: http://json.org