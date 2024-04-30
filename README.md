# bel-framework/bel-json

A JSON encoder and decoder built on top of the `json` module proposed in [EEP 68](https://www.erlang.org/eeps/eep-0068) and [introduced in OTP 27](https://www.erlang.org/news/168#highlights-for-rc2).
Uses [json_polyfill](https://github.com/williamthome/json_polyfill) to support OTP versions < 27.

## Encode

```erlang
1> bel_json:encode(#{foo => bar}).
<<"{\"foo\":\"bar\"}">>
```

### Null

```erlang
1> bel_json:encode([foo, null, undefined, bar], #{null_values => [null, undefined]}).
<<"[\"foo\",null,null,\"bar\"]">>
```

### Proplist

```erlang
1> bel_json:encode([{foo, foo}, bar], #{list_codecs => [proplist]}).
<<"{\"foo\":\"foo\",\"bar\":true}">>
```

### Datetime

```erlang
1> bel_json:encode([{foo, foo}, bar], #{tuple_codecs => [datetime]}).
<<"{\"foo\":\"2024-04-29T22:34:35Z\"}">>
```

### Timestamp

```erlang
1> bel_json:encode({0,0,0}, #{tuple_codecs => [timestamp]}).
<<"\"1970-01-01T00:00:00.000Z\"">>
```

### IPv4

```erlang
1> bel_json:encode({0,0,0,0}, #{tuple_codecs => [ipv4]}).
<<"\"0.0.0.0\"">>
```

### IPv6

```erlang
1> bel_json:encode({16#fe80,0,0,0,16#204,16#acff,16#fe17,16#bf38}, #{tuple_codecs => [ipv6]}).
<<"\"fe80::204:acff:fe17:bf38\"">>
```

### Record

```erlang
% -record(foo, {foo, bar}).
1> bel_json:encode(#foo{foo = foo, bar = bar}, #{tuple_codecs => [{record, [{foo, record_info(fields, foo)}]}]}).
<<"{\"foo\":\"foo\",\"bar\":\"bar\"}">>
```

## Decode

```erlang
3> bel_json:decode(<<"{\"foo\":\"bar\"}">>).
#{<<"foo">> => <<"bar">>}
```

## Build

```shell
$ rebar3 compile
```
