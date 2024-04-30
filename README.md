# bel-framework/bel-json

An JSON parser and generator lib for OTP.

## Encode

```erlang
1> iolist_to_binary(bel_json:encode(#{foo => bar})).
<<"{\"foo\":\"bar\"}">>
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
