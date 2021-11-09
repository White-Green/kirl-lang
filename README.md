[日本語版はこちら](./README_JA.md)

# kirl-lang
An experimental project for statically typed scripting language.

## required for running
[Rust toolchain](https://www.rust-lang.org) for your computer

## install
```shell
$ cargo install --git https://github.com/White-Green/kirl-lang
```
or
```shell
$ git clone https://github.com/White-Green/kirl-lang
$ cargo install kirl-lang/kirl
```

## running
create below file named "hello.kirl".
```
import std::io;

"Hello, World!".io::println();
```
and run this file by the below command.
```shell
$ kirl hello.kirl
```
