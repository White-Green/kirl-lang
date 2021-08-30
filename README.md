# kirl-lang
An experimental project for static-typed script programming language.

## required for running
[Rust toolchain](https://www.rust-lang.org) for your computer

## install
```shell
$ cargo install --git https://github.com/White-Green/kirl-lang --branch main
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
