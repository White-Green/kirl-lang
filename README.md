[English version is here](./README_EN.md)
# kirl-lang
静的型付けされたスクリプト言語の実験的プロジェクト

## 実行環境
[Rustツールチェイン](https://www.rust-lang.org) をインストールしてください

## インストール方法
```shell
$ cargo install --git https://github.com/White-Green/kirl-lang
```
または
```shell
$ git clone https://github.com/White-Green/kirl-lang
$ cargo install kirl-lang/kirl
```

## 実行
以下の内容のファイルを"hello.kirl"という名前で作成します
```
import std::io;

"Hello, World!".io::println();
```
その後、以下のコマンドで実行します
```shell
$ kirl hello.kirl
```
