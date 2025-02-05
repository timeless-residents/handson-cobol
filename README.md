# COBOL Development Environment Setup for M2 Mac

このリポジトリは、M2 Mac上でGnuCOBOL開発環境を構築するための手順をまとめたものです。

## GnuCOBOLとは

GnuCOBOLは、COBOLプログラミング言語のオープンソース実装です。以下の特徴があります：

- COBOL 85標準規格に準拠
- COBOL 2002、2014の機能の一部をサポート
- C言語へのコンパイルによる高いポータビリティ
- 他のプログラミング言語とのリンクが可能
- フリーソフトウェアとして利用可能（GNU General Public License）
- アクティブなコミュニティによる継続的な開発とサポート

主に以下のような用途で使用されています：
- レガシーシステムの移行
- バッチ処理プログラムの開発
- ビジネスアプリケーションの開発
- 教育用途

## 前提条件

- Apple Silicon搭載Mac（M1/M2/M3シリーズ）
- macOS Sonoma以降
- Homebrewがインストールされていること

## インストール手順

### 1. 依存パッケージのインストール

必要なパッケージをアンインストールして再インストールします：

```bash
# 既存のパッケージを削除
brew uninstall --ignore-dependencies berkeley-db
brew uninstall gnucobol

# Berkeley DBを再インストール
brew install berkeley-db
```

### 2. 環境変数の設定

以下の環境変数を設定します：

```bash
export PATH="/opt/homebrew/opt/berkeley-db/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/berkeley-db/lib"
export CPPFLAGS="-I/opt/homebrew/opt/berkeley-db/include"
export BDB_CFLAGS="-I/opt/homebrew/opt/berkeley-db/include"
export BDB_LIBS="-L/opt/homebrew/opt/berkeley-db/lib -ldb"
```

### 3. GnuCOBOLのビルドとインストール

```bash
./configure --prefix=/usr/local \
    --with-db=/opt/homebrew/opt/berkeley-db \
    CFLAGS="-arch arm64 ${CPPFLAGS}" \
    LDFLAGS="-arch arm64 ${LDFLAGS}" \
    CC="clang" \
    CPP="clang -E"

make
sudo make install
```

## 使用方法

### サンプルプログラム

以下は簡単なHello Worldプログラムの例です：

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, COBOL on M2 Mac!".
           STOP RUN.
```

### コンパイルと実行

プログラムのコンパイルと実行は以下のコマンドで行います：

```bash
# コンパイル
cobc -x hello.cob

# 実行
./hello
```

## トラブルシューティング

### 一般的な問題と解決方法

1. アーキテクチャの問題が発生した場合：
   - 環境変数の設定を確認
   - configureオプションを確認

2. ライブラリが見つからない場合：
   - Homebrewでのインストール状態を確認
   - パスが正しく設定されているか確認

## 参考リンク

- [GnuCOBOL公式サイト](https://gnucobol.sourceforge.io/)
- [GnuCOBOLドキュメント](https://gnucobol.sourceforge.io/guides.html)
- [Homebrewドキュメント](https://docs.brew.sh/)

## ライセンス

このセットアップ手順は、MITライセンスで公開されています。