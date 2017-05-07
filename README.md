# HallizKeeR
Raspberry PiでI2Cデバイスからデータを取り、
それを記録するアプリ。

記録先は、今はローカルのsyslog。
Rasberry Piでは色々と制約が多く、
あまり多くのことをやりたくないので、
シンプルで古典的なログ手段を使うことにした。
リモートにログを送るのであれば、
このアプリでなくsyslogの設定で対応する。

> local5.* @@some-remote-server.net

などとすれば、そのサーバでログを受け取れる。


以前はローカルのPostgreSQLにデータを突っ込んでいたが、
データが多くなってきたときにRPiでは不安がある。
また、PostgreSQLをRPi上にセットアップするのも
わりと手間が多い。
そこで、データを別サーバに持たせ、
原始的なsyslogを使う方向に落ち着いた。

## 実行
> cabal install --only-dependencies  
> cabal run

`cabal`は普通にaptで入れておく。
RPiでは`stack`が使えないらしいので、
仕方なく古い`cabal`でパッケージ管理やビルドをしている。