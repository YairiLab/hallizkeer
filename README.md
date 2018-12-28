# HallizKeeR
Raspberry Piで、I2Cデバイスからデータを取り、
それを標準出力に出す。  
Rasberry Piでは色々と制約が多く、
あまり多くのことをやりたくない。
そこで、難しい処理は別システムでやる前提で、
RPiはセンシングに徹するよう役割分担した。

## 実行
> stack build  
> stack exec hallizkeer

stackは、あらかじめで
`curl -sSL https://get.haskellstack.org/ | sh`
などで入れておく。  
使うStackage LTSは11.8。
RPiだと、GHC 8.2.2までしか対応しないので、
このLTSにせざるをえない。

## 出力の流れ
標準出力はシンプルだが、そこに出してしまえば、
ファイルに保存するのはパイプで簡単。  
また、`logger`コマンドは標準出力を受け取って、syslogに記録できる。

syslogでリモートにログを送るのであれば、syslogの設定で、

> user.* @@some-remote-server.net

などとすれば、そのサーバにログが送られる。  
いったんRPiから出てしまえば、より高度な処理ができる。
必要であれば受け取ったログサーバ側でLogstash等を使い、
他のデータ形式に変換すればいい。
