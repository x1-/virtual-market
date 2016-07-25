# 仮想株式市場を提供するwebサービスです。

日経の過去データを使って仮想株式取引を実現します。
各パラメータはQueryStringで表現します。

## 機能

### buy

株を購入します。(ロングの場合)
solパラメータでショートを指定すると空売りになります。

#### parameters 

* ユーザID( `id` )
* 銘柄コード( `code` )
* 通常／信用( `account`: `credit`, `cash` )
* ロング／ショート( `sol`: `long`, `short` )
* 成行／指値( `how`: `market`, `limit` )
* 指値の場合の金額( `price` )
* 株数( `number` )
* 指値の場合の有効期限( `expiration`: `YYYY-mm-dd HH:MM` )
を指定します。

### sell

株を売却します。(ロングの場合)
solパラメータでショートを指定すると買い戻しになります。

#### parameters 

* ユーザID( `id` )
* 銘柄コード( `code` )
* ロング／ショート( `sol`: `long`, `short` )
* 成行／指値( `how`: `market`, `limit` )
* 指値の場合の金額( `price` )
* 株数( `number` )
* 指値の場合の有効期限( `expiration`: `YYYY-mm-dd HH:MM` )
を指定します。

### price

株価や各指標を取得します。

#### parameters 

* 銘柄コード( `code` )
* 期間開始( `start` )
* 期間終了( `end` )
* ティック( `tick`: `1m`, `5m`, `1d` )
を指定します。

### stocks/available

売買可能な銘柄一覧を取得します。

#### parameters 

なし

### user/info

ユーザ情報を取得します。

#### parameters 

* ユーザID( `id` )

#### response 

> * 口座残高
> * 買い付け可能額
> * 保有銘柄

### user/contract

約定情報を返します。

#### parameters 

* ユーザID( `id` )

### time

現在時刻を返却します。

#### parameters 

なし
