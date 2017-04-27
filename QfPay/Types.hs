module QfPay.Types where

import ClassyPrelude
import Data.Aeson
import Data.Time
import Database.Persist.Sql                 (PersistField(..), PersistFieldSql(..))

class ToParamValue a where
  toParamValue :: a -> Text

instance ToParamValue Text where
  toParamValue = id

-- 按照生成 QfToken 的接口参数要求而选择的实现
instance ToParamValue LocalTime where
  toParamValue = fromString . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"


newtype RpcErrorCode = RpcErrorCode { unRpcErrorCode :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToParamValue
           , PersistField, PersistFieldSql
           )


-- | 远程调用的返回报文
-- 文档没有说明错误情况下，有没有有意义的 data 值
-- 这里先理解为可同时存在
data RpcResponse a = RpcResponse
  { rpcRespErrorText :: Text
  , rpcRespErrorCode :: RpcErrorCode
  , rpcRespErrorMsg  :: Text
  , rpcRespData      :: a
  }
  deriving (Show)

instance FromJSON a => FromJSON (RpcResponse a) where
-- {{{1
  parseJSON = withObject "RpcResponse" $ \ o -> do
    RpcResponse <$> o .: "resperr"
                <*> o .: "respcd"
                <*> o .: "respmsg"
                <*> o .: "data"
-- }}}1


-- | 纯错误信息，不包含数据
data RpcError = RpcError
  { rpcErrorCode :: RpcErrorCode
  , rpcErrorText :: Text
  , rpcErrorMsg  :: Text
  }
  deriving (Show)

instance Exception RpcError

rpcErrorInResponse :: RpcResponse a -> RpcError
rpcErrorInResponse x = RpcError (rpcRespErrorCode x)
                              (rpcRespErrorText x)
                              (rpcRespErrorMsg x)


newtype AppCode = AppCode { unAppCode :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToParamValue
           , PersistField, PersistFieldSql
           )


newtype ServerKey = ServerKey { unServerKey :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON
           , PersistField, PersistFieldSql
           )


newtype OrderId = OrderId { unOrderId :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToParamValue
           , PersistField, PersistFieldSql
           )

newtype Token = Token { unToken :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToParamValue
           , PersistField, PersistFieldSql
           )

newtype OrderToken = OrderToken { unOrderToken :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToParamValue
           , PersistField, PersistFieldSql
           )

newtype QfToken = QfToken { unQfToken :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToParamValue
           , PersistField, PersistFieldSql
           )

newtype UserId = UserId { unUserId :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToParamValue
           , PersistField, PersistFieldSql
           )

newtype OutSN = OutSN { unOutSN :: Text }
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToParamValue
           , PersistField, PersistFieldSql
           )

type OutUser = Text

-- | 支付类型
data PayType = PayTypeAli
             | PayTypeWx
             | PayTypeUnionPay
             | PayTypeQpos
             | PayTypeManual
             | PayTypeNone
             deriving (Show, Eq, Ord)

instance ToParamValue PayType where
-- {{{1
  toParamValue PayTypeAli      = "1"
  toParamValue PayTypeWx       = "2"
  toParamValue PayTypeUnionPay = "3"
  toParamValue PayTypeQpos     = "4"
  toParamValue PayTypeManual   = "5"
  toParamValue PayTypeNone     = "6"
-- }}}1


data CallerType = CallerServer
                | CallerApp
                | CallerWeb
                deriving (Show, Eq, Ord, Enum, Bounded)

instance ToParamValue CallerType where
-- {{{1
  toParamValue CallerServer = "server"
  toParamValue CallerApp    = "app"
  toParamValue CallerWeb    = "h5"
-- }}}1


data RpcEnvType = RpcSandbox  -- ^ 沙箱测试环境
                | RpcProduction -- ^ 正式生产环境
                deriving (Show, Eq, Ord, Enum, Bounded)


-- | 接口中的金额以分作为单位
data MoneyAmount = MoneyAmount { unMoneyAmount :: Int }
  deriving (Show, Eq, Ord)

instance ToParamValue MoneyAmount where
  toParamValue (MoneyAmount a) = tshow a


-- | 从单位是元的数字转成 WxPayMoneyAmount
moneyAmountFromYuanEither :: (Show a, Num a, RealFrac a, IsString s)
                          => a
                          -> Either s MoneyAmount
-- {{{1
moneyAmountFromYuanEither y =
  if abs (fromIntegral fen_int - fen) > 0.001
     then Left $ fromString $
                  "Cannot convert to convert to WxPayMoneyAmount loselessly: " <> show y
                  <> "because " <> show fen_int <> " != " <> show fen
     else Right $ MoneyAmount fen_int
  where
    fen = y * fromIntegral (100 :: Int)
    fen_int = round fen
-- }}}1


moneyAmountFromYuan :: (Show a, Num a, RealFrac a)
                    => a
                    -> MoneyAmount
moneyAmountFromYuan y =
  either error id $ moneyAmountFromYuanEither y


moneyAmountToYuan :: Integral a => MoneyAmount -> a
moneyAmountToYuan = fromIntegral . unMoneyAmount



-- vim: set foldmethod=marker:
