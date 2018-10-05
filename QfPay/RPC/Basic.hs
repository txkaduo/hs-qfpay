module QfPay.RPC.Basic where

-- {{{1 imports
import ClassyPrelude
import Data.Aeson
import Data.Time

import QfPay.Types
import QfPay.RPC
-- }}}1


data CreateToken = CreateToken NominalDiffTime Token

instance FromJSON CreateToken where
  parseJSON = withObject "CreateToken" $ \ o -> do
    CreateToken <$> fmap fromIntegral (o .: "expire" :: _ Int64)
                <*> o .: "order_token"


-- | 见扫描支付文档: 获取token接口
-- 文档未解释所返回的token的实际含义，仅知道会用在 pre_create 接口调用中
rpcCreateToken :: RpcMonad m
               => RpcEnv
               -> CommonParams
               -> OutUser
               -> m (RpcResponse CreateToken)
-- {{{1
rpcCreateToken rpc_env common_params out_user = do
  remoteCallGet rpc_env "/auth/v1/token" params
  where
    params =
      insertMap "out_user" (toParamValue out_user)
      $ (initParamsCommon common_params :: Map _ _)
-- }}}1


data CreateOrderToken = CreateOrderToken NominalDiffTime OrderToken (Maybe Text) (Maybe Text)

instance FromJSON CreateOrderToken where
  parseJSON = withObject "CreateOrderToken" $ \ o -> do
    CreateOrderToken
                <$> fmap fromIntegral (o .: "expire" :: _ Int64)
                <*> o .: "order_token"
                <*> o .:? "qrimg"
                <*> o .:? "qrurl"


-- | pre_create 接口出现在多个文档里，未仔细比较各处文档描述的异同
-- 返回的主要数据是 OrderToken, 用于下一步接口调用（创建订单）
-- 已留意到的要点包括:
-- * 扫码支付时，caller只能是server
-- * 扫码支付文档中说，要生成二维码时token参数为必须
rpcCreateOrderToken :: RpcMonad m
                    => RpcEnv
                    -> CommonParams
                    -> OutSN
                    -> MoneyAmount
                    -> Maybe Token
                    -> m (RpcResponse CreateOrderToken)
-- {{{1
rpcCreateOrderToken rpc_env common_params out_sn total_amt m_token = do
  remoteCallGet rpc_env "/order/v1/pre_create" params
  where
    params =
      insertMap "out_sn" (toParamValue out_sn)
      $ insertMap "total_amt" (toParamValue total_amt)
      $ fromMaybe id (fmap (insertMap "token" . toParamValue) m_token)
      $ (initParamsCommon common_params :: Map _ _)
-- }}}1


data CreateQfToken = CreateQfToken
  { createQfTokenOrderId      :: OrderId
  , createQfTokenQfToken      :: QfToken
  , createQfTokenSettleUserId :: UserId    -- settle userid
  , createQfTokenCreateUserId :: UserId    -- create userid
  }

instance FromJSON CreateQfToken where
-- {{{1
  parseJSON = withObject "CreateQfToken" $ \ o -> do
    CreateQfToken
                <$> o .: "order_id"
                <*> o .: "qf_token"
                <*> o .: "settle_userid"
                <*> o .: "create_userid"
-- }}}1


-- | 见线下收款文档：获取qf_token
-- qf_token 的用途同样并不明确，仅在 set_result 接口里出现
rpcCreateQfToken :: RpcMonad m
                 => RpcEnv
                 -> CommonParams
                 -> Token
                 -> OutSN
                 -> MoneyAmount
                 -> Text   -- ^ goods name
                 -> Maybe LocalTime -- ^ 过期时间
                 -> m (RpcResponse CreateQfToken)
-- {{{1
rpcCreateQfToken rpc_env common_params token out_sn total_amt goods_name m_expire_time = do
  remoteCallPost rpc_env "/order/v1/simple_create" params
  where
    params =
      insertMap "out_sn" (toParamValue out_sn)
      $ insertMap "total_amt" (toParamValue total_amt)
      $ insertMap "token" (toParamValue token)
      $ insertMap "goods_name" (toParamValue goods_name)
      $ fromMaybe id (insertMap "expire_time" . toParamValue <$> m_expire_time)
      $ (initParamsCommon common_params :: Map _ _)
-- }}}1


-- vim: set foldmethod=marker:
