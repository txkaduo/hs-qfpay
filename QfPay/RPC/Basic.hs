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


data CreateOrderToken = CreateOrderToken NominalDiffTime OrderToken

instance FromJSON CreateOrderToken where
  parseJSON = withObject "CreateOrderToken" $ \ o -> do
    CreateOrderToken
                <$> fmap fromIntegral (o .: "expire" :: _ Int64)
                <*> o .: "order_token"


rpcCreateOrderToken :: RpcMonad m
                    => RpcEnv
                    -> CommonParams
                    -> OutSN
                    -> MoneyAmount
                    -> m (RpcResponse CreateOrderToken)
-- {{{1
rpcCreateOrderToken rpc_env common_params out_sn total_amt = do
  remoteCallGet rpc_env "/order/v1/pre_create" params
  where
    params =
      insertMap "out_sn" (toParamValue out_sn)
      $ insertMap "total_amt" (toParamValue total_amt)
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
