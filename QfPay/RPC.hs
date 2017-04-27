{-# LANGUAGE ScopedTypeVariables #-}
module QfPay.RPC
  ( RpcEnv(..)
  , CommonParams(..)
  , RpcMonad, ParamsMap
  , initParamsCommon
  , remoteCallGet
  , remoteCallGet'
  , remoteCallPost
  , remoteCallPost'
  , callbackParamsSign
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Control.Arrow       (left)
import qualified Crypto.Hash         as CH
import           Data.Aeson          as A
import           Network.HTTP.Client (Manager)
import           Network.HTTP.Simple

import QfPay.Types
import QfPay.Error
-- }}}1


type ParamsMap map = (MapValue map ~ Text, ContainerKey map ~ Text, IsMap map)
type RpcMonad m = (MonadIO m, MonadThrow m)


baseURL :: RpcEnvType -> String
baseURL RpcSandbox    = "https://qtapi.qfpay.com"
baseURL RpcProduction = "https://qtsandbox.qfpay.com"


initialRequest :: MonadThrow m => RpcEnvType -> ByteString -> m Request
initialRequest env_type sub_path = do
  r0 <- parseRequest (baseURL env_type)
  return $ setRequestPath sub_path r0


data RpcEnv = RpcEnv RpcEnvType ServerKey Manager

data CommonParams = CommonParams CallerType AppCode

initParamsCommon :: ParamsMap map => CommonParams -> map
-- {{{1
initParamsCommon (CommonParams caller_type app_code) =
  mapFromList
    [ ("caller", toParamValue caller_type)
    , ("app_code", toParamValue app_code)
    ]
-- }}}1

remoteCallGet :: (RpcMonad m, FromJSON a, ParamsMap map)
              => RpcEnv
              -> ByteString
              -> map
              -> m (RpcResponse a)
-- {{{1
remoteCallGet (RpcEnv env_type server_key manager) sub_path params = do
  request0 <- initialRequest env_type sub_path
  let request
        = setRequestManager manager
        $ setRequestMethod "GET"
        $ setRequestQueryStringParams server_key params
        $ request0

  response <- httpJSON request
  return $ getResponseBody response
-- }}}1


-- | throw RpcError when remote server report an error
remoteCallGet' :: (RpcMonad m, FromJSON a, ParamsMap map)
               => RpcEnv
               -> ByteString
               -> map
               -> m a
remoteCallGet' rpc_env url_path params = do
-- {{{1
  rpc_resp <- remoteCallGet rpc_env url_path params
  either throwM return $ errorOrPayload rpc_resp
-- }}}1


remoteCallPost :: (RpcMonad m, FromJSON a, ParamsMap map)
               => RpcEnv
               -> ByteString
               -> map
               -> m (RpcResponse a)
-- {{{1
remoteCallPost (RpcEnv env_type server_key manager) sub_path params = do
  request0 <- initialRequest env_type sub_path
  let request
        = setRequestManager manager
        $ setRequestMethod "POST"
        $ setRequestPostParams server_key params
        $ request0

  response <- httpJSON request
  return $ getResponseBody response
-- }}}1


remoteCallPost' :: (RpcMonad m, FromJSON a, ParamsMap map)
               => RpcEnv
               -> ByteString
               -> map
               -> m a
remoteCallPost' rpc_env url_path params = do
-- {{{1
  rpc_resp <- remoteCallPost rpc_env url_path params
  either throwM return $ errorOrPayload rpc_resp
-- }}}1


setRequestQueryStringParams :: (ParamsMap map)
                            => ServerKey
                            -> map
                            -> Request
                            -> Request
-- {{{1
setRequestQueryStringParams server_key params0 =
  setRequestQueryString $ flip map (mapToList params) $ \ (k, v) -> (encodeUtf8 k, Just (encodeUtf8 v))
  where
    sign = rpcSignParams server_key params0
    params = insertMap "sign" sign params0
-- }}}1


setRequestPostParams :: (ParamsMap map)
                     => ServerKey
                     -> map
                     -> Request
                     -> Request
-- {{{1
setRequestPostParams server_key params0 =
  setRequestBodyURLEncoded $ flip map (mapToList params) $ \ (k, v) -> (encodeUtf8 k, encodeUtf8 v)
  where
    sign = rpcSignParams server_key params0
    params = insertMap "sign" sign params0
-- }}}1


rpcSignParams :: (ParamsMap map)
              => ServerKey
              -> map
              -> Text
-- {{{1
rpcSignParams server_key params = toUpper . decodeUtf8 $
  CH.digestToHexByteString . (CH.hash :: _ -> CH.Digest CH.MD5) $
    encodeUtf8 $
      (<> unServerKey server_key) $
      intercalate "&" $
        catMaybes $
          flip map (sortWith fst $ mapToList params) $ \ (k, v) -> do
            guard $ not $ k == "sign" || k == "sign_type"
            return $ k <> "=" <> v
-- }}}1


callbackParamsSign :: ServerKey
                   -> ByteString
                   -> Either Text Text
-- {{{1
callbackParamsSign server_key body = do
  params :: Map Text Value <- left pack $ A.eitherDecodeStrict body
  params2 <- forM (mapToList params) $ \ (k, v) -> do
    v' <- case v of
            A.String t -> return t
            A.Number t -> return $ tshow t
            A.Bool t   -> return $ toLower $ tshow t
            _          -> Left $ "Not a simple scalar value in field " <> k <> ": " <> tshow v

    return (k, v')

  return $ rpcSignParams server_key params2
-- }}}1
  

-- vim: set foldmethod=marker:
