module QfPay.Error where

import ClassyPrelude

import QfPay.Types


errorOrPayload :: RpcResponse a -> Either RpcError a
errorOrPayload resp =
  if rpcRespErrorCode resp == errorCodeSuccess
     then Right $ rpcRespData resp
     else Left $ rpcErrorInResponse resp


errorCodeSuccess :: RpcErrorCode
errorCodeSuccess = RpcErrorCode "0000"


-- vim: set foldmethod=marker:
