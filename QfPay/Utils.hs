module QfPay.Utils where

import ClassyPrelude
import Data.List                            ((!!))
import System.Random                        (randomIO)
import Web.PathPieces

import QfPay.Types


-- | 商户内订单号的推荐算法
-- 订单号须对应某个数据库表id
-- 同时，为减少重复的机会，在前面加上一段随机字母串
-- 这是因为虽然数据库表id本身是唯一的，但出现重复的机会还有很多
-- * 不同的系统共用一个微信接口账号
-- * 相同的系统重新部署使得表id重新计算
newUniqueId :: PathPiece a => Int -> a -> IO Text
newUniqueId pre_len k = do
  prefix <- randomString pre_len $ ['a'..'z'] <> ['A'..'Z']
  return $ fromString prefix <> toPathPiece k


newOutSN :: PathPiece a => Int -> a -> IO OutSN
newOutSN pre_len k = fmap OutSN $ newUniqueId pre_len k


randomPick :: MonadIO m => [a] -> m a
randomPick choices = do
    idx' <- liftIO randomIO
    let idx = abs idx' `rem` chlen
    return $ choices !! idx
    where
        chlen = length choices


randomString :: MonadIO m => Int -> [Char] -> m [Char]
randomString len chars = replicateM len (randomPick chars)
