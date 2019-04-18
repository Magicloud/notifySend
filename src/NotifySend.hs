{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module NotifySend where

import Data.Int
import Data.Map
import Data.Maybe
import Data.Word
import DBus
import DBus.Client

notifyMethod :: MethodCall
notifyMethod = (methodCall "/org/freedesktop/Notifications" "org.freedesktop.Notifications" "Notify") { methodCallDestination = Just "org.freedesktop.Notifications" }

notify :: String -> Maybe Word32 -> String -> String -> String -> Int -> IO Word32
notify appName id2replace iconName summary body timeout = do
  client <- connectSession
  reply <- call_ client
    notifyMethod { methodCallBody = [ toVariant appName
                                    , toVariant $ maybe 0 id id2replace
                                    , toVariant iconName
                                    , toVariant summary
                                    , toVariant body
                                    , toVariant ([] :: [String])
                                    , toVariant (empty :: Map String Variant)
                                    , toVariant $ toInt32 timeout ] } -- s u s s s as a{sv} i
  disconnect client
  return $ fromJust $ fromVariant $ head $ methodReturnBody reply
  where
    toInt32 :: (Integral a) => a -> Int32
    toInt32 = fromIntegral
