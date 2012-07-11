{-# LANGUAGE OverloadedStrings #-}

module Message (
    parseMessage
  , getAddress
  , getMessage
  , getMessageSync
  , getMessageData
) where

import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.Number (Number(..))
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

parseMessage :: B.ByteString -> [Value]
parseMessage = either (const []) (:[]) . parseOnly json

getAddress :: Value -> Maybe T.Text
getAddress (Object o) = HashMap.lookup "address" o >>= maybeString
getAddress _          = Just "fuck" -- Nothing

getMessage :: Value -> Maybe Value
getMessage (Object o) = HashMap.lookup "message" o
getMessage _          = Nothing

getMessageSync :: Value -> Maybe Integer
getMessageSync (Object o) = HashMap.lookup "sync" o >>= maybeNumberInteger
getMessageSync _          = Nothing

getMessageData :: Value -> Maybe [(T.Text, Value)]
getMessageData (Object o) = fmap HashMap.toList $ HashMap.lookup "data" o >>= maybeObject
getMessageData _          = Nothing

maybeString (String t) = Just t
maybeString _          = Nothing

maybeNumberInteger (Number (I x)) = Just x
maybeNumberInteger _              = Nothing

maybeObject (Object o) = Just o
maybeObject _          = Nothing

