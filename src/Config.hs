{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module Config (
    WriterConfig(..)
  , ProbeConfig(..)
  , RrdConfig(..)
  , loadConfig
) where

import Control.Applicative
import Data.Configurator
import Data.Configurator.Types
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

data WriterConfig = WriterConfig
  { wcDebug :: Bool
  , wcProbes :: ProbeConfigMap } deriving Show

type ProbeConfigMap = Map.HashMap T.Text ProbeConfig

data ProbeConfig = ProbeConfig
  { pcName :: T.Text
  , pcAddress :: T.Text
  , pcRrdConfigs :: RrdConfigMap } deriving Show

type RrdConfigMap = Map.HashMap T.Text RrdConfig

data RrdConfig = RrdConfig
  { rrdFilename :: T.Text
  , rrdStep :: Int
  , rrdDataSources :: [T.Text]
  , rrdArchives :: [T.Text] } deriving Show

instance Configured [T.Text] where
  convert (List x) = mapM convert x
  convert _        = Nothing

loadConfig :: FilePath -> Bool -> IO WriterConfig
loadConfig configFile debug = do
  c <- load [Required configFile]
  WriterConfig debug <$> 
    (require c "probes" >>= requireProbeConfigMap c)

requireProbeConfigMap :: Config -> [T.Text] -> IO ProbeConfigMap
requireProbeConfigMap c names = do
  cs <- mapM (requireProbeConfig c) names
  return $ Map.fromList $ zip (map pcAddress cs) cs

requireProbeConfig :: Config -> T.Text -> IO ProbeConfig
requireProbeConfig c name =
  ProbeConfig name <$>
    require c (T.append name ".address") <*>
    (require c (T.append name ".data") >>= requireRrdConfigMap c name)

requireRrdConfigMap :: Config -> T.Text -> [T.Text] -> IO RrdConfigMap
requireRrdConfigMap c name vars = do
  let names = map (\x -> T.concat [ name, ".", x ]) vars
  cs <- mapM (requireRrdConfig c) names
  return $ Map.fromList $ zip vars cs

requireRrdConfig :: Config -> T.Text -> IO RrdConfig
requireRrdConfig c name = do
  RrdConfig <$>
    require c (T.append name ".filename") <*>
    require c (T.append name ".step") <*>
    require c (T.append name ".ds") <*>
    require c (T.append name ".rra")

