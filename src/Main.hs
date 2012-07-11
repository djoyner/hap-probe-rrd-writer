{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Monoid (mempty)
import qualified Data.Text as T
import System.Console.CmdArgs.Explicit
import System.Exit
import qualified System.IO as IO

import Config
import Message
import RRD
import ZeroMQ

main = do
  -- Process command line arguments
  !args <- processArgs argsMode
  when (isJust $ lookup flHelp args) $ do
    print $ helpText [] HelpFormatDefault argsMode
    exitSuccess

  let debug      = isJust $ lookup flDebug args
      configFile = case (lookup flConfig) args of
                     Just x -> x
                     Nothing -> error "Missing config file (-c)"

  -- Load the config file
  wc <- loadConfig configFile debug

  runResourceT $ 
    -- Source JSON-encoded probe data (via ZeroMQ SUB socket)
    (sourceBuffers $ T.unpack $ wcSubAddr wc) $=
    -- Parse messages, discarding failures
    (CL.concatMap parseMessage) $=
    -- Decode messages, discarding failures
    (Main.mapMaybe $ decodeMessage wc) $$
    -- Sink into the RRD writer
    (CL.mapM_ $ (liftIO . uncurry writeValues))

-- Idea borrowed from conduit-0.5.1
mapMaybe f = NeedInput push close
  where
    push  = haveMore (NeedInput push close) (return ()) . maybeToList . f
    close = mempty

decodeMessage :: WriterConfig -> Value -> Maybe (Integer, [(RrdConfig, Value)])
decodeMessage wc v = do
  addr <- getAddress v
  pc <- HashMap.lookup addr $ wcProbes wc
  msg <- getMessage v
  sync <- getMessageSync msg
  ds <- getMessageData msg
  return (sync, lookupRrdConfigs pc ds)

lookupRrdConfigs :: ProbeConfig -> [(T.Text, Value)] -> [(RrdConfig, Value)]
lookupRrdConfigs pc ds = do
  (var, val) <- ds
  maybe [] (\rrd -> [(rrd, val)]) $ HashMap.lookup var $ pcRrdConfigs pc

writeValues :: Integer -> [(RrdConfig, Value)] -> IO ()
writeValues sync ds = mapM_ (uncurry $ writeValue sync) ds

writeValue :: Integer -> RrdConfig -> Value -> IO ()
writeValue sync rrd (Number n) = updateRrd rrd sync n
writeValue _ _ _ = return ()

-- Command line argument processing
argsMode :: Mode [(Name, String)]
argsMode =
  (modeEmpty []) {
    modeNames = [ "hap-probe-writer" ]
  , modeHelp = "Home Automation Project: probe data writer"
  , modeGroupFlags = toGroup [
      flagNone [flDebug, "D"] (\v -> (flDebug, ""):v) "Enable debug output"
    , flagReq [flConfig, "c"] (updateArg flConfig) "FILE" "Path to config file"
    , flagHelpSimple ((flHelp, ""):)
    ]
  }
  where
    updateArg fl x v = Right $ (fl, x):v

flDebug    = "debug"
flConfig   = "config"
flHelp     = "help"

