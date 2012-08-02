{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
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
      subscribe  = case (lookup flSubscribe) args of
                     Just x -> x
                     Nothing -> error "Missing subscribe address (-s)"

  -- Load the config file
  wc <- loadConfig configFile debug

  -- Everything runs in conduit ;)
  runResourceT $ 
    -- Source JSON-encoded probe messages (via ZeroMQ SUB socket)
    (sourceBuffers subscribe debug) $=
    -- Print raw messages for debug
    (CL.mapM $ (\x -> liftIO (when (debug) $ B8.hPutStrLn IO.stderr x) >> return x)) $=
    -- Parse to values, discarding parse failures
    (CL.concatMap parseMessage) $=
    -- Resolve to RRD configs, discarding lookup failures
    (CL.concatMap $ (maybeToList . resolveToRrdConfigs wc)) $$
    -- Sink into the RRD writer
    (CL.mapM_ $ (liftIO . uncurry writeValues))

resolveToRrdConfigs :: WriterConfig -> Value -> Maybe (Integer, [(RrdConfig, Value)])
resolveToRrdConfigs wc v = do
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
writeValue _ _ _               = return ()

-- Command line argument processing
argsMode :: Mode [(Name, String)]
argsMode =
  (modeEmpty []) {
    modeNames = [ "hap-probe-writer" ]
  , modeHelp = "Home Automation Project: probe data writer"
  , modeGroupFlags = toGroup [
      flagNone [flDebug, "D"] (\v -> (flDebug, ""):v) "Enable debug output"
    , flagReq [flConfig, "c"] (updateArg flConfig) "FILE" "Path to config FILE"
    , flagReq [flSubscribe, "s"] (updateArg flSubscribe) "ADDRESS" "Sink data from ZeroMQ SUB socket connected to ADDRESS"
    , flagHelpSimple ((flHelp, ""):)
    ]
  }
  where
    updateArg fl x v = Right $ (fl, x):v

flDebug     = "debug"
flConfig    = "config"
flSubscribe = "subscribe"
flHelp      = "help"

