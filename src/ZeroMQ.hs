module ZeroMQ (
  sourceBuffers
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Conduit
import qualified System.IO as IO
import System.ZMQ as Z

sourceBuffers :: (MonadResource m) => String -> Source m B.ByteString
sourceBuffers addr = sourceIO
  (do
    c <- Z.init 1
    s <- socket c Sub
    connect s addr
    subscribe s ""
    return (c, s))
  (\(c, s) -> close s >> term c)
  (\(c, s) -> liftIO (receive s []) >>= (return . IOOpen))

