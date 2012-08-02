module ZeroMQ (
  sourceBuffers
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import Data.Conduit
import qualified System.IO as IO
import System.ZMQ as Z

sourceBuffers :: (MonadResource m) => String -> Bool -> Source m B.ByteString
sourceBuffers addr debug = sourceIO
  (do
    when debug $ IO.hPutStrLn IO.stderr $ "Acquiring ZeroMQ SUB socket " ++ addr
    c <- Z.init 1
    s <- socket c Sub
    connect s addr
    subscribe s ""
    return (c, s))
  (\(c, s) -> do
    when debug $ IO.hPutStrLn IO.stderr $ "Releasing ZeroMQ SUB socket"
    close s
    term c)
  (\(_, s) -> liftIO (receive s []) >>= (return . IOOpen))

