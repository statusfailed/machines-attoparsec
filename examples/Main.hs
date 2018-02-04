{-# LANGUAGE RankNTypes #-}
module Main where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString ()
import qualified Data.ByteString as BS
import System.IO

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8

import Data.Machine
import Data.Machine.Attoparsec


-- | k is probably always going to be 'isEOF' :p
ioSource :: MonadIO m => IO Bool -> IO a -> SourceT m a 
ioSource k f = construct go where
  go = do
    cond <- liftIO k
    unless cond $ do
      liftIO f >>= yield >> go

ioM :: MonadIO m => (a -> IO b) -> ProcessT m a b
ioM f = autoM (liftIO . f)

csv :: Parser [Integer]
csv = decimal `sepBy` (char ',') `endedBy` endOfLine

endedBy :: Parser a -> Parser b -> Parser a
endedBy p t = p >>= \x -> t >> return x

-- | Remove values from a process
removing :: Monad m => (a -> Maybe b) -> ProcessT m a b
removing f = repeatedly $ await >>= maybe stop yield . f

parsed :: Monad m => Parser o -> MachineT m (Is BS.ByteString) o
parsed p = parsingMaybe p ~> removing id

-- | An example program which parses comma-separated numbers, then adds them together.
-- Exits immediately on parser failure.
-- Try typing the following into stdin:
--    1,2,3<CR>
main :: IO ()
main = runT_ $ input ~> parsed csv ~> mapping sum ~> fold1 (+) ~> autoM print
  where
    input = ioSource isEOF (BS.hGetSome stdin bufsize) -- 1MB buffer
    bufsize = 2 ^ (20 :: Int)
