{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Machine.Attoparsec where

import Prelude hiding ((.))
import Control.Applicative
import Control.Monad

import Data.Machine
import Data.Attoparsec.ByteString

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Repeatedly run a 'Parser a' until there is no more input, or a failure occurs.
parsing :: Monad m => Parser o -> ProcessT m ByteString (Result o)
parsing p = construct $ await >>= go
  where 
    go buf = do
      case BS.null buf of
           True  -> (await <|> stop) >>= go
           False -> result (parse p buf)

    -- | Deal with the result of a parser
    result r@(Fail _ _ _) = yield r >> stop
    result (Done i r)         = yield (Done BS.empty r) >> go i
    result (Partial f)      = (liftM f await <|> stop) >>= result
    {-# INLINABLE go #-}
    {-# INLINABLE result #-}

{-# INLINABLE parsing #-}

parsingEither :: Monad m => Parser o -> ProcessT m ByteString (Either String o)
parsingEither p = eitherResult <$> parsing p

parsingMaybe :: Monad m => Parser o -> ProcessT m ByteString (Maybe o)
parsingMaybe p  = maybeResult <$> parsing p
