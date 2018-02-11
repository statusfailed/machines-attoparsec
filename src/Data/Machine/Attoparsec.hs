{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Machine.Attoparsec
  ( parsing
  , parsingEither
  , parsingMaybe
  ) where

import Prelude hiding ((.))
import Control.Applicative
import Control.Monad

import Data.Machine
import qualified Data.Attoparsec.Internal.Types as T
import Data.Attoparsec.ByteString

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

parsingWith :: Monad m
  => (T.Parser i o -> i -> IResult i o) -- ^ Run a parser taking input type 'i'
  -> (i -> Bool) -- ^ Check whether an input string is empty
  -> i -- ^ the empty input string to feed to a parser continuation
  -> T.Parser i o -- ^ The parser to run
  -> ProcessT m i (IResult i o)
parsingWith run isNull empty parser = construct $ await >>= go
  where 
    go buf = do
      case isNull buf of
           True  -> (await <|> stop) >>= go
           False -> result (run parser buf)

    -- | Deal with the result of a parser
    result r@(Fail _ _ _) = yield r >> stop
    result (Done i r)     = yield (Done empty r) >> go i
    result (Partial f)    = (liftM f await <|> stop) >>= result
    {-# INLINABLE go #-}
    {-# INLINABLE result #-}

{-# INLINABLE parsingWith #-}

-- | Repeatedly run a 'Parser a' until there is no more input, or a failure occurs.
parsing :: Monad m => Parser o -> ProcessT m ByteString (Result o)
parsing = parsingWith parse BS.null BS.empty
{-parsing p = construct $ await >>= go-}
  {-where -}
    {-go buf = do-}
      {-case BS.null buf of-}
           {-True  -> (await <|> stop) >>= go-}
           {-False -> result (parse p buf)-}

    {--- | Deal with the result of a parser-}
    {-result r@(Fail _ _ _) = yield r >> stop-}
    {-result (Done i r)     = yield (Done BS.empty r) >> go i-}
    {-result (Partial f)    = (liftM f await <|> stop) >>= result-}
--    {-# INLINABLE go #-}
--    {-# INLINABLE result #-}

{-# INLINABLE parsing #-}

parsingEither :: Monad m => Parser o -> ProcessT m ByteString (Either String o)
parsingEither p = eitherResult <$> parsing p

parsingMaybe :: Monad m => Parser o -> ProcessT m ByteString (Maybe o)
parsingMaybe p  = maybeResult <$> parsing p
