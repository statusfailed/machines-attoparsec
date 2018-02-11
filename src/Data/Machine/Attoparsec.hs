{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Machine.Attoparsec
  ( parsing
  , parsingByteString
  , parsingText
  , parsingEither
  , parsingMaybe
  ) where

import Prelude hiding ((.))
import Control.Applicative
import Control.Monad

import Data.Machine (ProcessT, construct, await, yield, stop)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.Attoparsec.Internal.Types as T
import Data.Attoparsec.ByteString as ABS
import Data.Attoparsec.Text as AT

-- | Repeatedly run a 'T.Parser a' until there is no more input, or a failure occurs.
parsingWith :: Monad m
  => (T.Parser i o -> i -> IResult i o) -- ^ Run a parser taking input type 'i'
  -> (i -> Bool) -- ^ Check whether an input string is empty
  -> i -- ^ the empty input string to feed to a parser continuation
  -> T.Parser i o -- ^ The parser to run
  -> ProcessT m i (IResult i o)
parsingWith run isNull emptyInput parser = construct $ await >>= go
  where 
    go buf = do
      case isNull buf of
           True  -> (await <|> stop) >>= go
           False -> result (run parser buf)

    -- | Deal with the result of a parser
    result r@(Fail _ _ _) = yield r >> stop
    -- ^ immediately stop on error
    result (Done i r)     = yield (Done emptyInput r) >> go i
    -- ^ Yield a 'Done' output with no leftover input, and pass remaining to
    -- next iteration of the parser
    result (Partial f)    = (liftM f await <|> result (f emptyInput)) >>= result
    -- ^ Await for more input. If await fails, force 'Partial' to terminate.
    {-# INLINABLE go #-}
    {-# INLINABLE result #-}

{-# INLINABLE parsingWith #-}

-- | Repeatedly run a 'T.Parser a' until there is no more input, or a failure occurs.
parsing :: Monad m => ABS.Parser o -> ProcessT m ByteString (ABS.Result o)
parsing = parsingByteString
{-# INLINABLE parsing #-}

-- | Repeatedly run a 'ByteString' 'ABS.Parser' until there is no more input, or a failure occurs.
parsingByteString
  :: Monad m => ABS.Parser o -> ProcessT m ByteString (ABS.Result o)
parsingByteString = parsingWith ABS.parse BS.null BS.empty

-- | Repeatedly run a 'Text' 'AT.Parser' until there is no more input, or a failure occurs.
parsingText :: Monad m => AT.Parser o -> ProcessT m Text (AT.Result o)
parsingText = parsingWith AT.parse Text.null Text.empty
{-# INLINABLE parsingText #-}

parsingEither :: Monad m
  => ABS.Parser o -> ProcessT m ByteString (Either String o)
parsingEither p = ABS.eitherResult <$> parsing p

parsingMaybe :: Monad m
  => ABS.Parser o -> ProcessT m ByteString (Maybe o)
parsingMaybe p  = ABS.maybeResult <$> parsing p
