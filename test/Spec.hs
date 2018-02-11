{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
{-import Test.Tasty.SmallCheck as SC-}
{-import Test.Tasty.QuickCheck as QC-}
import Test.Tasty.HUnit

import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.Attoparsec.ByteString.Char8 as ABS
{-import qualified Data.Attoparsec.Text as AT-}
import qualified Data.ByteString as BS
{-import qualified Data.Text as Text-}

import Data.Machine (ProcessT, run, supply)
import Data.Machine.Attoparsec

main :: IO ()
main = defaultMain tests

--- simple parsers for testing ---

-- | 'endedBy a b' parses 'a', followed by 'b', and returns the value parsed by
-- 'a'.
bsEndedBy :: ABS.Parser a -> ABS.Parser b -> ABS.Parser a
bsEndedBy p t = p >>= \x -> t >> return x

-- | 'csv' parses a list of integers on a single line, separated by a comma.
bsCsv :: ABS.Parser [Integer]
bsCsv = ABS.decimal `ABS.sepBy` (ABS.char ',') `bsEndedBy` ABS.endOfLine

tests :: TestTree
tests = testGroup "Tests"
  [ byteStringParsingTests ]

parsingDecimals :: Monad m => ProcessT m BS.ByteString (ABS.Result Integer)
parsingDecimals = parsingByteString ABS.decimal

-- | Test a bytestring "parsing" machine by supplying it with chunks of input.
-- Produces a list of results, or the first error encountered.
testBS :: [BS.ByteString] -> ABS.Parser o -> Either String [o]
testBS input parser =
  sequence . fmap ABS.eitherResult . run . supply input $
    parsingByteString parser

byteStringParsingTests :: TestTree
byteStringParsingTests = testGroup "Parser unit tests"
  [ testCase "Parse decimal (single chunk)" $
      let expected = Right [1 :: Integer]
          actual   = testBS ["1"] ABS.decimal
      in  expected @=? actual
  , testCase "Parse CSV (single chunk)" $
      let expected = Right [[1, 2, 3 :: Integer]]
          actual   = testBS ["1,2,3\n"] bsCsv
      in  expected @=? actual
  , testCase "Parse CSV (multiple chunks)" $
      let expected = Right [[1, 2, 3 :: Integer]]
          actual   = testBS ["1,2,", "3\n"] bsCsv
      in  expected @=? actual
  , testCase "Parse Multi-Line CSV (multiple chunks)" $
      let expected = Right [[1, 2, 3 :: Integer], [4, 5, 6]]
          actual   = testBS ["1,2,", "3\n4,", "5,", "6\n"] bsCsv
      in  expected @=? actual
  ]
