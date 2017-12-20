module Acme.EMR
  ( interactIO
  , decodeDelimited
  , encodeDelimited
  , encodeKeyed
  , keyDelimiter
  , decode
  , group
  , lines
  ) where

import Control.Arrow (second, (&&&))
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Char (ord)
import qualified Data.Csv as Csv
import Data.Function (on)
import Data.List (groupBy)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.String.Conv (toS)
import qualified Data.Vector as V
import Prelude hiding (lines)
import Safe (readMay)
import System.IO (isEOF)

interactIO :: (ByteString -> Maybe ByteString) -> IO ()
interactIO f = go
  where
    go = do
      done <- isEOF
      unless done $ do
        i <- BS.getLine
        case f i of
          Nothing -> go
          Just b -> BS.putStr b >> go

-- | Attempt to decode a 'ByteString' representing a single line of input
decodeDelimited
  :: Csv.FromRecord a
  => Char -> ByteString -> Maybe a
decodeDelimited delim bs =
  case Csv.decodeWith opts Csv.NoHeader (fromStrict bs) of
    Left _ -> Nothing
    Right v -> Just $ V.head v
  where
    opts = Csv.defaultDecodeOptions {Csv.decDelimiter = fromIntegral $ ord delim}

-- | Encode a value to a 'ByteString' using its 'Csv.ToRecord' instance
encodeDelimited
  :: Csv.ToRecord a
  => Char -> a -> ByteString
encodeDelimited delim a = toStrict $ Csv.encodeWith opts [a]
  where
    opts = Csv.defaultEncodeOptions {Csv.encDelimiter = fromIntegral $ ord delim}

keyDelimiter :: Char
keyDelimiter = '\t'

-- | Output a value prefixed by a key
encodeKeyed
  :: (a -> ByteString)
     -- ^ function to extract a key from the value and encode it to a 'ByteString'
  -> (a -> ByteString)
     -- ^ function to encode the value to a 'ByteString'
  -> a
     -- ^ the value
  -> ByteString
encodeKeyed keyEnc vEnc v = keyEnc v <> toS [keyDelimiter] <> vEnc v

decodeKeyed
  :: Read k
  => (ByteString -> Maybe v) -> (ByteString, ByteString) -> Maybe (k, v)
decodeKeyed vDec (mKey, mVal) = do
  k <- readMay $ toS mKey
  v <- vDec mVal
  pure (k, v)

-- | Group the values in a list of (k, v) pairs together
group
  :: Eq k
  => [(k, v)] -> [(k, [v])]
group = map (fst . head &&& fmap snd) . groupBy ((==) `on` fst)

-- | Read a list of 'ByteString's and decode into (k, v) pairs,
-- using the 'Read' instance for 'k', and the provided decoder for 'v'
decode
  :: Read k
  => (ByteString -> Maybe v) -> [ByteString] -> [(k, v)]
decode dec = mapMaybe (decodeKeyed dec) . map (second (BS.drop 1) . BS.break (== keyDelimiter))

-- | Read in all of stdin as a list of ByteStrings
lines :: IO [ByteString]
lines = BS.lines <$> BS.getContents
