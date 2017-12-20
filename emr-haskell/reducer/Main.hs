module Main where

import Acme.EMR
import qualified Acme.EMR as EMR
import Acme.Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

myDecode :: ByteString -> Maybe AcmeCustomerAttributes
myDecode b = decodeDelimited keyDelimiter b

myEncode :: AcmeSegmentInfo -> ByteString
myEncode ac = encodeDelimited keyDelimiter ac

toKvList :: [AcmeCustomerAttributes] -> [(Int, AcmeCustomerAttributes)]
toKvList = fmap (\a -> (_acSegmentId a, a))

f :: [(Int, [AcmeCustomerAttributes])] -> [AcmeSegmentInfo]
f = fmap (\(a, b) -> AcmeSegmentInfo a $ length b)

writeByteStrings :: [ByteString] -> IO ()
writeByteStrings = mapM_ BS.putStrLn

main :: IO ()
main = do
  inputs <- EMR.lines
  let acList = catMaybes $ fmap myDecode inputs
      kvs = EMR.group $ sortOn fst $ toKvList acList
      bs = fmap myEncode $ f kvs
  writeByteStrings bs
