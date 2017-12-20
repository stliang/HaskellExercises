{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String.Conv (toS)

import Acme.EMR
import Acme.Types
import Data.ByteString.Char8 (ByteString)

main :: IO ()
myDecode :: ByteString -> Maybe AcmeCustomerAttributes
myDecode b = decodeDelimited keyDelimiter b

myEncode :: AcmeCustomerAttributes -> ByteString
myEncode ac = encodeDelimited keyDelimiter ac

worker :: ByteString -> Maybe ByteString
worker = fmap myEncode . myFilter . myDecode

myFilter :: Maybe AcmeCustomerAttributes -> Maybe AcmeCustomerAttributes
myFilter Nothing = Nothing
myFilter (Just ac) =
  let age = _acAge ac
      score = _acChurnScore ac
  in if age >= 18 && age <= 32 && score >= 0.95
       then Just ac
       else Nothing

main = interactIO worker
