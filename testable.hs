{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.State.Lazy as S

importantBusinessAction
  :: World m
  => m ()
importantBusinessAction = do
  writeLine "Please enter your name: "
  name <- readLine
  if "" == name
    then do
      writeLine "I really really need a name!"
      importantBusinessAction
    else writeLine $ "Hello, " ++ name ++ "!"

class Monad m =>
      World m where
  writeLine :: String -> m ()
  readLine :: m String

instance World IO where
  writeLine = putStrLn
  readLine = getLine

type FakeIO = S.State FakeState

data FakeState = FS
  { fsWrittenLines :: [String]
  , fsReadLine :: FakeIO String
  }

def :: FakeState
def = FS {fsWrittenLines = [], fsReadLine = return ""}

instance World FakeIO where
  writeLine s = do
    st <- S.get
    let oldLines = fsWrittenLines st
    S.put st {fsWrittenLines = s : oldLines}
  readLine = do
    st <- S.get
    let readLineAction = fsReadLine st
    readLineAction

runFakeWorld :: b -> State b a -> (a, b)
runFakeWorld = flip S.runState

--main = do
main :: IO ()
main = importantBusinessAction

test :: IO ()
test = do
  let readLine_for_test :: FakeIO String
      readLine_for_test = do
        S.modify $ \s -> s {fsReadLine = return "Joe"}
        return ""
  let initState = def {fsReadLine = readLine_for_test}
  let ((), endState) = runFakeWorld initState importantBusinessAction
  forM_ (reverse $ fsWrittenLines endState) $ \line -> print line
