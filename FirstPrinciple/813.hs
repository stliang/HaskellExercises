import Control.Monad
import Data.Char

palindrome :: IO ()
palindrome =
  forever $ do
    line1 <- getLine
    case (line1 == reverse line1) of
      True -> putStrLn "It's a palindrome!"
      False -> putStrLn "Nope!"

palindrome' :: IO ()
palindrome' =
  forever $ do
    line1 <- getLine
    let listWords = words line1
    forM_ listWords f
  where
    f x =
      case (x == reverse x) of
        True -> putStrLn "It's a palindrome!"
        False -> putStrLn "Nope!"

palindrome'' :: IO ()
palindrome'' =
  forever $ do
    line1 <- getLine
    let line2 = fmap toLower line1
    case (line2 == reverse line2) of
      True -> putStrLn "It's a palindrome!"
      False -> putStrLn "Nope!"

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Name? "
  name <- getLine
  putStr "Age? "
  age <- getLine
  let age' = read age :: Integer
  let ePerson = mkPerson name age'
  output ePerson
  where
    output eitherP =
      case eitherP of
        (Left err) -> putStrLn $ "Unable to make a person due to " ++ show err
        (Right (Person n a)) -> putStrLn $ "Person name: " ++ show n ++ " Age: " ++ show a
