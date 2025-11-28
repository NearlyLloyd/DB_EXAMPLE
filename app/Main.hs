{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

main :: IO ()
main = do
    conn <- open "app/Database/test.db"
    --execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 4" :: String))
    r <- query_ conn "SELECT * from test" :: IO [TestField]
    mapM_ print r
    close conn


myUser :: Int -> String -> User
myUser n s = User { userId = n, name = s }

data Book = Book{
    bookId :: Int,
    title :: String,
    author :: String,
    status :: Bool 
} deriving (Show)


data User = User{
    userId :: Int,
    name :: String
}deriving (Show)

getName :: User -> String
getName (User _ n ) = n

getId :: User -> Int
getId (User n _) = n


-- import Control.Applicative
-- import Database.SQLite.Simple
-- import Database.SQLite.Simple.FromRow




