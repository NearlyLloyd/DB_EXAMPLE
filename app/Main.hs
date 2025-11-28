{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Users = Users Int String deriving (Show)

data TestField = TestField Int String deriving (Show)


instance FromRow Users where
    fromRow = Users <$> field <*> field



main :: IO ()
main = do
    conn <- open "app/Database/test.db"
    --execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 4" :: String))
    execute_ conn "CREATE TABLE IF NOT EXISTS Users (id INTEGER PRIMARY KEY, Name TEXT)"
    --execute conn "DELETE FROM users" ()
    --execute conn "DROP TABLE users"()

    --execute conn "INSERT INTO Users (Name) VALUES (?)"(Only ("John":: String))
    r <- query_ conn "SELECT * from Users" :: IO [Users]
    mapM_ print r
    close conn



selectUser :: String -> IO ()
selectUser s = do
    conn <- open "app/Database/test.db"
    r <- queryNamed conn "SELECT * FROM USERS WHERE Name = :Name"[":Name" := s] :: IO[Users]
    mapM_ print r
    close conn






-- myUser :: Int -> String -> User
-- myUser n s = User { userId = n, name = s }

data Book = Book{
    bookId :: Int,
    title :: String,
    author :: String,
    status :: Bool,
    user_borrowed_ID :: Maybe Int
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




