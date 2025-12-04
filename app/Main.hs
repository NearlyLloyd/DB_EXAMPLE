{-# LANGUAGE OverloadedStrings #-}
import System.Exit
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad


data Users = Users Int String deriving (Show)

data TestField = TestField Int String deriving (Show)


instance FromRow Users where
    fromRow = Users <$> field <*> field



main :: IO ()
main = do
    flowHelper
    conn <- open "app/Database/test.db"
    --execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 4" :: String))
    execute_ conn "CREATE TABLE IF NOT EXISTS Users (id INTEGER PRIMARY KEY, Name TEXT)"
    --execute conn "DELETE FROM users" ()
    --execute conn "DROP TABLE users"()

    --execute conn "INSERT INTO Users (Name) VALUES (?)"(Only ("John":: String))
    --r <- query_ conn "SELECT * from Users" :: IO [Users]
    --mapM_ print r
    close conn



flowHelper::IO () 
flowHelper = do
    putStrLn "Enter a command: (type help if you are stuck)"
    cmd <-getLine
    handleCommand cmd

handleCommand:: String -> IO ()
handleCommand cmd = do
    case cmd of
        "help" -> help
        "exit" -> exitSuccess
        ":q" -> exitSuccess
        _ -> putStrLn "ERROR: that command is not recognised, type help for a list of valid commands\n"
    flowHelper
    
    


selectUser :: String -> IO ()
selectUser s = do
    conn <- open "app/Database/test.db"
    r <- queryNamed conn "SELECT * FROM USERS WHERE Name = :Name"[":Name" := s] :: IO[Users]
    mapM_ print r
    close conn

help :: IO ()
help = do 
    putStrLn "\nCommandList:"
    putStrLn "help - gives a list of the commands and their associated use case"
    putStrLn "exit - exits out of the program"


exit:: IO ()
exit = putStrLn "bye bye"


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




