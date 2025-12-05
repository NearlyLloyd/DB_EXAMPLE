{-# LANGUAGE OverloadedStrings #-}
import System.Exit
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad


data Users = Users Int String deriving (Show)
data Books = Books Int String String String deriving (Show)
data TestField = TestField Int String deriving (Show)


instance FromRow Users where
    fromRow = Users <$> field <*> field

instance FromRow Books where
    fromRow = Books <$> field <*> field <*> field <*> field



main :: IO ()
main = do
    
    conn <- open "app/Database/test.db"
    --execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 4" :: String))
    execute_ conn "CREATE TABLE IF NOT EXISTS Users (id INTEGER PRIMARY KEY, Name TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS Books (id INTEGER PRIMARY KEY, Title TEXT, Author TEXT, Status TEXT)"
    --execute conn "DELETE FROM users" ()
    --execute conn "DROP TABLE users"()
    --execute conn "INSERT INTO Users (Name) VALUES (?)"(Only ("John":: String))
    --r <- query_ conn "SELECT * from Users" :: IO [Users]
    --mapM_ print r
    close conn
    flowHelper


--allows to control the inputs and control unexpected commands 
flowHelper::IO () 
flowHelper = do
    putStrLn "Enter a command: (type help if you are stuck)"
    cmd <-getLine
    handleCommand cmd

--handles inputs given from flowHelper, list of all valid commands here, making the program more reliable
handleCommand:: String -> IO ()
handleCommand cmd = do
    case cmd of
        "help" -> help
        "exit" -> exitSuccess
        ":q" -> exitSuccess
        "addBook" -> addBook
        "listBooks" -> listBooks
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
    putStrLn "addBook - adds a new book"
    putStrLn "removeBook - removes a book if it is available"
    putStrLn "*NOT IMPLEMENTED*"
    putStrLn "addUser - adds a new user"
    putStrLn "removeUser - removes a user if they have no borrowed books"
    putStrLn "borrowBook - marks a book as borrowed by a user"
    putStrLn "returnBook - marks a book as returned by the borrowing user"
    putStrLn "availableBooks - lists all available books"
    putStrLn "borrowedBooks - lists all borrowed books with the borrowers"
    putStrLn "listUsers - lists all users"




-- myUser :: Int -> String -> User
-- myUser n s = User { userId = n, name = s }



addBook :: IO()
addBook = do
    putStrLn "Enter Book title: "
    title <- getLine
    putStrLn "Enter Author name: "
    author <- getLine
    conn  <- open "app/Database/test.db"
    execute conn "INSERT INTO Books(Title,Author,Status) VALUES (?,?,?)" [title, author, "Available"]
    close conn

removeBook ::IO()
removeBook = do
    putStrLn "Enter Book Title: "
    title <- getLine
    putStrLn "Enter Author: "
    author <- getLine
    conn  <- open "app/Database/test.db"
    r <- queryNamed conn "SELECT * FROM Books WHERE (Title = :Title AND Author = :Author AND Status = :Status)" [":Title" := title,":Author" := author, ":Status" := ("Available"  :: String)] :: IO[Books]
    
    mapM_ print r
    close conn


listBooks :: IO()
listBooks = do
        conn <- open "app/Database/test.db"
        r <- query_ conn "SELECT * FROM Books" :: IO[Books]
        mapM_ print r




