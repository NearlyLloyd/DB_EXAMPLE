{-# LANGUAGE OverloadedStrings #-}
import System.Exit
import Control.Applicative
import Database.SQLite.Simple
import Control.Monad
import qualified Data.Text as Text

data Users = Users Int String deriving (Show)
data Books = Books Int String String String (Maybe Int) deriving (Show)
data ID = ID Int deriving(Show)

data UsersBooksJoined = UsersBooksJoined Int String Int String String String (Maybe Int) deriving (Show)

instance FromRow Users where
    fromRow = Users <$> field <*> field

instance FromRow Books where
    fromRow = Books <$> field <*> field <*> field <*> field <*> field

instance FromRow UsersBooksJoined where
    fromRow = UsersBooksJoined <$> field <*> field <*> field <*> field <*> field <*> field <*> field


main :: IO ()
main = do
    conn <- open "app/Database/test.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS Users (UserID INTEGER PRIMARY KEY, Name TEXT)"
    execute_ conn "CREATE TABLE IF NOT EXISTS Books (BookID INTEGER PRIMARY KEY, Title TEXT, Author TEXT, Status TEXT,BorrowedBy int)"
    --execute conn "DELETE FROM users" ()
    --execute conn "DROP TABLE Books"()
    --execute conn "DROP TABLE Users"()

    --execute conn "INSERT INTO Users (Name) VALUES (?)"(Only ("John":: String))
    --r <- query_ conn "SELECT * from Users" :: IO [Users]
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
        "availableBooks" -> listBooks
        "removeBook" -> removeBook
        "addUser" -> addUser
        "borrowBook" -> borrowBook
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
    putStrLn "addUser - adds a new user"
    putStrLn "borrowBook - marks a book as borrowed by a user"
    putStrLn "*NOT IMPLEMENTED*"
    putStrLn "removeUser - removes a user if they have no borrowed books"
    putStrLn "returnBook - marks a book as returned by the borrowing user"
    putStrLn "availableBooks - lists all available books"
    putStrLn "borrowedBooks - lists all borrowed books with the borrowers"
    putStrLn "listUsers - lists all users"



addBook :: IO ()
addBook = do
    putStrLn "Enter Book title: "
    title <- getLine
    putStrLn "Enter Author name: "
    author <- getLine
    conn  <- open "app/Database/test.db"
    execute conn "INSERT INTO Books(Title,Author,Status,BorrowedBy) VALUES (?,?,?,null)" [title, author, "Available"]
    close conn

removeBook ::IO ()
removeBook = do
    putStrLn "Enter Book Title: "
    titleQuery <- getLine
    putStrLn "Enter Author: "
    authorQuery <- getLine
    conn  <- open "app/Database/test.db"
    --r <- queryNamed conn "SELECT (BookID,Title,Author) FROM Books WHERE (Title = :Title AND Author = :Author AND Status = :Status)" [":Title" := titleQuery,":Author" := authorQuery, ":Status" := ("Available"  :: String)] :: IO[Books]
    r <- queryNamed conn "SELECT BookID,Title,Author FROM Books WHERE (Title = :Title AND Author = :Author AND Status = :Status)" [":Title" := titleQuery,":Author" := authorQuery, ":Status" := ("Available"  :: String)]
    _ <- queryNamed conn "DELETE FROM Books WHERE (Title = :Title AND Author = :Author AND Status = :Status)" [":Title" := titleQuery,":Author" := authorQuery, ":Status" := ("Available"  :: String)] :: IO[Books]


    forM_ r $ \(id,title,author) ->
        putStrLn $ Text.unpack "Title: " ++ title ++ " Written by: " ++ author ++ " REMOVED. ID: "++show (id ::Int)
    close conn


listBooks :: IO ()
listBooks = do
        conn <- open "app/Database/test.db"
        r <- query_ conn "SELECT * FROM Books" :: IO[Books]
        mapM_ print r

addUser :: IO ()
addUser = do
    putStrLn "Enter the user's name"
    name <- getLine
    conn <- open "app/Database/test.db"
    execute conn "INSERT INTO Users(Name) VALUES (?)" [name]
    let outputText = "User: " ++ name ++ " added to database!"
    putStrLn outputText 
    close conn

borrowBook :: IO ()
borrowBook = do
    putStrLn "Enter user's ID: "
    userId <- getLine
    putStrLn "Enter Book ID: "
    bookId <- getLine
    conn <- open "app/Database/test.db"
    execute conn "UPDATE Books SET BorrowedBy = ?, Status = 'Borrowed' WHERE BookID = ? AND Status = 'Available' "[userId,bookId]
    close conn

removeUser :: IO ()
removeUser = do

    conn <- open "app/Database/test.db"
    r <- query_ conn "SELECT UserID,Name FROM Users INNER JOIN Books ON Users.UserID = Books.borrowedBy WHERE Users.UserID = 1"

    forM_ r $ \(id,name) ->
        putStrLn $ Text.unpack name ++show (id ::Int)
    --mapM_ print r
        
    close conn




