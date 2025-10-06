{-# LANGUAGE DeriveGeneric #-}
module Todo where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory (doesFileExist ,createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data Todo = Todo
  { todoId    :: Int
  , task      :: T.Text
  , completed :: Bool
  } deriving (Show, Generic)

instance FromJSON Todo
instance ToJSON Todo

-- File path
todoFile :: FilePath
todoFile = "data/todos.json"

-- Load todos from file
loadTodos :: IO [Todo]
loadTodos = do
  exists <- doesFileExist todoFile
  if exists
    then do
      content <- BL.readFile todoFile
      case decode content of
        Just todos -> return todos
        Nothing    -> return []
    else return []

-- Save todos to file
saveTodos :: [Todo] -> IO ()
saveTodos todos = do
    let dir = takeDirectory todoFile
    createDirectoryIfMissing True dir   -- create folder if it doesn't exist
    BL.writeFile todoFile (encode todos)

-- Add a new todo
addTodo :: T.Text -> IO ()
addTodo t = do
  todos <- loadTodos
  let newId = if null todos then 1 else todoId (last todos) + 1
      newTodo = Todo newId t False
  saveTodos (todos ++ [newTodo])
  putStrLn $ "Added todo: " ++ T.unpack t

-- List all todos
listTodos :: IO ()
listTodos = do
  todos <- loadTodos
  mapM_ printTodo todos
  where
    printTodo (Todo i t c) =
      putStrLn $ show i ++ ". [" ++ (if c then "x" else " ") ++ "] " ++ T.unpack t

-- Mark todo as done
markDone :: Int -> IO ()
markDone tid = do
  todos <- loadTodos
  let todos' = map (\t -> if todoId t == tid then t { completed = True } else t) todos
  saveTodos todos'
  putStrLn $ "Marked todo " ++ show tid ++ " as done"

-- Delete todo
deleteTodo :: Int -> IO ()
deleteTodo tid = do
  todos <- loadTodos
  let todos' = filter (\t -> todoId t /= tid) todos
  saveTodos todos'
  putStrLn $ "Deleted todo " ++ show tid
