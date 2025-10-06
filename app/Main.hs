module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import Todo

main :: IO ()
main = do
  args <- getArgs

  saveTodos []
  
  case args of
    ("add":xs)    -> addTodo (T.pack $ unwords xs)
    ["list"]      -> listTodos
    ["done", n]   -> markDone (read n)
    ["delete", n] -> deleteTodo (read n)
    _             -> putStrLn "Usage: todo [add|list|done|delete] ..."
