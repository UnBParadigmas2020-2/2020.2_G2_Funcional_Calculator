module Main where

import System.Console.Haskeline
import Control.Monad.Trans
import Text.Printf

import Parser
import Eval

process :: String -> IO ()
process line = do
  let result = parser line
  case result of
    Left err -> print err
    Right ex -> case eval ex of
      Nothing -> putStrLn "Impossivel calcular"
      Just result -> putStrLn $ printf "%.3f" result

main :: IO ()
main = runInputT defaultSettings loop
      where
        loop = do
          calcInput <- getInputLine "calc> "
          case calcInput of
            Nothing -> outputStrLn "Desligando..."
            Just input -> liftIO (process input) >> loop