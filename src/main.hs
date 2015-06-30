module Main where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as L

import System.Environment
import System.Directory
import System.FilePath as Path

import Types
import Parse


main :: IO ()
main =
  do (filename:_) <- getArgs
     workingDir <- getCurrentDirectory
     contents <- L.readFile (Path.combine workingDir filename)
     let stl = runGet parseSTL contents
     putStrLn . show $ stl

