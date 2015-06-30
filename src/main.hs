module Main where

import           Control.Applicative
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.Int
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.ByteString.Lazy    as L

import System.Environment
import System.Directory
import System.FilePath as Path

import Types

getHeader :: Get T.Text
getHeader =
  do bs <- getByteString 80
     case T.decodeUtf8' bs of
       Left _ -> empty
       Right v -> return v

getInt32 :: Get Int
getInt32 = (fromIntegral . fromIntegral) <$> getWord32le

getInt16 :: Get Int
getInt16 = (fromIntegral . fromIntegral) <$> getWord16le

getFloat :: Get Float
getFloat = getFloat32le

parseVector :: Get (V3 Float)
parseVector =
  do a <- getFloat
     b <- getFloat
     c <- getFloat
     return $ V3 a b c

parseTriangle :: Get Triangle
parseTriangle =
  do n <- parseVector
     v1 <- parseVector
     v2 <- parseVector
     v3 <- parseVector
     attr <- getInt16
     return $ Triangle n (V3 v1 v2 v3) attr

repeatParse :: Int -> Get a -> Get [a]
repeatParse 0 a = return []
repeatParse n a =
  do x <- a
     xs <- repeatParse (n-1) a
     return (x:xs)

parseFile :: Get STL
parseFile =
  do header <- fmap T.unpack $ getHeader
     c <- getInt32
     triangles <- repeatParse c parseTriangle
     return (STL header c triangles)

main :: IO ()
main =
  do (filename:_) <- getArgs
     workingDir <- getCurrentDirectory
     contents <- L.readFile (Path.combine workingDir filename)
     let stl = runGet parseFile contents
     putStrLn . show $ stl

