module Main where

import Codec.Picture
import Data.ByteString (readFile)
import Data.Time
import Data.Yaml
import Prelude hiding (readFile)
import System.Environment
import Traycer

main :: IO ()
main = do
  args <- getArgs
  let (filePath:imageName:_) =
        case length args of
          2 -> args
          x -> error $ "Two arguments expected, " ++ show x ++ " given."
  putStrLn $ "Rendering " ++ filePath ++ " as " ++ imageName ++ "."
  contents <- readFile filePath
  let config =
        either
          error
          id
          (decodeEither contents :: Either String (Config Double Int))
  start <- getCurrentTime
  savePngImage imageName $ renderImage config
  end <- getCurrentTime
  putStrLn $ "Finished rendering in " ++ show (end `diffUTCTime` start) ++ "."
