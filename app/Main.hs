module Main where

import Traycer
import System.Environment
import System.Random
import Data.Yaml
import Data.Time
import Codec.Picture
import qualified Data.ByteString as B

main :: IO ()
main = do
  (filePath:imageName:_) <- getArgs
  gen <- getStdGen
  contents <- B.readFile filePath
  let config = decode contents :: Maybe (Config Double Int)
  start <- getCurrentTime
  case flip renderImage gen <$> config of
    Just image -> do
      savePngImage imageName image
      end <- getCurrentTime
      putStrLn
        $ "Finished rendering in "
        ++ show (end `diffUTCTime` start)
        ++ ", saved to "
        ++ show imageName
        ++ "."
    Nothing -> putStrLn "An error has occured, check your config file or args."
