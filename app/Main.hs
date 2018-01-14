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
  args <- getArgs
  let (filePath, imageName) = if length args >= 2
                              then (head args, args !! 1)
                              else error "Please give atleast two arguments."
  putStrLn
    $ "Rendering "
    ++ filePath
    ++ " as "
    ++ imageName
    ++ "."
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
    Nothing -> error "Unable to parse config file."
