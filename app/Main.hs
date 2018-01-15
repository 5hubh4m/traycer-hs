module Main where

import Traycer
import System.Environment
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
  contents <- B.readFile filePath
  let config = either error id (decodeEither contents :: Either String (Config Double Int))
  start <- getCurrentTime
  savePngImage imageName $ renderImage config
  end <- getCurrentTime
  putStrLn $ "Finished rendering in "
           ++ show (end `diffUTCTime` start)
           ++ "."
  
