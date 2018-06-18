module Lib
    ( httpCacheToIo,
      HttpCache,
      runHttpCache
    ) where

import System.IO
import Network.Wreq as HTTP
import Control.Lens

data HttpCache a
  = ReadFile String (Maybe String -> HttpCache a)
  | WriteFile String String (() -> HttpCache a) 
  | HttpGet String (String -> HttpCache a)
  | Finished a

runHttpCache :: String -> String -> HttpCache String
runHttpCache cachePath url =
    ReadFile cachePath (\cached ->
        case cached of
            Nothing ->
                HttpGet url (\content ->
                    WriteFile cachePath content (\_ -> 
                        Finished content    
                    )    
                )
            Just content ->
                Finished content
    )

httpCacheToIo :: HttpCache String -> IO String
httpCacheToIo val = case val of
    ReadFile path next -> do
        content <- readFile path
        if content == ""
        then
            httpCacheToIo (next Nothing)
        else 
            httpCacheToIo (next $ Just content)
    
    WriteFile path content next -> do
        res <- writeFile path content
        httpCacheToIo (next ())
    
    HttpGet url next -> do
        resp <- HTTP.get url
        httpCacheToIo (next $ show $ resp ^. responseBody)
    
    Finished result ->
        pure result

someFunc :: IO ()
someFunc = putStrLn "someFunc"
