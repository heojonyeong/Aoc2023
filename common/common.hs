module Common where

readAndSplit :: String -> IO [String]
readAndSplit x = do file <- readFile x
                    return (lines file)
