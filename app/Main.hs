module Main (main) where

import DemonLib

main :: IO ()
main = do
    demonsJsonFile <- B.readfile overclockedDemonDataJsonFilePath

