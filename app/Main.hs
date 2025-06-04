module Main (main) where

import DemonLib
import qualified Data.Map as M
import Data.List (find)
import Data.Char (toLower)
import System.IO (hFlush, stdout)
import qualified Data.Aeson.Key as AK

main :: IO ()
main = do
    maybeDemons <- getOverclockedDemons
    case maybeDemons of
        Nothing -> putStrLn "Failed to load demon data"
        Just demons -> do
            putStrLn "Digital Devil Companion"
            commandLoop demons

commandLoop :: [Demon] -> IO ()
commandLoop demons = do
    putStr "command> "
    hFlush stdout
    input <- getLine
    case words input of
        ["quit"] -> putStrLn "Goodbye!"
        ["list"] -> do
            mapM_ (putStrLn . AK.toString . demonName) demons
            commandLoop demons
        ("info":nameParts) -> do
            let name = map toLower (unwords nameParts)
                match d = map toLower (AK.toString (demonName d)) == name
            case find match demons of
                Nothing -> putStrLn "Demon not found." >> commandLoop demons
                Just d -> do
                    printDemonInfo d
                    commandLoop demons
        _ -> putStrLn "Unknown command" >> commandLoop demons

printDemonInfo :: Demon -> IO ()
printDemonInfo d = do
    putStrLn $ "Name: " ++ AK.toString (demonName d)
    putStrLn $ "Race: " ++ show (demonRace d)
    putStrLn $ "Level: " ++ show (demonLevel d)
    putStrLn "Skills:"
    mapM_ (\(s,l) -> putStrLn $ "  " ++ s ++ " (" ++ show l ++ ")")
        (M.toList $ demonSkills d)
    putStrLn "Affinities:"
    mapM_ (\(e,a) -> putStrLn $ "  " ++ show e ++ ": " ++ show a)
        (M.toList $ demonAffinities d)
