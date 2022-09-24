module DemonLib where

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B

type Level = Int

data Race = Megami
    | Deity
    | Vile
    | Snake
    | Dragon
    | Divine
    | Avian
    | Fallen
    | Avatar
    | Beast
    | Wilder
    | Genma
    | Fairy
    | Tyrant
    | Kishin
    | Touki
    | Jaki
    | Femme
    deriving (Show, Eq)

data Demon = Demon {
    demonName :: String,
    demonRace :: Race,
    demonLevel :: Int,
    demonSkills :: Map.Map Skill Level,
    demonAffinities :: Map.Map Element Affinity
} deriving (Show, Eq)

-- Stat = HP
--     | MP
--     | Strength
--     | Magic
--     | Vitality
--     | Agility

data Stats = Stats {
    hp :: Int,
    mp :: Int,
    strength :: Int,
    magic :: Int,
    vitality :: Int,
    agility :: Int
} deriving (Show, Eq)

data Skill = Skill {
    skillName :: String,
    skillElement :: Element,
    skillCost :: Int,
    skillPreRequisite :: Stats
} deriving (Show, Eq)

data Element = Passive
    | Auto 
    | Physical 
    | Fire 
    | Ice 
    | Force 
    | Electric 
    | Curse 
    | Almighty
    | Recovery
    deriving (Show, Eq)

data Affinity = Neutral
    | Weak
    | Resist
    | Null
    | Absorb
    | Reflect
    deriving (Show, Eq)

--  Constants
vanillaDemonDataJsonFilePath = "assets/ove-demon-data.json"
overclockedDemonDataJsonFilePath = "assets/ove-demon-data.json"
-- 

someFunc :: IO ()
someFunc = putStrLn "someFunc"
