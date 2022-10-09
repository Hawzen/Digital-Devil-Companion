{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}


module DemonLib where

import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as B

import Data.Aeson
    ( (.:),
      FromJSON(parseJSON),
      FromJSONKey,
      Value(Object),
      ToJSON,
      ToJSONKey )
import qualified Data.Aeson as AE
import Data.Aeson.Key as AK
import Data.Aeson.KeyMap as AKM
import GHC.Generics

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
    deriving (Show, Eq, Generic, Read)

instance FromJSON Race
instance ToJSON Race


data Demon = Demon {
    demonName :: AK.Key,
    demonRace :: Race,
    demonLevel :: Int,
    demonSkills :: M.Map SkillName Level,
    demonAffinities :: M.Map Element Affinity
} deriving (Show, Eq, Generic)

instance {-# OVERLAPS #-} FromJSON [Demon] where
    parseJSON (Object v) = mapM parseItem $ AKM.toList v
        where
            parseItem (k, Object v2) =
                Demon k <$>
                (readRace <$> v2 .: AK.fromString "race") <*>
                v2 .: AK.fromString "lvl" <*>
                (M.fromList <$> v2 .: AK.fromString  "command") <*>
                (translateToAffinitites <$> v2 .: AK.fromString "resists")

            readRace raceJSON =
                read raceJSON :: Race

            translateToAffinitites affinityJSON =
                M.fromList $ zip  [Physical, Fire, Ice, Electric, Force, Curse] $ Prelude.map translateSingleAffinity affinityJSON

            translateSingleAffinity :: Char -> Affinity
            translateSingleAffinity affinityJSONSingle =
                case affinityJSONSingle of
                    '-' -> Neutral
                    's' -> Resist
                    'w' -> Weak
                    'n' -> Nullify
                    'd' -> Absorb
                    'r' -> Reflect

    parseJSON _ = mempty

instance ToJSON Demon


-- data Stat = HP
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
} deriving (Show, Eq, Generic)

instance FromJSON Stats
instance ToJSON Stats


type SkillName = String

data Skill = Skill {
    skillName :: SkillName,
    skillElement :: Element,
    skillCost :: Int,
    skillPreRequisite :: Stats
} deriving (Show, Eq, Generic)

instance FromJSON Skill
instance ToJSON Skill
instance FromJSONKey Skill
instance ToJSONKey Skill


data Element =
    Passive
    | Auto
    | Physical
    | Fire
    | Ice
    | Force
    | Electric
    | Curse
    | Almighty
    | Recovery
    deriving (Show, Eq, Generic, Ord)

instance FromJSON Element
instance ToJSON Element
instance FromJSONKey Element
instance ToJSONKey Element


data Affinity =
    Neutral
    | Weak
    | Resist
    | Nullify
    | Absorb
    | Reflect
    deriving (Show, Eq, Generic)

instance FromJSON Affinity
instance ToJSON Affinity


--  Constants
vanillaDemonDataJsonFilePath :: String
vanillaDemonDataJsonFilePath = "assets/ove-demon-data.json"
overclockedDemonDataJsonFilePath :: String
overclockedDemonDataJsonFilePath = "assets/ove-demon-data.json"


--  Cool functions
getOverclockedDemons :: IO (Maybe [Demon])
getOverclockedDemons = do
    demonsJsonFile <- B.readFile overclockedDemonDataJsonFilePath
    let maybeDemons = AE.decode demonsJsonFile :: Maybe [Demon]
    return maybeDemons


someFunc :: IO ()
someFunc = putStrLn "someFunc"