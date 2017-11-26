{-# LANGUAGE DeriveGeneric #-}

module Data.Esac
where

import Data.Ratio
import GHC.Generics
import Data.Aeson

data EsacJson = EsacJson {
  name :: String
  , title :: String
  , source :: String
  , region :: String
  , signature :: String
  , key :: String
  , melody :: String
  , remarks :: String
  } deriving (Show, Generic)

instance ToJSON EsacJson

instance FromJSON EsacJson

data Esac = Esac {
  esacKey :: EsacKey
  , notes :: [EsacNote]
  } deriving (Show)

data EsacKey = EsacKey {
  shortSignature :: String
  , shortestNote :: Note
  , baseSound :: Sound
  , metre :: Ratio Int
  } deriving (Show)

data EsacNote = EsacNote {
  octave :: Int
  , interval :: Interval
  , sharpness :: PitchMod
  , duration :: Float
  } deriving (Show, Eq)

data Interval = Interval Int
  deriving (Show, Eq)

data PitchMod = Sharp | Flat | None
  deriving (Eq)

instance Show PitchMod where
  show Sharp = "#"
  show Flat = "b"
  show _ = ""

instance Enum PitchMod where
  fromEnum Sharp = 1
  fromEnum Flat = -1
  fromEnum _ = 0
  toEnum 1 = Sharp
  toEnum (-1) = Flat
  toEnum _ = None

data Note = Note {
  noteDiv :: Int
  } deriving (Show)

data BaseSound = C | D | E | F | G | A | B
  deriving (Show, Eq)

data Sound = Sound BaseSound PitchMod
  deriving (Show, Eq)

cdur :: [BaseSound]
cdur = let
  interval = [C, D, E, F, G, A, B]
  in interval ++ cdur


makeInterval :: BaseSound -> [BaseSound]
makeInterval bs = take 7 . dropWhile (bs /=) $ cdur

addInterval :: Sound -> Interval -> Sound
addInterval s@(Sound bs m) (Interval interval) = let
  customInterval = makeInterval bs
  in Sound (customInterval !! (interval - 1)) m

intervalDisplacement :: [BaseSound] -> Sound -> Int
intervalDisplacement interval to@(Sound base mod) = let
  disp = foldl (\val n -> val + intervalNoteValue n) 0 $ takeWhile (base /=) $ interval
  in disp + fromEnum mod

intervalNoteValue :: BaseSound -> Int
intervalNoteValue E = 1
intervalNoteValue _ = 2

