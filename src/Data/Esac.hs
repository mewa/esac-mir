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
  , num :: Int
  , sharpness :: PitchMod
  , duration :: Float
  } deriving (Show)

data PitchMod = Sharp | Flat | None
  deriving (Show)

data Note = Note {
  noteDiv :: Int
  } deriving (Show)

data Sound = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B
  deriving (Enum, Show)
