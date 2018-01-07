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

defaultEsacJson = EsacJson "" "" "" "" "" "" "" ""

esacMelodyJson melody key = defaultEsacJson { melody = melody, key = key }

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

fromInterval (Interval val) = val

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
  deriving (Enum, Show, Eq)

data Sound = Sound BaseSound PitchMod
  deriving (Eq, Show)

cdur :: [BaseSound]
cdur = let
  interval = [C, D, E, F, G, A, B]
  in interval ++ cdur

makeFullInterval :: Sound -> [Sound]
makeFullInterval (Sound snd pmod) = let
  infInt = makeInterval snd ++ infInt
  interval = concatMap (mkIntSound) $ infInt
  in case pmod of
       None -> take 12 interval
       Sharp -> take 12 . drop 1 $ interval
       Flat -> take 12 . drop 11 $ interval

mkIntSound snd
  | snd == E = [s]
  | snd == B = [Sound snd None]
  | otherwise = [Sound snd None, Sound snd Sharp]
  where
    s = Sound snd None

makeInterval :: BaseSound -> [BaseSound]
makeInterval bs = take 7 . dropWhile (bs /=) $ cdur

addInterval :: Sound -> Interval -> Sound
addInterval s@(Sound bs m) (Interval interval) = let
  customInterval = makeInterval bs
  in if interval == 0
     then s
     else applyPitchMod m $ Sound (customInterval !! (interval - 1)) None

addMidiKey :: Sound -> Int -> Sound
addMidiKey to@(Sound baseSound pmod) key = let
  baseInterval = (makeFullInterval to)
  in baseInterval !! (mod key 12)

intervalDisplacement :: Sound -> Sound -> Int
intervalDisplacement baseSound to@(Sound base mod) = let
  interval = makeFullInterval baseSound
  disp = length $ takeWhile (to /=) $ interval
  in disp

intervalFromNum :: Int -> (Int, PitchMod)
intervalFromNum num = let
  ints = zip [1..] $ scanl (+) 0 intervalValues
  (int, val) = head $ dropWhile ((< num) . snd) ints
  in (int, toEnum $ num - val)

halftones :: Sound -> Int
halftones (Sound base mod) = let
  halftones = intervalHalftones (Interval $ fromEnum base + 1)
  in addMod halftones mod

addMod :: Int -> PitchMod -> Int
addMod note Sharp = note + 1
addMod note Flat = note - 1
addMod note _ = note

intervalNoteValue :: BaseSound -> Int
intervalNoteValue E = 1
intervalNoteValue B = 1
intervalNoteValue _ = 2

soundValue (Sound base mod) = intervalNoteValue base + fromEnum mod

applyPitchMod :: PitchMod -> Sound -> Sound
applyPitchMod Sharp sound = addMidiKey sound 1
applyPitchMod Flat sound = addMidiKey sound (-1)
applyPitchMod _ s = s


intervalValues :: [Int]
intervalValues = [2, 2, 1, 2, 2, 2, 1]

intervalHalftones :: Interval -> Int
intervalHalftones interval = sum $ take (fromInterval interval - 1) intervalValues

-- +W,W,H,W,W,W,H
-- 1,2,3,4,5,6,7
-- 0,1,2,3,4,5,6 (int - 1)
-- C,D,E,F,G,A,B
soundFromEsacNote base n@(EsacNote oct (Interval int) pmod dur) = let
  prevSound = soundFromEsacNote base $ n { interval = Interval $ int - 1 }
  in case int - 1 of
       0 -> applyPitchMod pmod base
       3 -> addMidiKey prevSound 1
       _ -> addMidiKey prevSound 2

ses mod x = soundFromEsacNote (Sound C None) (EsacNote 3 (Interval x) mod 1)
