module Data.Midi
  where

data MidiNote = MidiNote {
  pitch :: Int
  , duration :: Float
  } deriving (Show)
