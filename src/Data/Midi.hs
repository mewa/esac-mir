module Data.Midi
  where

data MidiNote =
  MidiNote {
  pitch :: Int
  , duration :: Float
  , start :: Float
  }
  | MidiPause {
      duration :: Float
      }
  deriving (Show)
