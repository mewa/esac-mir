module Data.Esac.Converter
  where

import Codec.Midi
import Codec.ByteString.Builder
import Data.Esac.Parser as E
import Data.Midi as M
import Control.Monad.Reader

midi :: Tempo -> Int -> Reader Esac Midi
midi tempo octave = do
  esac <- ask
  let ticksPerShortest = round $ 96 / ((/4) . fromIntegral . noteDiv . shortestNote $ esac)
  return $ Midi SingleTrack (TicksPerBeat 96)
    [makeTempo tempo : makeTrack ticksPerShortest (midiNotes esac octave)]

makeTrack :: Ticks -> [MidiNote] -> Track Ticks
makeTrack baseDuration notes = join $ fmap (makeNote baseDuration) notes

makeNote :: Ticks -> MidiNote -> Track Ticks
makeNote baseDuration note = let
  dur = round $ (fromIntegral $ baseDuration) * M.duration note
  in [(0, NoteOn 0 (pitch note) 40)
     , (dur, NoteOff 0 (pitch note) 40)]

makeTempo :: Tempo -> (Ticks, Message)
makeTempo tempo = (0, TempoChange (floor $ 1000000.0 / (fromIntegral tempo / 60.0)))

midiNotes :: Esac -> Int -> [MidiNote]
midiNotes esac octave = fmap midiNote $ notes esac
  where
    midiNote note = let
      pitch = (+ (octave * 12)) . fromEnum . lookupNote (baseSound esac) $ note
      in MidiNote pitch (E.duration note)

midiBytes = toLazyByteString . buildMidi
  
