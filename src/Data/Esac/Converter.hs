module Data.Esac.Converter
  where

import Codec.Midi
import Codec.ByteString.Builder
import Data.Esac as E
import Data.Esac.Parser as E
import Data.Midi as M
import Control.Monad.Reader
import Text.Parsec

-- ********************
-- ESAC -> MIDI conversion
-- ********************

midiFromEsac :: Tempo -> Int -> Reader Esac Midi
midiFromEsac tempo octave = do
  esac <- ask
  let ticksPerShortest = round $ 96 / ((/4) . fromIntegral . noteDiv . shortestNote . esacKey $ esac)
  return $ Midi SingleTrack (TicksPerBeat 96)
    [makeTempo tempo : makeTrack ticksPerShortest (midiNotes esac octave)]

makeTrack :: Ticks -> [MidiNote] -> Track Ticks
makeTrack baseDuration notes = let
  trackNotes = join $ fmap (makeNote baseDuration) notes
  track = trackNotes ++ [(0, TrackEnd)]
  in track

makeNote :: Ticks -> MidiNote -> Track Ticks
makeNote baseDuration note = let
  dur = round $ (fromIntegral $ baseDuration) * M.duration note
  in [(0, NoteOn 0 (pitch note) 40)
     , (dur, NoteOff 0 (pitch note) 40)]

makeTempo :: Tempo -> (Ticks, Message)
makeTempo tempo = (0, TempoChange (floor $ 1000000.0 / (fromIntegral tempo / 60.0)))

midiNotes :: Esac -> Int -> [MidiNote]
midiNotes esac octave = fmap (mkMidiNote (baseSound . esacKey $ esac) octave) $ notes esac

mkMidiNote :: Sound -> Int -> EsacNote -> MidiNote
mkMidiNote base baseOctave note = let
  intSnd@(Sound esacSound pm) = addInterval base (interval note)
  baseSound = halftones base
  noteHalftones = addMod (intervalHalftones (interval note)) $ sharpness note
  octave = baseOctave + E.octave note
  pitch = (octave * 12) + baseSound + noteHalftones
  in MidiNote pitch (E.duration note)

midiBytes = toLazyByteString . buildMidi
  
-- ********************
-- ESAC <-> JSON conversion
-- ********************

esacFromJson :: EsacJson -> Either ParseError Esac
esacFromJson json = do
  melody <- parse parseMelody "Invalid melody (MEL)" . melody $ json
  key <- parse parseKey "Invalid key (KEY)" . E.key $ json
  return $ Esac key melody

-- ********************
-- ESAC -> String
-- ********************

esacMelody :: Int -> [EsacNote] -> String
esacMelody base = (++ " //") . join . fmap showEsacNote
  where
    showEsacNote (EsacNote oct (Interval interval) sh dur) = let
      octave = if oct < base then
              replicate (base - oct) '-'
            else
              replicate (oct - base) '+'
      in octave ++ show (interval) ++ show sh
