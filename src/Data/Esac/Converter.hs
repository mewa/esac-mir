module Data.Esac.Converter
  where

import Codec.Midi
import Codec.ByteString.Builder
import Data.Aeson
import Data.Esac as E
import Data.Ratio
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
makeNote _ (MidiPause _) = []
makeNote baseDuration note = let
  dur = round $ (fromIntegral $ baseDuration) * M.duration note
  start = round $ (fromIntegral $ baseDuration) * M.start note
  in [(start, NoteOn 0 (pitch note) 40)
     , (dur, NoteOff 0 (pitch note) 40)]

makeTempo :: Tempo -> (Ticks, Message)
makeTempo tempo = (0, TempoChange (floor $ 1000000.0 / (fromIntegral tempo / 60.0)))

midiNotes :: Esac -> Int -> [MidiNote]
midiNotes esac octave = foldPauses . fmap (mkMidiNote (baseSound . esacKey $ esac) octave) . foldTuplets esac $ notes esac

foldPauses = foldr foldPause []
  where
    foldPause (MidiPause dur) (e:es) = addPause e dur : es
    foldPause e [] = [e]
    foldPause e l = e : l
    addPause e@(MidiPause dur) pause = e { M.duration = dur + pause }
    addPause e pause = e { start = M.start e + pause }

foldTuplets esac = foldr foldTuplet []
  where
    foldTuplet (EsacTuplet t) e = makeTuplet t ++ e
    foldTuplet (EsacSound e) l = e : l
    makeTuplet (Tuplet k notes) = notes

mkMidiNote :: Sound -> Int -> EsacNote -> MidiNote
mkMidiNote base baseOctave note = case interval note of
  Interval 0 -> MidiPause $ E.duration note
  _ -> let
    intSnd@(Sound esacSound pm) = addInterval base (interval note)
    baseSound = halftones base
    noteHalftones = addMod (intervalHalftones (interval note)) $ sharpness note
    octave = baseOctave + E.octave note
    pitch = (octave * 12) + baseSound + noteHalftones
    in MidiNote pitch (E.duration note) 0

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

esacMelody :: [EsacSound] -> String
esacMelody = (++ " //") . join . fmap showEsacMelody
  where
    showEsacMelody (EsacSound n) = showEsacNote n
    showEsacMelody (EsacTuplet (Tuplet k t)) = ('(' :) . (++ ")") . join . fmap showEsacNote $ t
    showEsacNote (EsacNote oct (Interval interval) sh dur) = let
      octave = if oct < 0 then
              replicate (-oct) '-'
            else
              replicate oct '+'
      in octave ++ show (interval) ++ show sh

esacToJson :: Esac -> EsacJson
esacToJson esac = let
  melody = esacMelody . notes $ esac
  signature = shortSignature . esacKey $ esac
  in esacMelodyJson melody signature
