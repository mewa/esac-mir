module Data.Midi.Parser
  where

import Codec.Midi as M
import Data.Esac
import Data.Esac.Parser
import Codec.ByteString.Parser as M
import Data.Esac.Converter
import Text.Parsec as P
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import Data.Midi as M
import Control.Monad.State.Lazy as S
import qualified Data.Map as Map
import Data.Ratio

exampleKey = "10101a 04 D# 3/4"
exampleMelody = "1++23b4#  ---5_.4#  654#5  5b2_. //"

exampleEsac :: Esac
exampleEsac = let (Right r) = do
                    melody <- parse parseMelody "Invalid melody (MEL)" $ exampleMelody
                    key <- parse parseKey "Invalid key (KEY)" $ exampleKey
                    return $ Esac key melody
              in r

exampleMidi = runReader (midiFromEsac 90 5) $ exampleEsac

exampleMidiBytes :: BS.ByteString
exampleMidiBytes = midiBytes $ exampleMidi

esacFromMidiBytes :: Sound -> Tempo -> Int -> BS.ByteString -> Either String Esac
esacFromMidiBytes baseSound tempo octave midiData = do
  midi <- fmap toSingleTrack $ M.runParser parseMidi midiData
  let track = Prelude.head . tracks $ midi
      melody = filter (isNote . snd) track
      (TicksPerBeat ticksPerBeat) = timeDiv midi
      midiNotes = midiNotesFromTrack ticksPerBeat melody
      shortest = findShortestNote midiNotes
      esacNotes = esacNotesFromMidi baseSound octave midiNotes
      signature = makeSignature baseSound metre shortest esacNotes
      metre = (3 % 4)
  return $ Esac (EsacKey signature shortest baseSound metre) esacNotes

makeSignature :: Sound -> Ratio Int -> Note -> [EsacNote] -> String
makeSignature (Sound baseSound mod) metre (Note val) esacNotes = let
  sig = padr 6 "00000"
  sh = take 2 . reverse . ("00" ++) . reverse . show $ val
  snd = padl 2 $ show baseSound ++ show mod
  in sig ++ " " ++ sh ++ " " ++ snd ++ " " ++ show (numerator metre) ++ "/" ++ show (denominator metre) 

padl :: Int -> String -> String
padl n s = let
  padding = take (n - length s) $ repeat ' '
  in padding ++ s

padr n s = reverse . padl n . reverse $ s

findShortestNote :: [MidiNote] -> Note
findShortestNote notes = let
  timeline = fmap M.duration notes
  in Note $ (4 *) . round . minimum . filter (/= 0) $ timeline

chunks _ [] = []
chunks n lst = let (chunk, rest) = splitAt n lst
  in chunk : chunks n rest

data TrackState = TrackState { time :: Ticks, activeNotes :: Map.Map Key Ticks }
  deriving (Show)

esacNotesFromMidi :: Sound -> Int -> [MidiNote] -> [EsacNote]
esacNotesFromMidi baseSound octave midiNotes@(baseNote:_) = let
  baseSoundValue = mod (pitch baseNote) 12
  baseKey = baseSoundValue + 12 * octave
  esacNotes = fmap (makeEsacNote octave baseSound) midiNotes
  in esacNotes

-- Take arbitrary base octave (EsacNote octave is relative to this value) and base sound and create EsacNote from MidiNote
makeEsacNote :: Int -> Sound -> MidiNote -> EsacNote
makeEsacNote octave base@(Sound baseSound pmod) (MidiNote pit dur _) = let
  noteNumAtBase = pit - halftones base
  noteOctave = div noteNumAtBase 12
  noteNum = mod noteNumAtBase 12
  -- Interval has to be calculated as offset from base
  (int, sharpness) = intervalFromNum noteNum
  in EsacNote (noteOctave - octave) (Interval int) sharpness dur

midiNotesFromTrack :: Ticks -> Track Ticks -> [MidiNote]
midiNotesFromTrack ticks notes = let
  midiNotes = fmap scaleDuration $ evalState (parseMidiNotes notes) $ TrackState 0 Map.empty
  in midiNotes
  where
    scaleDuration n@(MidiNote _ dur _) = n { M.duration = dur / (fromIntegral ticks) }

parseMidiNotes :: Track Ticks -> S.State TrackState [MidiNote]
parseMidiNotes (((offset, (NoteOn _ k v))) : notes) = do
  modify (\s -> let t = time s + offset
                in s { activeNotes = Map.insert k t (activeNotes s), time = t })
  parseMidiNotes notes
parseMidiNotes (((offset, (NoteOff _ k v))) : notes) = do
  s <- get
  let t = time s + offset
  (Just t0) <- gets (Map.lookup k . activeNotes)
  put $ s { activeNotes = Map.delete k (activeNotes s), time = t }
  rest <- parseMidiNotes notes
  return $ (MidiNote k (fromIntegral $ t - t0) 0) : rest
parseMidiNotes _ = return []

isNote (NoteOn _ _ _) = True
isNote (NoteOff _ _ _) = True
isNote _ = False

-- esacNote :: MidiNote
