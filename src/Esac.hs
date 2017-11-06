module Esac
  (
    esacToMidi
  , midiBytes
  ) where

import Control.Monad
import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (State) 
import Codec.Midi
import Control.Monad.State.Lazy
import Codec.ByteString.Builder

data MidiNote = MidiNote {
  octave :: Int
  , pitch :: Int
  , duration :: Float
  } deriving (Show)

data MidiState = MidiState {
  now :: Ticks
  , baseDuration :: Ticks
  , basePitch :: Key
  } deriving (Show)

makeTrack :: Ticks -> [MidiNote] -> Track Ticks
makeTrack base notes = join $ evalState (mapM makeNote notes) (MidiState 0 base 40)

makeNote :: MidiNote -> State MidiState (Track Ticks)
makeNote note = do
  s <- get
  let dur = round $ (fromIntegral $ baseDuration s) * duration note
  return $ [(0, NoteOn 0 (pitch note + basePitch s) 40)
           , (dur, NoteOff 0 (pitch note + basePitch s) 40)]

midi :: [MidiNote] -> Tempo -> Int -> Midi
midi notes tempo shortest = let
  ticksPerShortest = round $ 96 / (fromIntegral shortest / 4)
  in Midi SingleTrack (TicksPerBeat 96)
     [makeTempo tempo : makeTrack ticksPerShortest notes]


makeTempo tempo = (0, TempoChange (floor $ 1000000.0 / (fromIntegral tempo / 60.0)))

esacToMidi melody = parseEsac melody >>= \notes -> return (midi notes 90 8)

midiBytes = toLazyByteString . buildMidi

parseEsac :: String -> Either ParseError [MidiNote]
parseEsac = parse parseMelody "(Not a valid EsAC)"

parseEsac' :: String -> Either ParseError [MidiNote]
parseEsac' = parse parseFreeMelody "(Not a valid EsAC)"

parseMelody :: GenParser Char st [MidiNote]
parseMelody = do
  (try $ do
      string " //"
      eof
      return [])
    <|> (do
            note <- (try (many space) >> parseNote)
                    <|> parseNote
            notes <- parseMelody
            return $ note : notes)

parseFreeMelody = do
  notes <- flip sepBy spaces parseNote
  eof
  return $ notes

parseNote :: GenParser Char st MidiNote
parseNote = do
  skipMany $ char '^'
  octave <- parseOctave
  pitch <- parseNotePitch
  duration <- parseNoteDuration
  return $ MidiNote octave pitch duration

parseNoteDuration :: GenParser Char st Float
parseNoteDuration = do
  len <- many $ char '_'
  dot <- option 0 (try $ char '.' >> return 0.5)
  return $ fromIntegral 2^(length len) + dot

parseNotePitch :: GenParser Char st Int
parseNotePitch = do
  val <- digit
  mod <- option ' ' (try $ oneOf "#b")
  return $ 2 * digitToInt val + pitchMod mod
  where
    pitchMod e = case e of
      '#' -> 1
      'b' -> -1
      _ -> 0
      
parseOctave :: GenParser Char st Int
parseOctave = do
  val <- many $ oneOf "+-"
  return $ foldl countOct 0 val
  where
    countOct oct e = case e of
      '+' -> oct + 1
      '-' -> oct - 1
      _ -> oct

