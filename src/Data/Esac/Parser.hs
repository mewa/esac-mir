module Data.Esac.Parser
  (
    Esac
  , EsacNote
  , Note
  , Sound
  , lookupSound
  , lookupNote
  , readSound
  , parseEsac
  , shortestNote
  , baseSound
  , duration
  , notes
  , noteDiv
  , applyPitchMod
  , sharpness
  ) where

import Control.Monad
import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (State) 
import Codec.Midi
import Control.Monad.State.Lazy
import Codec.ByteString.Builder

data PitchMod = Sharp | Flat | None
  deriving (Show)

pitchMod c = case c of
  '#' -> Sharp
  'b' -> Flat
  _ -> None

data Note = Note {
  noteDiv :: Int
  } deriving (Show)

data Sound = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B
  deriving (Enum, Show)

octaves = let
  octave = [C, D, E, F, G, A, B]
  in cycle octave

lookupSound :: Sound -> Int -> Sound
lookupSound base sound = head . drop (fromEnum base + sound) $ octaves

applyPitchMod Sharp = sharp
applyPitchMod Flat = flat
applyPitchMod _ = id

sharp :: Sound -> Sound
sharp = toEnum . (\x -> mod (x + 1) 12) . fromEnum

flat :: Sound -> Sound
flat = toEnum . (\x -> mod (x - 1) 12) . fromEnum

lookupNote base note = applyPitchMod (sharpness note) . lookupSound base . num $ note

readSound s = case s of
    "C" -> C
    "C#" -> CSharp
    "D" -> D
    "D#" -> DSharp
    "E" -> E
    "F" -> F
    "G" -> G
    "G#" -> GSharp
    "A" -> A
    "A#" -> ASharp
    "B" -> B

data Esac = Esac {
  baseSound :: Sound
  , shortestNote :: Note
  , notes :: [EsacNote]
  } deriving (Show)

data EsacNote = EsacNote {
  octave :: Int
  , num :: Int
  , sharpness :: PitchMod
  , duration :: Float
  } deriving (Show)

parseEsac :: String -> Either ParseError Esac
parseEsac esac = parse parseMelody "(Not a valid EsAC)" esac >>= return . Esac C (Note 4)

parseMelody :: GenParser Char st [EsacNote]
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

parseNote :: GenParser Char st EsacNote
parseNote = do
  skipMany $ char '^'
  oct <- parseOctave
  pit <- parseNotePitch
  dur <- parseNoteDuration
  return $ EsacNote oct (fst pit) (snd pit) dur

parseNoteDuration :: GenParser Char st Float
parseNoteDuration = do
  len <- many $ char '_'
  dot <- option 0 (try $ char '.' >> return 0.5)
  return $ fromIntegral 2^(length len) + dot

parseNotePitch :: GenParser Char st (Int, PitchMod)
parseNotePitch = do
  val <- digit
  mod <- option ' ' (try $ oneOf "#b")
  return $ (digitToInt val - 1, pitchMod mod)
      
parseOctave :: GenParser Char st Int
parseOctave = do
  val <- many $ oneOf "+-"
  return $ foldl countOct 0 val
  where
    countOct oct e = case e of
      '+' -> oct + 1
      '-' -> oct - 1
      _ -> oct

