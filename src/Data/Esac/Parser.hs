module Data.Esac.Parser
  (
    Esac
  , EsacNote
  , Note
  , Sound
  , lookupSound
  , lookupNote
  , readSound
  , parseMelody
  , parseKey
  , parseOctave
  , shortestNote
  , baseSound
  , metre
  , duration
  , notes
  , noteDiv
  , applyPitchMod
  , sharpness
  , parse
  ) where

import Data.Esac
import Control.Monad
import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (State)
import Codec.Midi
import Control.Monad.State.Lazy
import Codec.ByteString.Builder

pitchMod :: Char -> PitchMod
pitchMod c = case c of
  '#' -> Sharp
  'b' -> Flat
  _ -> None

octaves :: [Sound]
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

lookupNote :: Sound -> EsacNote -> Sound
lookupNote base note = applyPitchMod (sharpness note) . lookupSound base . num $ note

readSound :: String -> Maybe Sound
readSound "C" = Just C
readSound "C#" = Just CSharp
readSound "D" = Just D
readSound "D#" = Just DSharp
readSound "E" = Just E
readSound "F" = Just F
readSound "G" = Just G
readSound "G#" = Just GSharp
readSound "A" = Just A
readSound "A#" = Just ASharp
readSound "B" = Just B
readSound _ = Nothing

{-
melody parsing (MEL)
-}
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

{-
key parsing (SIG)
-}

parseKey :: GenParser Char st EsacKey
parseKey = do
  ssig <- parseShortSignature
  many1 space
  shortest <- fmap Note parseShortestNote
  many1 space
  sound <- parseBaseSound
  many1 space
  metre <- parseMetre
  return $ EsacKey ssig shortest sound metre

parseShortSignature :: GenParser Char st String
parseShortSignature = count 6 anyChar

parseShortestNote :: GenParser Char st Int
parseShortestNote = count 2 digit >>= return . read

parseBaseSound :: GenParser Char st Sound
parseBaseSound = manyTill (noneOf " ") (lookAhead space)
  >>= \s -> case readSound s of
              Just s -> return s
              Nothing -> unexpected . show $ s

parseMetre :: GenParser Char st (Ratio Int)
parseMetre = do
  nominator <- fmap read $ manyTill digit (char '/')
  denominator <- fmap read $ many digit
  return $ nominator % denominator
