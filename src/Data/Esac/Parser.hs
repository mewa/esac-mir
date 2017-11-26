module Data.Esac.Parser
  (
    Esac
  , EsacNote
  , Note
  , Sound
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

readSound :: String -> Maybe Sound
readSound "Cb" = Just $ Sound C Flat
readSound "C" = Just $ Sound C None
readSound "C#" = Just $ Sound C Sharp
readSound "Db" = Just $ Sound D Flat
readSound "D" = Just $ Sound D None
readSound "D#" = Just $ Sound D Sharp
readSound "Eb" = Just $ Sound E Flat
readSound "E" = Just $ Sound E None
readSound "E#" = Just $ Sound E Sharp
readSound "Fb" = Just $ Sound F Flat
readSound "F" = Just $ Sound F None
readSound "F#" = Just $ Sound F Sharp
readSound "Gb" = Just $ Sound G Flat
readSound "G" = Just $ Sound G None
readSound "G#" = Just $ Sound G Sharp
readSound "Ab" = Just $ Sound A Flat
readSound "A" = Just $ Sound A None
readSound "A#" = Just $ Sound A Sharp
readSound "Bb" = Just $ Sound B Flat
readSound "B" = Just $ Sound B None
readSound "B#" = Just $ Sound B Sharp
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
  (interval, pitchMod) <- parseNoteInterval
  dur <- parseNoteDuration
  return $ EsacNote oct interval pitchMod dur

parseNoteDuration :: GenParser Char st Float
parseNoteDuration = do
  len <- many $ char '_'
  dot <- option 0 (try $ char '.' >> return 0.5)
  return $ fromIntegral 2^(length len) + dot

parseNoteInterval :: GenParser Char st (Interval, PitchMod)
parseNoteInterval = do
  val <- oneOf ['0', '1', '2', '3', '4', '6', '7']
  mod <- option ' ' (try $ oneOf "#b")
  return $ (Interval $ digitToInt val, pitchMod mod)
      
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
