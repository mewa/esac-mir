module Esac
    ( parseEsac
    , parseEsac'
    ) where

import Control.Monad
import Data.Char
import Data.Ratio
import Text.ParserCombinators.Parsec

data MidiNote = MidiNote {
  octave :: Int
  , pitch :: Int
  , duration :: Float
  } deriving (Show)

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

