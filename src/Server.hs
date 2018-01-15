{-# LANGUAGE OverloadedStrings #-}
module Server
  (
    serve
  ) where

import Data.Esac
import Data.Esac.Parser
import Data.Esac.Converter
import Data.Either
import Data.Midi.Parser
import Data.Monoid
import Control.Monad.IO.Class
import Web.Scotty
import Control.Monad.Reader
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString as BS
import Codec.Midi
import Network.HTTP.Types.Status
import Network.Wai.Parse
import Debug.Trace
import qualified Data.ByteString.Base64.URL.Lazy as Base64
import qualified Data.Map as Map

defaultParam :: (Parsable a) => TL.Text -> a -> ActionM a
defaultParam name value = rescue (param name) (const $ return value)

serve :: Int -> IO ()
serve port = scotty port $ do
  post "/esac2midi" $ do
    tempo <- defaultParam "tempo" 90
    octave <- defaultParam "octave" 5
    format <- defaultParam "format" "" :: ActionM String
    shouldParse <- defaultParam "parse" "" :: ActionM String
    esac <- case length shouldParse of
      0 -> jsonData :: ActionM EsacJson
      _ -> do
        e <- fmap (LC.unpack . fileContent . snd . head) files
        case parse parseRawEsac "Invalid raw ESAC" e of
          Left e -> do
            status badRequest400
            text . TL.pack . show $ e
            finish
          Right e -> return e
    liftIO $ print esac
    case esacFromJson esac of
      Right m -> do
        let midi = midiBytes . runReader (midiFromEsac tempo octave) $ m
        case format of
          "file" -> midiFileResponse midi
          _ -> base64MidiResponse midi
      Left e -> do
        status badRequest400
        text . TL.pack . show $ e
  post "/midi2esac" $ do
    (_, bytes) <- fmap head files
    (Just baseSound) <- fmap readSound $ rescue (param "key") (const $ return "C") :: ActionM (Maybe Sound)
    let res = do
          esac <- trace (show baseSound) esacFromMidiBytes baseSound 90 5 . fileContent $ bytes
          return $ esacToJson esac
    case res of
      Left e -> do
        status badRequest400
        text . TL.pack . show $ e
      Right mel -> json mel
  post "/esacjson" $ do
    esac <- fmap (LC.unpack . fileContent . snd . head) files
    case parse parseRawEsac "" esac of
      Left e -> do
        status badRequest400
        text . TL.pack . show $ e
      Right esac -> do
        json esac

base64midi = mappend "data:audio/midi;base64," . Base64.encode

base64MidiResponse midi = do
  setHeader "Content-Type" "application/json"
  json $ Map.fromList [("midi64" :: String, LC.unpack $ base64midi midi)]

midiFileResponse midi = do
  setHeader "Content-Type" "audio/midi"
  raw midi
