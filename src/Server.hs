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

serve :: Int -> IO ()
serve port = scotty port $ do
  post "/esac2midi" $ do
    tempo <- rescue (param "tempo") (const $ return 90) :: ActionM Int
    octave <- rescue (param "octave") (const $ return 5) :: ActionM Int
    format <- rescue (param "format") (const $ return "") :: ActionM String
    esac <- jsonData :: ActionM EsacJson
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
          esac <- trace (show baseSound) esacFromMidiBytes baseSound 90 4 . fileContent $ bytes
          return $ esacToJson esac
    case res of
      Left e -> do
        status badRequest400
        text . TL.pack . show $ e
      Right mel -> json mel

base64midi = mappend "data:audio/midi;base64," . Base64.encode

base64MidiResponse midi = do
  setHeader "Content-Type" "application/json"
  json $ Map.fromList [("midi64" :: String, LC.unpack $ base64midi midi)]

midiFileResponse midi = do
  setHeader "Content-Type" "audio/midi"
  raw midi
