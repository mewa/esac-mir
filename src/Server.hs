{-# LANGUAGE OverloadedStrings #-}
module Server
  (
    serve
  ) where

import Data.Esac
import Data.Esac.Converter
import Data.Either
import Control.Monad.IO.Class
import Web.Scotty
import Control.Monad.Reader
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as L
import Codec.Midi
import Data.Aeson
import Network.HTTP.Types.Status

serve :: Int -> IO ()
serve port = scotty port $ do
  post "/esac2midi" $ do
    tempo <- rescue (param "tempo") (const $ return 90) :: ActionM Int
    octave <- rescue (param "octave") (const $ return 4) :: ActionM Int
    esac <- jsonData :: ActionM EsacJson
    liftIO $ print esac
    case esacFromJson esac of
      Right m -> do
        setHeader "Content-Type" "audio/midi"
        raw . midiBytes . runReader (midiFromEsac tempo octave) $ m
      Left e -> do
        status badRequest400
        text . TL.pack . show $ e
