{-# LANGUAGE OverloadedStrings #-}
module Server
  (
    serve
  ) where

import Esac
import Data.Either
import Control.Monad.IO.Class
import Web.Scotty
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as L

serve port = scotty port $ do
  post "/" $ do
    esac <- fmap L.unpack body :: ActionM String
    liftIO $ print esac
    case esacToMidi esac of
      Right m -> do
        setHeader "Content-Type" "audio/midi"
        raw . midiBytes $ m
      Left e ->
        text . TL.pack . show $ e
