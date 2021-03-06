{-
    Copyright (C) 2018  Marcin Chmiel <marcin.k.chmiel@gmail.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

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
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString as BS
import Numeric
import Codec.Midi
import Network.HTTP.Types.Status
import Network.Wai.Parse
import Debug.Trace
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.Map as Map
import qualified Storage.Db as Db
import Database.MongoDB (Pipe, cast, Document, Value, val)
import Control.Monad.Except
import Text.Read (readMaybe)

defaultParam :: (Parsable a) => TL.Text -> a -> ActionM a
defaultParam name value = rescue (param name) (const $ return value)

readEsac :: ActionM EsacJson
readEsac = do
  shouldParse <- defaultParam "parse" "" :: ActionM String
  case length shouldParse of
    0 -> jsonData :: ActionM EsacJson
    _ -> do
      len <- fmap length files
      when (len == 0) $ do
        status badRequest400
        finish
      e <- fmap (LC.unpack . fileContent . snd . head) files
      case parse parseRawEsac "Invalid raw ESAC" e of
        Left e -> do
          status badRequest400
          text . TL.pack . show $ e
          finish
        Right e -> return e

justOrTerminate :: Maybe a -> ActionM a
justOrTerminate val = case val of
  Nothing -> status badRequest400 >>= return finish
  Just v -> return v

serve :: Int -> String -> IO ()
serve port connstr= do
  pipe <- Db.connect connstr
  server port pipe

server :: Int -> Pipe -> IO ()
server port pipe = scotty port $ do
  getEsacList "/esac/list" pipe
  getEsac "/esac/:id/" pipe
  addEsac "/esac" pipe
  updateEsac "/esac/:id" pipe
  deleteEsac "/esac/:id" pipe
  findEsac "/esac/search" pipe

  post "/esac2midi" $ do
    tempo <- defaultParam "tempo" 90
    octave <- defaultParam "octave" 5
    format <- defaultParam "format" "" :: ActionM String
    esac <- readEsac
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

getEsac url pipe = get url $ do
  id <- param "id"
  liftIO $ putStrLn $ "GET ESAC " ++ id
  let oid = readMaybe id
  oid <- justOrTerminate oid
  esac <- Db.run pipe $ Db.getEsac $ oid
  esac <- justOrTerminate esac
  json esac

getEsacList url pipe = get url $ do
  liftIO . putStrLn $ "GET ESAC list"
  esacs <- Db.run pipe $ Db.getEsacs
  json esacs

addEsac url pipe = put url $ do
  liftIO . putStrLn $ "PUT new ESAC"
  esac <- readEsac
  id <- Db.run pipe $ Db.addEsac esac
  json $ Map.fromList [("id" :: T.Text, TL.pack . show $ id)]

updateEsac url pipe = patch url $ do
  id <- param "id"
  liftIO . putStrLn $ "PATCH ESAC " ++ id
  esac <- readEsac
  let oid = readMaybe id
  oid <- justOrTerminate oid
  Db.run pipe $ Db.updateEsac oid esac
  json ()

deleteEsac url pipe = delete url $ do
  id <- param "id"
  liftIO . putStrLn $ "DELETE ESAC " ++ id
  let oid = readMaybe id
  oid <- justOrTerminate oid
  Db.run pipe $ Db.removeEsac oid
  json ()

findEsac url pipe = post url $ do
  filters <- jsonData :: ActionM [Db.EsacFilter]
  liftIO . putStrLn $ "POST search ESAC with: " ++ show filters
  results <- Db.run pipe $ Db.findEsac filters
  json results

base64midi = mappend "data:audio/midi;base64," . Base64.encode

base64MidiResponse midi = do
  setHeader "Content-Type" "application/json"
  json $ Map.fromList [("midi64" :: String, LC.unpack $ base64midi midi)]

midiFileResponse midi = do
  setHeader "Content-Type" "audio/midi"
  raw midi
