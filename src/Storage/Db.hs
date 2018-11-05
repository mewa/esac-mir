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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Storage.Db
where

import Data.Esac
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map.Strict as Map
import Database.MongoDB
import Data.Bson
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Char
import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import GHC.Generics

data EsacFilter = EsacFilter {
  field :: T.Text
  , term :: T.Text
  } deriving (Generic, Show)

instance FromJSON EsacFilter

run pipe action = access pipe master "esacdb" action

connect = Database.MongoDB.connect . host

collection_esacs = "esacs"

field_melody = "melody"
field_melodyRaw = "melody_raw"
field_melodyRhythm = "melody_rhythm"

esacBson :: EsacJson -> Document
esacBson esac = let
  toMap = Aeson.fromJSON . Aeson.toJSON :: EsacJson -> Aeson.Result (Map.Map T.Text T.Text)
  toDocument k v l = (k =: v) : l
  (Aeson.Success result) = Map.foldrWithKey toDocument [] <$> toMap esac
  in result

computedEsacBson :: EsacJson -> Document
computedEsacBson esac = let
  isPitchMod c = c == '-' || c == '+' || c == '#' || c == 'b'
  isNote c = isDigit c || isPitchMod c
  eraseNote c = if isDigit c
    then 'x'
    else c
  melody_raw = filter isNote . melody $ esac
  melody_rhythm = filter (not . isPitchMod) . fmap eraseNote . melody $ esac
  in [field_melodyRaw =: melody_raw, field_melodyRhythm =: melody_rhythm] ++ esacBson esac

type EsacJsonId = Aeson.Value

esacJson :: Document -> EsacJsonId
esacJson doc = let
  esac = defaultEsacJson {
    name = at "name" doc
    , title = at "title" doc
    , source = at "source" doc
    , region = at "region" doc
    , signature = at "signature" doc
    , key = at "key" doc
    , melody = at "melody" doc
    , remarks = at "remarks" doc
    }
  id = at "_id" doc :: ObjectId
  (Aeson.Object json) = Aeson.toJSON esac
  in Aeson.Object $ HashMap.insert "id" (Aeson.String . T.pack $ show id) json

jsonAction d = fmap (fmap esacJson) d

getEsacs :: (MonadIO m) => Action m [EsacJsonId]
getEsacs = jsonAction $ find (select [] collection_esacs) >>= rest

getEsac :: (MonadIO m) => ObjectId -> Action m (Maybe EsacJsonId)
getEsac id = jsonAction $ findOne (select ["_id" =: id] collection_esacs)

addEsac :: (MonadIO m) => EsacJson -> Action m Value
addEsac esac = insert collection_esacs $ computedEsacBson esac

removeEsac :: (MonadIO m) => ObjectId -> Action m ()
removeEsac id = delete $ select ["_id" =: id] collection_esacs

updateEsac :: (MonadIO m) => ObjectId -> EsacJson -> Action m ()
updateEsac id esac = replace (select ["_id" =: id] collection_esacs) $ computedEsacBson esac

findEsac :: (MonadIO m) => [EsacFilter] -> Action m [EsacJsonId]
findEsac filters = jsonAction $ find (select (foldl makeFilter [] filters) collection_esacs) >>= rest

makeFilter :: Document -> EsacFilter -> Document
makeFilter acc f = let
  label = field f
  regex = Regex (escape $ term f) "im"
  in (label =: regex) : acc

escape s = T.concatMap escapeChar s

escapeChar c = case c of
  '\\' -> T.pack $ '\\' : c : []
  '.' -> T.pack $ '\\' : c : []
  '^' -> T.pack $ '\\' : c : []
  '$' -> T.pack $ '\\' : c : []
  '+' -> T.pack $ '\\' : c : []
  _ -> T.pack $ c : []
