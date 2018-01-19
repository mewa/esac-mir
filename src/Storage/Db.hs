{-# LANGUAGE OverloadedStrings #-}
module Storage.Db
where

import Data.Esac
import qualified Data.Map.Strict as Map
import Database.MongoDB
import Data.Bson
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Char
import Control.Monad.IO.Class

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
  isPitchMod c = c == '#' || c == 'b'
  isNote c = isDigit c || isPitchMod c
  eraseNote c = if isDigit c
    then 'x'
    else c
  melody_raw = filter isNote . melody $ esac
  melody_rhythm = filter (not . isPitchMod) . fmap eraseNote . melody $ esac
  in [field_melodyRaw =: melody_raw, field_melodyRhythm =: melody_rhythm] ++ esacBson esac

esacJson :: Document -> EsacJson
esacJson doc = defaultEsacJson {
  name = at "name" doc
  , title = at "title" doc
  , source = at "source" doc
  , region = at "region" doc
  , signature = at "signature" doc
  , key = at "key" doc
  , melody = at "melody" doc
  , remarks = at "remarks" doc
  }

jsonAction d = fmap (fmap esacJson) d

getEsacs :: (MonadIO m) => Action m [EsacJson]
getEsacs = jsonAction $ find (select [] collection_esacs) >>= rest

getEsac :: (MonadIO m) => ObjectId -> Action m (Maybe EsacJson)
getEsac id = jsonAction $ findOne (select ["_id" =: id] collection_esacs)

addEsac :: (MonadIO m) => EsacJson -> Action m Value
addEsac esac = insert collection_esacs $ computedEsacBson esac

removeEsac :: (MonadIO m) => ObjectId -> Action m ()
removeEsac id = delete $ select ["_id" =: id] collection_esacs

updateEsac :: (MonadIO m) => ObjectId -> EsacJson -> Action m ()
updateEsac id esac = modify (select ["_id" =: id] collection_esacs) $ computedEsacBson esac

findMelody :: (MonadIO m) => T.Text -> Action m [Document]
findMelody pattern = rest =<< find (select [field_melody =: Regex pattern ""] collection_esacs)

findMelodyRaw :: (MonadIO m) => T.Text -> Action m [Document]
findMelodyRaw pattern = rest =<< find (select [field_melodyRaw =: Regex pattern ""] collection_esacs)

findMelodyRhythm :: (MonadIO m) => T.Text -> Action m [Document]
findMelodyRhythm pattern = rest =<< find (select [field_melodyRhythm =: Regex pattern "*m*"] collection_esacs)
