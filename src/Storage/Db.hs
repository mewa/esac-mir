{-# LANGUAGE OverloadedStrings #-}
module Storage.Db
where

import Data.Esac
import qualified Data.Map.Strict as Map
import Database.MongoDB
import Data.Bson
import qualified Data.Aeson as Aeson
import qualified Data.Text as TL
import Data.Char

run pipe action = access pipe master "esacdb" action

collection_esacs = "esacs"

field_melody = "melody"
field_melodyRaw = "melody_raw"
field_melodyRhythm = "melody_rhythm"

esacBson :: EsacJson -> Document
esacBson esac = let
  toMap = Aeson.fromJSON . Aeson.toJSON :: EsacJson -> Aeson.Result (Map.Map TL.Text TL.Text)
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

getEsacs :: Action IO [Document]
getEsacs = find (select [] collection_esacs) >>= rest

getEsac :: TL.Text -> Action IO (Maybe Document)
getEsac id = findOne (select ["_id" =: id] collection_esacs)

addEsac :: EsacJson -> Action IO Value
addEsac esac = insert collection_esacs $ computedEsacBson esac

removeEsac :: TL.Text -> Action IO ()
removeEsac id = delete $ select ["_id" =: id] collection_esacs

updateEsac :: TL.Text -> EsacJson -> Action IO ()
updateEsac id esac = modify (select ["_id" =: id] collection_esacs) $ computedEsacBson esac

findMelody :: TL.Text -> Action IO [Document]
findMelody pattern = rest =<< find (select [field_melody =: Regex pattern ""] collection_esacs)

findMelodyRaw :: TL.Text -> Action IO [Document]
findMelodyRaw pattern = rest =<< find (select [field_melodyRaw =: Regex pattern ""] collection_esacs)

findMelodyRhythm :: TL.Text -> Action IO [Document]
findMelodyRhythm pattern = rest =<< find (select [field_melodyRhythm =: Regex pattern "*m*"] collection_esacs)
