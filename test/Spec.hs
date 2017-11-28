import Test.Hspec
import Test.QuickCheck
import Data.Esac.Parser
import Data.Esac
import Data.Esac.Converter
import Control.Monad.Reader
import Data.Midi.Parser
import Codec.Midi
import Data.Midi
import Data.Either

main :: IO ()
main = hspec $ do
  esacParser
  soundOps
  midiConverter

esacParser = describe "ESAC parser" $ do

  context "octave parsing" $ do

    it "parses higher octave" $ do
      property $ \(NonNegative n) -> parse parseOctave "" (replicate n '+') === (Right n)

    it "parses lower octave" $ do
      property $ \(NonNegative n) -> parse parseOctave "" (replicate n '-') === (Right (-n))

    it "parses mixed octave" $ do
      property $ \(NonNegative a) (NonNegative b) ->
        parse parseOctave "" (replicate a '+' ++ replicate b '-') === (Right (a - b))

  context "melody parsing" $ do

    it "parses correct pitch" $ do
      forAll (listOf $ choose (0, (7 :: Int))) $
        \melodyNotes ->
          let esacMel = (concat (fmap show melodyNotes)) ++ " //"
              melody = case parse parseMelody ("error while parsing: " ++ esacMel) esacMel of
                Left err -> error $ show err
                Right r -> r
          in esacMelody 0 melody === esacMel

    it "doesn't parse incorrect melody" $ do
      forAll (listOf1 $ choose (8, (9 :: Int))) $
        \melodyNotes ->
          let esacMel = (concat (fmap show melodyNotes)) ++ " //"
              melody = parse parseMelody "test err" esacMel
          in melody `shouldSatisfy` isLeft

soundOps = describe "Sound interval" $ do
  it "creates C interval" $ do
    let baseSound = Sound C None
        fullInt = makeFullInterval baseSound
        expectedInt = [Sound C None, Sound C Sharp, Sound D None, Sound D Sharp, Sound E None, Sound F None, Sound F Sharp, Sound G None, Sound G Sharp, Sound A None, Sound A Sharp, Sound B None]
        in fullInt `shouldBe` expectedInt

  it "creates C flat interval" $ do
    let baseSound = Sound C Flat
        fullInt = makeFullInterval baseSound
        expectedInt = [Sound B None, Sound C None, Sound C Sharp, Sound D None, Sound D Sharp, Sound E None, Sound F None, Sound F Sharp, Sound G None, Sound G Sharp, Sound A None, Sound A Sharp]
        in fullInt `shouldBe` expectedInt

  it "creates C sharp interval" $ do
    let baseSound = Sound C Sharp
        fullInt = makeFullInterval baseSound
        expectedInt = [Sound C Sharp, Sound D None, Sound D Sharp, Sound E None, Sound F None, Sound F Sharp, Sound G None, Sound G Sharp, Sound A None, Sound A Sharp, Sound B None, Sound C None]
        in fullInt `shouldBe` expectedInt

midiConverter = describe "ESAC-MIDI converter" $ do
  
  context "MIDI -> ESAC" $ do

    it "MidiNote -> EsacNote" $ do
      let int = makeFullInterval $ Sound C None
      forAll (choose (0, 11 :: Int)) $
        \num ->

          forAll (choose (0, 11 :: Int)) $
          \bsIndex ->

            forAll (choose (0, 10 :: Int)) $
            \octave ->

              let baseSound = int !! bsIndex
                  midiNote = MidiNote (octave * 12 + num) 1
                  esacNote = makeEsacNote octave baseSound midiNote
              in addMidiKey baseSound num === soundFromEsacNote baseSound esacNote

    it "MIDI -> ESAC -> MIDI = identity" $ do
      forAll (listOf1 $ choose (1, (7 :: Int))) $
        \melodyNotes ->
          let melody = (concat (fmap show melodyNotes)) ++ " //"
              key = "10101a 04 C  3/4"

              -- create example ESAC
              (Right exampleEsac) = do
                mel <- parse parseMelody "Invalid melody (MEL)" $ melody
                key <- parse parseKey "Invalid key (KEY)" $ key
                return $ Esac key mel

              -- create example midi
              exampleMidi = runReader (midiFromEsac 90 5) $ exampleEsac

              -- get converted Esac from example midi
              (Right convertedEsac) = esacFromMidiBytes 90 5 $ midiBytes exampleMidi

              -- get Midi from converted esac
              newMidi = runReader (midiFromEsac 90 5) convertedEsac
              
          in tracks exampleMidi === tracks newMidi
