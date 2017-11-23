import Test.Hspec
import Test.QuickCheck
import Data.Esac.Parser
import Data.Esac
import Data.Esac.Converter

main :: IO ()
main = hspec $ do
  esacParser

esacParser = describe "ESAC parser" $ do
  describe "parses octave" $ do
    it "higher octave" $ do
      property $ \(NonNegative n) -> parse parseOctave "" (replicate n '+') === (Right n)
    it "lower octave" $ do
      property $ \(NonNegative n) -> parse parseOctave "" (replicate n '-') === (Right (-n))
    it "mixed octave" $ do
      property $ \(NonNegative a) (NonNegative b) ->
        parse parseOctave "" (replicate a '+' ++ replicate b '-') === (Right (a - b))
  describe "parses melody" $ do
    it "correct pitch" $ do
      forAll (listOf $ choose (1, (9 :: Int))) $
        \melodyNotes ->
          let esacMel = (concat (fmap show melodyNotes)) ++ " //"
              (Right melody) = parse parseMelody "" esacMel
          in esacMelody 0 melody === esacMel
