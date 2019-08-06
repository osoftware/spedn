import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import qualified Test.Tasty
import           Test.Tasty.Hspec

import           ContractProps
import           EncodingProps

main :: IO ()
main = do
    test <- testSpec "spedn" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ modifyMaxSuccess (*10) $ do
    describe "Contract" $ do
        it "typechecks" $ property prop_typechecks
        it "leaves a clean stack" $ property prop_clean_stack
        it "does not emit invalid opcodes" $ property prop_no_invalid_opcodes
    describe "Minimal encoding" $
        it "uses most significant bit as sign" $ property prop_sign_bit
