module Day17Spec (spec) where

import Day17
import Test.Hspec

import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "legalDirections" $ do
        it "finds that from starting at 'hijkl', can only go down" $ do
            let start = Path "hijkl" "" 0 (0, 0)
            legalDirections start `shouldBe` (Set.fromList "D")
        it "then can go either left or right" $ do
            let next = Path "hijkl" "D" 1 (0, 1)
            legalDirections next `shouldBe` (Set.fromList "RU")
    describe "day17" $ do
        it "finds path 'DDRRRD' for passcode 'ihgpwlah'" $ do
            day17 "ihgpwlah" `shouldBe` "DDRRRD"
        it "finds path 'DDUDRLRRUDRD' for passcode 'kglvqrro'" $ do
            day17 "kglvqrro" `shouldBe` "DDUDRLRRUDRD"
        it "finds path 'DRURDRUDDLLDLUURRDULRLDUUDDDRR' for passcode 'ulqzkmiv'" $ do
            day17 "ulqzkmiv" `shouldBe` "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
        it "finds path 'DUDRLRRDDR' for actual input 'lpvhkcbi'" $ do
            day17 "lpvhkcbi" `shouldBe` "DUDRLRRDDR"
    describe "day17'" $ do
        it "finds path of length 370 for passcode 'ihgpwlah'" $ do
            --day17' "ihgpwlah" `shouldBe` 370
            pendingWith "too slow"
        it "finds path of length 492 for passcode 'kglvqrro'" $ do
            --day17' "kglvqrro" `shouldBe` 492
            pendingWith "too slow"
        it "finds path of length 830 for passcode 'ulqzkmiv'" $ do
            --day17' "ulqzkmiv" `shouldBe` 830
            pendingWith "too slow"
        it "finds path of length 788 for actual input 'lpvhkcbi'" $ do
            --day17' "lpvhkcbi" `shouldBe` 788
            pendingWith "too slow"
