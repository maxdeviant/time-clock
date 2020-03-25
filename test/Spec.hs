import Chronos.Types
import TimeClock as TimeClock
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "TimeClock.clockIn" $ do
    it ("clocks in at the specified time") $ do
      let
        expected =
          Right TimeClock.empty { clockedInAt = Just $ TimeOfDay 8 0 0 }
      TimeClock.clockIn (TimeOfDay 8 0 0) TimeClock.empty `shouldBe` expected

