import Chronos.Types
import TimeClock.Types
import TimeClock.Internal (HoursWorked(..))
import qualified TimeClock as TimeClock
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "TimeClock.clockIn" $ do
    it ("clocks in at the specified time") $ do
      let
        expected =
          Right TimeClock.empty { clockedInAt = Just $ TimeOfDay 8 0 0 }
      TimeClock.clockIn (TimeOfDay 8 0 0) TimeClock.empty `shouldBe` expected

  describe "TimeClock.clockOut" $ do
    it ("clocks out at the specified time") $ do
      let
        timeClock = TimeClock.empty { clockedInAt = Just $ TimeOfDay 9 0 0 }
      let expected = Right (HoursWorked 8 0, TimeClock.empty)
      TimeClock.clockOut (TimeOfDay 17 0 0) timeClock `shouldBe` expected
