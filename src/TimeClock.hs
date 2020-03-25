module TimeClock
  ( empty
  , clockIn
  , clockOut
  )
where

import TimeClock.Types
import TimeClock.Internal (HoursWorked(..))
import Chronos.Types
import qualified Chronos as C
import Data.Map (Map)
import qualified Data.Map as Map

empty :: TimeClock
empty = TimeClock { timeWorked = Map.empty, clockedInAt = Nothing }

clockIn :: ClockInTime -> TimeClock -> Either ClockInError TimeClock
clockIn time timeClock = case clockedInAt timeClock of
  Nothing -> Right timeClock { clockedInAt = Just time }
  Just _  -> Left AlreadyClockedIn

clockOut
  :: ClockOutTime -> TimeClock -> Either ClockOutError (HoursWorked, TimeClock)
clockOut time timeClock = case clockedInAt timeClock of
  Just clockedInAt ->
    Right (hoursWorked clockedInAt time, timeClock { clockedInAt = Nothing })
  Nothing -> Left NotClockedIn

hoursWorked :: ClockInTime -> ClockOutTime -> HoursWorked
hoursWorked clockedInAt clockedOutAt =
  let
    deltaHours = C.timeOfDayHour clockedOutAt - C.timeOfDayHour clockedInAt
    deltaMinutes =
      C.timeOfDayMinute clockedOutAt - C.timeOfDayMinute clockedOutAt

    totalMinutes = deltaHours * 60 + deltaMinutes
  in HoursWorked
    (totalMinutes `quot` 60)
    (roundToNearestQuarterHour $ totalMinutes `rem` 60)

roundToNearestQuarterHour :: Integral a => a -> a
roundToNearestQuarterHour minutes = 15 * ((minutes `div` 15) + direction)
  where direction = if minutes `mod` 15 >= 7 then 1 else 0
