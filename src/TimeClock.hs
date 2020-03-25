module TimeClock
  ( TimeClock(..)
  , empty
  , clockIn
  , HoursWorked
  )
where

import Chronos.Types
import qualified Chronos as C
import Data.Map (Map)
import qualified Data.Map as Map

data TimeClock = TimeClock
    { timeWorked :: Map DayOfWeek HoursWorked
    , clockedInAt :: Maybe TimeOfDay
    } deriving (Eq, Show)

empty :: TimeClock
empty = TimeClock { timeWorked = Map.empty, clockedInAt = Nothing }

data ClockInError = AlreadyClockedIn deriving (Eq, Show)

clockIn :: ClockInTime -> TimeClock -> Either ClockInError TimeClock
clockIn time timeClock = case clockedInAt timeClock of
  Nothing -> Right timeClock { clockedInAt = Just time }
  Just _  -> Left AlreadyClockedIn

data ClockOutError = NotClockedIn deriving (Eq, Show)

clockOut
  :: ClockOutTime -> TimeClock -> Either ClockOutError (HoursWorked, TimeClock)
clockOut time timeClock = case clockedInAt timeClock of
  Just clockedInAt ->
    Right (hoursWorked clockedInAt time, timeClock { clockedInAt = Nothing })
  Nothing -> Left NotClockedIn

type ClockInTime = TimeOfDay
type ClockOutTime = TimeOfDay

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

type Hour = Int

type FractionalHour = Int

data HoursWorked = HoursWorked Hour FractionalHour deriving (Eq, Show)

roundToNearestQuarterHour :: Integral a => a -> a
roundToNearestQuarterHour minutes = 15 * ((minutes `div` 15) + direction)
  where direction = if minutes `mod` 15 >= 7 then 1 else 0
