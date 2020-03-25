module TimeClock.Types
  ( TimeClock(..)
  , HoursWorked
  , ClockInTime
  , ClockOutTime
  , ClockInError(..)
  , ClockOutError(..)
  )
where

import TimeClock.Internal (HoursWorked)
import Chronos.Types
import Data.Map (Map)

data TimeClock = TimeClock
  { timeWorked :: Map DayOfWeek HoursWorked
  , clockedInAt :: Maybe ClockInTime
  } deriving (Eq, Show)

type ClockInTime = TimeOfDay

type ClockOutTime = TimeOfDay

data ClockInError = AlreadyClockedIn deriving (Eq, Show)

data ClockOutError = NotClockedIn deriving (Eq, Show)
