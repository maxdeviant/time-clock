module TimeClock.Internal
  ( HoursWorked(..)
  )
where

type Hour = Int

type FractionalHour = Int

data HoursWorked = HoursWorked Hour FractionalHour deriving (Eq, Show)
