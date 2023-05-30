module FSM (Guard, State(..), Action, Event, Row(..), Table) where

import Data.List

data State = Initial | Final | State String
type Guard = String
type Action = String
type Event = String

data Row = Row State (Maybe Event) [Guard] (Maybe Action) State

instance Show State where
    showsPrec d Initial = showsPrec d "*"
    showsPrec d Final = showsPrec d "(*)"
    showsPrec d (State name) = showsPrec d name

instance Eq State where
    (==) Initial Initial = True
    (==) Final Final = True
    (==) (State l) (State r) = l == r

instance Show Row where
    show (Row source event guards action dest) = "Row " ++ showState source
                                                 ++ maybe "" (\e -> " + " ++ e) event
                                                 ++ showGuards guards
                                                 ++ maybe "" (\a -> " / " ++ a) action
                                                 ++ " = " ++ showState dest
        where
          showGuards [] = ""
          showGuards guards = " " ++ show guards ++ " "

          showState Initial = "*"
          showState Final = "(*)"
          showState (State name) = name

    showList table = (intercalate "\n" (map show table) ++)

type Table = [Row]
