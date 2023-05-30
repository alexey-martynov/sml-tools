module SML (parseFile, parseStr) where

import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String
import qualified FSM(Guard, State(..), Action, Event, Row(..), Table)

deduplicateInitialState :: FSM.Table -> FSM.Table
deduplicateInitialState table = reverse $ snd $ foldl' dedup ([], []) table
    where
      dedup (states, fsm) row@(FSM.Row FSM.Initial Nothing [] Nothing s) =
          case find (s ==) states of
            Just _ -> (states, fsm)
            Nothing -> (s:states, row:fsm)
      dedup (states, fsm) row = (states, row:fsm)

parseFile :: String -> IO (Either ParseError FSM.Table)
parseFile file = do
  parsed <- parseFromFile table file
  return $ case parsed of
             Left error -> Left error
             Right table -> Right $ deduplicateInitialState table

parseStr :: String -> Either ParseError FSM.Table
parseStr str = case parse table "" str of
                 Left error -> Left error
                 Right table -> Right $ deduplicateInitialState table

cxxComment :: Parsec String () String
cxxComment = string "//" >> (manyTill anyChar (try endOfLine))
cxxIdentifier = many $ satisfy (\c -> isAlphaNum c || c == '_') :: Parsec String () String
cxxQualifiedIdentifier = sepBy1 (cxxIdentifier >>= \i -> spaces >> return i) (string "::") >>= return . (intercalate "::") :: Parsec String () String
cxxNegatedIdentifier = choice [char '!' >> cxxQualifiedIdentifier >>= (\i -> return $ "!" ++ i) :: Parsec String () String,
                               cxxQualifiedIdentifier]

spacesOrComment = spaces >> (optional cxxComment) >> spaces

smlNamespace = optional $ string "boost::sml::"
smlIdentifier str = string str :: Parsec String () String
smlTemplate str = (smlIdentifier str) >> spaces >> (between (char '<' >> spaces) (spaces >> char '>' >> return "")) cxxQualifiedIdentifier :: Parsec String () String
smlOnEntry = (string "ntry")
            >> spaces
            >> (between (char '<' >> spaces) (spaces >> char '>' >> return "") cxxQualifiedIdentifier)
            >>= \e -> return (if e == "_" then "on_entry" else "on_entry(" ++ e ++ ")") :: Parsec String () String
smlOnExit = (string "xit")
            >> spaces
            >> (between (char '<' >> spaces) (spaces >> char '>' >> return "") cxxQualifiedIdentifier)
            >>= \e -> return (if e == "_" then "on_exit" else "on_exit(" ++ e ++ ")") :: Parsec String () String

stateType = many $ satisfy (\c -> isAlphaNum c || c == '_' || c == ':') :: Parsec String () String
stateName = (char '\"') >> (many $ satisfy (\c -> isAlphaNum c || c == '_')) >>= \x -> (string "\"_s" >> return x) :: Parsec String () String
stateId = choice [stateName, stateType]

state = choice [char '*' >> stateId >>= \s -> return [FSM.Initial, FSM.State s],
                stateId >>= \s -> return $ if s == "X" || s == "boost::sml::X" then [FSM.Final] else [FSM.State s]
               ] :: Parsec String () [FSM.State]

event = char '+' >> spaces >> smlNamespace >> choice [smlTemplate "event",
                                                      string "on_e" >> choice [smlOnEntry, smlOnExit]
                                                     ]

guards = between (char '[' >> spaces) (char ']') (sepBy1 (cxxNegatedIdentifier >>= \i -> spaces >> return i) (char ',' >> spaces)) :: Parsec String () [FSM.Guard]

action = char '/' >> spaces >> (optional $ char '&' >> spaces) >> cxxQualifiedIdentifier :: Parsec String () FSM.Action

destinationState = char '=' >> spaces >> state >>= \s -> return $ head s :: Parsec String () FSM.State

row = do
  src <- state
  spaces
  ev <- optionMaybe event
  spaces
  gs <- option [] guards
  spaces
  act <- optionMaybe action
  spaces
  dst <- optionMaybe destinationState

  return (createRows src ev gs act dst)
  where
    createRows [s] ev gs act dst = [FSM.Row s ev gs act (maybe s id dst)]
    createRows [FSM.Initial, s] ev gs act dst = [FSM.Row FSM.Initial Nothing [] Nothing s,
                                                 FSM.Row s ev gs act (maybe s id dst)
                                                ]

tableStart = (optional smlNamespace)
             >> spacesOrComment
             >> (smlIdentifier "make_transition_table")
             >> spacesOrComment
             >> char '('

tableContent = spacesOrComment >> sepBy1 (row >>= (\r -> spacesOrComment >> return r)) (char ',' >> spacesOrComment) >>= return . concat :: Parsec String () FSM.Table

table :: Parsec String () FSM.Table
table = (manyTill anyChar (try tableStart))
        >> tableContent
        >>= \t -> spacesOrComment
                 >> char ')'
                 >> spacesOrComment
                 >> char ';'
                 >> return t
