module Cluedo where

import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.State.Strict (StateT(..))

import Data.List (find, intercalate)

import Data.Maybe (fromJust)

import System.Console.Haskeline.Completion(CompletionFunc)

import Cluedo.Model

type Cluedo m = StateT (Table m) m

data Table m = Table
    { players     :: [Player]
    , out         :: Player
    , envelope    :: Player
    , cmdComplete :: CompletionFunc (Cluedo m)
    , log         :: [LogEntry]
    }

getCardStatus :: Card -> Player -> Status
getCardStatus c p = snd $ fromJust $ find ((c ==) . fst) (cards p)
                                -- at least one element is guaranteed.

getPlayerCards :: Monad m => String -> Cluedo m (Maybe [(Card, Status)])
getPlayerCards n = do
    st <- get
    if n == "envelope"
        then
            return $ Just $ cards $ envelope st
        else return $ do
            player <- find ((n ==) . name) (players st)
            return $ cards player

isTurnEntry :: LogEntry -> Bool
isTurnEntry (TurnEntry {}) = True
isTurnEntry _ = False

isAccusation :: LogEntry -> Bool
isAccusation (Accusation {}) = True
isAccusation _ = False

data LogEntry = TurnEntry
                    { asker      :: String
                    , cardsAsked :: [Card]
                    , replies    :: [Reply]
                    }
              | Accusation String [Card]
                deriving (Show)

printLogEntry :: LogEntry -> String
printLogEntry (TurnEntry asker cardsAsked replies) =
    asker ++ " \n"
        ++ "    " ++ (intercalate " " $ map show cardsAsked) ++ "\n"
        ++ "    " ++ (intercalate "\n    " $ map printReply replies)
printLogEntry (Accusation suggester cards) =
    "accusation:\t" ++ suggester ++ " \n"
        ++ "    " ++ (intercalate " " $ map show cards)

data Reply = Reply
                { replier :: String
                , repliedCard :: CardReply
                }
             deriving Show

printReply :: Reply -> String
printReply (Reply name card) = name ++ "\t" ++ (show card)

data CardReply = CardReply Card
               | UnknownCard
               | EmptyCard

instance Show CardReply where
    show (CardReply c) = show c
    show UnknownCard = "UnknownCard"
    show EmptyCard = "EmptyCard"

isCardReply :: CardReply -> Bool
isCardReply (CardReply _) = True
isCardReply _ = False

fromCardReply :: CardReply -> Maybe Card
fromCardReply (CardReply c) = Just c
fromCardReply _ = Nothing

parseCardReply :: String -> Maybe CardReply
parseCardReply "EmptyCard" = Just EmptyCard
parseCardReply "UnknownCard" = Just UnknownCard
parseCardReply s = do
    c <- parseCard s
    return $ CardReply c
