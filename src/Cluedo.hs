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

getPlayerCards :: Monad m => String -> Cluedo m (Maybe [(Card, Status)])
getPlayerCards n = do
    st <- get
    if n == "envelope"
        then
            return $ Just $ cards $ envelope st
        else return $ do
            player <- find ((n ==) . name) (players st)
            return $ cards player
printLogEntry :: LogEntry -> String
printLogEntry (TurnEntry asker cardsAsked replies) =
    asker ++ " \n"
        ++ "    " ++ (intercalate " " $ map show cardsAsked) ++ "\n"
        ++ "    " ++ (intercalate "\n    " $ map printReply replies)
printLogEntry (Accusation suggester cards) =
    "accusation:\t" ++ suggester ++ " \n"
        ++ "    " ++ (intercalate " " $ map show cards)
