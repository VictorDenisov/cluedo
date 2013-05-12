module Cluedo where

import Prelude hiding (log, catch)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad (forM_, when)

import Data.List (intercalate, find, isPrefixOf, sortBy, groupBy, intersect)

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

addLogEntry :: Monad m => LogEntry -> Cluedo m ()
addLogEntry logEntry = do
    st <- get
    put $ st {log = logEntry : log st}

allNames :: Monad m => Cluedo m [String]
allNames = do
    st <- get
    return $ map name $ players st

setPlayers :: Monad m => [Player] -> Cluedo m ()
setPlayers l = do
    st <- get
    put $ st {players = l}

setOut :: Monad m => Player -> Cluedo m ()
setOut l = do
    st <- get
    put $ st {out = l}

setEnvelope :: Monad m => Player -> Cluedo m ()
setEnvelope l = do
    st <- get
    put $ st {envelope = l}

setPlayerCard :: Monad m => String -> Card -> Cluedo m ()
setPlayerCard n c = do
    st <- get
    setPlayers $ map (setCard n c) (players st)
    setOut $ setCard n c (out st)
    setEnvelope $ setCard n c (envelope st)

clearPlayerCard :: Monad m => String -> Card -> Cluedo m ()
clearPlayerCard n c = do
    st <- get
    setPlayers $ map (clearCard n c) (players st)
    setOut $ clearCard n c (out st)
    setEnvelope $ clearCard n c (envelope st)

retrieveAskedCards :: Monad m => Cluedo m ([(String, [Card])])
retrieveAskedCards = do
    st <- get
    let pairs = map (\(TurnEntry p cs _) -> (p, cs))
                    $ filter isTurnEntry (log st)
    let sortedPairs = sortBy (\x y -> fst x `compare` fst y) pairs
    let uniquePairs = groupBy (\x y -> fst x == fst y) sortedPairs
    return $ (flip map) uniquePairs
                            $ \ls -> (fst $ head ls, concat $ map snd ls)

processLogEntry :: Monad m => LogEntry -> Cluedo m ()
processLogEntry logEntry@(TurnEntry {})  = do
    st <- get
    forM_ (replies logEntry) $ \(Reply name card) ->
        case card of
            EmptyCard -> mapM_ (clearPlayerCard name) (cardsAsked logEntry)
            UnknownCard -> do
                cards <- getPlayerCards name
                case cards of
                    Nothing ->
                        fail $ "error during getting cards of " ++ name
                    Just cs -> do
                        let exceptAbsent
                                = filter
                                    ((No /=) . snd)
                                    $ filter
                                        ((\x -> x `elem` (cardsAsked logEntry))
                                            . fst)
                                        cs
                        if length exceptAbsent == 1
                            then setPlayerCard name (fst $ head exceptAbsent)
                            else return ()

            CardReply c -> setPlayerCard name c
processLogEntry (Accusation _ suggestedCards) = do
    envelope <- getPlayerCards "envelope"
    case envelope of
        Nothing ->
            fail "error during getting cards of envelope"
        Just cs -> do
            let cardsWithStatus = filter
                                    ((\x -> x `elem` (suggestedCards)) . fst)
                                    cs
            let yesCards = filter ((Yes ==) . snd) cardsWithStatus
            let questionCards = filter ((Unknown ==) . snd) cardsWithStatus
            if length yesCards == 2
                then clearPlayerCard "envelope" (fst $ head questionCards)
                else return ()

fixFullList :: Monad m => String -> [(Card, Status)] -> Int -> Cluedo m ()
fixFullList pName cs count = do
    let yesCards = filter ((Yes ==) . snd) cs
    let otherCards = filter ((Yes /=) . snd) cs
    if length yesCards == count
        then mapM_ ((clearPlayerCard pName) . fst) otherCards
        else return ()

fixPlusesList :: Monad m => String -> [(Card, Status)] -> Int -> Cluedo m ()
fixPlusesList pName cs plusCount = do
    let yesCards = filter ((Yes ==) . snd) cs
    let unknownCards = filter ((Unknown ==) . snd) cs
    if length yesCards + length unknownCards == plusCount
        then mapM_ ((setPlayerCard pName) . fst) unknownCards
        else return ()

fixPlayerPluses :: Monad m => Cluedo m ()
fixPlayerPluses = do
    st <- get
    forM_ (players st) $ \p ->
        fixPlusesList
                (name p)
                (cards p)
                ((cardCount - 3) `div` (length $ players st))

fixCategoryPluses :: Monad m => Cluedo m ()
fixCategoryPluses = do
    st <- get
    let env = envelope st
    fixPlusesList (name env) (pieces env) 1
    fixPlusesList (name env) (weapons env) 1
    fixPlusesList (name env) (rooms env) 1

fixNobodyHasCard :: Monad m => Cluedo m ()
fixNobodyHasCard = do
    st <- get
    let allPlayers = (envelope st) : (out st) : (players st)
    forM_ (map (findSinglePlayerWithNonNegativeCardStatus allPlayers) allCards)
          $ \v ->
            case v of
                Nothing -> return ()
                Just (n, c) -> setPlayerCard n c

fixPlayerHasAllCards :: Monad m => Cluedo m ()
fixPlayerHasAllCards = do
    st <- get
    forM_ (players st) $ \p -> do
        fixFullList
                (name p)
                (cards p)
                ((cardCount - 3) `div` (length $ players st))

fixOutHasAllCards :: Monad m => Cluedo m ()
fixOutHasAllCards = do
    st <- get
    let ot = out st
    fixFullList
        "out"
        (cards ot)
        ((cardCount - 3) `rem` (length $ players st))

fixOneCardInCategory :: Monad m => Cluedo m ()
fixOneCardInCategory = do
    st <- get
    let env = envelope st
    fixFullList (name env) (pieces env) 1
    fixFullList (name env) (weapons env) 1
    fixFullList (name env) (rooms env) 1

rectifyTable :: Monad m => Cluedo m ()
rectifyTable = do
    st <- get
    mapM_ processLogEntry (log st)
    fixOutHasAllCards
    fixNobodyHasCard
    fixPlayerHasAllCards
    fixOneCardInCategory
    fixPlayerPluses
    fixCategoryPluses
