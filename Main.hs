import Prelude hiding (log, catch)
import Control.Applicative ((<$>))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_, when)
import Control.Arrow (first)

import Data.List (intercalate, find, isPrefixOf, sortBy, groupBy)
import Data.Maybe (isJust, fromJust)

import System.Console.Haskeline ( InputT
                                , Completion(..)
                                , runInputT
                                , Settings(..)
                                , getInputLine)
import System.Console.Haskeline.History (historyLines)
import System.Console.Haskeline.Completion(CompletionFunc)
import System.Console.Haskeline.MonadException( catch
                                              , MonadException
                                              , Exception
                                              , IOException
                                              , throwIO)
import System.Exit (exitSuccess)

buildCompletions = map (\name -> Completion name name True)

allKnownCards = allPieces ++ allRooms ++ allWeapons
allCards = EmptyCard : UnknownCard : allKnownCards

allKnownCardsStrings = map show allKnownCards
allCardsStrings = map show allCards

cardCount = length allKnownCards

commandList = ["rectify", "setcard", "turn", "print", "accusate"]

printCommandList = ["log", "table"]

emptyCompleter :: MonadIO m => CompletionFunc (Cluedo m)
emptyCompleter (leftLine, _) = return (leftLine, [])

cardCompleter :: MonadIO m => CompletionFunc (Cluedo m)
cardCompleter (leftLine, _) = do
    let line = reverse leftLine
    let ws = words line
    if length ws == 0
        then return (leftLine, buildCompletions allKnownCardsStrings)
        else case head leftLine of
                ' ' -> return (leftLine, buildCompletions allKnownCardsStrings)
                _ -> return $ listCompleter leftLine (last ws) allKnownCardsStrings

listCompleter :: String -> String -> [String] -> (String, [Completion])
listCompleter leftLine card list =
    ( drop (length card) leftLine
    , buildCompletions $ filter (card `isPrefixOf`) list)

completeCommand :: MonadIO m => CompletionFunc (Cluedo m)
completeCommand (leftLine, _) = do
    let line = reverse leftLine
    let ws = words line
    let cmdName = head ws
    names <- allNames
    case cmdName of
        "turn" -> do
            if head leftLine == ' '
                then if length ws >= 2
                        then return (leftLine, [])
                        else do
                            return (leftLine, buildCompletions names)
                else return $ listCompleter leftLine (last ws) names
        "print" -> case (head leftLine, length ws) of
            (' ', 1) -> return (leftLine, buildCompletions printCommandList)
            (' ', 2) -> return (leftLine, [])
            (_, 2) -> return $ listCompleter leftLine (last ws) printCommandList
            (_, _) -> return (leftLine, [])
        "setcard" -> case (head leftLine, length ws) of
            (' ', 1) -> return (leftLine, buildCompletions ("envelope" : names))

            (' ', 2) -> do
                let playerName = ws !! 1
                cards <- getPlayerCards playerName
                case cards of
                    Nothing -> return (leftLine, []) -- list of cards is nothing if player name is unknown
                    Just cs -> do
                        let unknowns = map (show . fst)
                                                $ filter ((Unknown ==) . snd) cs
                        return (leftLine, buildCompletions unknowns)

            (_, 2) -> return $ listCompleter
                                        leftLine
                                        (last ws)
                                        ("envelope" : names)
            (' ', 3) -> return (leftLine, [])

            (_, 3) -> do
                let playerName = ws !! 1
                cards <- getPlayerCards playerName -- list of cards is nothing if player name is unknown
                case cards of
                    Nothing -> return (leftLine, [])
                    Just cs -> do
                        let unknowns = map (show . fst) $ filter ((Unknown ==) . snd) cs
                        return $ listCompleter leftLine (last ws) unknowns


            (_, _) -> return (leftLine, [])
        "accusate" -> case (head leftLine, length ws) of
            (' ', 1) -> return (leftLine, buildCompletions names)
            (' ', 2) -> return (leftLine, buildCompletions allKnownCardsStrings)
            (_, 2) -> return $ listCompleter leftLine (last ws) names
            (' ', 3) -> return (leftLine, buildCompletions allKnownCardsStrings)
            (_, 3) -> return $ listCompleter leftLine (last ws) allKnownCardsStrings
            (' ', 4) -> return (leftLine, buildCompletions allKnownCardsStrings)
            (_, 4) -> return $ listCompleter leftLine (last ws) allKnownCardsStrings
            (' ', 5) -> return (leftLine, [])
            (_, 5) -> return $ listCompleter leftLine (last ws) allKnownCardsStrings
            (_, _) -> return (leftLine, [])
        _ -> return (leftLine, [])

basicCommandLineComplete :: MonadIO m => CompletionFunc (Cluedo m)
basicCommandLineComplete a@(leftLine, _) = do
    let line = reverse leftLine
    let ws = words line
    case length ws of
        0 -> return (leftLine, buildCompletions commandList)
        1 -> if head leftLine == ' '
                then completeCommand a
                else return ("", buildCompletions $ filter ((last ws) `isPrefixOf`) commandList)
        v -> completeCommand a

commandLineComplete :: (MonadIO m, Functor m) => CompletionFunc (Cluedo m)
commandLineComplete arg = do
    complete <- cmdComplete <$> get
    complete arg

cmdPrompt :: String -> String
cmdPrompt "" = "(cluedo) "
cmdPrompt s = "(cluedo) " ++ s ++ " # "

allPieces = [Green, Mustard, Peacock, Plum, Scarlett, White]

allWeapons = [Wrench, Candle, Knife, Revolver, Pipe, Rope]

allRooms = [ Bathroom, Study, Dining, Billiard, Garage
           , Bedroom, Guestroom, Kitchen, Yard]

data Card = Scarlett
          | Mustard
          | White
          | Green
          | Peacock
          | Plum

          | Candle
          | Knife
          | Pipe
          | Revolver
          | Rope
          | Wrench

          | Kitchen
          | Billiard
          | Library
          | Dining
          | Bathroom
          | Study
          | Garage
          | Bedroom
          | Yard
          | Guestroom

          | UnknownCard
          | EmptyCard
            deriving (Eq, Show)

isPieceCard :: Card -> Bool
isPieceCard c = c `elem` allPieces

isWeaponCard :: Card -> Bool
isWeaponCard c = c `elem` allWeapons

isRoomCard :: Card -> Bool
isRoomCard c = c `elem` allRooms

parseCard :: String -> Maybe Card
parseCard s = s `lookup` ((map show allCards) `zip` allCards)

data Reply = Reply
                { replier :: String
                , repliedCard :: Card
                }
             deriving Show

data Status = Yes
            | No
            | Unknown
              deriving (Eq)

instance Show Status where
    show Yes      = "+"
    show No       = "-"
    show Unknown  = "?"

data Player = Player
                { name    :: String
                , cards   :: [(Card, Status)]
                }

pieces :: Player -> [(Card, Status)]
pieces p = filter (\(c,_) -> c `elem` allPieces) (cards p)

weapons :: Player -> [(Card, Status)]
weapons p = filter (\(c,_) -> c `elem` allWeapons) (cards p)

rooms :: Player -> [(Card, Status)]
rooms p = filter (\(c,_) -> c `elem` allRooms) (cards p)

fullPlayer :: String -> Player
fullPlayer name = Player name (allKnownCards `zip` (repeat Unknown))

emptyPlayer :: String -> Player
emptyPlayer name = Player name (allKnownCards `zip` (repeat No))

getCard :: Card -> Player -> Status
getCard EmptyCard _ = Unknown
getCard UnknownCard _ = Unknown
getCard c p = snd $ fromJust $ find ((c ==) . fst) (cards p)
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

data LogEntry = TurnEntry
                    { asker      :: String
                    , cardsAsked :: [Card]
                    , replies    :: [Reply]
                    }
              | Accusation String [Card]
                deriving (Show)

isTurnEntry :: LogEntry -> Bool
isTurnEntry (TurnEntry {}) = True
isTurnEntry _ = False

isAccusation :: LogEntry -> Bool
isAccusation (Accusation {}) = True
isAccusation _ = False

data Table m = Table
    { players     :: [Player]
    , out         :: Player
    , envelope    :: Player
    , cmdComplete :: CompletionFunc (Cluedo m)
    , log         :: [LogEntry]
    }

type Cluedo m = StateT (Table m) m

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

setCard :: String -> Card -> Player -> Player
setCard n c pl | n == name pl = pl {cards = map (setCardElem c) (cards pl)}
setCard n c pl = pl {cards = map (clearCardElem c) (cards pl)}

setCardElem :: Card -> (Card, Status) -> (Card, Status)
setCardElem p (pv, st) | p == pv = (pv, Yes)
setCardElem p (pv, st) = (pv, st)

clearPlayerCard :: Monad m => String -> Card -> Cluedo m ()
clearPlayerCard n c = do
    st <- get
    setPlayers $ map (clearCard n c) (players st)
    setOut $ clearCard n c (out st)
    setEnvelope $ clearCard n c (envelope st)

clearCard :: String -> Card -> Player -> Player
clearCard n c pl | n == name pl = pl {cards = map (clearCardElem c) (cards pl)}
clearCard n c pl = pl

clearCardElem :: Card -> (Card, Status) -> (Card, Status)
clearCardElem p (pv, st) | p == pv = (pv, No)
clearCardElem p (pv, st) = (pv, st)

retrieveAskedCards :: Monad m => Cluedo m ([(String, [Card])])
retrieveAskedCards = do
    st <- get
    let pairs = map (\(TurnEntry p cs _) -> (p, cs)) $ filter isTurnEntry (log st)
    let sortedPairs = sortBy (\x y -> fst x `compare` fst y) pairs
    let uniquePairs = groupBy (\x y -> fst x == fst y) sortedPairs
    return $ (flip map) uniquePairs $ \ls -> (fst $ head ls, concat $ map snd ls)

playerAskedCard :: [(String, [Card])] -> String -> Card -> Bool
playerAskedCard ls pn c = case lookup pn ls of
    Nothing -> False
    Just cs -> c `elem` cs

printTable :: MonadIO m => Cluedo m ()
printTable = do
    st <- get
    let allPlayers = ((envelope st) : (players st)) ++ [(out st)]
    ls <- retrieveAskedCards
    let isAskedCard p c = if playerAskedCard ls p c
                            then "*"
                            else " "
    let printer getter card = intercalate "\t" $
                                        map (\p -> show (getter card p)
                                                   ++ "  "
                                                   ++ (isAskedCard (name p) card))
                                            allPlayers

    let cardPrinter card =
            liftIO $ putStrLn $ (show card) ++ ":\t"
                                        ++ (printer getCard card)

    liftIO $ putStrLn $ "\t" ++ (intercalate "\t" $ map name allPlayers)
    mapM_ cardPrinter allPieces
    liftIO $ putStrLn ""
    mapM_ cardPrinter allWeapons
    liftIO $ putStrLn ""
    mapM_ cardPrinter allRooms

main = evalStateT (runInputT
                        (Settings (commandLineComplete) Nothing True)
                        (initialSetup >> mainLoop))
                    (Table { players     = []
                           , out         = emptyPlayer "out"
                           , envelope    = fullPlayer "envelope"
                           , cmdComplete = basicCommandLineComplete
                           , log         = []
                           })

askPlayerNames :: InputT (Cluedo IO) ()
askPlayerNames = do
    liftIO $ putStrLn $ "Please enter players names"
    l <- getInputLine $ cmdPrompt ""
    case l of
        Nothing -> liftIO exitSuccess
        Just "" -> askPlayerNames
        Just v ->  lift $ setPlayers $ (emptyPlayer "me") : (map fullPlayer (words v))

askCards :: String -> ([Card] -> Bool) -> InputT (Cluedo IO) [Card]
askCards prompt cardsOk = withCompleter cardCompleter $ do
    l <- getInputLine prompt
    case l of
        Nothing -> fail "askCards aborted"
        Just "" -> do
            liftIO $ putStrLn "No cards entered. Asking again."
            again
        Just v ->  do
            let cardNames = words v
            let cards = map parseCard cardNames
            if Nothing `elem` cards
                then do
                    liftIO $ putStrLn "Couldn't parse some cards. Asking again."
                    again
                else
                    let justCards = map fromJust cards in
                    if cardsOk justCards
                        then return justCards
                        else do
                            liftIO $ putStrLn $
                                "Cards do not satisfy correctness "
                                ++ "condition. Asking again."
                            again
    where
            again = askCards prompt cardsOk

askMyCards :: InputT (Cluedo IO) ()
askMyCards = do
    playerCount <- lift $ length <$> players <$> get
    let cardNumber = ((cardCount - 3) `div` playerCount)
    liftIO $ putStrLn $ "Please enter your cards (" ++ (show cardNumber) ++ ")"
    cards <- askCards
                (cmdPrompt "")
                $ \cs -> length cs == cardNumber
    lift $ mapM_ (setPlayerCard "me") cards

askOutCards :: InputT (Cluedo IO) ()
askOutCards = do
    playerCount <- lift $ length <$> players <$> get
    let cardNumber = ((cardCount - 3) `rem` playerCount)
    if cardNumber == 0
        then liftIO $ putStrLn "Out is empty. No cards to be entered."
        else do
            liftIO $ putStrLn $ "Please enter out (" ++ (show cardNumber) ++ ")"
            cards <- askCards
                        (cmdPrompt "")
                        $ \cs -> length cs == cardNumber
            lift $ mapM_ (setPlayerCard "out") cards

initialSetup :: InputT (Cluedo IO) ()
initialSetup = do
    liftIO $ putStrLn "Cluedo board game assistant version 1.0\n"
    withCompleter emptyCompleter askPlayerNames
    askMyCards
    askOutCards
    lift printTable

replyComplete :: (MonadIO m, Functor m) => CompletionFunc (Cluedo m)
replyComplete (leftLine, _) = do
    let line = reverse leftLine
    let ws = words line
    playerNames <- map name <$> players <$> get
    case (head leftLine, length ws) of
        (_, 0) -> return ("", buildCompletions playerNames)
        (' ', 1) -> return (leftLine, buildCompletions allCardsStrings)
        (_, 1) -> return (drop (length $ last ws) leftLine, buildCompletions $ filter (line `isPrefixOf`) playerNames)
        (' ', 2) -> return (leftLine, [])
        (_, 2) -> return (drop (length $ last ws) leftLine, buildCompletions $ filter (last ws `isPrefixOf`) allCardsStrings)
        (_, _) -> return (leftLine, [])

askReply :: String -> InputT (Cluedo IO) Reply
askReply cmdPrompt = withCompleter replyComplete $ do
    liftIO $ putStrLn "Enter reply"
    playerNames <- lift $ map name <$> players <$> get
    l <- getInputLine cmdPrompt
    case l of
        Nothing -> fail "askReply aborted"
        Just v -> do
            let ws = words v
            let card = parseCard $ last ws
            case (head ws `elem` playerNames, card) of
                (True, Just c) -> return $ Reply (head ws) c
                _ -> do
                    liftIO $ putStrLn "Incorrect input. Asking again."
                    askReply cmdPrompt

withCompleter :: CompletionFunc (Cluedo IO)
              -> InputT (Cluedo IO) a
              -> InputT (Cluedo IO) a
withCompleter c blc = do
    st <- lift get
    let prevC = cmdComplete st
    lift $ put $ st {cmdComplete = c}
    a <- blc
    st <- lift get
    lift $ put $ st {cmdComplete = prevC}
    return a

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
                        let exceptAbsent = filter ((No /=) . snd) $ filter ((\x -> x `elem` (cardsAsked logEntry)) . fst) cs
                        if length exceptAbsent == 1
                            then setPlayerCard name (fst $ head exceptAbsent)
                            else return ()

            c -> setPlayerCard name c
processLogEntry (Accusation _ suggestedCards) = do
    envelope <- getPlayerCards "envelope"
    case envelope of
        Nothing ->
            fail "error during getting cards of envelope"
        Just cs -> do
            let cardsWithStatus = filter ((\x -> x `elem` (suggestedCards)) . fst) cs
            let yesCards = filter ((Yes ==) . snd) cardsWithStatus
            let questionCards = filter ((Unknown ==) . snd) cardsWithStatus
            if length yesCards == 2
                then clearPlayerCard "envelope" (fst $ head questionCards)
                else return ()

lookThrough :: [Player] -> Card -> Maybe (String, Card)
lookThrough ps c =
        let statuses = map (getCard c) ps
            noCount = length $ filter (No ==) statuses in
        if noCount == (pc - 1)
            then let p = fromJust $ find ((No /=) . (getCard c)) ps
                     in Just (name p, c)
            else Nothing

    where pc = length ps

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
        fixPlusesList (name p) (cards p) ((cardCount - 3) `div` (length $ players st))

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
    forM_ (map (lookThrough allPlayers) allKnownCards) $ \v ->
        case v of
            Nothing -> return ()
            Just (n, c) -> setPlayerCard n c

fixPlayerHasAllCards :: Monad m => Cluedo m ()
fixPlayerHasAllCards = do
    st <- get
    forM_ (players st) $ \p -> do
        fixFullList (name p) (cards p) ((cardCount - 3) `div` (length $ players st))

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
    fixNobodyHasCard
    fixPlayerHasAllCards
    fixOneCardInCategory
    fixPlayerPluses
    fixCategoryPluses

addLogEntry :: Monad m => LogEntry -> Cluedo m ()
addLogEntry logEntry = do
    st <- get
    put $ st {log = logEntry : log st}

enterTurn :: String -> InputT (Cluedo IO) ()
enterTurn playerName = do
    liftIO $ putStrLn "Enter named cards"
    cards <- askCards
                (cmdPrompt ("turn " ++ playerName))
                $ \cs -> (length cs == 3) && (any isPieceCard cs)
                                          && (any isRoomCard cs)
                                          && (any isWeaponCard cs)
    playerCount <- lift $ length <$> players <$> get
    r <- sequence $ replicate (playerCount - 1) $ askReply
            (cmdPrompt ("turn " ++ playerName ++ " reply"))
    let logEntry = TurnEntry playerName cards r
    lift $ addLogEntry logEntry
    lift rectifyTable

cardsShowedTo :: String -> [LogEntry] -> [Card]
cardsShowedTo player log = concat $ (flip map) playerRequests $ \e ->
        map repliedCard $ filter (("me" ==) . replier) (replies e)
    where
        playerRequests = filter ((player ==) . asker) $ filter isTurnEntry log

reportError :: IOException -> InputT (Cluedo IO) ()
reportError e = liftIO $ putStrLn $ "IO error " ++ (show e)

printShowedCards :: MonadIO m => String -> Cluedo m ()
printShowedCards nm = when (nm /= "me") $ do
    st <- get
    let cardsShowed = cardsShowedTo nm (log st)
    if null cardsShowed
        then liftIO $ putStrLn "No cards showed earlier."
        else liftIO $ putStrLn $ "Cards showed: " ++ (intercalate ", " $ map show cardsShowed)

printReply :: Reply -> String
printReply (Reply name card) = name ++ "\t" ++ (show card)

printLog :: LogEntry -> String
printLog (TurnEntry asker cardsAsked replies) =
    asker ++ " \n"
        ++ "    " ++ (intercalate " " $ map show cardsAsked) ++ "\n"
        ++ "    " ++ (intercalate "\n    " $ map printReply replies)
printLog (Accusation suggester cards) =
    "accusation:\t" ++ suggester ++ " \n"
        ++ "    " ++ (intercalate " " $ map show cards)

data Command = SetCardCommand String Card

parseSetCardCommand :: (Functor m, Monad m)
                    => [String] -> Cluedo m (Maybe Command)
parseSetCardCommand ws = do
    playerNames <- map name <$> players <$> get
    let nm = ws !! 1
    let parsedCard = parseCard $ ws !! 2
    if not $ nm `elem` ("envelope" : playerNames)
        then return Nothing
        else do
            cards <- getPlayerCards nm
            case (parsedCard, cards) of
                (Nothing, Nothing) -> return Nothing
                (Nothing, Just _) -> return Nothing
                (Just _, Nothing) -> return Nothing
                (Just card, Just cs) -> do
                    let unknowns = map fst $ filter ((Unknown ==) . snd) cs
                    if card `elem` unknowns
                        then return $ Just $ SetCardCommand nm card
                        else return Nothing

mainLoop :: InputT (Cluedo IO) ()
mainLoop = do
    l <- getInputLine $ cmdPrompt ""
    playerNames <- lift $ map name <$> players <$> get
    case l of
        Nothing -> liftIO exitSuccess
        Just "" -> mainLoop
        Just v  -> do
            let ws = words v
            case head ws of
                "turn" -> let nm = last ws in
                          if nm `elem` playerNames
                              then do
                                lift $ printShowedCards nm
                                (enterTurn nm) `catch` reportError
                              else liftIO $ putStrLn "Incorrect player's name"
                "setcard" -> do
                    command <- lift $ parseSetCardCommand ws
                    case command of
                        Nothing -> liftIO
                                    $ putStrLn "Error in player name or card."
                        Just (SetCardCommand pname card) ->
                            lift $ setPlayerCard pname card
                "print" -> case ws !! 1 of
                    "log" -> do
                        logList <- lift $ map printLog <$> log <$> get
                        liftIO $ putStrLn $ intercalate "\n" logList
                    "table" -> lift printTable
                "accusate" ->
                    lift $ addLogEntry $ Accusation (ws !! 1) $ map (fromJust . parseCard) (drop 2 ws)
                "rectify" -> lift rectifyTable
                _      -> liftIO $ putStrLn "Unknown command"
    mainLoop
