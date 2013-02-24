import Prelude hiding (log, catch)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.State (MonadState(..))
import Control.Monad.Error (ErrorT(..), MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_, when)

import Data.List (intercalate, find, isPrefixOf, sortBy, groupBy, intersect)
import Data.Maybe (isJust, isNothing, fromJust, catMaybes)

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

main = evalStateT (runInputT (Settings (commandLineComplete) Nothing True)
                             (initialSetup >> mainLoop))
                  (Table { players     = []
                         , out         = fullPlayer "out"
                         , envelope    = fullPlayer "envelope"
                         , cmdComplete = basicCommandLineComplete
                         , log         = []
                         })

initialSetup :: InputT (Cluedo IO) ()
initialSetup = do
    liftIO $ putStrLn "Cluedo board game assistant version 1.0\n"
    withCompleter emptyCompleter askPlayerNames
    askMyCards
    askOutCards
    lift rectifyTable
    lift printTable

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
                "turn" -> do
                    command <- lift $ parseCommand parseTurnCommand v
                    case command of
                        Right (TurnCommand nm) -> do
                            lift $ printShowedCards nm
                            (enterTurn nm) `catch` reportError
                        Left errMsg ->
                            liftIO $ putStrLn
                                    $ "Incorrect format of turn command: "
                                      ++ errMsg
                "setcard" -> do
                    command <- lift $ parseCommand parseSetCardCommand v
                    case command of
                        Right (SetCardCommand pname card) ->
                            lift $ setPlayerCard pname card
                        Left errMsg -> liftIO
                                $ putStrLn $ "Error in player name or card: "
                                             ++ errMsg
                "print" -> case ws !! 1 of
                    "log" -> do
                        logList <- lift $ map printLog <$> log <$> get
                        if null logList
                            then liftIO $ putStrLn "No log entries"
                            else liftIO $ putStrLn $ intercalate "\n" logList
                    "table" -> lift printTable
                "accusate" ->
                    lift $ addLogEntry
                                $ Accusation (ws !! 1)
                                        $ map (fromJust . parseCard) (drop 2 ws)
                "rectify" -> lift rectifyTable
                _      -> liftIO $ putStrLn "Unknown command"
    mainLoop


type CommandParser m a = ErrorT String (StateT [String] m) a
data Command = SetCardCommand String Card
             | TurnCommand String

parseCommand :: (Functor m, Monad m)
             => CommandParser (Cluedo m) Command
             -> String
             -> Cluedo m (Either String Command)
parseCommand cmd s = evalStateT (runErrorT cmd) (words s)

parseTurnCommand :: (Functor m, Monad m)
                 => CommandParser (Cluedo m) Command
parseTurnCommand = do
    parseTurn
    pn <- parsePlayerName
    return $ TurnCommand pn

parseSetCardCommand :: (Functor m, Monad m)
                    => CommandParser (Cluedo m) Command
parseSetCardCommand = do
    parseSetCard
    pn <- parsePlayerName
    card <- parseUnknownCard pn
    return $ SetCardCommand pn card

nextToken :: Monad m => CommandParser m String
nextToken = do
    l <- get
    let token = head l
    put $ tail l
    return token

parseSetCard :: Monad m => CommandParser m ()
parseSetCard = do
    v <- nextToken
    when (v /= "setcard") $ fail "not setcard"

parseTurn :: Monad m => CommandParser m ()
parseTurn = do
    v <- nextToken
    when (v /= "turn") $ fail "not turn"

parsePlayerName :: (Functor m, Monad m)
                => CommandParser (Cluedo m) String
parsePlayerName = do
    v <- nextToken
    playerNames <- map name <$> players <$> (lift $ lift get)
    when (not $ v `elem` ("envelope" : playerNames)) $ fail "Unknown player name"
    return v

parseAnyCard :: Monad m => CommandParser (Cluedo m) Card
parseAnyCard = do
    v <- nextToken
    let parsedCard = parseCard v
    when (isNothing parsedCard) $ fail "Invalid card"
    return $ fromJust parsedCard

parseUnknownCard :: Monad m => String -> CommandParser (Cluedo m) Card
parseUnknownCard pn = do
    card <- parseAnyCard
    cards <- lift $ lift $ getPlayerCards pn
    when (isNothing cards) $ fail "Player has no cards"
    let cs = fromJust cards
    let unknowns = map fst $ filter ((Unknown ==) . snd) cs
    when (not $ card `elem` unknowns) $ fail "Can't set known card"
    return card

buildCompletions = map (\name -> Completion name name True)

allCards = allPieces ++ allRooms ++ allWeapons

allCardsStrings = map show allCards

cardCount = length allCards

commandList = ["rectify", "setcard", "turn", "print", "accusate"]

printCommandList = ["log", "table"]

emptyCompleter :: MonadIO m => CompletionFunc (Cluedo m)
emptyCompleter (leftLine, _) = return (leftLine, [])

cardCompleter :: MonadIO m => CompletionFunc (Cluedo m)
cardCompleter (leftLine, _) = do
    let line = reverse leftLine
    let ws = words line
    if length ws == 0
        then return (leftLine, buildCompletions allCardsStrings)
        else case head leftLine of
                ' ' -> return (leftLine, buildCompletions allCardsStrings)
                _ -> return $ listCompleter
                                    leftLine
                                    (last ws)
                                    allCardsStrings

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
                    Nothing -> return (leftLine, [])
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
                cards <- getPlayerCards playerName
                case cards of
                    Nothing -> return (leftLine, [])
                    Just cs -> do
                        let unknowns = map (show . fst)
                                           $ filter ((Unknown ==) . snd) cs
                        return $ listCompleter leftLine (last ws) unknowns


            (_, _) -> return (leftLine, [])
        "accusate" -> case (head leftLine, length ws) of
            (' ', 1) -> return (leftLine, buildCompletions names)
            (' ', 2) -> return (leftLine, buildCompletions allCardsStrings)
            (_, 2) -> return $ listCompleter leftLine (last ws) names
            (' ', 3) -> return ( leftLine
                               ,buildCompletions allCardsStrings)
            (_, 3) -> return $ listCompleter
                                        leftLine
                                        (last ws)
                                        allCardsStrings
            (' ', 4) -> return ( leftLine
                               , buildCompletions allCardsStrings)
            (_, 4) -> return $ listCompleter
                                        leftLine
                                        (last ws)
                                        allCardsStrings
            (' ', 5) -> return (leftLine, [])
            (_, 5) -> return $ listCompleter
                                        leftLine
                                        (last ws)
                                        allCardsStrings
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
                else return ("", buildCompletions
                                    $ filter
                                        ((last ws) `isPrefixOf`)
                                        commandList)
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

            deriving (Eq, Show)

data CardReply = CardReply Card
               | UnknownCard
               | EmptyCard

isCardReply :: CardReply -> Bool
isCardReply (CardReply _) = True
isCardReply _ = False

fromCardReply :: CardReply -> Maybe Card
fromCardReply (CardReply c) = Just c
fromCardReply _ = Nothing

instance Show CardReply where
    show (CardReply c) = show c
    show UnknownCard = "UnknownCard"
    show EmptyCard = "EmptyCard"

isPieceCard :: Card -> Bool
isPieceCard c = c `elem` allPieces

isWeaponCard :: Card -> Bool
isWeaponCard c = c `elem` allWeapons

isRoomCard :: Card -> Bool
isRoomCard c = c `elem` allRooms

parseCard :: String -> Maybe Card
parseCard s = s `lookup` ((map show allCards) `zip` allCards)

parseCardReply :: String -> Maybe CardReply
parseCardReply "EmptyCard" = Just EmptyCard
parseCardReply "UnknownCard" = Just UnknownCard
parseCardReply s = do
    c <- parseCard s
    return $ CardReply c

data Reply = Reply
                { replier :: String
                , repliedCard :: CardReply
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
fullPlayer name = Player name (allCards `zip` (repeat Unknown))

getCardStatus :: Card -> Player -> Status
--getCardStatus EmptyCard _ = Unknown
--getCardStatus UnknownCard _ = Unknown
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
    let pairs = map (\(TurnEntry p cs _) -> (p, cs))
                    $ filter isTurnEntry (log st)
    let sortedPairs = sortBy (\x y -> fst x `compare` fst y) pairs
    let uniquePairs = groupBy (\x y -> fst x == fst y) sortedPairs
    return $ (flip map) uniquePairs
                            $ \ls -> (fst $ head ls, concat $ map snd ls)

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
    let printer getter card = intercalate "\t"
                                    $ map (\p -> show (getter card p)
                                                 ++ "  "
                                                 ++ (isAskedCard (name p) card))
                                          allPlayers

    let cardPrinter card =
            liftIO $ putStrLn $ (show card) ++ ":\t"
                                        ++ (printer getCardStatus card)

    liftIO $ putStrLn $ "\t" ++ (intercalate "\t" $ map name allPlayers)
    mapM_ cardPrinter allPieces
    liftIO $ putStrLn ""
    mapM_ cardPrinter allWeapons
    liftIO $ putStrLn ""
    mapM_ cardPrinter allRooms

askPlayerNames :: InputT (Cluedo IO) ()
askPlayerNames = do
    liftIO $ putStrLn $ "Please enter players names"
    l <- getInputLine $ cmdPrompt ""
    case l of
        Nothing -> liftIO exitSuccess
        Just "" -> askPlayerNames
        Just v ->  lift $ setPlayers
                            $ (fullPlayer "me") : (map fullPlayer (words v))

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
                $ \cs -> (length cs == cardNumber)
                      && (all (`elem` allCards) cs)
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

replyComplete :: (MonadIO m, Functor m) => [Card] -> CompletionFunc (Cluedo m)
replyComplete cardsAsked (leftLine, _) = do
    let line = reverse leftLine
    let ws = words line
    playerNames <- map name <$> players <$> get
    case (head leftLine, length ws) of
        (_, 0) -> return ("", buildCompletions playerNames)
        (' ', 1) -> do
            let playerName = head ws
            cards <- getPlayerCards playerName
            case cards of
                Nothing -> return (leftLine, [])
                Just cs -> do
                    let havingCards = map fst $ filter ((No /=) . snd) cs
                    let canBeAnsweredCards = cardsAsked `intersect` havingCards
                    let res = map show $ EmptyCard : UnknownCard : (map CardReply canBeAnsweredCards)
                    return (leftLine, buildCompletions res)
        (_, 1) -> return ( drop (length $ last ws) leftLine
                         , buildCompletions
                                $ filter (line `isPrefixOf`) playerNames)
        (' ', 2) -> return (leftLine, [])
        (_, 2) -> do
            let playerName = head ws
            cards <- getPlayerCards playerName
            case cards of
                Nothing -> return (leftLine, [])
                Just cs -> do
                    let havingCards = map fst $ filter ((No /=) . snd) cs
                    let canBeAnsweredCards = cardsAsked `intersect` havingCards
                    let res = map show $ EmptyCard : UnknownCard : (map CardReply canBeAnsweredCards)
                    return ( drop (length $ last ws) leftLine
                          , buildCompletions
                                $ filter (last ws `isPrefixOf`) res)
        (_, _) -> return (leftLine, [])

askReply :: String -> [Card] -> InputT (Cluedo IO) Reply
askReply cmdPrompt cardsAsked = withCompleter (replyComplete cardsAsked) $ do
    liftIO $ putStrLn "Enter reply"
    playerNames <- lift $ map name <$> players <$> get
    l <- getInputLine cmdPrompt
    case l of
        Nothing -> fail "askReply aborted"
        Just v -> do
            let ws = words v
            let cardReply = parseCardReply $ last ws
            case (head ws `elem` playerNames, cardReply) of
                (True, Just c) -> return $ Reply (head ws) c
                _ -> do
                    liftIO $ putStrLn "Incorrect input. Asking again."
                    askReply cmdPrompt cardsAsked

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

findPlayerPossiblyHasCard :: [Player] -> Card -> Maybe (String, Card)
findPlayerPossiblyHasCard ps c =
        let statuses = map (getCardStatus c) ps
            noCount = length $ filter (No ==) statuses in
        if noCount == (length ps - 1)
            then let p = fromJust $ find ((No /=) . (getCardStatus c)) ps
                     in Just (name p, c)
            else Nothing

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
    forM_ (map (findPlayerPossiblyHasCard allPlayers) allCards) $ \v ->
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
            cards
    let logEntry = TurnEntry playerName cards r
    lift $ addLogEntry logEntry
    lift rectifyTable

cardsShowedTo :: String -> [LogEntry] -> [Card]
cardsShowedTo player log = concat $ (flip map) playerRequests $ \e ->
        catMaybes $ map fromCardReply $ map repliedCard $ filter (("me" ==) . replier) (replies e)
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
        else liftIO $ putStrLn
                         $ "Cards showed: "
                           ++ (intercalate ", " $ map show cardsShowed)

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
