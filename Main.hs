import Prelude hiding (log, catch)
import Control.Applicative ((<$>))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_, when)
import Control.Arrow (first)

import Data.List (intercalate, find, isPrefixOf)
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

allKnownCards = map PieceCard allPieces ++ map RoomCard allRooms ++ map WeaponCard allWeapons
allCards = (show EmptyCard) : (show UnknownCard) : allKnownCardsStrings

allKnownCardsStrings = map show allPieces ++ map show allRooms ++ map show allWeapons
allCardsStrings = (show EmptyCard) : (show UnknownCard) : allKnownCardsStrings

cardCount = length allKnownCardsStrings

commandList = ["rectify", "setcard", "turn", "print"]

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
                _ -> return (drop (length $ last ws) leftLine, buildCompletions $ filter ((last ws) `isPrefixOf`) allCardsStrings)

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
                else return (drop (length $ last ws) leftLine, buildCompletions $ filter ((last ws) `isPrefixOf`) names)
        "print" -> case (head leftLine, length ws) of
            (' ', 2) -> return (leftLine, [])
            (_, 2) -> return (drop (length $ last ws) leftLine, buildCompletions $ filter ((last ws) `isPrefixOf`) printCommandList)
            (' ', 1) -> return (leftLine, buildCompletions $ printCommandList)
        "setcard" -> case (head leftLine, length ws) of
            (' ', 3) -> return (leftLine, [])
            (_, 3) -> return (drop (length $ last ws) leftLine, buildCompletions $ filter ((last ws) `isPrefixOf`) allCardsStrings)
            (' ', 2) -> return (leftLine, buildCompletions allCardsStrings)
            (_, 2) -> return (drop (length $ last ws) leftLine, buildCompletions $ filter ((last ws) `isPrefixOf`) names)
            (' ', 1) -> return (leftLine, buildCompletions names)
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

data Piece = Scarlett
           | Mustard
           | White
           | Green
           | Peacock
           | Plum
             deriving  (Eq, Show, Read)

allPieces = [Green, Mustard, Peacock, Plum, Scarlett, White]
allPieceCards = map PieceCard allPieces

data Weapon = Candle
            | Knife
            | Pipe
            | Revolver
            | Rope
            | Wrench
              deriving (Eq, Show, Read)

allWeapons = [Wrench, Candle, Knife, Revolver, Pipe, Rope]
allWeaponCards = map WeaponCard allWeapons

data Room = Kitchen
          | Billiard
          | Library
          | Dining
          | Bathroom
          | Study
          | Garage
          | Bedroom
          | Yard
          | Guestroom
            deriving (Eq, Show, Read)

allRooms = [ Bathroom, Study, Dining, Billiard, Garage
           , Bedroom, Guestroom, Kitchen, Yard]
allRoomCards = map RoomCard allRooms

data Card = PieceCard Piece
          | WeaponCard Weapon
          | RoomCard Room
          | UnknownCard
          | EmptyCard
            deriving (Eq, Show)

isPieceCard :: Card -> Bool
isPieceCard (PieceCard _) = True
isPieceCard _ = False

isWeaponCard :: Card -> Bool
isWeaponCard (WeaponCard _) = True
isWeaponCard _ = False

isRoomCard :: Card -> Bool
isRoomCard (RoomCard _) = True
isRoomCard _ = False

printCard :: Card -> String
printCard EmptyCard = "EmptyCard"
printCard UnknownCard = "UnknownCard"
printCard (RoomCard r) = show r
printCard (WeaponCard w) = show w
printCard (PieceCard p) = show p

parseCard :: String -> Maybe Card
parseCard "EmptyCard" = Just EmptyCard
parseCard "UnknownCard" = Just UnknownCard
parseCard s = case s `lookup` weaponPairs of
            Just v -> Just $ WeaponCard v
            Nothing -> case s `lookup` roomPairs of
                        Just v -> Just $ RoomCard v
                        Nothing -> case s `lookup` piecePairs of
                                    Just v -> Just $ PieceCard v
                                    Nothing -> Nothing
    where
        weaponPairs = ((map show allWeapons) `zip` allWeapons)
        roomPairs = ((map show allRooms) `zip` allRooms)
        piecePairs = ((map show allPieces) `zip` allPieces)

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
                , rooms   :: [(Room, Status)]
                , pieces  :: [(Piece, Status)]
                , weapons :: [(Weapon, Status)]
                }

fullPlayer :: String -> Player
fullPlayer name = Player name (allRooms `zip` (repeat Unknown))
                              (allPieces `zip` (repeat Unknown))
                              (allWeapons `zip` (repeat Unknown))

emptyPlayer :: String -> Player
emptyPlayer name = Player name (allRooms `zip` (repeat No))
                               (allPieces `zip` (repeat No))
                               (allWeapons `zip` (repeat No))

hasPiece :: Player -> Piece -> Status
hasPiece player piece = snd $ fromJust $ find ((piece ==) . fst) (pieces player)

hasRoom :: Player -> Room -> Status
hasRoom player room = snd $ fromJust $ find ((room ==) . fst) (rooms player)

hasWeapon :: Player -> Weapon -> Status
hasWeapon player weapon =  snd $ fromJust $ find ((weapon ==) . fst) (weapons player)

getCards :: Player -> [(Card, Status)]
getCards p = map (first RoomCard) (rooms p)
          ++ map (first PieceCard) (pieces p)
          ++ map (first WeaponCard) (weapons p)

getCard :: Card -> Player -> Status
getCard EmptyCard _ = Unknown
getCard UnknownCard _ = Unknown
getCard c p = snd $ fromJust $ find ((c ==) . fst) (getCards p)
                                -- at least one element is guaranteed.

getPlayerCards :: Monad m => String -> Cluedo m (Maybe [(Card, Status)])
getPlayerCards n = do
    st <- get
    return $ do
        player <- find ((n ==) . name) (players st)
        return $ getCards player

data LogEntry = LogEntry
                    { asker      :: String
                    , cardsAsked :: [Card]
                    , replies    :: [Reply]
                    } deriving (Show)

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
setCard n (PieceCard p) pl | n == name pl = pl {pieces = map (setPiece p) (pieces pl)}
setCard n (PieceCard p) pl = pl {pieces = map (clearPiece p) (pieces pl)}
setCard n (RoomCard r) pl | n == name pl = pl {rooms = map (setRoom r) (rooms pl)}
setCard n (RoomCard r) pl = pl {rooms = map (clearRoom r) (rooms pl)}
setCard n (WeaponCard w) pl | n == name pl = pl {weapons = map (setWeapon w) (weapons pl)}
setCard n (WeaponCard w) pl = pl {weapons = map (clearWeapon w) (weapons pl)}

clearPlayerCard :: Monad m => String -> Card -> Cluedo m ()
clearPlayerCard n c = do
    st <- get
    setPlayers $ map (clearCard n c) (players st)
    setOut $ clearCard n c (out st)
    setEnvelope $ clearCard n c (envelope st)

clearCard n (PieceCard p) pl | n == name pl = pl {pieces = map (clearPiece p) (pieces pl)}
clearCard n (PieceCard p) pl = pl
clearCard n (RoomCard r) pl | n == name pl = pl {rooms = map (clearRoom r) (rooms pl)}
clearCard n (RoomCard r) pl  = pl
clearCard n (WeaponCard w) pl | n == name pl = pl {weapons = map (clearWeapon w) (weapons pl)}
clearCard n (WeaponCard w) pl  = pl

setPiece :: Piece -> (Piece, Status) -> (Piece, Status)
setPiece p (pv, st) | p == pv = (pv, Yes)
setPiece p (pv, st) = (pv, st)

clearPiece :: Piece -> (Piece, Status) -> (Piece, Status)
clearPiece p (pv, st) | p == pv = (pv, No)
clearPiece p (pv, st) = (pv, st)

setRoom :: Room -> (Room, Status) -> (Room, Status)
setRoom r (rv, st) | r == rv = (rv, Yes)
setRoom r (rv, st) = (rv, st)

clearRoom :: Room -> (Room, Status) -> (Room, Status)
clearRoom r (rv, st) | r == rv = (rv, No)
clearRoom r (rv, st) = (rv, st)

setWeapon :: Weapon -> (Weapon, Status) -> (Weapon, Status)
setWeapon w (wv, st) | w == wv = (wv, Yes)
setWeapon w (wv, st) = (wv, st)

clearWeapon :: Weapon -> (Weapon, Status) -> (Weapon, Status)
clearWeapon w (wv, st) | w == wv = (wv, No)
clearWeapon w (wv, st) = (wv, st)

printTable :: MonadIO m => Cluedo m ()
printTable = do
    st <- get
    let allPlayers = ((envelope st) : (players st)) ++ [(out st)]
    let printer getter piece = (intercalate "\t" $ map (show . (`getter` piece)) allPlayers)
    let piecePrinter piece =
            liftIO $ putStrLn $ (show piece) ++ ":\t"
                                        ++ (printer hasPiece piece)
    let roomPrinter room =
            liftIO $ putStrLn $ (show room) ++ ":\t"
                                        ++ (printer hasRoom room)
    let weaponPrinter weapon =
            liftIO $ putStrLn $ (show weapon) ++ ":\t"
                                        ++ (printer hasWeapon weapon)

    liftIO $ putStrLn $ "\t" ++ (intercalate "\t" $ map name allPlayers)
    mapM_ piecePrinter allPieces
    liftIO $ putStrLn ""
    mapM_ weaponPrinter allWeapons
    liftIO $ putStrLn ""
    mapM_ roomPrinter allRooms

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
        Just "" -> again
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
processLogEntry logEntry = do
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
        fixPlusesList (name p) (getCards p) ((cardCount - 3) `div` (length $ players st))

fixCategoryPluses :: Monad m => Cluedo m ()
fixCategoryPluses = do
    st <- get
    let env = envelope st
    let pieceCards = map (first PieceCard) (pieces env)
    fixPlusesList (name env) pieceCards 1
    let weaponCards = map (first WeaponCard) (weapons env)
    fixPlusesList (name env) weaponCards 1
    let roomCards = map (first RoomCard) (rooms env)
    fixPlusesList (name env) roomCards 1

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
        fixFullList (name p) (getCards p) ((cardCount - 3) `div` (length $ players st))

fixOneCardInCategory :: Monad m => Cluedo m ()
fixOneCardInCategory = do
    st <- get
    let env = envelope st
    let pieceCards = map (first PieceCard) (pieces env)
    fixFullList (name env) pieceCards 1
    let weaponCards = map (first WeaponCard) (weapons env)
    fixFullList (name env) weaponCards 1
    let roomCards = map (first RoomCard) (rooms env)
    fixFullList (name env) roomCards 1

rectifyTable :: Monad m => Cluedo m ()
rectifyTable = do
    st <- get
    mapM_ processLogEntry (log st)
    fixNobodyHasCard
    fixPlayerHasAllCards
    fixOneCardInCategory
    fixPlayerPluses
    fixCategoryPluses

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
    let logEntry = LogEntry playerName cards r
    st <- lift get
    lift $ put $ st {log = logEntry : log st}
    lift rectifyTable

cardsShowedTo :: String -> [LogEntry] -> [Card]
cardsShowedTo player log = concat $ (flip map) playerRequests $ \e ->
        map repliedCard $ filter (("me" ==) . replier) (replies e)
    where
        playerRequests = filter ((player ==) . asker) log

reportError :: IOException -> InputT (Cluedo IO) ()
reportError e = liftIO $ putStrLn $ "IO error " ++ (show e)

printShowedCards :: MonadIO m => String -> Cluedo m ()
printShowedCards nm = when (nm /= "me") $ do
    st <- get
    let cardsShowed = cardsShowedTo nm (log st)
    if null cardsShowed
        then liftIO $ putStrLn "No cards showed earlier."
        else liftIO $ putStrLn $ "Cards showed: " ++ (intercalate ", " $ map printCard cardsShowed)

printReply :: Reply -> String
printReply (Reply name card) = name ++ "\t" ++ (printCard card)

printLog :: LogEntry -> String
printLog (LogEntry asker cardsAsked replies) =
    asker ++ " \n"
        ++ "    " ++ (intercalate " " $ map printCard cardsAsked) ++ "\n"
        ++ "    " ++ (intercalate "\n    " $ map printReply replies)

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
                    let nm = ws !! 1
                    let card = parseCard $ ws !! 2
                    if (nm `elem` ("envelope" : playerNames)) && (isJust card)
                        then lift $ setPlayerCard nm (fromJust card)
                        else liftIO $ putStrLn "Error in player name or card."
                "print" -> case ws !! 1 of
                    "log" -> do
                        logList <- lift $ map printLog <$> log <$> get
                        liftIO $ putStrLn $ intercalate "\n" logList
                    "table" -> lift printTable
                "rectify" -> lift rectifyTable
                _      -> liftIO $ putStrLn "Unknown command"
    mainLoop
