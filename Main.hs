import Prelude hiding (log)
import Control.Applicative ((<$>))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_)
import Control.Arrow (first)

import Data.List (intercalate, find, isPrefixOf)
import Data.Maybe (fromJust)

import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import System.Console.Haskeline.Completion(CompletionFunc)
import System.Console.Haskeline.MonadException
import System.Exit (exitSuccess)

buildCompletions = map (\name -> Completion name name True)

allKnownCards = map show allPieces ++ map show allRooms ++ map show allWeapons
allCards = (show EmptyCard) : (show UnknownCard) : allKnownCards

cardCount = length allKnownCards

commandList = ["cards", "turn", "print"]

printCommandList = ["log", "table"]

completeCommand :: MonadIO m => CompletionFunc (Cluedo m)
completeCommand (leftLine, _) = do
    let line = reverse leftLine
    let ws = words line
    let cmdName = head ws
    case cmdName of
        "cards" -> do
            if head leftLine == ' '
                then return (leftLine, buildCompletions allCards)
                else return (drop (length $ last ws) leftLine, buildCompletions $ filter ((last ws) `isPrefixOf`) allCards)
        "turn" -> do
            names <- allNames
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

data Weapon = Candle
            | Knife
            | Pipe
            | Revolver
            | Rope
            | Wrench
              deriving (Eq, Show, Read)

allWeapons = [Wrench, Candle, Knife, Revolver, Pipe, Rope]

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

data Card = PieceCard Piece
          | WeaponCard Weapon
          | RoomCard Room
          | UnknownCard
          | EmptyCard
            deriving (Eq, Show)

parseCard :: String -> Card
parseCard "EmptyCard" = EmptyCard
parseCard s = case s `lookup` weaponPairs of
            Just v -> WeaponCard v
            Nothing -> case s `lookup` roomPairs of
                        Just v -> RoomCard v
                        Nothing -> case s `lookup` piecePairs of
                                    Just v -> PieceCard v
                                    Nothing -> UnknownCard
    where
        weaponPairs = ((map show allWeapons) `zip` allWeapons)
        roomPairs = ((map show allRooms) `zip` allRooms)
        piecePairs = ((map show allPieces) `zip` allPieces)

data Reply = Reply String Card
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
askCards prompt cardsOk = do
    l <- getInputLine prompt
    case l of
        Nothing -> liftIO exitSuccess
        Just "" -> again
        Just v | "cards " `isPrefixOf` v ->  do
            let cardNames = tail $ words v -- drop cards command
            let cards = map parseCard cardNames
            if cardsOk cards
                then return cards
                else again
        Just _ ->  do
            liftIO $ putStrLn $ "cards command should be used to enter cards"
            again
    where
            again = askCards prompt cardsOk

askMyCards :: InputT (Cluedo IO) ()
askMyCards = do
    liftIO $ putStrLn $ "Please enter your cards"
    playerCount <- lift $ length <$> players <$> get
    cards <- askCards
                (cmdPrompt "")
                $ \cs -> length cs == ((cardCount - 3) `div` playerCount)
    lift $ mapM_ (setPlayerCard "me") cards

askOutCards :: InputT (Cluedo IO) ()
askOutCards = do
    liftIO $ putStrLn $ "Please enter cards in out"
    playerCount <- lift $ length <$> players <$> get
    cards <- askCards
                (cmdPrompt "")
                $ \cs -> length cs == ((cardCount - 3) `rem` playerCount)
    lift $ mapM_ (setPlayerCard "out") cards

initialSetup :: InputT (Cluedo IO) ()
initialSetup = do
    askPlayerNames
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
        (' ', 1) -> return (leftLine, buildCompletions allCards)
        (_, 1) -> return (drop (length $ last ws) leftLine, buildCompletions $ filter (line `isPrefixOf`) playerNames)
        (' ', 2) -> return (leftLine, [])
        (_, 2) -> return (drop (length $ last ws) leftLine, buildCompletions $ filter (last ws `isPrefixOf`) allCards)
        (_, _) -> return (leftLine, [])

askReply :: String -> InputT (Cluedo IO) Reply
askReply cmdPrompt = withCompleter replyComplete $ do
    liftIO $ putStrLn "Enter reply"
    l <- getInputLine cmdPrompt
    --TODO verify correctness
    case l of
        Nothing -> liftIO exitSuccess
        Just v -> do
            let ws = words v
            return $ Reply (head ws) (parseCard $ last ws)

withCompleter :: CompletionFunc (Cluedo IO)
              -> InputT (Cluedo IO) a
              -> InputT (Cluedo IO) a
withCompleter c blc = do
    st <- lift get
    let prevC = cmdComplete st
    lift $ put $ st {cmdComplete = c}
    a <- blc
    lift $ put $ st {cmdComplete = prevC}
    return a

processLogEntry :: MonadIO m => LogEntry -> Cluedo m ()
processLogEntry logEntry = do
    st <- get
    forM_ (replies logEntry) $ \(Reply name card) ->
        case card of
            EmptyCard -> mapM_ (clearPlayerCard name) (cardsAsked logEntry)
            UnknownCard -> do
                cards <- getPlayerCards name
                case cards of
                    Nothing ->
                        liftIO $ putStrLn
                                    $ "error during getting cards of " ++ name
                    Just cs -> do
                        let exceptAbsent = filter ((No /=) . snd) $ filter ((\x -> x `elem` (cardsAsked logEntry)) . fst) cs
                        liftIO $ putStrLn $ show exceptAbsent
                        if length exceptAbsent == 1
                            then setPlayerCard name (fst $ head exceptAbsent)
                            else return ()

            c -> setPlayerCard name c

enterTurn :: String -> InputT (Cluedo IO) ()
enterTurn playerName = do
    liftIO $ putStrLn "Enter named cards"
    cards <- askCards
                (cmdPrompt ("turn " ++ playerName))
                $ \cs -> length cs == 3
    playerCount <- lift $ length <$> players <$> get
    r <- sequence $ replicate (playerCount - 1) $ askReply
            (cmdPrompt ("turn " ++ playerName))
    let logEntry = LogEntry playerName cards r
    lift $ processLogEntry logEntry
    st <- lift get
    lift $ put $ st {log = logEntry : log st}

mainLoop :: InputT (Cluedo IO) ()
mainLoop = do
    l <- getInputLine $ cmdPrompt ""
    case l of
        Nothing -> liftIO exitSuccess
        Just "" -> mainLoop
        Just v  -> do
            let ws = words v
            case head ws of
                "turn" -> enterTurn $ last ws
                "print" -> case ws !! 1 of
                    "log" -> do
                        logList <- lift $ map show <$> log <$> get
                        liftIO $ putStrLn $ intercalate "\n" logList
                    "table" -> lift printTable
                _      -> liftIO $ putStrLn "other crap"
    mainLoop
