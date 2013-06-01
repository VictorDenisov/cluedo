import Prelude hiding (log, catch)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.State (MonadState(..))
import Control.Monad.Error (ErrorT(..), MonadError(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_, when)

import Data.List (intercalate, find, isPrefixOf, sortBy, groupBy, intersect, (\\))
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
import Cluedo
import Cluedo.Model
import Cluedo.Utils

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
                    "log"   -> lift printLog
                    "table" -> lift printTable
                "accusate" ->
                    lift $ addLogEntry
                                $ Accusation (ws !! 1)
                                        $ map (fromJust . parseCard) (drop 2 ws)
                "rectify" -> lift rectifyTable
                _         -> liftIO $ putStrLn "Unknown command"
    mainLoop

printLog :: (Functor m, MonadIO m) => Cluedo m ()
printLog = do
    logList <- map printLogEntry <$> log <$> get
    if null logList
        then liftIO $ putStrLn "No log entries"
        else liftIO $ putStrLn $ intercalate "\n" logList


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

commandList = ["rectify", "setcard", "turn", "print", "accusate"]

printCommandList = ["log", "table"]

emptyCompleter :: MonadIO m => CompletionFunc (Cluedo m)
emptyCompleter (leftLine, _) = return (leftLine, [])

-- TODO completer doesn't verify count.
cardCompleter :: MonadIO m => Int -> [Card] -> CompletionFunc (Cluedo m)
cardCompleter count allowedCards (leftLine, _) = return $
    second buildCompletions $ generateCardCompletionList count allowedCards leftLine

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

askCards :: CompletionFunc (Cluedo IO)
         -> String
         -> ([Card] -> Bool)
         -> InputT (Cluedo IO) [Card]
askCards completer prompt cardsOk = withCompleter completer $ do
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
            again = askCards completer prompt cardsOk

askMyCards :: InputT (Cluedo IO) ()
askMyCards = do
    playerCount <- lift $ length <$> players <$> get
    let cardNumber = ((cardCount - 3) `div` playerCount)
    liftIO $ putStrLn $ "Please enter your cards (" ++ (show cardNumber) ++ ")"
    cards <- askCards
                (cardCompleter cardNumber allCards)
                (cmdPrompt "")
                $ \cs -> (length cs == cardNumber)
                      && (all (`elem` allCards) cs)
    lift $ mapM_ (setPlayerCard "me") cards

askOutCards :: InputT (Cluedo IO) ()
askOutCards = do
    playerCount <- lift $ length <$> players <$> get
    (Just myCardsStatuses) <- lift $ getPlayerCards "me"
    let myCards = map fst $ filter ((Yes ==) . snd) myCardsStatuses
    let cardNumber = ((cardCount - 3) `rem` playerCount)
    if cardNumber == 0
        then liftIO $ putStrLn "Out is empty. No cards to be entered."
        else do
            liftIO $ putStrLn $ "Please enter out (" ++ (show cardNumber) ++ ")"
            cards <- askCards
                        (cardCompleter cardNumber (allCards \\ myCards))
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
            case parseReply playerNames (words v) of
                Just x -> return x
                Nothing -> do
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

enterTurn :: String -> InputT (Cluedo IO) ()
enterTurn playerName = do
    liftIO $ putStrLn "Enter named cards"
    cards <- askCards
                (cardCompleter 3 allCards)
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

reportError :: MonadIO m => IOException -> m ()
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

