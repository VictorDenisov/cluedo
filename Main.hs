import Control.Applicative ((<$>))
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.IO.Class (MonadIO)

import Data.List (intercalate, find, isPrefixOf)
import Data.Maybe (fromJust)

import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import System.Console.Haskeline.Completion(CompletionFunc)
import System.Exit (exitSuccess)

buildCompletions = map (\name -> Completion name name True)

commandLineComplete :: Monad m => CompletionFunc m
commandLineComplete (leftLine, _) = do
    let line = reverse leftLine
    let ws = words line
    let allCards = map show allPieces ++ map show allRooms ++ map show allWeapons
    case length ws of
        0 -> return (leftLine, buildCompletions ["cards"])
        1 -> if head leftLine == ' '
                then return (leftLine, buildCompletions allCards)
                else return ("", buildCompletions ["cards"])
        v -> if head leftLine == ' '
                then return (leftLine, buildCompletions allCards)
                else return (drop (length $ last ws) leftLine, buildCompletions $ filter ((last ws) `isPrefixOf`) allCards)


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

parseCard :: String -> Card
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

data Status = Yes
            | No
            | Unknown

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

data Table = Table
    { players  :: [Player]
    , out      :: Player
    , envelope :: Player
    }

type Cluedo = StateT Table

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

main = runInputT (Settings (commandLineComplete) Nothing True)
                $ evalStateT
                    (initialSetup)
                    (Table { players  = []
                           , out      = emptyPlayer "out"
                           , envelope = fullPlayer "envelope"
                           })

askPlayerNames :: Cluedo (InputT IO) ()
askPlayerNames = do
    liftIO $ putStrLn $ "Please enter players names"
    l <- lift $ getInputLine $ cmdPrompt ""
    case l of
        Nothing -> liftIO exitSuccess
        Just "" -> askPlayerNames
        Just v ->  setPlayers $ (emptyPlayer "me") : (map fullPlayer (words v))

askCards :: String -> Cluedo (InputT IO) ()
askCards playerName = do
    l <- lift $ getInputLine $ cmdPrompt ""
    case l of
        Nothing -> liftIO exitSuccess
        Just "" -> askCards playerName
        Just v | "cards " `isPrefixOf` v ->  do
            let cardNames = tail $ words v -- drop cards command
            let cards = map parseCard cardNames
            mapM_ (setPlayerCard playerName) cards
        Just _ ->  do
            liftIO $ putStrLn $ "cards command should be used to enter cards"
            askCards playerName

askMyCards :: Cluedo (InputT IO) ()
askMyCards = do
    liftIO $ putStrLn $ "Please enter your cards"
    askCards "me"

askOutCards :: Cluedo (InputT IO) ()
askOutCards = do
    liftIO $ putStrLn $ "Please enter cards in out"
    askCards "out"

initialSetup :: Cluedo (InputT IO) ()
initialSetup = do
    askPlayerNames
    askMyCards
    askOutCards
    printTable

mainLoop :: Cluedo (InputT IO) ()
mainLoop = undefined
