import Control.Applicative ((<$>))
import Control.Monad.State (StateT(..), MonadState(..), evalStateT)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.IO.Class (MonadIO)

import Data.List (intercalate, find)
import Data.Maybe (fromJust)

import System.Console.Haskeline
import System.Console.Haskeline.History (historyLines)
import System.Console.Haskeline.Completion(CompletionFunc)
import System.Exit (exitSuccess)

commandLineComplete :: Monad m => CompletionFunc m
commandLineComplete = undefined

cmdPrompt = "(cluedo) "

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
    l <- lift $ getInputLine cmdPrompt
    case l of
        Nothing -> liftIO exitSuccess
        Just "" -> askPlayerNames
        Just v ->  setPlayers $ (emptyPlayer "me") : (map fullPlayer (words v))

initialSetup :: Cluedo (InputT IO) ()
initialSetup = do
    askPlayerNames
    printTable

mainLoop :: Cluedo (InputT IO) ()
mainLoop = undefined
