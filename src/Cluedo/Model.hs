module Cluedo.Model
where

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

allPieces = [Green, Mustard, Peacock, Plum, Scarlett, White]

allWeapons = [Wrench, Candle, Knife, Revolver, Pipe, Rope]

allRooms = [ Bathroom, Study, Dining, Billiard, Garage
           , Bedroom, Guestroom, Kitchen, Yard]

allCards = allPieces ++ allRooms ++ allWeapons

allCardsStrings = map show allCards

cardCount = length allCards

isPieceCard :: Card -> Bool
isPieceCard c = c `elem` allPieces

isWeaponCard :: Card -> Bool
isWeaponCard c = c `elem` allWeapons

isRoomCard :: Card -> Bool
isRoomCard c = c `elem` allRooms

parseCard :: String -> Maybe Card
parseCard s = s `lookup` ((map show allCards) `zip` allCards)

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

fullPlayer :: String -> Player
fullPlayer name = Player name (allCards `zip` (repeat Unknown))

pieces :: Player -> [(Card, Status)]
pieces p = filter ((`elem` allPieces) . fst) (cards p)

weapons :: Player -> [(Card, Status)]
weapons p = filter ((`elem` allWeapons) . fst) (cards p)

rooms :: Player -> [(Card, Status)]
rooms p = filter ((`elem` allRooms) . fst) (cards p)
