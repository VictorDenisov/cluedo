module Cluedo.Model
where
import Data.List (find)

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (catMaybes)

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

{- | This function is unsafe and assumes player has this card. It's safe for
fullPlayer. -}
getCardStatus :: Card -> Player -> Status
getCardStatus c p = snd $ fromJust $ find ((c ==) . fst) (cards p)

setCardTuple :: Card -> (Card, Status) -> (Card, Status)
setCardTuple o (c, s) | o == c = (c, Yes)
setCardTuple o (c, s) = (c, s)

clearCardTuple :: Card -> (Card, Status) -> (Card, Status)
clearCardTuple o (c, s) | o == c = (c, No)
clearCardTuple o (c, s) = (c, s)

setCard :: String -> Card -> Player -> Player
setCard n c pl | n == name pl = pl {cards = map (setCardTuple c) (cards pl)}
setCard n c pl = pl {cards = map (clearCardTuple c) (cards pl)}

clearCard :: String -> Card -> Player -> Player
clearCard n c pl | n == name pl = pl {cards = map (clearCardTuple c) (cards pl)}
clearCard n c pl = pl

data CardReply = CardReply Card
               | UnknownCard
               | EmptyCard
                 deriving Eq

instance Show CardReply where
    show (CardReply c) = show c
    show UnknownCard = "UnknownCard"
    show EmptyCard = "EmptyCard"

isCardReply :: CardReply -> Bool
isCardReply (CardReply _) = True
isCardReply _ = False

fromCardReply :: CardReply -> Maybe Card
fromCardReply (CardReply c) = Just c
fromCardReply _ = Nothing

parseCardReply :: String -> Maybe CardReply
parseCardReply "EmptyCard" = Just EmptyCard
parseCardReply "UnknownCard" = Just UnknownCard
parseCardReply s = CardReply <$> parseCard s

data Reply = Reply
                { replier :: String
                , repliedCard :: CardReply
                }
             deriving (Show, Eq)

parseReply :: [String] -> [String] -> Maybe Reply
parseReply playerNames tokens | length tokens /= 2 = Nothing
parseReply playerNames tokens = let playerToken = tokens !! 0
                                    cardToken = tokens !! 1 in
    if playerToken `elem` playerNames
        then Reply playerToken <$> parseCardReply cardToken
        else Nothing

printReply :: Reply -> String
printReply (Reply name card) = name ++ "\t" ++ (show card)

data LogEntry = TurnEntry
                    { asker      :: String
                    , cardsAsked :: [Card] -- TODO convert this to tuple of three cards
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

printLogEntry :: LogEntry -> String
printLogEntry (TurnEntry asker cardsAsked replies) =
    asker ++ " \n"
        ++ "    " ++ (intercalate " " $ map show cardsAsked) ++ "\n"
        ++ "    " ++ (intercalate "\n    " $ map printReply replies)
printLogEntry (Accusation suggester cards) =
    "accusation:\t" ++ suggester ++ " \n"
        ++ "    " ++ (intercalate " " $ map show cards)

cardsShowedTo :: String -> [LogEntry] -> [Card]
cardsShowedTo player log =
    concat $ map (catMaybes . map fromCardReply . myCardReplies) playerRequests
    where
        playerRequests = filter ((player ==) . asker) turnLogEntries
        turnLogEntries = filter isTurnEntry log
        myCardReplies turnEntry = map repliedCard (myReplies turnEntry)
        myReplies turnEntry = filter (("me" ==) . replier) (replies turnEntry)

--TODO test this function.
findSinglePlayerWithNonNegativeCardStatus :: [Player]
                                             -> Card
                                             -> Maybe (String, Card)
findSinglePlayerWithNonNegativeCardStatus ps c =
        if noCount == (length ps - 1)
            then let p = fromJust $ find ((No /=) . (getCardStatus c)) ps
                     in Just (name p, c)
            else Nothing
    where
        noCount = length $ filter (No ==) statuses
        statuses = map (getCardStatus c) ps

-- TODO move to Utils module. It shouldn't be in Model module.
generateCardCompletionList :: [Card] -> String -> [String]
generateCardCompletionList allowedCards "" = map show allowedCards
generateCardCompletionList allowedCards s =
        if head s == ' '
            then filter (not . (`elem` ws)) allowedCardsStrings
            else filter ((last ws) `isPrefixOf`) allowedCardsStrings
    where
        ws = words line
        line = reverse s
        allowedCardsStrings = map show allowedCards
