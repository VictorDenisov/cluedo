module Test.AllTests
where

import Test.HUnit
import Data.List (isPrefixOf)

import qualified Cluedo.Model as Model
import qualified Cluedo.Utils as Utils

testCardCount :: Assertion
testCardCount = 21 @=? Model.cardCount

testIsPieceCard_isPiece :: Assertion
testIsPieceCard_isPiece = (Model.isPieceCard Model.White) @? "white is a piece"

testIsPieceCard_isNotPiece :: Assertion
testIsPieceCard_isNotPiece =
    (not $ Model.isPieceCard Model.Bathroom)
    @?
    "bathroom is not a piece"

testIsWeaponCard_isWeapon :: Assertion
testIsWeaponCard_isWeapon =
    Model.isWeaponCard Model.Knife
    @?
    "knife is a weapon"

testIsWeaponCard_isNotWeapon :: Assertion
testIsWeaponCard_isNotWeapon =
    (not $ Model.isWeaponCard Model.Peacock)
    @?
    "peacock is not a weapon"

testIsRoomCard_isRoom :: Assertion
testIsRoomCard_isRoom = Model.isRoomCard Model.Garage @? "garage is a room"

testIsRoomCard_isNotRoom :: Assertion
testIsRoomCard_isNotRoom =
    (not $ Model.isRoomCard Model.Pipe)
    @?
    "pie is not a room"

testParseCard_isCard :: Assertion
testParseCard_isCard = Model.parseCard "Plum" @=? Just Model.Plum

testParseCard_notCard :: Assertion
testParseCard_notCard = Model.parseCard "NotCard" @=? Nothing

withUnknown l = zip l (repeat Model.Unknown)

testPieces_fullPlayer :: Assertion
testPieces_fullPlayer =
    Model.pieces (Model.fullPlayer "test") @=? withUnknown Model.allPieces

testWeapons_fullPlayer :: Assertion
testWeapons_fullPlayer =
    Model.weapons (Model.fullPlayer "test") @=? withUnknown Model.allWeapons

testRooms_fullPlayer :: Assertion
testRooms_fullPlayer =
    Model.rooms (Model.fullPlayer "test") @=? withUnknown Model.allRooms

testGetCardStatus :: Assertion
testGetCardStatus =
    Model.Unknown @=? Model.getCardStatus
                                Model.Bathroom
                                (Model.fullPlayer "test")

playerWithPeacockYes :: Model.Player
playerWithPeacockYes = Model.Player "test" $ withUnknown Model.allWeapons
                                          ++ withUnknown Model.allRooms
                                          ++ [ (Model.Scarlett, Model.Unknown)
                                             , (Model.Mustard , Model.Unknown)
                                             , (Model.White   , Model.Unknown)
                                             , (Model.Green   , Model.Unknown)
                                             , (Model.Peacock , Model.Yes    )
                                             , (Model.Plum    , Model.Unknown)
                                             ]

testGetPeacockStatus :: Assertion
testGetPeacockStatus =
    Model.Yes @=? Model.getCardStatus
                                Model.Peacock playerWithPeacockYes

testSetCardTuple :: Assertion
testSetCardTuple =
    (Model.Plum, Model.Yes)
    @=?
    Model.setCardTuple Model.Plum (Model.Plum, Model.Unknown)

testSetCardTuple_notSetForDifferentCard :: Assertion
testSetCardTuple_notSetForDifferentCard =
    (Model.Plum, Model.Unknown)
    @=?
    Model.setCardTuple Model.Peacock (Model.Plum, Model.Unknown)

testClearCardTuple :: Assertion
testClearCardTuple =
    (Model.Plum, Model.No)
    @=?
    Model.clearCardTuple Model.Plum (Model.Plum, Model.Unknown)

testClearCardTuple_notClearForDifferentCard :: Assertion
testClearCardTuple_notClearForDifferentCard =
    (Model.Plum, Model.Unknown)
    @=?
    Model.clearCardTuple Model.Peacock (Model.Plum, Model.Unknown)

testSetCard_givenCardAndPlayerIsSet :: Assertion
testSetCard_givenCardAndPlayerIsSet =
    let result = Model.setCard
                            playerName
                            Model.Peacock
                            (Model.fullPlayer playerName)
    in
        Model.Yes @=? Model.getCardStatus Model.Peacock result
    where playerName = "player"

testSetCard_givenCardAndWrongPlayerIsSet :: Assertion
testSetCard_givenCardAndWrongPlayerIsSet =
    let result = Model.setCard
                            "differentPlayer"
                            Model.Peacock
                            (Model.fullPlayer "player")
    in
        Model.No @=? Model.getCardStatus Model.Peacock result

testClearCard_givenCardAndPlayerIsCleared :: Assertion
testClearCard_givenCardAndPlayerIsCleared =
    let result = Model.clearCard
                            playerName
                            Model.Peacock
                            (Model.fullPlayer playerName)
    in
        Model.No @=? Model.getCardStatus Model.Peacock result
    where playerName = "player"

testClearCard_givenCardAndWrongPlayerIsCleared :: Assertion
testClearCard_givenCardAndWrongPlayerIsCleared =
    let result = Model.clearCard
                            "differentPlayer"
                            Model.Peacock
                            (Model.fullPlayer "player")
    in
        Model.Unknown @=? Model.getCardStatus Model.Peacock result

testParseCardReply_EmptyCard :: Assertion
testParseCardReply_EmptyCard =
    (Just Model.EmptyCard) @=? Model.parseCardReply "EmptyCard"

testParseCardReply_UnknownCard :: Assertion
testParseCardReply_UnknownCard =
    (Just Model.UnknownCard) @=? Model.parseCardReply "UnknownCard"

testParseCardReply_Peacock :: Assertion
testParseCardReply_Peacock =
    (Just $ Model.CardReply Model.Peacock) @=? Model.parseCardReply "Peacock"

testParseCardReply_InvalidCard :: Assertion
testParseCardReply_InvalidCard =
    Nothing @=? Model.parseCardReply "InvalidCard"

testParseReply_Valid :: Assertion
testParseReply_Valid =
    (Just $ Model.Reply "player1" (Model.CardReply Model.White))
    @=?
    (Model.parseReply ["player2", "player1"] ["player1", "White"])

testParseReply_UnknownPlayer :: Assertion
testParseReply_UnknownPlayer =
    Nothing
    @=?
    (Model.parseReply ["player2", "player1"] ["playerUnknown", "White"])

testParseReply_CardInvalid :: Assertion
testParseReply_CardInvalid =
    Nothing
    @=?
    (Model.parseReply ["player2", "player1"] ["player1", "CardInvalid"])

testParseReply_WrongTokenCount :: Assertion
testParseReply_WrongTokenCount =
    Nothing
    @=?
    (Model.parseReply ["player2", "player1"] [ "player1"
                                             , "White"
                                             , "additionalToken"])

testPrintReply :: Assertion
testPrintReply =
    "name\tEmptyCard"
    @=?
    (Model.printReply $ Model.Reply "name" Model.EmptyCard)

testIsTurnEntry_TurnEntry :: Assertion
testIsTurnEntry_TurnEntry =
    True @=? (Model.isTurnEntry $ Model.TurnEntry "" [] [])

testIsTurnEntry_NotTurnEntry :: Assertion
testIsTurnEntry_NotTurnEntry =
    False @=? (Model.isTurnEntry $ Model.Accusation "" [])

testIsAccusation_Accusation :: Assertion
testIsAccusation_Accusation =
    True @=? (Model.isAccusation $ Model.Accusation "" [])

testIsAccusation_NotAccusation :: Assertion
testIsAccusation_NotAccusation =
    False @=? (Model.isAccusation $ Model.TurnEntry "" [] [])

testPrintLogEntry_TurnEntry :: Assertion
testPrintLogEntry_TurnEntry =
    ("asker \n" ++
    "    Peacock\n" ++
    "    replier\tEmptyCard")
    @=?
    (Model.printLogEntry $
            Model.TurnEntry
                    "asker"
                    [Model.Peacock]
                    [Model.Reply "replier" Model.EmptyCard])

testPrintLogEntry_Accusation :: Assertion
testPrintLogEntry_Accusation =
    ("accusation:\tsuggester \n" ++
    "    Peacock")
    @=?
    (Model.printLogEntry $
            Model.Accusation
                    "suggester"
                    [Model.Peacock])

testCardsShowedTo_NonExistentPlayer :: Assertion
testCardsShowedTo_NonExistentPlayer =
    []
    @=?
    (Model.cardsShowedTo
            "NonExistentPlayer"
            [ Model.TurnEntry
                    "player1"
                    [ Model.Peacock
                    , Model.Wrench
                    ]
                    [ Model.Reply
                        "player2"
                        (Model.CardReply Model.Peacock)
                    ]
            ]
    )

testCardsShowedTo_MePlayerIsAbsent :: Assertion
testCardsShowedTo_MePlayerIsAbsent =
    []
    @=?
    (Model.cardsShowedTo
            "player1"
            [ Model.TurnEntry
                    "player1"
                    [ Model.Peacock
                    , Model.Wrench
                    ]
                    [ Model.Reply
                        "player2"
                        (Model.CardReply Model.Peacock)
                    ]
            ]
    )

testCardsShowedTo_ListedPlayers :: Assertion
testCardsShowedTo_ListedPlayers =
    [Model.Peacock]
    @=?
    (Model.cardsShowedTo
            "player1"
            [ Model.TurnEntry
                    "player1"
                    [ Model.Peacock
                    , Model.Wrench
                    ]
                    [ Model.Reply
                        "me"
                        (Model.CardReply Model.Peacock)
                    , Model.Reply
                        "player2"
                        (Model.CardReply Model.Wrench)
                    ]
            ]
    )

testFindPlayerPossiblyHasCard_hasCardYes :: Assertion
testFindPlayerPossiblyHasCard_hasCardYes =
    (Just ("player1", Model.Peacock))
    @=?
    (Model.findSinglePlayerWithNonNegativeCardStatus
                [ (Model.Player
                    "player1"
                    [ (Model.Peacock, Model.Yes)
                    , (Model.White, Model.No)
                    ])
                , (Model.Player
                    "player2"
                    [ (Model.Peacock, Model.No)
                    , (Model.White, Model.No)
                    ])
                ]
                Model.Peacock
    )

testFindPlayerPossiblyHasCard_hasCardUnknown :: Assertion
testFindPlayerPossiblyHasCard_hasCardUnknown =
    (Just ("player1", Model.Peacock))
    @=?
    (Model.findSinglePlayerWithNonNegativeCardStatus
                [ (Model.Player
                    "player1"
                    [ (Model.Peacock, Model.Unknown)
                    , (Model.White, Model.No)
                    ])
                , (Model.Player
                    "player2"
                    [ (Model.Peacock, Model.No)
                    , (Model.White, Model.No)
                    ])
                ]
                Model.Peacock
    )

testFindPlayerPossiblyHasCard_hasCardNothing :: Assertion
testFindPlayerPossiblyHasCard_hasCardNothing =
    Nothing
    @=?
    (Model.findSinglePlayerWithNonNegativeCardStatus
                [ (Model.Player
                    "player1"
                    [ (Model.Peacock, Model.Unknown)
                    , (Model.White, Model.No)
                    ])
                , (Model.Player
                    "player2"
                    [ (Model.Peacock, Model.Unknown)
                    , (Model.White, Model.No)
                    ])
                ]
                Model.Peacock
    )

testGenerateCardCompletionList_EmptyStringAllCards :: Assertion
testGenerateCardCompletionList_EmptyStringAllCards =
    ("", Model.allCardsStrings)
    @=?
    (Utils.generateCardCompletionList 1 Model.allCards "")

testGenerateCardCompletionList_PeacockPrefix :: Assertion
testGenerateCardCompletionList_PeacockPrefix =
    ("", ["Peacock"])
    @=?
    (Utils.generateCardCompletionList 1 Model.allCards $ reverse "Pea")

testGenerateCardCompletionList_PeacockPrefixSecondToken :: Assertion
testGenerateCardCompletionList_PeacockPrefixSecondToken =
    (" etihW", ["Peacock"])
    @=?
    (Utils.generateCardCompletionList 2 Model.allCards $ reverse "White Pea")

testGenerateCardCompletionList_EndingSpaceNonMentionedCards :: Assertion
testGenerateCardCompletionList_EndingSpaceNonMentionedCards =
    (" etihW", (filter ("White" /=) Model.allCardsStrings))
    @=?
    (Utils.generateCardCompletionList 2 Model.allCards $ reverse "White ")

testGenerateCardCompletionList_OnlyFromAllowedCards :: Assertion
testGenerateCardCompletionList_OnlyFromAllowedCards =
    ("", ["White", "Wrench"])
    @=?
    (Utils.generateCardCompletionList 1 [Model.White, Model.Wrench] "")

testGenerateCardCompletionList_ListOfCardsIsFull :: Assertion
testGenerateCardCompletionList_ListOfCardsIsFull =
    (" etihW", [])
    @=?
    (Utils.generateCardCompletionList 1 Model.allCards $ reverse "White ")

testGenerateCardCompletionList_ListOfCardsHasSpace :: Assertion
testGenerateCardCompletionList_ListOfCardsHasSpace =
    (" etihW", ["Peacock"])
    @=?
    (Utils.generateCardCompletionList 2 Model.allCards $ reverse "White Pea")

testGenerateCardCompletionList_PartialNonRepeated :: Assertion
testGenerateCardCompletionList_PartialNonRepeated =
    (" etihW", [])
    @=?
    (Utils.generateCardCompletionList 2 Model.allCards $ reverse "White Whi")

testGenerateCardCompletionList_LastWordShouldBeCompleted :: Assertion
testGenerateCardCompletionList_LastWordShouldBeCompleted =
    (" etihW", ["Rope"])
    @=?
    (Utils.generateCardCompletionList 2 Model.allCards $ reverse "White Rope")

testIsUnique_allDifferent :: Assertion
testIsUnique_allDifferent =
    True @=? (Utils.isUnique ["A", "B", "C"])

testIsUnique_hasSimilar :: Assertion
testIsUnique_hasSimilar =
    False @=? (Utils.isUnique ["A", "B", "B"])
