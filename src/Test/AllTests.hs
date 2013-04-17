module Test.AllTests
where

import Test.HUnit

import qualified Cluedo.Model as Model

testCardCount :: Assertion
testCardCount = 21 @=? Model.cardCount

testIsPieceCard_isPiece :: Assertion
testIsPieceCard_isPiece = (Model.isPieceCard Model.White) @? "white is a piece"

testIsPieceCard_isNotPiece :: Assertion
testIsPieceCard_isNotPiece = (not $ Model.isPieceCard Model.Bathroom) @? "bathroom is not a piece"

testIsWeaponCard_isWeapon :: Assertion
testIsWeaponCard_isWeapon = Model.isWeaponCard Model.Knife @? "knife is a weapon"

testIsWeaponCard_isNotWeapon :: Assertion
testIsWeaponCard_isNotWeapon = (not $ Model.isWeaponCard Model.Peacock) @? "peacock is not a weapon"

testIsRoomCard_isRoom :: Assertion
testIsRoomCard_isRoom = Model.isRoomCard Model.Garage @? "garage is a room"

testIsRoomCard_isNotRoom :: Assertion
testIsRoomCard_isNotRoom = (not $ Model.isRoomCard Model.Pipe) @? "pie is not a room"

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
    (Model.Plum, Model.Yes) @=? Model.setCardTuple Model.Plum (Model.Plum, Model.Unknown)

testSetCardTuple_notSetForDifferentCard :: Assertion
testSetCardTuple_notSetForDifferentCard =
    (Model.Plum, Model.Unknown) @=? Model.setCardTuple Model.Peacock (Model.Plum, Model.Unknown)

testClearCardTuple :: Assertion
testClearCardTuple =
    (Model.Plum, Model.No) @=? Model.clearCardTuple Model.Plum (Model.Plum, Model.Unknown)

testClearCardTuple_notClearForDifferentCard :: Assertion
testClearCardTuple_notClearForDifferentCard =
    (Model.Plum, Model.Unknown) @=? Model.clearCardTuple Model.Peacock (Model.Plum, Model.Unknown)

testSetCard_givenCardAndPlayerIsSet :: Assertion
testSetCard_givenCardAndPlayerIsSet =
    let result = Model.setCard playerName Model.Peacock (Model.fullPlayer playerName)
    in
        Model.Yes @=? Model.getCardStatus Model.Peacock result
    where playerName = "player"

testSetCard_givenCardAndWrongPlayerIsSet :: Assertion
testSetCard_givenCardAndWrongPlayerIsSet =
    let result = Model.setCard "differentPlayer" Model.Peacock (Model.fullPlayer "player")
    in
        Model.No @=? Model.getCardStatus Model.Peacock result
