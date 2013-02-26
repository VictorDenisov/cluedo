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
