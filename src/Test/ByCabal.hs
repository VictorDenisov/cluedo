module Test.ByCabal
where

import qualified Distribution.TestSuite as Cabal
import Test.HUnit
import Test.HunitAdaptor
import Test.AllTests

tests :: IO [Cabal.Test]
tests = return [ createTest "testCardCount" testCardCount
               , createTest "testIsPieceCard" testIsPieceCard_isPiece
               , createTest "testIsNotPieceCard" testIsPieceCard_isNotPiece
               , createTest "testIsWeaponCard" testIsWeaponCard_isWeapon
               , createTest "testIsNotWeaponCard" testIsWeaponCard_isNotWeapon
               , createTest "testIsRoomCard" testIsRoomCard_isRoom
               , createTest "testIsNotRoomCard" testIsRoomCard_isNotRoom
               , createTest "testParseCard" testParseCard_isCard
               , createTest "testParseCard_notCard" testParseCard_notCard
               , createTest "testPieces_fullPlayer" testPieces_fullPlayer
               , createTest "testWeapons_fullPlayer" testWeapons_fullPlayer
               , createTest "testRooms_fullPlayer" testRooms_fullPlayer
               ]

