import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.AllTests

import Test.HUnit

main = defaultMain tests

tests = [ testGroup "cardCount"
            [ testCase "cardCount" testCardCount
            ]
        , testGroup "isPieceCard"
            [ testCase "isPieceCard_isPiece" testIsPieceCard_isPiece
            , testCase "isPieceCard_isNotPiece" testIsPieceCard_isNotPiece
            ]
        , testGroup "isWeaponCard"
            [ testCase "isWeaponCard_isWeapon" testIsWeaponCard_isWeapon
            , testCase "isWeaponCard_isNotWeapon" testIsWeaponCard_isNotWeapon
            ]
        , testGroup "isRoomCard"
            [ testCase "isRoomCard_isWeapon" testIsRoomCard_isRoom
            , testCase "isRoomCard_isNotWeapon" testIsRoomCard_isNotRoom
            ]
        , testGroup "parseCard"
            [ testCase "isCard" testParseCard_isCard
            , testCase "isNotCard" testParseCard_notCard
            ]
        , testGroup "full player"
            [ testCase "testPieces_fullPlayer" testPieces_fullPlayer
            , testCase "testWeapons_fullPlayer" testWeapons_fullPlayer
            , testCase "testRooms_fullPlayer" testRooms_fullPlayer
            ]
        , testGroup "getCardStatus"
            [ testCase "testGetCardStatus" testGetCardStatus
            , testCase "testGetPeacockStatus" testGetPeacockStatus
            ]
        , testGroup "getSetCard"
            [ testCase
                    "testSetCard_givenCardAndPlayerIsSet"
                    testSetCard_givenCardAndPlayerIsSet
            , testCase
                    "testSetCard_givenCardAndWrongPlayerIsSet"
                    testSetCard_givenCardAndWrongPlayerIsSet
            ]
        ]
