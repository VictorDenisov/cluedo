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
        , testGroup "clearCard"
            [ testCase
                    "testClearCard_givenCardAndPlayerIsCleared"
                    testClearCard_givenCardAndPlayerIsCleared
            , testCase
                    "testClearCard_givenCardAndWrongPlayerIsCleared"
                    testClearCard_givenCardAndWrongPlayerIsCleared
            ]
        , testGroup "parseCardReply"
            [ testCase
                    "testParseCardReply_EmptyCard"
                    testParseCardReply_EmptyCard
            , testCase
                    "testParseCardReply_UnknownCard"
                    testParseCardReply_UnknownCard
            , testCase
                    "testParseCardReply_Peacock"
                    testParseCardReply_Peacock
            , testCase
                    "testParseCardReply_InvalidCard"
                    testParseCardReply_InvalidCard
            ]
        , testGroup "parseReply"
            [ testCase
                    "testParseReply_Valid"
                    testParseReply_Valid
            , testCase
                    "testParseReply_UnknownPlayer"
                    testParseReply_UnknownPlayer
            , testCase
                    "testParseReply_CardInvalid"
                    testParseReply_CardInvalid
            , testCase
                    "testParseReply_WrongTokenCount"
                    testParseReply_WrongTokenCount
            ]
        , testGroup "printReply"
            [ testCase
                    "testPrintReply"
                    testPrintReply
            ]
        , testGroup "isTurnEntry"
            [ testCase
                    "testIsTurnEntry_TurnEntry"
                    testIsTurnEntry_TurnEntry
            , testCase
                    "testIsTurnEntry_NotTurnEntry"
                    testIsTurnEntry_NotTurnEntry
            ]
        , testGroup "isAccusation"
            [ testCase
                    "testIsAccusation_Accusation"
                    testIsAccusation_Accusation
            , testCase
                    "testIsAccusation_NotAccusation"
                    testIsAccusation_NotAccusation
            ]
        , testGroup "printLogEntry"
            [ testCase
                "testPrintLogEntry_TurnEntry"
                testPrintLogEntry_TurnEntry
            , testCase
                "testPrintLogEntry_Accusation"
                testPrintLogEntry_Accusation
            ]
        , testGroup "cardsShowedTo"
            [ testCase
                "testCardsShowedTo_NonExistentPlayer"
                testCardsShowedTo_NonExistentPlayer
            , testCase
                "testCardsShowedTo_MePlayerIsAbsent"
                testCardsShowedTo_MePlayerIsAbsent
            ]
        ]
