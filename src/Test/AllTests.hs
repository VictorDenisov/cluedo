module Test.AllTests
where

import qualified Distribution.TestSuite as Cabal
import Test.HUnit
import Test.HunitAdaptor

import qualified Cluedo.Model as Model

tests :: IO [Cabal.Test]
tests = return [ createTest "testCardCount" testCardCount
               , createTest "testIsPieceCard" testIsPieceCard_isPiece
               , createTest "testIsNotPieceCard" testIsPieceCard_isNotPiece
               ]

testCardCount :: Assertion
testCardCount = 21 @=? Model.cardCount

testIsPieceCard_isPiece :: Assertion
testIsPieceCard_isPiece = (Model.isPieceCard Model.White) @? "white is a piece"

testIsPieceCard_isNotPiece :: Assertion
testIsPieceCard_isNotPiece = (not $ Model.isPieceCard Model.Bathroom) @? "bathroom is not a piece"
