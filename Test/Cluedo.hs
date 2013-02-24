module Test.Cluedo
where

import qualified Distribution.TestSuite as Cabal
import qualified Test.HUnit as Hunit
import qualified Test.HUnit.Lang as HuLang
import Test.HunitAdaptor

tests :: IO [Cabal.Test]
tests = return [ createTest "testComposeValues" testComposeValues
               ]

testComposeValues :: Hunit.Assertion
testComposeValues = Hunit.assertEqual "compose values" 3 3
