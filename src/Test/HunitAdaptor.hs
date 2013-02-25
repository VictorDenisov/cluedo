module Test.HunitAdaptor
where
import qualified Distribution.TestSuite as Cabal
import qualified Test.HUnit as Hunit
import qualified Test.HUnit.Lang as HuLang

stripNewLine :: Char -> Char
stripNewLine '\n' = ' '
stripNewLine c = c

testRunner :: Hunit.Assertion -> IO Cabal.Progress
testRunner t = do
    v <- HuLang.performTestCase t
    case v of
        Nothing -> return $ Cabal.Finished Cabal.Pass
        Just (True, msg) -> return $ Cabal.Finished $ Cabal.Fail $ map stripNewLine msg
        Just (False, msg) -> return $ Cabal.Finished $ Cabal.Error $ map stripNewLine msg

createTest :: String -> Hunit.Assertion -> Cabal.Test
createTest name t = Cabal.Test $ Cabal.TestInstance (testRunner t) name [] [] undefined
