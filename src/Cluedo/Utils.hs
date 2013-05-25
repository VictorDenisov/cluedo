module Cluedo.Utils
where

import qualified Cluedo.Model as M
import Data.List (isPrefixOf)

generateCardCompletionList :: [M.Card] -> String -> [String]
generateCardCompletionList allowedCards "" = map show allowedCards
generateCardCompletionList allowedCards s =
        if head s == ' '
            then filter (not . (`elem` ws)) allowedCardsStrings
            else filter ((last ws) `isPrefixOf`) allowedCardsStrings
    where
        ws = words line
        line = reverse s
        allowedCardsStrings = map show allowedCards

