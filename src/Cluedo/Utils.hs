module Cluedo.Utils
where

import qualified Cluedo.Model as M
import Data.List (isPrefixOf)

generateCardCompletionList :: [M.Card] -> String -> (String, [String])
generateCardCompletionList allowedCards "" = ("", map show allowedCards)
generateCardCompletionList allowedCards s =
        if head s == ' '
            then (s, filter (not . (`elem` ws)) allowedCardsStrings)
            else ( drop (length (last ws)) s
                 , filter ((last ws) `isPrefixOf`) allowedCardsStrings)
    where
        ws = words line
        line = reverse s
        allowedCardsStrings = map show allowedCards
