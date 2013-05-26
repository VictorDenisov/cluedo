module Cluedo.Utils
where

import qualified Cluedo.Model as M
import Data.List (isPrefixOf)

generateCardCompletionList :: Int -> [M.Card] -> String -> (String, [String])
generateCardCompletionList count allowedCards "" | count <= 0 = ("", [])
generateCardCompletionList count allowedCards "" = ("", map show allowedCards)
generateCardCompletionList count allowedCards s =
        if head s == ' '
            then ( s
                 , if length ws < count
                    then notPresentCards
                    else []
                 )
            else ( drop (length (last ws)) s
                 , if length ws <= count
                    then filter ((last ws) `isPrefixOf`) notPresentCards
                    else []
                 )
    where
        ws = words line
        line = reverse s
        allowedCardsStrings = map show allowedCards
        notPresentCards = filter (not . (`elem` ws)) allowedCardsStrings
