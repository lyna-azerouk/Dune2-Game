module Mouse where

import SDL
import Data.Int
import Linear.Affine

{-

getMouseLocation :: Event -> Maybe [Int]
getMouseLocation event =
    case eventPayload event of
        MouseButtonEvent mouseButton ->
            Just $ foldr (\cor list -> list ++ [fromIntegral cor]) [] (mouseButtonEventPos mouseButton)
        _ -> Nothing

-}

getMouseLocation :: Event -> Maybe (V2 Int)
getMouseLocation event =
    case eventPayload event of
        MouseButtonEvent mouseButton ->
            let P coordinates = fmap fromIntegral (mouseButtonEventPos mouseButton) in
                Just coordinates
        _ -> Nothing