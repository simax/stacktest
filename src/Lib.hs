module Lib where

import           Data.List                      ( intercalate )
import           Prelude                 hiding ( div )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Html = String
type Email = String

div :: [Html] -> Html
div children = "<div>" ++ combine children ++ "</div>"

h1 :: [Html] -> Html
h1 children = "<h1>" ++ combine children ++ "</h1>"

p :: [Html] -> Html
p children = "<p>" ++ combine children ++ "</p>"

combine :: [Html] -> Html
combine = intercalate ""
