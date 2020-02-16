module Main where

import           Lib
import           Control.Monad.Reader           ( Reader
                                                , ask
                                                , runReader
                                                )
import           Prelude                 hiding ( div )

main :: IO ()
main = putStrLn "what is your email address?" >> getLine >>= \email ->
    putStrLn . show $ runReader view email

view :: Reader Email Html
view = page >>= \page' -> return $ div [page']

page :: Reader Email Html
page = content >>= \content' -> return $ div [topNav, content']

topNav :: Html
topNav = div [h1 ["OurSite.com"]]

content :: Reader Email Html
content = ask >>= \email -> right >>= \right' ->
    return $ div [h1 ["Custom Content for " ++ email], left, right']

left :: Html
left = div [p ["this is the left side"]]

right :: Reader Email Html
right = article >>= \article' -> return $ div [article']

article :: Reader Email Html
article =
    widget >>= \widget' -> return $ div [p ["this is an article"], widget']

widget :: Reader Email Html
widget = ask >>= \email -> return
    $ div [p ["Hey " ++ email ++ ", we've got a great offer for you!"]]

