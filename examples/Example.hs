module Main (main) where

import Bindings.Netcode.IO (c'netcode_init, c'netcode_term)

main :: IO ()
main = do
    c'netcode_init
    c'netcode_term
    putStrLn "Success!"