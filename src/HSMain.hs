{-# LANGUAGE ForeignFunctionInterface #-}

module HSMain where

import Main

foreign export ccall my_hs_main :: IO ()

my_hs_main :: IO ()
my_hs_main = main
