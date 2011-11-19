module Main where

import Text.XVL

main::IO()
main = interact processXVL
       where processXVL input = case parseXVL input of
                                     Left parseError -> "Error: " ++ show parseError
                                     Right xvlDoc -> show xvlDoc