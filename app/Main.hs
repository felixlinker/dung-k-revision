module Main where

import Lib.StableMarriage
import Lib.Stb
import qualified Language.Dung.AF as AF

main :: IO ()
main = do
    let mPrefs  = [[0,1,2], [0,2,1], [0,1,2]]
        wPrefs  = [[0,2,1], [0,1,2], [0,1,2]]
        mPrefs' = [[0,2,1,3], [0,1,2,3], [3,0,2,1], [0,3,2,1]]
        wPrefs' = [[0,1,2,3], [0,2,1,3], [3,0,2,1], [3,2,0,1]]
        af = fromListsToAF mPrefs wPrefs
        af' = fromListsToAF mPrefs' wPrefs'
    putStrLn $ "AF:  " ++ (show $ solve mPrefs wPrefs)
    putStrLn $ "AF': " ++ (show $ solve mPrefs' wPrefs')
    putStrLn $ "*^k: " ++ (show $ AF.stableF $ revision af' af)