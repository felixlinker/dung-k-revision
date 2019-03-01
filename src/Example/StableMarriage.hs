module Example.StableMarriage
    ( problemFromLists
    , problemToAF
    , fromListsToAF
    , solve
    ) where

import qualified Language.Dung.AF as AF
import qualified Data.Set as Set

data Preferences a b = Prefs a [b]
type StbMrgList = [[Int]]
type StbMrgPreferences = Preferences Int Int
type StbMrgProblem = ([StbMrgPreferences], [StbMrgPreferences])
type StbMrgAF = AF.DungAF (Int, Int)

problemFromLists :: StbMrgList -> StbMrgList -> StbMrgProblem
problemFromLists mPrefs wPrefs = (mkPrefs mPrefs, mkPrefs wPrefs) where
    mkPrefs :: StbMrgList -> [StbMrgPreferences]
    mkPrefs prefs = map (uncurry Prefs) $ zip [0,1..] prefs

problemToAF :: StbMrgProblem -> StbMrgAF
problemToAF (mPrefs, wPrefs) =
    let args = [ (m, w) | (Prefs m _) <- mPrefs, (Prefs w _) <- wPrefs ]
        atts = (foldToAtts mPrefs) ++ (map swapTuples $ foldToAtts wPrefs)
    in (AF.AF args atts) where
        toAtts :: StbMrgPreferences -> [((Int, Int), (Int, Int))]
        toAtts (Prefs _ []) = []
        toAtts (Prefs x (p:ps)) = [ ((x,p), (x,p')) | p' <- ps ] ++ toAtts (Prefs x ps)
        foldToAtts :: [StbMrgPreferences] -> [((Int, Int), (Int, Int))]
        foldToAtts ps = foldl (++) [] $ map toAtts ps
        swapTuples :: ((a, a), (a, a)) -> ((a, a), (a, a))
        swapTuples ((a, b), (c, d)) = ((b, a), (d, c))

fromListsToAF :: StbMrgList -> StbMrgList -> StbMrgAF
fromListsToAF = curry $ problemToAF . (uncurry problemFromLists)

solve :: StbMrgList -> StbMrgList -> [[(Int, Int)]]
solve = curry $ AF.stableF . (uncurry fromListsToAF)
