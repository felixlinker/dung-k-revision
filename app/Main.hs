module Main where

import Example.StableMarriage
import Control.Monad
import Control.Applicative
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP
import Data.Function
import Data.Tuple
import Util
import Parser
import Help
import qualified Language.Dung.Logics.Stb as Stb
import qualified Language.Dung.AF as AF
import qualified Data.Map.Strict as Map

afCmdParser :: String -> Maybe (String, AF.DungAF Int)
afCmdParser = (fmap fst) . maybeLast . readP_to_S parseAf

opLookup :: Context Int -> Maybe (String, String, String) -> Maybe (String, AF.DungAF Int, AF.DungAF Int)
opLookup ctx names = do
    (returnName, fName, gName) <- names
    f <- Map.lookup fName ctx
    g <- Map.lookup gName ctx
    return (returnName, f, g)

opCmdParser :: Context Int -> String -> Maybe (String, AF.DungAF Int, AF.DungAF Int)
opCmdParser ctx = (opLookup ctx) . (fmap fst) . maybeLast . readP_to_S parseOp

headParser :: Context Int -> String -> Maybe String
headParser ctx = fmap fst . maybeLast . readP_to_S parseHead

headCmdParser :: Context Int -> String -> Maybe (AF.DungAF Int)
headCmdParser ctx = (>>= (`Map.lookup` ctx)) . headParser ctx

quitCmd :: Cmd
quitCmd ctx _ = (ctx, "", False)

afCmd :: Cmd
afCmd ctx l = case afCmdParser l of
    Nothing -> (ctx, "couldn't parse af", True)
    Just (name, af) -> (Map.insert name af ctx, show af, True)

expCmd :: Cmd
expCmd ctx l = case opCmdParser ctx l of
    Nothing -> (ctx, "couldn't parse operands", True)
    Just (rName, f, g) ->   let r = f Stb.+ g
                            in (insertMaybe rName r ctx, show r, True)

revCmd :: Cmd
revCmd ctx l = case opCmdParser ctx l of
    Nothing -> (ctx, "couldn't parse operands", True)
    Just (rName, f, g) ->   let r = f Stb.* g
                            in (Map.insert rName r ctx, show r, True)

showCmd :: Cmd
showCmd ctx l = case headCmdParser ctx l of
    Nothing -> (ctx, "no such af", True)
    Just af -> (ctx, show af, True)

helpCmd :: Cmd
helpCmd ctx l = case headParser ctx l of
    Nothing -> (ctx, helpAll, True)
    Just cmd -> (ctx, help [cmd], True)

cmds :: Map.Map String Cmd
cmds = Map.fromList [ ("quit", quitCmd)
                    , ("q", quitCmd)
                    , ("af", afCmd)
                    , ("exp", expCmd)
                    , ("rev", revCmd)
                    , ("show", showCmd)
                    , ("help", helpCmd)
                    , ("h", helpCmd)
                    ]

main :: IO ()
main = mainLoop Map.empty

mainLoop :: CmdContext -> IO ()
mainLoop ctx = do
    l <- getLine
    (ctx', message, continue) <-    let (cmd, r) = cmdLookup . cmdParser $ l
                                    in eval cmd ctx r
    unless (null message) (putStrLn message)
    when continue $ mainLoop ctx'

cmdParser :: String -> Maybe (String, String)
cmdParser = maybeLast . readP_to_S parseCmd

cmdLookup :: Maybe (String, String) -> (Maybe Cmd, String)
cmdLookup Nothing = (Nothing, "")
cmdLookup (Just (cmd, r)) = (Map.lookup cmd cmds, r)

eval :: Maybe Cmd -> CmdContext -> String -> IO CmdRet
eval Nothing c = \_ -> return (c, "couldn't parse", True)
eval (Just f) c = return . f c
