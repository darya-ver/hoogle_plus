{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, LambdaCase #-}
module InternalTypeGen where

import Data.List (isInfixOf, nub, reverse)

import qualified Test.LeanCheck.Function.ShowFunction as SF
import qualified Test.ChasingBottoms as CB
import qualified Test.SmallCheck.Series as SS

defaultShowFunctionDepth = 4 :: Int

instance Eq a => Eq (CB.Result a) where
  (CB.Value a) == (CB.Value b) = a == b
  CB.NonTermination == CB.NonTermination = True
  (CB.Exception _) == (CB.Exception _) = True
  _ == _ = False

isFailedResult result = case result of
  CB.NonTermination -> True
  CB.Exception _ -> True
  CB.Value a | "_|_" `isInfixOf` a -> True
  CB.Value a | "Exception" `isInfixOf` a -> True
  _ -> False

newtype Inner a = Inner a deriving (Eq)
instance SS.Serial m a => SS.Serial m (Inner a) where series = SS.newtypeCons Inner
instance (SF.ShowFunction a) => Show (Inner a) where
  show (Inner fcn) = SF.showFunctionLine defaultShowFunctionDepth fcn

formOutput :: [String] -> CB.Result String -> String
formOutput args ret = unwords args ++ " ==> " ++ (showCBResult ret)

printDupResult :: [String] -> [CB.Result String] -> IO ()
printDupResult args rets = (putStrLn . show) result
  where result = (unwords args, map showCBResult rets) :: (String, [String])

showCBResult :: CB.Result String -> String
showCBResult = \case
                  CB.Value a | "_|_" `isInfixOf` a -> "bottom"
                  CB.Value a -> a
                  CB.NonTermination -> "diverge"
                  CB.Exception ex -> show ex

anyDuplicate :: Eq a => [a] -> Bool
anyDuplicate xs = length (nub xs) /= length xs

showFunc :: SF.ShowFunction a => a -> String
showFunc fcn = "(" ++ trimed ++ ")"
  where
    str = SF.showFunctionLine defaultShowFunctionDepth fcn
    trimed = case (reverse str) of
              ('.':'.':'.':' ':xs) -> reverse xs
              _ -> str
