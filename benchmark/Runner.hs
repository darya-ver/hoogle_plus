module Runner where

import BTypes
import BConfig
import Types.Program
import Types.Environment
import Types.Experiments
import Synquid.Util
import HooglePlus.Synthesize
import Database.Environment

import System.Timeout
import Control.Concurrent.Chan
import Control.Concurrent.ParallelIO.Local
import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import GHC.Conc (getNumCapabilities)
import GHC.Exception
import Data.Ratio ((%))

runExperiments :: [Experiment] -> IO [ResultSummary]
runExperiments exps = do
  maxThreads <- getNumCapabilities
  let threads = (ceiling ((fromIntegral (maxThreads * 3)) % (fromIntegral 4))) -- Use about 3/4 of the cores
  eitherMbResults <- withPool threads (runPool exps)
  return $ map summarizeResult (zip exps eitherMbResults)

runPool :: [Experiment] -> Pool -> IO [Either SomeException (Maybe [(RProgram, TimeStatistics)])]
runPool exps pool = do
  parallelE pool (listOfExpsToDo exps)
  where
    listOfExpsToDo :: [Experiment] -> [IO (Maybe [(RProgram, TimeStatistics)])]
    listOfExpsToDo = map runExperiment

runExperiment :: Experiment -> IO (Maybe [(RProgram, TimeStatistics)])
runExperiment (env, envName, q, params, paramName) = do
  let queryStr = query q
  goal <- envToGoal env queryStr
  timeout defaultTimeoutus $ synthesize params goal

summarizeResult :: (Experiment, Either SomeException (Maybe [(RProgram, TimeStatistics)])) -> ResultSummary
summarizeResult ((_, envN, q, _, paramN), r) = let
  results = case r of
    Left err -> Left (RuntimeException err)
    Right (Nothing) -> Left TimeoutException
    Right (Just results) -> let
      safeTransitions = snd $ errorhead "missing transitions" $ (Map.toDescList (numOfTransitions firstR))
      safeTypes = snd $ errorhead "missing types" $ (Map.toDescList (numOfPlaces firstR))
      soln = mkOneLine (show solnProg)
      (solnProg, firstR) =  errorhead "missing first solution" results
      in Right Result {
      resSolution = soln,
      resTFirstSoln = totalTime firstR,
      resTEncFirstSoln = encodingTime firstR,
      resLenFirstSoln = pathLength firstR,
      resRefinementSteps = iterations firstR,
      resTransitions = safeTransitions,
      resTypes = safeTypes
      }
  in ResultSummary {
    envName = envN,
    paramName = paramN,
    queryName = name q,
    queryStr = query q,
    result = results
    }
  where
    errorhead msg xs = fromMaybe (error msg) $ listToMaybe xs
