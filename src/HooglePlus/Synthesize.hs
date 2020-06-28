module HooglePlus.Synthesize(synthesize, envToGoal) where

import Database.Environment
import Database.Util
import qualified HooglePlus.Abstraction as Abstraction
import PetriNet.PNSolver
import HooglePlus.Refinement
import Synquid.Error
import Synquid.Logic
import Synquid.Parser
import Synquid.Pretty
import Synquid.Program
import Synquid.Resolver
import Synquid.Type
import Synquid.Util
import Types.Common
import Types.Environment
import Types.Experiments
import Types.Program
import Types.Solver
import Types.Type
import Types.TopDown

import Control.Applicative ((<$>))
import Control.Concurrent.Chan
--import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Time.Clock
import Data.Time.Format
import System.CPUTime
import System.Exit
import Text.Parsec.Indent
import Text.Parsec.Pos
import Text.Printf (printf)


updateEnvWithBoundTyVars :: RSchema -> Environment -> (Environment, RType)
updateEnvWithBoundTyVars (Monotype ty) env = (env, ty)
updateEnvWithBoundTyVars (ForallT x ty) env = updateEnvWithBoundTyVars ty (addTypeVar x env)

updateEnvWithSpecArgs :: RType -> Environment -> (Environment, RType)
updateEnvWithSpecArgs ty@(ScalarT _ _) env = (env, ty)
updateEnvWithSpecArgs (FunctionT x tArg tRes) env = updateEnvWithSpecArgs tRes $ addVariable x tArg $ addArgument x tArg env

envToGoal :: Environment -> String -> IO Goal
envToGoal env queryStr = do
  let transformedSig = "goal :: " ++ queryStr ++ "\ngoal = ??"
  let parseResult = flip evalState (initialPos "goal") $ runIndentParserT parseProgram () "" transformedSig
  case parseResult of
    Left parseErr -> putDoc (pretty $ toErrorMessage parseErr) >> putDoc empty >> error "uh oh"
    Right (funcDecl:decl:_) -> case decl of
      Pos _ (SynthesisGoal id uprog) -> do
        let Pos _ (FuncDecl _ sch) = funcDecl
        let goal = Goal id env sch uprog 3 $ initialPos "goal"
        let spec = runExcept $ evalStateT (resolveSchema (gSpec goal)) (initResolverState { _environment = env })
        case spec of
          Right sp -> return $ goal { gEnvironment = env, gSpec = sp }
          Left parseErr -> putDoc (pretty parseErr) >> putDoc empty >> exitFailure

      _ -> error "parse a signature for a none goal declaration"

synthesize :: SearchParams -> Goal -> Chan Message -> IO ()
synthesize searchParams goal messageChan = do
    let env''' = gEnvironment goal
    let (env'', monospec) = updateEnvWithBoundTyVars (gSpec goal) env'''
    let (env', destinationType) = updateEnvWithSpecArgs monospec env''

    let useHO = _useHO searchParams
    let rawSyms = env' ^. symbols
    let hoCands = env' ^. hoCandidates
    
    env <- do
      let syms = Map.filter (not . isHigherOrder . toMonotype) rawSyms
      return $
          env'
              {_symbols = Map.withoutKeys syms $ Set.fromList hoCands, _hoCandidates = []}

    -- used for figuring out which programs to filter (those without all arguments)
    let numArgs = length (Map.elems (env ^. arguments))

    -- start timing and print out how long it took
    start <- getCPUTime
    foo <- dfsTop env messageChan 3 (shape destinationType) numArgs
    end <- getCPUTime
    
    let diff = fromIntegral (end - start) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)

    return () 


-- 
-- start off calling dfs with an empty memoize map
--
dfsTop :: Environment -> Chan Message -> Int -> SType -> Int -> IO [String]
dfsTop env messageChan depth hole numArgs = flip evalStateT emptyComps $ do
  
  -- collect all the component types (which we might use to fill the holes)
  let components = Map.toList (env ^. symbols)

  -- map each hole ?? to a list of component types that unify with the hole
  unifiedFuncs <- getUnifiedFunctions env messageChan components hole :: StateT Comps IO [(Id, SType)]

  -- get the first valid program from each of the functions in unifiedFuncs
  -- fmap concat $ mapM getFirstValidProgram unifiedFuncs :: StateT Comps IO [String]
  fmap concat <$> mapM getFirstValidProgram unifiedFuncs :: StateT Comps IO [String]

  where
    getFirstValidProgram x = do 
                      sampleProgram <- dfs env messageChan depth x
                      let filtered = filter (filterParams numArgs) sampleProgram
                      unless (null filtered) (lift $ putStrLn $ head filtered)
                      return sampleProgram

-- 
-- determines if the result has the appropriate arguments given the number of args
-- 
filterParams :: Int -> String -> Bool
filterParams 0       _ = error "filterParams error: shouldn't have 0 args!" -- TODO maybe should be true here? 
filterParams 1       x = "arg0" `isInfixOf` x
filterParams numArgs x = isInfixOf ("arg" ++ (show (numArgs - 1))) x && filterParams (numArgs - 1) x

--
-- gets list of components/functions that unify with a given type
-- 
getUnifiedFunctions :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> StateT Comps IO [(Id, SType)]
getUnifiedFunctions envv messageChan xs goalType = do

  modify $ set components []

  st <- get
  let memoized = st ^. memoize :: Map SType [(Id, SType)]

  case Map.lookup goalType memoized of
    Just cs -> do
      return cs
    Nothing -> do
      helper envv messageChan xs goalType
      st <- get
      let cs = st ^. components
      modify $ set memoize (Map.insert goalType cs (st ^. memoize))
      return $ st ^. components

  where 
    helper :: Environment -> Chan Message -> [(Id, RSchema)] -> SType -> StateT Comps IO ()
    helper _ _ [] _ = return ()

    -- skip components with @@ or Nil -- TODO fix this so that we can use these things too
    helper envv messageChan ( v@(id, schema) : ys) goalType
      | isInfixOf "@@"      id = helper envv messageChan ys goalType
      | isInfixOf "Nil"     id = helper envv messageChan ys goalType
      | isInfixOf "Nothing" id = helper envv messageChan ys goalType
      | otherwise = do

        let initSolverState = emptySolverState { _messageChan = messageChan }
        let t1 = shape (lastType (toMonotype schema))
        let t2 = goalType

        st' <- execStateT (solveTypeConstraint envv t1 t2) initSolverState

        let sub =  st' ^. typeAssignment
        let checkResult = st' ^. isChecked

        let schema' = stypeSubstitute sub (shape $ toMonotype schema)

        st <- get
        if (checkResult) 
          then do
            modify $ set components ((id, schema') : st ^. components) 
          else return ()
        
        helper envv messageChan ys goalType

--
-- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
--
isGround :: SType -> Bool
isGround (FunctionT id arg0 arg1) = False
isGround _ = True

-- 
-- runs dfs of given depth and keeps trying to find complete programs (no filtering yet)
--
dfs :: Environment -> Chan Message -> Int -> (Id, SType) -> StateT Comps IO [String]
dfs _ _ 0 (id, schema) = do -- stop if depth is 0
  if (isGround schema) then return ["(" ++ id ++ ")"] else return []
dfs env messageChan depth (id, schema) = do
  
  -- check if schema is ground
  if (isGround schema) 
  then return ["(" ++ id ++ ")"]
  else do

    st <- get

    -- collect all the argument types (the holes ?? we need to fill)
    let args = allArgTypes schema

    -- collect all the component types (which we might use to fill the holes)
    let components = Map.toList (env ^. symbols)

    -- map each hole ?? to a list of component types that unify with the hole
    argUnifiedFuncs <- mapM (getUnifiedFunctions env messageChan components) args :: StateT Comps IO [[(Id, SType)]]

    -- recurse, solving each unified component as a goal, solution is a list of programs
    -- the first element of solutionsPerArg is the list of first argument solutions
    solutionsPerArg <- mapM (fmap concat . mapM (dfs env messageChan (depth - 1))) argUnifiedFuncs :: StateT Comps IO [[String]]

    -- each arg hole is a list of programs
    -- take cartesian product of args and prepend our func name
    -- to get the list of resulting programs solving our original goal
    -- the first element of programsPerArg is a list of programs that fit as first argument
    let programsPerArg = sequence solutionsPerArg :: [[String]]
    let formatFn args = "(" ++ intercalate " " (id:args) ++ ")" -- takes ["(a)","(b)"] to "(f (a) (b))"
    let finalResultList = map formatFn programsPerArg
    return finalResultList