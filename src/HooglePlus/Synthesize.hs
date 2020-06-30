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
import PetriNet.Util -- TODO can we do this? lol 

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
    printf "running dfsTop on %s\n" (show $ shape destinationType)
    foo <- dfsTop env messageChan 3 (shape destinationType) numArgs
    
    printf "done running dfsTop on %s\n" (show $ shape destinationType)

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
  -- lift $ printf "here1"
  -- map each hole ?? to a list of component types that unify with the hole
  unifiedFuncs <- getUnifiedFunctions env messageChan components hole :: StateT Comps IO [(Id, SType)]
  -- lift $ printf "here2"
  -- get the first valid program from each of the functions in unifiedFuncs
  -- fmap concat $ mapM getFirstValidProgram unifiedFuncs :: StateT Comps IO [String]
  fmap concat <$> mapM getFirstValidProgram (take 13 unifiedFuncs) :: StateT Comps IO [String]
  where
    getFirstValidProgram x = do 
                      sampleProgram <- map show <$> dfs env messageChan depth x
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
    -- helper envv messageChan ( v@(id, schema) : ys) goalType
      -- | isInfixOf "@@"      id = helper envv messageChan ys goalType
      -- | isInfixOf "Nil"     id = helper envv messageChan ys goalType
      -- | isInfixOf "Nothing" id = helper envv messageChan ys goalType
      -- | otherwise = do
    helper envv messageChan ( v@(id, schema) : ys) goalType = do

        let initSolverState = emptySolverState { _messageChan = messageChan }
-----------------------

        -- let t1 = shape (lastType (toMonotype schema)) :: SType
        -- let t2 = goalType :: SType

        -- st' <- execStateT (solveTypeConstraint envv t1 t2) initSolverState

        -- let sub =  st' ^. typeAssignment
        -- let checkResult = st' ^. isChecked

        -- let schema' = stypeSubstitute sub (shape $ toMonotype schema)

        -- st <- get
        -- if (checkResult) 
        --   then do
        --     modify $ set components ((id, schema') : st ^. components) 
        --   else return ()
        
-----------------------
---------------
        (freshVars, solverState) <- runStateT (freshType schema) initSolverState
        let t1 = shape (lastType freshVars) -- turn it into an SType of the return value
        let t2 = goalType :: SType
        
        st' <- execStateT (solveTypeConstraint envv t1 t2) solverState
        let sub =  st' ^. typeAssignment
        let checkResult = st' ^. isChecked

        let schema' = stypeSubstitute sub (shape freshVars)

        st <- get
        if (checkResult) 
          then do
            modify $ set components ((id, schema') : st ^. components) 
          else return ()
        helper envv messageChan ys goalType
---------------


        -- helper envv messageChan ys goalType

        -- let t1 = shape (lastType (toMonotype schema)) :: SType -- (a0 -> b0)
        -- let t2 = goalType :: SType

        -- -- | Type skeletons (parametrized by refinements)
        -- data TypeSkeleton r =
        -- ScalarT (BaseType r) r |
        -- FunctionT Id (TypeSkeleton r) (TypeSkeleton r) |
        
        -- -- | Replace all bound type variables with fresh free variables
        -- freshType :: MonadIO m => RSchema -> PNSolver m RType
        -- freshType = freshType' Map.empty []
        --   where
        --     freshType' subst constraints (ForallT a sch) = do
        --         a' <- freshId "A"
        --         freshType' (Map.insert a (vart a' ftrue) subst) (a':constraints) sch
        --     freshType' subst constraints (Monotype t) = return (typeSubstitute subst t)

        --
        -- forall a0 (forall b (\(a,b) -> (b,a)))

        -- -- | Unrefined typed
        -- type SType = TypeSkeleton ()

        -- you were trying to unify t1:`(b, a)` with t2:`(a, b)`
        -- 
        -- let solver :: (MonadIO m) => PNSolver m (Bool, SType)
        --     solver = do
        --       freshVars <- freshType schema :: (MonadIO m) => PNSolver m RType
        --       let t1 = shape (lastType freshVars) -- turn it into an SType of the return value
        --       let t2 = goalType :: SType
        --       solveTypeConstraint envv t1 t2
        --       st' <- get
        --       let sub = st' ^. typeAssignment -- (a0 -> Int) the unified stuff (a -> a0) (a0 -> Int)
        --       let checkResult = st' ^. isChecked
        --       -- the rschema version of stypeSubstitute
        --       let schema' = stypeSubstitute sub (shape freshVars) :: SType
        --       return (checkResult, schema')

        -- (freshVars, solverState) <- runStateT (freshType schema) initSolverState

        -- -- solveTypeConstraint envv t1 t2
        -- st' <- get

        -- let sub = st' ^. typeAssignment -- (a0 -> Int) the unified stuff (a -> a0) (a0 -> Int)
        -- let checkResult = st' ^. isChecked
        -- -- the rschema version of stypeSubstitute
        -- let schema' = stypeSubstitute sub (shape freshVars) :: SType

        -- -- let (checkResult, schema') = evalStateT solver initSolverState :: _


        -- -- st' <- execStateT (solveTypeConstraint envv t1 t2) initSolverState

        -- let sub =  st' ^. typeAssignment -- (a0 -> Int) the unified stuff (a -> a0) (a0 -> Int)
        -- let checkResult = st' ^. isChecked
        -- -- the rschema version of stypeSubstitute
        -- let schema' = stypeSubstitute sub (shape $ toMonotype schema)

        -- st <- get
        -- if (checkResult) 
        --   then do
        --     modify $ set components ((id, schema') : st ^. components) 
        --   else return ()
        
        -- helper envv messageChan ys goalType

-----------------------

        

-----------------------

--
-- checks if a program is ground (has no more arguments to synthesize - aka function w/o args)
--
isGround :: SType -> Bool
isGround (FunctionT id arg0 arg1) = False
isGround _ = True

-- 
-- runs dfs of given depth and keeps trying to find complete programs (no filtering yet)
--
dfs :: Environment -> Chan Message -> Int -> (Id, SType) -> StateT Comps IO [RProgram]
dfs env messageChan depth (id, schema)
  | isGround schema = return $ [
      Program { content = PSymbol id, typeOf = refineTop env schema }
    ]
  | depth == 0 = return []  -- stop if depth is 0
  | otherwise = do

    st <- get


    -- collect all the argument types (the holes ?? we need to fill)
    let args = allArgTypes schema

    -- collect all the component types (which we might use to fill the holes)
    let components = Map.toList (env ^. symbols)

    -- map each hole ?? to a list of component types that unify with the hole
    argUnifiedFuncs <- mapM (getUnifiedFunctions env messageChan components) args :: StateT Comps IO [[(Id, SType)]]

    -- recurse, solving each unified component as a goal, solution is a list of programs
    -- the first element of solutionsPerArg is the list of solutions for the first argument
    -- e.g. 
    let recurse = dfs env messageChan (depth - 1)
    solutionsPerArg <- mapM (fmap concat . mapM recurse) argUnifiedFuncs :: StateT Comps IO [[RProgram]] -- [[a,b,c], [d,e,f]]
    
    -- each arg hole is a list of programs
    -- take cartesian product of args and prepend our func name
    -- to get the list of resulting programs solving our original goal
    -- the first element of programsPerArg is a list of programs that fit as first argument
    let programsPerArg = sequence solutionsPerArg :: [[RProgram]] -- [[a,d], [a,e], [a,f], [b,d], [b,e], [b,f], ...]
    let formatFn :: [RProgram] -> RProgram
        formatFn args = Program { content = PApp id args, typeOf = refineTop env schema }
    let finalResultList = map formatFn programsPerArg
    return finalResultList

    --  intercalate ", " ["Lorem", "ipsum", "dolor"]
    --   "Lorem, ipsum, dolor"

-- 
-- converts final String program into RProgram
--
-- example:
--      (fst (Pair (arg0) (arg0)))      ->       PApp "fst" [(PApp "Pair" [(PSymbol "arg0"), (PSymbol "arg0")]

-- mkProgram :: BareProgram RType -> Program RType
-- mkProgram bp = Program {
--     content = bp,
--     typeOf = AnyT
--   }

-- toRProgram :: String -> Program RType
-- toRProgram _      = mkProgram $ PApp "fst" [mkProgram (PApp "Pair" [mkProgram $ PSymbol "arg0"], mkProgram (PSymbol "arg0"))]
-- -- toRProgram (x:xs) = undefined

-- -- i put this in 'synthesize' above so it runs instead of doing synthesis
-- testToRProgram :: IO ()
-- testToRProgram = do
--   let inputType = AnyT
--   putStrLn $ show $ toRProgram "(fst (Pair (arg0) (arg0)))"
  
-- -- | Type skeletons (parametrized by refinements)
-- data TypeSkeleton r =
--   ScalarT (BaseType r) r |
--   FunctionT Id (TypeSkeleton r) (TypeSkeleton r) |
--   AnyT |
--   BotT 
--   deriving (Eq, Ord, Generic)

-- -- | Unrefined typed
-- type SType = TypeSkeleton ()

-- -- | Refined types
-- type RType = TypeSkeleton Formula

  
-- data BareProgram t =
--   PSymbol Id |                      -- ^ Symbol (variable or constant)
--   PApp Id [Program t] |              -- ^ Function application

-- data Program t = Program {
--   content :: BareProgram t,
--   typeOf :: t
-- }

-- type RProgram = Program RType
