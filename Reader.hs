import Control.Monad.Reader
import System.IO
import Dict hiding (lookup)

data Template = T String deriving (Show)
data Definition = D Template Template deriving (Show)
 
-- Our environment consists of an association list of named templates and
-- an association list of named variable values. 
data Environment = Env {templates::[(String,Template)],
                        variables::[(String,String)]} deriving (Show)
 
-- lookup a variable from the environment
lookupVar :: String -> Environment -> Maybe String
lookupVar name env = lookup name (variables env)
 
-- lookup a template from the environment
lookupTemplate :: String -> Environment -> Maybe Template
lookupTemplate name env = lookup name (templates env)
 
-- add a list of resolved definitions to the environment
addDefs :: [(String,String)] -> Environment -> Environment
addDefs defs env = env {variables = defs ++ (variables env)}
 
-- resolve a Definition and produce a (name,value) pair
resolveDef :: Definition -> Reader Environment (String,String)
resolveDef (D t d) = do name <- resolve t
                        value <- resolve d
                        return (name,value)
 
-- resolve a template into a string
resolve :: Template -> Reader Environment (String)
resolve (T s)    =  return s

-- Another try to understand this....

data Conf = Conf { state :: IO Dict }

getState :: Reader Conf (IO Dict)
getState = do conf <- ask
              return $ state conf



