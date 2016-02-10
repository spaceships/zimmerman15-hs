module Zim14.Circuit where

import qualified Data.Bimap as B
import qualified Data.Map as M

type Ref = Int
type ID  = Int

data Op = Add Ref Ref
        | Sub Ref Ref
        | Mul Ref Ref
        | Const ID Int
        | Input ID
        deriving (Eq, Ord, Show)

data Circ = Circ { outRef    :: Ref
                 , inpRefs   :: M.Map ID Ref
                 , constRefs :: M.Map ID Ref
                 , refMap    :: M.Map Ref Op
                 } deriving (Show)

type TestCase = (String, Bool)
