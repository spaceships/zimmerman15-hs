module Zim14.Circuit.Parser
  ( parseCirc
  ) where

import Zim14.Circuit

import Control.Monad (when)
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

data ParseSt = ParseSt { st_circ :: Circuit
                       , st_ts   :: [TestCase]
                       , st_ys   :: M.Map ID Integer
                       }

type ParseCirc = Parsec String ParseSt

getCirc :: ParseCirc Circuit
getCirc = st_circ <$> getState

getTests :: ParseCirc [TestCase]
getTests = st_ts <$> getState

getConsts :: ParseCirc (M.Map Ref Integer)
getConsts = st_ys <$> getState

modifyCirc :: (Circuit -> Circuit) -> ParseCirc ()
modifyCirc f = modifyState (\st -> st { st_circ = f (st_circ st) })

addTest :: TestCase -> ParseCirc ()
addTest t = modifyState (\st -> st { st_ts = t : st_ts st})

insertConst :: ID -> Integer -> ParseCirc ()
insertConst id c = modifyState (\st -> st { st_ys = M.insert id c (st_ys st)})

parseCirc :: String -> (Circuit, [TestCase])
parseCirc s = case runParser (circParser >> getState) emptySt "" s of
    Left err -> error (show err)
    Right st -> let ys = map snd $ M.toAscList (st_ys st)
                in ((st_circ st) { consts = ys }, reverse (st_ts st))
  where
    circParser = init >> rest >> eof
    init = many $ choice [parseParam, parseTest]
    rest = many $ choice [try parseGate, try parseInput]
    emptyCirc = Circuit (-1) M.empty M.empty []
    emptySt   = ParseSt emptyCirc [] M.empty

parseParam :: ParseCirc ()
parseParam = do
    char ':'
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseTest :: ParseCirc ()
parseTest = do
    string "# TEST"
    spaces
    inps <- many (oneOf ['0','1'])
    spaces
    out <- oneOf ['0','1']
    let inp = map (read . (:[])) inps
        res = (read . (:[])) out
    addTest (inp, res)
    endLine

parseInput :: ParseCirc ()
parseInput = do
    gateRef <- read <$> many1 digit
    spaces
    string "input"
    spaces
    parseX gateRef <|> parseY gateRef
    endLine

parseX :: Ref -> ParseCirc ()
parseX ref = do
    char 'x'
    id <- read <$> many1 digit
    insertOp ref (Input id)
    refs <- inpRefs <$> getCirc
    let inpRefs' = safeInsert ("redefinition of x" ++ show id) id ref refs
    modifyCirc (\c -> c { inpRefs = inpRefs' })

parseY :: Ref -> ParseCirc ()
parseY ref = do
    char 'y'
    id  <- read <$> many1 digit
    spaces
    val <- read <$> many1 digit
    insertOp ref (Const id)
    insertConst id val

parseGate :: ParseCirc ()
parseGate = do
    ref <- read <$> many1 digit
    spaces
    gateType <- oneOfStr ["gate", "output"]
    when (gateType == "output") $ do
        c <- getCirc
        if outRef c > 0 then
            error ("multiple outputs defined! ref" ++ show ref)
        else
            modifyCirc (\c -> c { outRef = ref })
    spaces
    opType <- oneOfStr ["ADD", "SUB", "MUL"]
    spaces
    xref <- read <$> many1 digit
    spaces
    yref <- read <$> many1 digit
    let op = case opType of
            "ADD" -> Add xref yref
            "MUL" -> Mul xref yref
            "SUB" -> Sub xref yref
    insertOp ref op
    endLine

safeInsert :: Ord a => String -> a -> b -> M.Map a b -> M.Map a b
safeInsert errorMsg x y map =
    if M.member x map
       then error errorMsg
       else M.insert x y map

oneOfStr :: [String] -> ParseCirc String
oneOfStr = foldr (\s m -> string s <|> m) (fail "no strings")

spaces :: ParseCirc ()
spaces = skipMany (oneOf " \t")

endLine :: ParseCirc ()
endLine = do
    skipMany (char ' ')
    eof <|> (endOfLine >> return ())
    return ()

insertOp :: Ref -> Op -> ParseCirc ()
insertOp ref op = do
    refs <- refMap <$> getCirc
    if M.member ref refs
        then error ("redefinition of ref " ++ show ref)
        else modifyCirc (\c -> c { refMap = M.insert ref op refs })
