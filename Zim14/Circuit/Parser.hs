module Zim14.Circuit.Parser
  ( parseCirc
  ) where

import Zim14.Circuit

import Control.Arrow (first, second)
import Control.Monad (when)
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

type ParseCirc = Parsec String (Circ, [TestCase])

getCirc :: ParseCirc Circ
getCirc = fst <$> getState

getTests :: ParseCirc [TestCase]
getTests = snd <$> getState

modifyCirc :: (Circ -> Circ) -> ParseCirc ()
modifyCirc = modifyState . first

addTest :: TestCase -> ParseCirc ()
addTest t = modifyState $ second (t :)

parseCirc :: String -> (Circ, [TestCase])
parseCirc s = case runParser (circParser >> getState) (emptyCirc, []) "" s of
    Left err      -> error (show err)
    Right (c, ts) -> (c, reverse ts)
  where
    circParser = init >> rest >> eof
    init = many $ choice [parseParam, parseTest]
    rest = many $ choice [try parseGate, try parseInput]
    emptyCirc  = Circ (-1) M.empty M.empty M.empty

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
    let inp = map toBool inps
        res = toBool out
    addTest (inp, res)
    endLine
  where
    toBool c = (read [c] :: Int) > 0

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
    insertOp ref (Const id val)
    refs <- constRefs <$> getCirc
    let refs' = safeInsert ("redefinition of y" ++ show id) id ref refs
    modifyCirc (\c -> c { constRefs = refs' })

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
