module OptParse where

import Debug
import Char (isAscii)
import List (intercalate)
import qualified DetParse as P
import Pretty
import qualified Boxes as B

data Arg = Flag String
         | Val  String
         | FlagWithValue String String

data Parser a = OptP ArgProps OptProps (String -> a)
              | FlagP ArgProps OptProps a
              | ArgP ArgProps (String -> a)
              | RestP ArgProps (String -> a)
              | CmdP ArgProps [(String, ArgProps, a, ParseSpec a)]

data ParseSpec a = ParseSpec [Parser a]

data ArgProps = ArgProps
  { metavarName :: String
  , helpText :: Maybe String 
  , argOptional :: Bool }

data OptProps = OptProps
  { longName :: String
  , shortName :: String }

data Mod = Mod 
  { optMod :: OptProps -> OptProps
  , argMod :: ArgProps -> ArgProps }

argProps :: Parser a -> ArgProps
argProps (OptP a _ _) = a
argProps (FlagP a _ _) = a
argProps (ArgP a _) = a
argProps (RestP a _) = a
argProps (CmdP a _) = a

idm :: Mod
idm = Mod id id

defaultArgProps :: ArgProps
defaultArgProps = ArgProps "" Nothing False

defaultOptProps :: OptProps
defaultOptProps = OptProps "" ""

long :: String -> Mod
long s = Mod (\o -> o { longName = s }) id

short :: String -> Mod
short s = Mod (\o -> o { shortName = s }) id

optional :: Mod
optional = Mod id (\a -> a { argOptional = True })

metavar :: String -> Mod
metavar s = Mod id (\a -> a { metavarName = s })

help :: String -> Mod
help s = Mod id (\a -> a { helpText = Just s })

(<>) :: Mod -> Mod -> Mod
(Mod o1 a1) <> (Mod o2 a2) = Mod (o2 . o1) (a2 . a1)

strOption :: (String -> a) -> Mod -> [Parser a]
strOption f (Mod o a) = [OptP (a defaultArgProps) (o defaultOptProps) f]

arg :: (String -> a) -> Mod -> [Parser a]
arg f (Mod _ a) = [ArgP (a defaultArgProps) f]

rest :: (String -> a) -> Mod -> [Parser a]
rest f (Mod _ a) = [RestP (a defaultArgProps) f]

flag :: a -> Mod -> [Parser a]
flag f (Mod o a) = [FlagP (a defaultArgProps) (o defaultOptProps) f]

(<.>) :: [a] -> [a] -> [a]
(<.>) = (++)

(<|>) :: [a] -> [a] -> [a]
(<|>) = (++)

command :: String -> Mod -> a -> [Parser a] -> [(String, ArgProps, a, ParseSpec a)]
command n (Mod _ a) d ps = [(n, a defaultArgProps, d, ParseSpec ps)]

argParse :: [Parser a] -> ParseSpec a
argParse = ParseSpec

commands :: Mod -> [(String, ArgProps, a, ParseSpec a)] -> [Parser a]
commands (Mod _ a) cmds = [CmdP (a defaultArgProps) cmds]

margin :: Int
margin = 5

printUsage :: String -> Int -> ParseSpec a -> IO ()
printUsage prog w spec = B.printBox $ usageBox prog w spec

renderUsage :: String -> Int -> ParseSpec a -> String
renderUsage prog w spec = B.render $ usageBox prog w spec

usageBox :: String -> Int -> ParseSpec a -> B.Box
usageBox prog w (ParseSpec ps) = usageLine B./+/ optBox B./+/ argBox B./+/ cmdsBox
 where
  opts = filter isOpt ps
  args = filter isArg ps
  cmds = filter isCmd ps
  formattedArgs = map formatArgForUsage $ filter (not . isOpt) ps
  usageLine = B.text prog B.<+> (B.text $ intercalate " " $ formattedArgs)
  maxOptLen = foldl max 0 $ map optLen opts
  optBox = B.table (map optRow opts) [
    maxOptLen + margin, w - maxOptLen - margin]
  maxArgLen = foldl max 0 $ map posnLen args
  argBox = B.table (map argRow args) [
    maxArgLen + margin, w - maxArgLen - margin]
  maxCmdsLen = foldl max 0 $ map cmdsLen cmds
  cmdsBox = B.vcat B.left $ (map (cmdsRows maxCmdsLen w) cmds) 

formatArgForUsage :: Parser a -> String
formatArgForUsage p = wrap $ metavarN p
 where
  wrap s = if isOptional p then "[" ++ s ++ "]" else s
  metavarN = metavarName . argProps

optRow :: Parser a -> [String]
optRow (OptP a o _) = [sh ++ " " ++ lo ++ " " ++ metavarName a, hlp]
 where 
  sh = "-" ++ (shortName o) ++ (if longName o /= "" then "," else "")
  lo = "--" ++ (longName o)
  hlp = case helpText a of 
    Nothing -> ""
    Just h  -> h
optRow (FlagP a o _) = [sh ++ " " ++ lo ++ " " ++ metavarName a, hlp]
 where 
  sh = "-" ++ (shortName o) ++ (if longName o /= "" then "," else "")
  lo = "--" ++ (longName o)
  hlp = case helpText a of 
    Nothing -> ""
    Just h  -> h
optRow (ArgP _ _)  = error "OptParse.optRow: called on ArgP"
optRow (RestP _ _) = error "OptParse.optRow: called on RestP"
optRow (CmdP _ _)  = error "OptParse.optRow: called on CmdP"

argRow :: Parser a -> [String]
argRow (ArgP a _) = [metavarName a, hlp]
 where
  hlp = case helpText a of
    Nothing -> ""
    Just  h -> h 
argRow (RestP a _) = [metavarName a, hlp]
 where
  hlp = case helpText a of
    Nothing -> ""
    Just  h -> h 
argRow (OptP _ _ _)  = error "OptParse.argRow: called on OptP"
argRow (FlagP _ _ _) = error "OptParse.argRow: called on FlagP"
argRow (CmdP _ _)    = error "OptParse.argRow: called on CmdP"

cmdsRows :: Int -> Int -> Parser a -> B.Box
cmdsRows max w (CmdP a cmds) = hdr B.// tbl
 where
  hdr = B.text $ "Options for " ++ (metavarName a)
  tbl = B.table (map cmdRow cmds) [max + margin, w - max - margin]
  cmdRow (n, x, _, _) = [n, getHelp x]
  getHelp x = case helpText x of
    Nothing -> ""
    Just  h -> h
cmdsRows _ _ (OptP _ _ _)  = error "OptParse.cmdsRows: called on OptP"
cmdsRows _ _ (FlagP _ _ _) = error "OptParse.cmdsRows: called on FlagP"
cmdsRows _ _ (ArgP _ _)    = error "OptParse.cmdsRows: called on ArgP"
cmdsRows _ _ (RestP _ _)   = error "OptParse.cmdsRows: called on RestP"

cmdsLen :: Parser a -> Int
cmdsLen (CmdP _ cmds) = foldl max 0 $ map cmdsLen' cmds
 where
  cmdsLen' (n, _, _, _) = length n
cmdsLen (ArgP _ _)    = 0
cmdsLen (OptP _ _ _)  = 0
cmdsLen (FlagP _ _ _) = 0
cmdsLen (RestP _ _)   = 0

posnLen :: Parser a -> Int
posnLen (ArgP a _)    = length (metavarName a)
posnLen (CmdP _ _)    = 0
posnLen (OptP _ _ _)  = 0
posnLen (FlagP _ _ _) = 0
posnLen (RestP _ _)   = 0

optLen' :: ArgProps -> OptProps -> Int
optLen' a o = length (shortName o) + 2 + length (longName o) + 3 + 
  length (metavarName a) + 2

optLen :: Parser a -> Int
optLen (OptP a o _)  = optLen' a o
optLen (FlagP a o _) = optLen' a o
optLen (ArgP _ _)    = 0
optLen (CmdP _ _)    = 0
optLen (RestP _ _)   = 0 

parse :: String -> ParseSpec a -> String -> Either String [a]
parse argv spec prog = case P.parse pArgs argv of
  Nothing -> Left $ parseError prog spec "Couldn't parse command line!"
  Just as -> parseArgs as spec prog

extractStrings :: [Arg] -> String
extractStrings [] = []
extractStrings ((Val s):as) = s ++ " " ++ (extractStrings as)
extractStrings ((FlagWithValue n v):as) = 
  "--" ++ n ++ "=" ++ v ++ " " ++ (extractStrings as)
extractStrings ((Flag n):as) = "-" ++ n ++ " " ++ (extractStrings as)

parseArgs :: [Arg] -> ParseSpec a -> String -> Either String [a]
parseArgs args sp@(ParseSpec specs) prog = parse' args rst []
 where
  opts = filter isOpt specs
  rst = filter (not . isOpt) specs
  parse' ((Val s):as) (p:ps) xs = case p of
    ArgP _ f    -> parse' as ps ((f s):xs)
    RestP _ f   -> parse' [] (p:ps) ((f $ extractStrings ((Val s):as)):xs)
    CmdP _ cmds -> case findCommand s cmds of
      Nothing -> Left $ parseError prog sp $ "Unknown command '" ++ s ++ "'."
      Just (cmd, _, d, spec) -> case parseArgs as spec (prog ++ " " ++ cmd) of
        Left e -> Left e
        Right xs' -> Right $ xs ++ [d] ++ xs'
    OptP _ _ _  -> error "OptP in list of positional candidates"
    FlagP _ _ _ -> error "FlagP in list of positional candidates"
  parse' ((FlagWithValue n v):as) ps xs = case filter (optMatches n) opts of
    [] -> Left $ parseError prog sp $ "Unknown option '" ++ n ++ "'."
    ((OptP _ _ f):_)  -> parse' as ps ((f v):xs)
    ((ArgP _ _):_)    -> error $ "OptParse.parseArgs: ArgP matches opt " ++ n
    ((FlagP _ _ _):_) -> error $ "OptParse.parseArgs: FlagP matches opt " ++ n
    ((RestP _ _):_)   -> error $ "OptParse.parseArgs: RestP matches opt " ++ n
    ((CmdP _ _):_)    -> error $ "OptParse.parseArgs: CmdP matches opt " ++ n
  parse' ((Flag n):as) ps xs = if n == "h" || n == "help"
    then Left $ parseError prog sp ""
    else case filter (optMatches n) opts of
      [] -> Left $ parseError prog sp $ "Unknown option '" ++ n ++ "'."
      ((OptP _ _ f):_)  -> case as of
        ((Val v):as')   -> parse' as' ps ((f v):xs)
        _           -> Left $ parseError prog sp $ "Option '" ++ n ++ "' expects a value."
      ((FlagP _ _ f):_) -> parse' as ps (f:xs)
      ((ArgP _ _):_)    -> error $ "OptParse.parseArgs: ArgP matches flag " ++ n
      ((RestP _ _):_)   -> error $ "OptParse.parseArgs: RestP matches flag " ++ n
      ((CmdP _ _):_)    -> error $ "OptParse.parseArgs: CmdP matches flag " ++ n
  parse' ((Val _):as) []    xs = parse' as [] xs
  parse' []           (p:ps) xs = if isOptional p
    then parse' [] ps xs
    else Left $ parseError prog sp $ 
      "Expected " ++ (argMetavar p) ++ ", but there are no arguments left."
  parse' []           []    xs = Right xs

parseError :: String -> ParseSpec a -> String -> String
parseError prog spec err = renderUsage prog 80 spec ++ "\n" ++ err

argMetavar :: Parser a -> String
argMetavar = metavarName . argProps

optMatches :: String -> Parser a -> Bool
optMatches _ (ArgP _ _)    = False
optMatches _ (CmdP _ _)    = False
optMatches _ (RestP _ _)   = False
optMatches n (OptP _ o _)  = n == (longName o) || n == (shortName o)
optMatches n (FlagP _ o _) = n == (longName o) || n == (shortName o)

isOptional :: Parser a -> Bool
isOptional = argOptional . argProps

findCommand :: String -> [(String, ArgProps, a, ParseSpec a)] -> Maybe (String, ArgProps, a, ParseSpec a)
findCommand s cmds = case cmd of
  [] -> Nothing
  (c:_) -> Just c
 where
  cmd = filter ((== s) . fst3) cmds
  fst3 (a, _, _, _) = a

isOpt :: Parser a -> Bool
isOpt (OptP _ _ _) = True
isOpt (ArgP _ _) = False
isOpt (RestP _ _) = False
isOpt (CmdP _ _) = False
isOpt (FlagP _ _ _) = True

isArg :: Parser a -> Bool
isArg (OptP _ _ _) = False
isArg (ArgP _ _) = True
isArg (RestP _ _) = True
isArg (CmdP _ _) = False
isArg (FlagP _ _ _) = False

isCmd :: Parser a -> Bool
isCmd (OptP _ _ _) = False
isCmd (ArgP _ _) = False
isCmd (RestP _ _) = False
isCmd (CmdP _ _) = True
isCmd (FlagP _ _ _) = False

pArgs :: P.Parser [Arg]
pArgs = (:) P.<$> pArg P.<*> (pWhitespace P.*> pArgs P.<|> P.yield [])

pWhitespace :: P.Parser Char
pWhitespace = P.check (== ' ') P.anyChar

pArg :: P.Parser Arg
pArg = Flag P.<$> pFlagNoValue
  P.<|> pFlagValue
  P.<|> Val P.<$> P.some pNonWhitespace

pFlagValue :: P.Parser Arg
pFlagValue = FlagWithValue 
  P.<$> (P.char '-' P.*> P.char '-' P.*> P.some pNonWhitespace) 
  P.<*> (P.char '=' P.*> P.some pNonWhitespace)

pFlagNoValue :: P.Parser String
pFlagNoValue = P.char '-' 
  P.*> ((P.char '-' P.*> P.some pNonWhiteEqual) P.<|> pAsciiNonWhitespace)

pNonWhiteEqual :: P.Parser Char
pNonWhiteEqual = P.check f P.anyChar
 where
  f c = c /= ' ' && c /= '='

pNonWhitespace :: P.Parser Char
pNonWhitespace = P.check (/= ' ') P.anyChar

pAsciiNonWhitespace :: P.Parser String
pAsciiNonWhitespace = (:[]) P.<$> P.check f P.anyChar
 where
  f c = isAscii c && c /= ' '

pAscii :: P.Parser Char
pAscii = P.check isAscii P.anyChar
