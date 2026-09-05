module BNFGenerator (bnfParser, generateHaskellCode, validate, ADT, getTime, getParserNames) where


import Parser
import Instances
import Data.Char (isLower, isAlphaNum)
import Data.Time (formatTime, defaultTimeLocale, getCurrentTime)
import Control.Applicative (Alternative (..))
import Data.List (intersperse)
import Data.Functor (void)

-- =====================================================
--                 ** DATA TYPES **
-- =====================================================

data ADT = Grammar [Production]
  deriving (Show, Eq)

data Production = Production String (Maybe [Char]) [[SymbolExpr]]
  deriving (Show, Eq)

-- SymbolExpr represents a symbol with optional modifiers
data SymbolExpr = SymbolExpr Symbol (Maybe Modifier)
  deriving (Show, Eq)

data Symbol
  = NonTerminal String (Maybe [SymbolExpr])  -- name and optional arguments for parameterised rules
  | Terminal String
  | Macro String
  | ParameterRef Char
  deriving (Show, Eq)

data Modifier
  = Tok
  | Star
  | Plus
  | Question
  deriving (Show, Eq)

-- =====================================================
--              ** PARSER COMBINATORS **
-- =====================================================

bnfParser :: Parser ADT
bnfParser = do
  prods <- some (productionTok <* optionalNewlines)
  optionalNewlines
  _ <- inlineSpaces
  eof
  pure (Grammar prods)
  where
    productionTok = do
      _ <- inlineSpaces
      (name, params) <- nonterminalWithParamsTok
      _ <- inlineSpaces
      _ <- stringTok "::="
      rhs <- rhsTok params
      pure (Production name params rhs)

-- -- =====================================================
-- NONTERMINALS AND PARAMETERS
-- -- =====================================================


-- Parse nonterminal with OPTIONAL parameters: <n> or <name(a, b, c)>
nonterminalWithParamsTok :: Parser (String, Maybe [Char])
nonterminalWithParamsTok = do
  _ <- is '<'
  name <- some (satisfy (\c -> isAlphaNum c || c == '_'))
  case name of
    (c:_) | not (isLower c) -> failed (UnexpectedString "NonTerminal must start with lowercase")
    _ -> pure ()

  -- Check for parameters
  params <- (is '(' *> parameterListTok <* is ')') <|> pure Nothing
  _ <- is '>'
  pure (name, params)

-- Parse parameter list: a, b, c
parameterListTok :: Parser (Maybe [Char])
parameterListTok = do
  first <- satisfy isLower
  rest <- many (inlineSpaces *> is ',' *> inlineSpaces *> satisfy isLower)
  pure (Just (first : rest))

-- =====================================================
-- RHS (RIGHT-HAND SIDE) AND ALTERNATIVES
-- =====================================================

-- Right-hand side with parameter validation
rhsTok :: Maybe [Char] -> Parser [[SymbolExpr]]
rhsTok params = sepBy1 (altTok params) (inlineSpaces *> is '|' *> inlineSpaces)

-- One alternative with parameter validation
altTok :: Maybe [Char] -> Parser [SymbolExpr]
altTok params = sepBy1 (symbolExprTok params) inlineSpace1

-- Parser for one or more non-newline spaces
inlineSpace1 :: Parser String
inlineSpace1 = some inlineSpace

-- =====================================================
-- SYMBOL EXPRESSIONS AND MODIFIERS
-- =====================================================

-- Symbol with optional modifier
symbolExprTok :: Maybe [Char] -> Parser SymbolExpr
symbolExprTok params = do
  -- Try to parse tok modifier first
  tokMod <- (is 't' *> is 'o' *> is 'k' *> inlineSpace1 *> pure (Just Tok)) <|> pure Nothing

  sym <- symbolTok params

  -- Try to parse postfix modifiers (*, +, ?)
  modifier <- case tokMod of
    Just Tok -> pure (Just Tok)
    Nothing -> parsePostfixModifier
    Just _ -> pure Nothing  -- Cover other cases

  pure (SymbolExpr sym modifier)

-- Parse postfix modifiers (*, +, ?)
parsePostfixModifier :: Parser (Maybe Modifier)
parsePostfixModifier =
  ((is '*' *> pure (Just Star)) <|>
   (is '+' *> pure (Just Plus)) <|>
   (is '?' *> pure (Just Question)) <|>
   pure Nothing)

-- =====================================================
-- SYMBOLS (NONTERMINAL, PARAMETER, MACRO, TERMINAL)
-- =====================================================

-- Symbol: nonterminal (with args), macro, parameter ref, quoted terminal, or bare terminal
symbolTok :: Maybe [Char] -> Parser Symbol
symbolTok params =
      (parameterRefTok params)
  <|> (nonterminalTok params)
  <|> (Macro <$> macroTok)
  <|> quotedTerminalTok
  <|> bareTerminalTok -- Extension

-- =====================================================
-- PARAMETER REFERENCES AND NONTERMINAL CALLS
-- =====================================================

-- Parse parameter reference [a]
parameterRefTok :: Maybe [Char] -> Parser Symbol
parameterRefTok (Just params) = do
  _ <- is '['
  name <- satisfy isLower
  _ <- is ']'
  if name `elem` params
    then pure (ParameterRef name)
    else failed (UnexpectedString ("Parameter [" ++ [name] ++ "] not defined"))
parameterRefTok Nothing = failed (UnexpectedString "Parameter used outside parameterised rule")

-- Nonterminal with optional parameterised rule application
nonterminalTok :: Maybe [Char] -> Parser Symbol
nonterminalTok params = do
  _ <- is '<'
  name <- some (satisfy (\c -> isAlphaNum c || c == '_'))
  case name of
    (c:_) | isLower c -> pure ()
    _ -> failed (UnexpectedString "NonTerminal must start with lowercase")

  -- Check for parameterised rule application: <name(args)>
  args <- (is '(' *> parameterisedArgsTok params <* is ')') <|> pure Nothing
  _ <- is '>'
  pure (NonTerminal name args)

-- Parse arguments to a parameterised rule
parameterisedArgsTok :: Maybe [Char] -> Parser (Maybe [SymbolExpr])
parameterisedArgsTok params = do
  first <- symbolExprTok params  -- No parameters in scope for arguments
  rest <- many (inlineSpaces *> is ',' *> inlineSpaces *> symbolExprTok params)
  pure (Just (first : rest))

-- =====================================================
-- MACROS
-- =====================================================

-- Macro: [int], [alpha], [newline]
macroTok :: Parser String
macroTok = do
  _ <- is '['
  name <- some (satisfy isAlphaNum)
  _ <- is ']'
  case name of
    "int"     -> pure name
    "alpha"   -> pure name
    "newline" -> pure name
    _         -> failed (UnexpectedString ("Unknown macro [" ++ name ++ "]"))


-- =====================================================
-- TERMINALS
-- =====================================================

-- Quoted terminal
quotedTerminalTok :: Parser Symbol
quotedTerminalTok = do
  _ <- is '"'
  content <- many (escapedChar <|> noneof "\"\n\\")
  _ <- is '"'
  pure (Terminal content)

anyChar :: Parser Char
anyChar = satisfy (const True)

-- Parse escaped characters inside quoted strings (some Extensions)
escapedChar :: Parser Char
escapedChar = do
  _ <- is '\\'
  c <- anyChar
  case c of
    'n'  -> pure '\n'
    't'  -> pure '\t'
    'r'  -> pure '\r'
    '"'  -> pure '"'
    '\\' -> pure '\\'
    _    -> pure c  -- unrecognized escapes are taken literally

-- Bare terminal support. This parser is intentionally tried after structured symbols.
bareTerminalTok :: Parser Symbol
bareTerminalTok = do
  token <- some (noneof " \t\r\n<>[]|\"()")
  pure (Terminal token)

-- =====================================================
-- HELPERS
-- =====================================================

-- optional newlines
optionalNewlines :: Parser ()
optionalNewlines = void (many (inlineSpaces *> oneof "\r\n")) <|> pure ()

-- Parse one or more with separator
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- =====================================================
--              ** CODE GENERATION **
-- =====================================================

generateHaskellCode :: ADT -> String
generateHaskellCode adt =
  let Grammar productions = cleanGrammar adt
      dataTypeLines = concatMap generateDataType productions
      parserLines = concatMap generateParser productions
      -- Remove trailing blank line if it exists
      allLines = dataTypeLines ++ parserLines
      trimmedLines = case reverse allLines of
        ("":rest) -> reverse rest
        _ -> allLines
  in unlines trimmedLines

-- =====================================================
-- DATA TYPE GENERATION
-- =====================================================

-- | Generates a Haskell data type declaration from a production rule.
-- For single-alternative rules with one field (no modifier or list modifiers),
-- generates a newtype instead of a data type for efficiency.
-- For multi-alternative rules, generates a data type with numbered constructors.
-- Handles parameterised types by including type parameters in the declaration.
generateDataType :: Production -> [String]
generateDataType (Production name params alternatives) =
  case alternatives of
    -- No alternatives: no data type to generate
    [] -> []

    -- Single alternative case that looks like a newtype
    [alt] | isSingleFieldExpr alt ->
      case alt of
        -- Check that the single field is a SymbolExpr
        [symExpr] ->
          case params of
            -- Non-parameterised version (simple newtype)
            Nothing ->
              [ "newtype " ++ capitalize name ++ " = "
                  ++ capitalize name ++ " "
                  ++ symbolExprToType symExpr Nothing
              , "    deriving Show"
              , ""
              ]

            -- Parameterised version (data type with type parameters)
            Just p ->
              [ "data " ++ capitalize name ++ " "
                  ++ unwords (map (:[]) p)
                  ++ " = " ++ capitalize name ++ " ("
                  ++ symbolExprToType symExpr (Just p) ++ ")"
              , "    deriving Show"
              , ""
              ]
        -- Not a valid single field structure -> skip
        _ -> []

    -- Multiple alternatives
    _ ->
      let constructors =
            zipWith -- Generate each constructor to number them
              (\idx alt ->
                 generateConstructorExpr (capitalize name) idx alt params)
              [1..]
              alternatives
      in case constructors of
        -- At least one constructor generated
        (first:rest) ->
          let dataPrefix =
                case params of
                  Just p ->
                    "data " ++ capitalize name ++ " "
                      ++ unwords (map (:[]) p)
                  Nothing -> "data " ++ capitalize name

              -- First constructor follows the "="
              typeHeader = dataPrefix ++ " = " ++ first

              -- Padding for aligning
              padLength = length dataPrefix + 1  -- +1 for space before =
              padStr = replicate padLength ' '
              -- Add "| Constructor" for the remainders
              pipes = map (\c -> padStr ++ "| " ++ c) rest
          in [typeHeader]
             ++ pipes
             ++ ["    deriving Show", ""]

        -- If no constructors generated
        _ -> []


-- =====================================================
-- TYPE CONVERSION HELPERS
-- =====================================================

-- Check if a single alternative has only one field with no modifier or list modifier (*, +)
-- Used to determine if we should generate a newtype instead of a data type
isSingleFieldExpr :: [SymbolExpr] -> Bool
isSingleFieldExpr alt = case alt of
    [SymbolExpr (NonTerminal _ _) modifier] -> modifier `elem` [Nothing, Just Star, Just Plus]
    [SymbolExpr (Terminal _) modifier] -> modifier `elem` [Nothing, Just Star, Just Plus]
    [SymbolExpr (Macro _) modifier] -> modifier `elem` [Nothing, Just Star, Just Plus]
    [SymbolExpr (ParameterRef _) modifier] -> modifier `elem` [Nothing, Just Star, Just Plus]
    _ -> False

-- Generate constructor for an alternative
generateConstructorExpr :: String -> Int -> [SymbolExpr] -> Maybe [Char] -> String
generateConstructorExpr typename idx symbols params =
  typename ++ show idx ++ " " ++ unwords (map (\s -> symbolExprToType s params) symbols)

-- Convert a symbol expression to its Haskell type
-- Wraps parameterized types in parentheses if they contain spaces (multiple type args)
symbolExprToType :: SymbolExpr -> Maybe [Char] -> String
symbolExprToType (SymbolExpr sym modifier) params =
  let baseType = symbolToType sym params
      -- Wrap parameterized types in parens if they have spaces (multiple type args)
      wrappedType = if ' ' `elem` baseType && modifier == Nothing
                    then "(" ++ baseType ++ ")"
                    else baseType
  in case modifier of
    Just Star -> "[" ++ baseType ++ "]"
    Just Plus -> "[" ++ baseType ++ "]"
    Just Question -> "(Maybe " ++ baseType ++ ")"
    Just Tok -> baseType
    Nothing -> wrappedType

-- Convert a symbol to its base Haskell type
symbolToType :: Symbol -> Maybe [Char] -> String
symbolToType (NonTerminal n Nothing) _ = capitalize n
symbolToType (NonTerminal n (Just args)) params =
  capitalize n ++ " " ++ unwords (map (\arg -> symbolExprToType arg params) args)
symbolToType (Terminal _) _ = "String"
symbolToType (Macro "int") _ = "Int"
symbolToType (Macro "alpha") _ = "String"
symbolToType (Macro "newline") _ = "Char"
symbolToType (Macro m) _ = m
symbolToType (ParameterRef c) _ = [c]

-- =====================================================
-- PARSER GENERATION
-- =====================================================

-- Generate a parser from a single production rule.
-- This function produces Haskell parser code as a list of strings.
generateParser :: Production -> [String]
generateParser (Production name params alternatives) =
  case alternatives of
    -- No alternatives: nothing to generate
    [] -> []
    -- Single alternative case
    [alt] ->
      let parserName = name
          -- Check if newtype (single field, no modifier or *, +)
          isNewtype = isSingleFieldExpr alt

          -- Determine constructor name
          constructorName =
            if isNewtype
            then capitalize name
            else capitalize name ++ "1"

          -- Generate the actual parser expression body (the right-hand side)
          parserBody =
            generateParserBodyExpr constructorName alt params
      in case params of
        -- No parameters for this nonterminal
        Nothing ->
          [ parserName ++ " :: Parser " ++ capitalize name -- type signature
          , parserName ++ " = " ++ parserBody -- parser definition
          , ""
          ]

        -- Parameterised nonterminal
        Just p ->
          let paramTypes =
                unwords (map (\c -> "Parser " ++ [c] ++ " ->") p)
                ++ " Parser (" ++ capitalize name ++ " "
                ++ unwords (map (:[]) p) ++ ")"
          in [ parserName ++ " :: " ++ paramTypes
             , parserName ++ " " ++ unwords (map (:[]) p)
                 ++ " = " ++ parserBody
             , ""
             ]

    -- Multiple alternatievs
    (first:rest) ->
      let parserName = name

          -- Generate parser body for the first alternative
          firstAlt =
            generateParserBodyExpr (capitalize name ++ "1") first params

          -- Generate parser bodies for the rest of the alternatives
          -- using zipWith to pair each alternative with an index (2,3,...)
          restAlts =
            zipWith
              (\idx alt ->
                generateParserBodyExpr
                  (capitalize name ++ show idx) alt params)
              [2 :: Int ..] rest

          -- Calculate padding to align <|> with =
          padding = replicate (length parserName + 1) ' '
          allLines =
            [parserName ++ " = " ++ firstAlt] ++
            map (\alt -> padding ++ "<|> " ++ alt) restAlts
      in case params of
        -- Non-parameterised version
        Nothing ->
          [ parserName ++ " :: Parser " ++ capitalize name
          ] ++ allLines ++ [""]

        -- Parameterised version
        Just p ->
          let paramTypes =
                unwords (map (\c -> "Parser " ++ [c] ++ " ->") p)
                ++ " Parser (" ++ capitalize name ++ " "
                ++ unwords (map (:[]) p) ++ ")"
              parameterPadding = replicate (length parserName + length (unwords (map (:[]) p)) + 2) ' '
              parameterLines =
                [parserName ++ " " ++ unwords (map (:[]) p) ++ " = " ++ firstAlt] ++
                map (\alt -> parameterPadding ++ "<|> " ++ alt) restAlts
          in [ parserName ++ " :: " ++ paramTypes
            ] ++ parameterLines ++ [""]

-- =====================================================
-- PARSER HELPER
-- =====================================================

-- | Generate parser body for a single alternative with a given constructor name.
-- Handles empty alternatives, single-symbol alternatives, and multi-symbol alternatives.
-- Uses <$>, <*> to combine multiple parsers.
generateParserBodyExpr :: String -> [SymbolExpr] -> Maybe [Char] -> String
generateParserBodyExpr constructorName symbols _ =
  case symbols of
    [] -> constructorName
    [sym] -> constructorName ++ " <$> " ++ symbolExprToParser sym
    _ -> constructorName ++ " <$> " ++
         unwords (intersperse "<*>" (map symbolExprToParser symbols))

-- =====================================================
-- SYMBOL -> PARSER CONVERSION
-- =====================================================

-- Convert symbol expression to parser
symbolExprToParser :: SymbolExpr -> String
symbolExprToParser (SymbolExpr sym modifier) =
  let baseParser = symbolToParser sym
  in case modifier of
    Just Tok -> case sym of
      Terminal str -> "(stringTok " ++ show str ++ ")"
      _ -> "(tok " ++ baseParser ++ ")"
    Just Star -> "(many " ++ baseParser ++ ")"
    Just Plus -> "(some " ++ baseParser ++ ")"
    Just Question -> "(optional " ++ baseParser ++ ")"
    Nothing -> baseParser

-- Convert symbol to parser
symbolToParser :: Symbol -> String
symbolToParser (NonTerminal n Nothing) = n
symbolToParser (NonTerminal n (Just args)) =
  "(" ++ n ++ " " ++ unwords (map symbolExprToParser args) ++ ")"
symbolToParser (Terminal str) = "(string " ++ show str ++ ")"
symbolToParser (Macro "int") = "int"
symbolToParser (Macro "alpha") = "(some alpha)"
symbolToParser (Macro "newline") = "(is '\\n')"
symbolToParser (Macro m) = m
symbolToParser (ParameterRef c) = [c]

-- =====================================================
-- UTILITY FUNCTIONS
-- =====================================================

-- | Convert a lowercase letter to uppercase using ASCII offset
toUpperChar :: Char -> Char
toUpperChar ch
  | ch >= 'a' && ch <= 'z' = toEnum (fromEnum ch - 32)  -- 32 is the ASCII offset between lowercase and uppercase
  | otherwise = ch

-- Capitalize string:
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpperChar c : cs

-- Extract parser names in definition order
getParserNames :: ADT -> [String]
getParserNames (Grammar productions) = [name | Production name _ _ <- productions]

-- Generic helper to remove duplicates from a list while preserving order
-- Keeps the first occurrence of each element
removeDups :: Eq a => [a] -> [a]
removeDups = removeDupsHelper []
  where
    removeDupsHelper _ [] = []
    removeDupsHelper seen (x:xs)
      | x `elem` seen = removeDupsHelper seen xs
      | otherwise = x : removeDupsHelper (x:seen) xs

-- =====================================================
--                ** VALIDATION **
-- =====================================================

-- Main validation function that returns warnings from first iteration
validate :: ADT -> [String]
validate (Grammar productions) =
  let duplicateWarnings = findDuplicates productions
      cleanedFromDuplicates = removeDuplicates productions
      undefinedWarnings = findUndefinedNonterminals cleanedFromDuplicates
      paramWarnings = findParameterMismatches cleanedFromDuplicates
      leftRecWarnings = findLeftRecursion cleanedFromDuplicates
  in duplicateWarnings ++ undefinedWarnings ++ paramWarnings ++ leftRecWarnings

-- =====================================================
-- DETECT DUPLICATES
-- =====================================================

-- Find duplicate rule names and return warnings
findDuplicates :: [Production] -> [String]
findDuplicates productions =
  let names = [name | Production name _ _ <- productions]
      duplicates = findDuplicateNames names []
  in ["Duplicate rule: " ++ name | name <- duplicates]

-- Helper: Find duplicate names in a list
findDuplicateNames :: [String] -> [String] -> [String]
findDuplicateNames [] _ = []
findDuplicateNames (name:rest) seen
  | name `elem` seen = name : findDuplicateNames rest seen
  | otherwise = findDuplicateNames rest (name:seen)

-- Remove duplicate productions, keeping only the first occurrence
removeDuplicates :: [Production] -> [Production]
removeDuplicates = removeDuplicatesHelper []
  where
    removeDuplicatesHelper :: [String] -> [Production] -> [Production]
    removeDuplicatesHelper _ [] = []
    removeDuplicatesHelper seen (prod@(Production name _ _):rest)
      | name `elem` seen = removeDuplicatesHelper seen rest
      | otherwise = prod : removeDuplicatesHelper (name:seen) rest

-- =====================================================
-- DETECT UNDEFINED NONTERMINALS
-- =====================================================

-- Find nonterminals that are used but never defined
findUndefinedNonterminals :: [Production] -> [String]
findUndefinedNonterminals productions =
  let defined = [name | Production name _ _ <- productions]
      used = concat [getUsedNonterminals alts | Production _ _ alts <- productions]
      undefinedNames = removeDups [u | u <- used, not (u `elem` defined)]
  in ["Undefined nonterminal: " ++ name | name <- undefinedNames]

-- Extract all nonterminals used in the alternatives of a rule
getUsedNonterminals :: [[SymbolExpr]] -> [String]
getUsedNonterminals alternatives =
  [name | alt <- alternatives,
          SymbolExpr (NonTerminal name _) _ <- alt]

-- =====================================================
-- DETECT LEFT RECURSION (WITH PARAM)
-- =====================================================

-- Find all left-recursive rules (direct and indirect)
findLeftRecursion :: [Production] -> [String]
findLeftRecursion productions =
  let defined = [name | Production name _ _ <- productions]
      leftRecursive = [name | name <- defined, isLeftRecursive name productions]
  in ["Left recursion in: " ++ name | name <- leftRecursive]

-- Check if a rule is left-recursive (directly or indirectly)
isLeftRecursive :: String -> [Production] -> Bool
isLeftRecursive ruleName productions =
  isLeftRecursiveHelper ruleName ruleName productions []

-- Helper function to detect left recursion with cycle detection
-- Tracks visited nodes to avoid infinite loops when checking indirect recursion
-- Handles parameterised nonterminals by resolving their first symbols
isLeftRecursiveHelper :: String -> String -> [Production] -> [String] -> Bool
isLeftRecursiveHelper target current productions visited
  | current `elem` visited = False  -- Already checked this path
  | otherwise =
      case findProduction current productions of
        Nothing -> False
        Just (Production _ _ alternatives) ->
          let visited' = current : visited
              firstNonterminals = [nt | alt <- alternatives,
                                       Just nt <- [getFirstNonterminal alt productions]]
          in any (\nt -> nt == target ||
                        isLeftRecursiveHelper target nt productions visited')
                 firstNonterminals

-- Get the first nonterminal in an alternative (if it exists)
-- Now resolves parameterised nonterminals to their definitions
getFirstNonterminal :: [SymbolExpr] -> [Production] -> Maybe String
getFirstNonterminal [] _ = Nothing
getFirstNonterminal (SymbolExpr (NonTerminal name Nothing) _ : _) _ = Just name
getFirstNonterminal (SymbolExpr (NonTerminal name (Just args)) _ : _) productions =
  -- Resolve the parameterised call by finding what the first symbol would be
  case findProduction name productions of
    Nothing -> Nothing
    Just (Production _ (Just params) alternatives) ->
      -- Get the first nonterminal from the parameterised rule's alternatives
      case alternatives of
        [] -> Nothing
        (firstAlt:_) -> resolveFirstNonterminal firstAlt params args productions
    Just (Production _ Nothing _) -> Just name
getFirstNonterminal (SymbolExpr (ParameterRef _) _ : _) _ = Nothing
getFirstNonterminal _ _ = Nothing

-- Resolve the first nonterminal in a parameterised rule after substituting arguments
-- If the first symbol is a parameter reference, substitute it with the corresponding argument
-- Otherwise, return the first nonterminal found
resolveFirstNonterminal :: [SymbolExpr] -> [Char] -> [SymbolExpr] -> [Production] -> Maybe String
resolveFirstNonterminal [] _ _ _ = Nothing
resolveFirstNonterminal (SymbolExpr (ParameterRef param) _ : rest) params args productions =
  case findParamIndex param params of
    Just idx | idx < length args ->
      case args !! idx of
        SymbolExpr (NonTerminal argName _) _ -> Just argName
        _ -> resolveFirstNonterminal rest params args productions
    _ -> resolveFirstNonterminal rest params args productions
resolveFirstNonterminal (SymbolExpr (NonTerminal name _) _ : _) _ _ _ = Just name
resolveFirstNonterminal (_ : rest) params args productions =
  resolveFirstNonterminal rest params args productions

-- Find the zero-based index of a parameter in the parameter list
-- Returns Nothing if the parameter is not found
findParamIndex :: Char -> [Char] -> Maybe Int
findParamIndex param params = findIndex param params 0
  where
    findIndex _ [] _ = Nothing
    findIndex p (x:xs) idx
      | p == x = Just idx
      | otherwise = findIndex p xs (idx + 1)

-- Find a production by name
findProduction :: String -> [Production] -> Maybe Production
findProduction name productions =
  case [p | p@(Production n _ _) <- productions, n == name] of
    (p:_) -> Just p
    [] -> Nothing

-- =====================================================
-- PARAMETER VALIDATION (EXTENSION)
-- =====================================================

-- Find nonterminals called with wrong number of arguments
findParameterMismatches :: [Production] -> [String]
findParameterMismatches productions =
  let paramCounts = [(name, length params) | Production name (Just params) _ <- productions]
      allCalls = concat [getParameterCalls alts | Production _ _ alts <- productions]
      mismatches = [call | call@(callName, argCount) <- allCalls,
                           case lookup callName paramCounts of
                             Just expectedCount -> argCount /= expectedCount
                             Nothing -> False]
      uniqueMismatches = removeDups mismatches
  in ["Parameter count mismatch in call to: " ++ name | (name, _) <- uniqueMismatches]

-- Extract all parameterised nonterminal calls with their argument counts
getParameterCalls :: [[SymbolExpr]] -> [(String, Int)]
getParameterCalls alternatives =
  [(name, length args) | alt <- alternatives,
                         SymbolExpr (NonTerminal name (Just args)) _ <- alt]

-- =====================================================
-- CLEANUP
-- =====================================================

-- Clean the grammar by iteratively removing problematic rules
cleanGrammar :: ADT -> ADT
cleanGrammar (Grammar productions) =
  let cleaned = iterativeClean productions
  in Grammar cleaned

-- Iteratively clean the grammar until no more problematic rules can be removed
-- Removes duplicates, undefined nonterminals, parameter mismatches, and left recursion
-- Repeats until a fixed point is reached (no more changes)
iterativeClean :: [Production] -> [Production]
iterativeClean productions =
  let step1 = removeDuplicates productions
      step2 = removeUndefined step1
      step3 = removeParameterMismatches step2
      step4 = removeLeftRecursive step3
  in if length step4 == length productions
     then productions  -- No change (done)
     else iterativeClean step4  -- Keep cleaning

-- Remove rules with undefined nonterminals
removeUndefined :: [Production] -> [Production]
removeUndefined productions =
  let defined = [name | Production name _ _ <- productions]
      isValid (Production _ _ alts) =
        let used = getUsedNonterminals alts
            undefinedNames = [u | u <- used, not (u `elem` defined)]
        in null undefinedNames
  in filter isValid productions

-- Remove rules with parameter mismatches
removeParameterMismatches :: [Production] -> [Production]
removeParameterMismatches productions =
  let paramCounts = [(name, length params) | Production name (Just params) _ <- productions]
      hasValidCalls (Production _ _ alts) =
        let calls = getParameterCalls alts
            invalidCalls = [callName | (callName, argCount) <- calls,
                                       case lookup callName paramCounts of
                                         Just expectedCount -> argCount /= expectedCount
                                         Nothing -> False]
        in null invalidCalls
  in filter hasValidCalls productions

-- Remove left-recursive rules
removeLeftRecursive :: [Production] -> [Production]
removeLeftRecursive productions =
  let leftRecNames = [name | name <- [n | Production n _ _ <- productions],
                             isLeftRecursive name productions]
  in [p | p@(Production name _ _) <- productions, not (name `elem` leftRecNames)]

-- =====================================================
--                   TIMESTAMPING
-- =====================================================

-- | Retrieve the current system time as a formatted string.
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime
