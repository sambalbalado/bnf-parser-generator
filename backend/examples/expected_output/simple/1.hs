data Expression = Expression1 Term String Expression
                | Expression2 Term
    deriving Show

data Term = Term1 Factor String Term
          | Term2 Factor
    deriving Show

data Factor = Factor1 String Expression String
            | Factor2 Number
    deriving Show

newtype Number = Number Int
    deriving Show

expression :: Parser Expression
expression = Expression1 <$> term <*> (string "+") <*> expression
           <|> Expression2 <$> term

term :: Parser Term
term = Term1 <$> factor <*> (string "*") <*> term
     <|> Term2 <$> factor

factor :: Parser Factor
factor = Factor1 <$> (string "(") <*> expression <*> (string ")")
       <|> Factor2 <$> number

number :: Parser Number
number = Number <$> int
