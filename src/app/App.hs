main :: IO ()
main = do
  expression <- getLine
  maybeTokens <- return (lexicallyAnalyse expression)