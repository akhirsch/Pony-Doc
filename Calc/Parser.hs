{-# LANGUAGE OverlappingInstances, TypeOperators #-}
module Calc.Parser where
  import Data.Comp
  import Text.ParserCombinators.Parsec
  import Calc.Base
  import Calc.Extension

  sigParser :: Parser (Term Sig)
  sigParser = opParser <|> valueParser

  valueParser :: Parser (Term Sig)
  valueParser = do
    num <- many digit
    return . iConst $ read num
  
  opParser :: Parser (Term Sig)
  opParser = addParser <|> subParser
  
  addParser :: Parser (Term Sig)
  addParser = do
    char '+'
    spaces
    term1 <- sigParser
    spaces
    term2 <- sigParser
    spaces
    return $ iAdd term1 term2
    
  subParser :: Parser (Term Sig)
  subParser = do 
    char '-'
    spaces
    term1 <- sigParser
    spaces
    term2 <- sigParser
    spaces
    return $ iSub term1 term2


  sig'Parser :: Parser (Term Sig')
  sig'Parser = multParser <|> do
    sig <- sigParser
    return $ deepInject2 sig

  multParser :: Parser (Term Sig')
  multParser = do
    char '*'
    spaces
    term1 <- sig'Parser
    spaces
    term2 <- sig'Parser
    spaces
    return $ iMul term1 term2
