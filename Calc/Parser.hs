{-# LANGUAGE OverlappingInstances, TypeOperators, FlexibleContexts, OverlappingInstances #-}
module Calc.Parser where
  import Data.Comp
  import Control.Monad.Fix
  import Text.ParserCombinators.Parsec
  import Calc.Base
  import Calc.Extension

  sigParserMaker :: (f :<: Sig) => Parser (Term f) -> Parser (Term Sig)
  sigParserMaker p = opParser p <|> valueParser p
  
  sigParser :: Parser (Term Sig)
  sigParser = fix sigParserMaker

  valueParser :: Parser (Term Sig)
  valueParser = do
    num <- many digit
    return . iConst $ read num
  
  opParser :: (f :<: Sig) => Parser (Term f) -> Parser (Term Sig)
  opParser p = addParser p <|> subParser p
               
  addParser :: (f :<: Sig) => Parser (Term f) -> Parser (Term Sig)
  addParser p = do
    char '+'
    spaces
    term1 <- p
    spaces 
    term2 <- p
    spaces
    return $ iAdd (deepInject2 term1) (deepInject2 term2)
    
  subParser :: (f :<: Sig) => Parser (Term f) -> Parser (Term Sig)
  subParser p = do 
    char '-'
    spaces
    term1 <- p
    spaces
    term2 <- p
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
