module Pretty (pPrint) where

import Syntax
import Prelude hiding ((<>))
import Text.PrettyPrint

class Pretty a where
  pp :: a -> Doc

pPrint :: Pretty a => a -> String
pPrint t = render $ pp t

instance Pretty Ast where
  pp (Hole) = text "[]"
  pp (HoleWithEnv _) = text "[env]"
  pp (SSkip) = text ";"
  pp (SIf e s1 s2) = text "if (" <> pp e <> text ") {"
                   $+$ nest 2 (pp s1) <+> text "} else {"
                   $+$ nest 2 (pp s2) $+$ text "}"
  pp (SWhile e s) = text "while (" <> pp e <> text ") {"
                   $+$ nest 2 (pp s) $+$ text "}"                   
  pp (SExpr e) = pp e <> text ";"
  pp (SBlock s) = text "{" $+$ nest 2 (pp s) $$ text "}"
  pp s@(SSeq s1 s2) = vcat (map pp (seq2list s))
    where seq2list (SSeq s1 s2) = s1 : seq2list s2
          seq2list s = [s]
  pp (SVarDecl s e) = text "var" <+> text s <+> text "=" <+> pp e <> text ";"
  pp (SAssign s e) = text s <+> text "=" <+> pp e <> text ";"
  
  pp (EVal v) = pp v
  pp (EVar s) = text s
  pp (EFun ss s) = text "fun(" <> hcat (punctuate comma (map text ss))
                   <> text ")" <+> pp s
  pp (ECall e es vs) = pp e <> text "(" <> (hcat $ punctuate comma ((map pp es) ++ (map pp vs))) <> text ")"
  pp (EDeref e) = text "*" <> pp e
  pp (ERef e) = text "ref" <+> pp e
  
--   pp (EReset e)
instance Pretty Value where
  pp (VInt i) = integer $ toInteger i
  pp (VBool True) = text "true"
  pp (VBool False) = text "false"
  pp (VRef v) = text "ref []" -- since not in IO, cannot show the content
  pp (VVoid) = text "void"
  pp (VClosure s b e) = text "closure"
  pp (VPrimFun _) = text "primfun"
  pp (VPrimFunIO _) = text "primfun io"
