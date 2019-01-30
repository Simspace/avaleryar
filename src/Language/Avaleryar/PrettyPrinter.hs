{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Avaleryar.PrettyPrinter where

import Text.PrettyPrint.Leijen.Text

import Language.Avaleryar

instance Pretty Pred where
  pretty (Pred p n) = pretty p <> "/" <> pretty n

instance (Pretty c, Pretty v) => Pretty (Term c v) where
  pretty (Var v) = "?" <> pretty v
  pretty (Val c) = pretty c

instance (Pretty c, Pretty v) => Pretty (Lit c v) where
  pretty (Lit (Pred p _) as) = pretty p <> parens (hsep . punctuate "," $ fmap pretty as)

instance (Pretty c, Pretty v) => Pretty (ARef c v) where
  pretty (ARTerm t)   = pretty t
  pretty (ARNative n) = colon <> pretty n

instance (Pretty c, Pretty v) => Pretty (BodyLit c v) where
  pretty (aref `Says` lit) = pretty aref <> space <> "says" <> space <> pretty lit

instance (Pretty c, Pretty v) => Pretty (Rule c v) where
  pretty (Rule hd body) = pretty hd <> bodyDoc body <> dot
    where bodyDoc [] = empty
          bodyDoc _  = space <> ":-"
                    <> group (nest 2 (line <> (vsep . punctuate "," $ fmap pretty body)))
