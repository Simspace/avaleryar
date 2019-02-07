{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Avaleryar.PrettyPrinter where

import           Data.Char                    (isSpace)
import qualified Data.Text                    as T
import           Text.PrettyPrint.Leijen.Text

import Language.Avaleryar.Syntax

instance Pretty Pred where
  pretty (Pred p n) = pretty p <> "/" <> pretty n

instance Pretty v => Pretty (Term v) where
  pretty (Var v) = "?" <> pretty v
  pretty (Val c) = pretty c

instance Pretty v => Pretty (Lit v) where
  pretty (Lit (Pred p _) as) = pretty p <> parens (hsep . punctuate "," $ fmap pretty as)

instance Pretty v => Pretty (ARef v) where
  pretty (ARTerm t)   = pretty t
  pretty (ARNative n) = colon <> pretty n

instance Pretty v => Pretty (BodyLit v) where
  pretty (aref `Says` lit) = pretty aref <> space <> "says" <> space <> pretty lit

instance Pretty v => Pretty (Rule v) where
  pretty (Rule hd body) = pretty hd <> bodyDoc body <> dot <> line
    where bodyDoc [] = empty
          bodyDoc _  = space <> ":-"
                    <> group (nest 2 (line <> (vsep . punctuate "," $ fmap pretty body)))

instance Pretty Value where
  pretty (I n) = pretty n
  pretty (B b) = if b then "#t" else "#f"
  pretty (T t) = if T.any isSpace t
                 then pretty (show t) -- want the quotes/escaping
                 else pretty t        -- display as a symbol

instance Pretty RawVar where
  pretty = pretty . unRawVar
