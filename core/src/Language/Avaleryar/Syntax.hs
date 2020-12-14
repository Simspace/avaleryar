{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-|

@
  may(?access) :-
    application says key(?key),
    hr says user-key(?user, ?key),
    :ldap says user-group(?user, ?group),
    ?group says user-status(?user, active),
    known-access(?access).
@

This code represents a 'Rule'.  It shows a way in which the 'Lit'eral identified by the 'Pred'icate
@may/2@ can be proven by satisfying every body literal in the rule.  Each 'BodyLit' comes from an
"assertion", which is a collection of rules.  The assertion from which a rule comes is referenced by
the 'ARef' which appears immediately to the left of @says@, as in @hr says user-key(?user, ?key)@.
An assertion reference may be any 'Term' (a 'Value' or a variable), or it may be a "native"
assertion, which is prefixed with a colon (as in @:ldap says user-group(?user, ?group)@).  Because
all rules exist in an assertion, the omitted @... says@ on the last line is implicitly understood to
be @current-assertion says known-access(?access)@ (where @current-assertion@ stands for the actual
name of the assertion in which @may\/1@ is defined).  In brief:

  * @?user@: Variable (we're polymorphic in variables, there are several different kinds)
  * @active@: 'Value'
  * @user-status(?user, active)@: 'Lit'
  * @?group says@: 'ARef'
  * @?group says user-status(?user,active)@: 'BodyLit'
  * @user-status/2@: 'Pred'

-}


module Language.Avaleryar.Syntax where

import           Control.DeepSeq              (NFData)
import           Data.Char                    (isSpace)
import           Data.Function                (on)
import           Data.Functor.Const           (Const(..))
import           Data.Map                     (Map)
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Void
import           GHC.Generics                 (Generic)
import           Text.Megaparsec              (SourcePos(..), pos1)
import           Text.PrettyPrint.Leijen.Text
    (Doc, Pretty(..), brackets, colon, dot, empty, group, hsep, line, nest, parens, punctuate, space, vsep)

data Value
  = I Int
  | T Text
  | B Bool
    deriving (Eq, Ord, Read, Show, Generic)

instance NFData Value

instance IsString Value where
  fromString = T . fromString

instance Pretty Value where
  pretty (I n) = pretty n
  pretty (B b) = if b then "#t" else "#f"
  pretty (T t) = if T.any isSpace t
                 then pretty (show t) -- want the quotes/escaping
                 else pretty t        -- display as a symbol

-- | A predicate is identified by its name and arity (i.e., the predicate of the literal @foo(bar, ?baz)@ is @foo/2@)
data Pred = Pred Text Int deriving (Eq, Ord, Read, Show, Generic)

instance NFData Pred

instance Pretty Pred where
  pretty (Pred p n) = pretty p <> "/" <> pretty n

-- | A term is either a 'Value' or a variable.  Terms are polymorphic in the variable type to
-- provide a bit of safety by keeping us from crossing various streams (e.g., separating runtime
-- unification variables from raw variables straight out of the parser helps avoid a bit of unwanted
-- variable capture).
data Term v = Val Value | Var v deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (Term v)

instance Pretty v => Pretty (Term v) where
  pretty (Var v) = "?" <> pretty v
  pretty (Val c) = pretty c

-- | A literal is identified by a 'Pred' and a list of 'Term's, where the arity in the 'Pred' is the
-- same as the length of the list of 'Term's in the argument list.
data Lit v = Lit Pred [Term v] deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (Lit v)

instance Pretty v => Pretty (Lit v) where
  pretty (Lit (Pred p _) as) = pretty p <> parens (hsep . punctuate "," $ fmap pretty as)

-- | Convenience constructor for 'Lit's.
lit :: Text -> [Term v] -> Lit v
lit pn as = Lit (Pred pn (length as)) as

-- | A reference to an assertion may either statically denote a native assertion or appear as a
-- 'Term'.
data ARef v = ARNative Text | ARTerm (Term v) | ARCurrent deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (ARef v)

instance Pretty v => Pretty (ARef v) where
  pretty (ARTerm t)   = pretty t
  pretty (ARNative n) = colon <> pretty n
  pretty ARCurrent    = mempty

prettyAssertion :: Value -> [Pred] -> Doc
prettyAssertion assn ps = pretty assn
                       <> ": "
                       <> group (nest 2 (line <> (vsep . fmap pretty $ ps)))

-- | A 'Lit'eral appearing in the body of a 'Rule' is always qualified by an 'ARef' to an assertion.
-- When no assertion appears in the concrete syntax, the parser inserts a reference to the assertion
-- currently being parsed.
data BodyLit v = Says (ARef v) (Lit v)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (BodyLit v)

instance Pretty v => Pretty (BodyLit v) where
  pretty (aref `Says` l) = pretty aref <> space <> "says" <> space <> pretty l

-- | A rule has a head and a body made of 'BodyLit's.
data Rule v = Rule (Lit v) [BodyLit v]
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable, Generic)

instance NFData v => NFData (Rule v)

instance Pretty v => Pretty (Rule v) where
  pretty (Rule hd body) = pretty hd <> bodyDoc body <> dot <> line
    where bodyDoc [] = empty
          bodyDoc _  = space <> ":-"
                    <> group (nest 2 (line <> (vsep . punctuate "," $ fmap pretty body)))

-- | Facts can be thought of as rules with no variables in the head and no body.  Instead, we
-- represent them as 'Lit's with variables of type 'Void' to ensure they are facts by construction.
type Fact = Lit Void

-- | Convenience constructor for 'Fact's.  Works directly on 'Valuable's because the whole point is
-- that there aren't any variables amongst the arguments.
fact :: Valuable v => Text -> [v] -> Fact
fact pn = lit pn . fmap val

-- | 'Fact's are vacuously 'Rule's.
factToRule :: Fact -> Rule v
factToRule fct = Rule (vacuous fct) []

-- | 'Directive's provide a side-channel for metadata to pass from assertion authors into an
-- implementation.  They're intended to be extracted at parse time, and are /never/ considered
-- during evaluation.  However, an intermediate processor might use information from a directive to
-- manipulate code /before/ it's loaded into the system and evaluated.  The motivating use cases for
-- directives are to declare test-suites and (eventually) mode declarations.
data Directive = Directive Fact [Fact] deriving (Eq, Ord, Show, Generic)

instance NFData Directive

-- TODO: Pretty this up.
instance Pretty Directive where
  pretty (Directive f fs) = ":- " <> pretty (vacuous @_ @TextVar f)
                                  <> group (nest 2 (line <> (vsep . fmap (pretty . vacuous @_ @TextVar) $ fs)))
                                  <> dot

-- | To ensure freshness, tag runtime variables ('EVar's) with the current value of an 'Epoch'
-- counter which we bump every time we allocate a new variable.
newtype Epoch = Epoch { getEpoch :: Int }
  deriving (Eq, Ord, Read, Show, Num, Enum, Generic, NFData)

-- | TODO: This should probably become a newtype
type TextVar = Text

type Query = Lit TextVar

-- | Convenience constructor for 'Query's.
query :: Text -> [Term TextVar] -> Query
query = lit

-- | At runtime, we unify on variables tagged with an 'Epoch' to help avoid undesirable variable capture.
data EVar = EVar Epoch TextVar deriving (Eq, Ord, Read, Show, Generic)

instance NFData EVar

instance Pretty EVar where
  pretty (EVar (Epoch e) v) = pretty v <> brackets (pretty e)

-- | Extract the 'TextVar' portion of an 'EVar'.
unEVar :: EVar -> TextVar
unEVar (EVar _ v) = v

-- | Raw variables are produced by the parser.
data RawVar = RawVar { unRawVar :: Text, rawLoc :: SourcePos }
  deriving (Read, Show, Generic)

instance NFData RawVar

instance Eq RawVar where
  (==) = (==) `on` unRawVar

instance Ord RawVar where
  compare = compare `on` unRawVar

instance IsString RawVar where
  fromString s = RawVar (T.pack s) (SourcePos "fromString" pos1 pos1)

instance Pretty RawVar where
  pretty = pretty . unRawVar

-- | The runtime substitution.
type Env = Map EVar (Term EVar)

-- | When satisfying a predicate, a variable in its argument may be used in "input mode", meaning
-- that the predicate expects it to be bound /before/ we attempt to satisfy it, or "output mode",
-- where it is the responsibility of the predicate to bind the variable to a value if it succeeds.
data Mode v = In v | Out v
  deriving (Eq, Ord, Read, Show, Generic)

instance NFData v => NFData (Mode v)

type ModedLit = Lit (Mode RawVar)

-- | Some types may be interpreted as a 'Value'.
class Valuable a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a

instance Valuable Value where
  toValue = id
  fromValue = Just

instance Valuable Text where
  toValue = T
  fromValue (T a) = Just a
  fromValue _     = Nothing

instance Valuable Int  where
  toValue = I
  fromValue (I a) = Just a
  fromValue _     = Nothing

instance Valuable Bool where
  toValue = B
  fromValue (B a) = Just a
  fromValue _     = Nothing

deriving instance Valuable a => Valuable (Const a (b :: k))

fromTerm :: Valuable a => Term v -> Maybe a
fromTerm (Val x) = fromValue x
fromTerm _       = Nothing

-- | Construct a 'Term' from anything 'Valuable'.
val :: Valuable a => a -> Term v
val = Val . toValue

-- | Some types may be interprected as a 'Fact'.
class Factual a where
  toFact :: a -> Fact

instance Factual Fact where
  toFact = id
