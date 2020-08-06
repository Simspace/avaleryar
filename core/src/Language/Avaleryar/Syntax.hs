{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
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

import Data.Functor.Const (Const(..))
import Data.Map           (Map)
import Data.String
import Data.Text          (Text)
import Data.Void

data Value
  = I Int
  | T Text
  | B Bool
    deriving (Eq, Ord, Read, Show)

instance IsString Value where
  fromString = T . fromString

-- | A predicate is identified by its name and arity (i.e., the predicate of the literal @foo(bar, ?baz)@ is @foo/2@)
data Pred = Pred Text Int deriving (Eq, Ord, Read, Show)

-- | A term is either a 'Value' or a variable.  Terms are polymorphic in the variable type to
-- provide a bit of safety by keeping us from crossing various streams (e.g., separating runtime
-- unification variables from raw variables straight out of the parser helps avoid a bit of unwanted
-- variable capture).
data Term v = Val Value | Var v deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A literal is identified by a 'Pred' and a list of 'Term's, where the arity in the 'Pred' is the
-- same as the length of the list of 'Term's in the argument list.
data Lit v = Lit Pred [Term v] deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | Convenience constructor for 'Lit's.
lit :: Text -> [Term v] -> Lit v
lit pn as = Lit (Pred pn (length as)) as

-- | A reference to an assertion may either statically denote a native assertion or appear as a
-- 'Term'.
data ARef v = ARNative Text | ARTerm (Term v) deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A 'Lit'eral appearing in the body of a 'Rule' is always qualified by an 'ARef' to an assertion.
-- When no assertion appears in the concrete syntax, the parser inserts a reference to the assertion
-- currently being parsed.
data BodyLit v = Says (ARef v) (Lit v)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A rule has a head and a body made of 'BodyLit's.
data Rule v = Rule (Lit v) [BodyLit v]
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | Facts can be thought of as rules with no variables in the head and no body.  Instead, we
-- represent them as 'Lit's with variables of type 'Void' to ensure they are facts by construction.
type Fact = Lit Void

-- | Convenience constructor for 'Fact's.  Works directly only 'Valuable's because the whole point
-- is that there aren't any variables amongst the arguments.
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
data Directive = Directive Fact [Fact] deriving (Eq, Ord, Show)

-- | To ensure freshness, tag runtime variables ('EVar's) with the current value of an 'Epoch'
-- counter which we bump every time we allocate a new variable.
newtype Epoch = Epoch { getEpoch :: Int }
  deriving (Eq, Ord, Read, Show, Num, Enum)

-- | TODO: This should probably become a newtype
type TextVar = Text

type Query = Lit TextVar

-- | Convenience constructor for 'Query's.
query :: Text -> [Term TextVar] -> Query
query = lit

-- | At runtime, we unify on variables tagged with an 'Epoch' to help avoid undesirable variable capture.
--
-- TODO: This should probably also become a newtype
type EVar = (Epoch, TextVar)

-- | Raw variables are produced by the parser.
newtype RawVar = RawVar { unRawVar :: Text }
  deriving (Eq, Ord, Read, Show, IsString)

-- | The runtime substitution.
type Env = Map EVar (Term EVar)

-- | When satisfying a predicate, a variable in its argument may be used in "input mode", meaning
-- that the predicate expects it to be bound /before/ we attempt to satisfy it, or "output mode",
-- where it is the responsibility of the predicate to bind the variable to a value if it succeeds.
data Mode v = In v | Out v
  deriving (Eq, Ord, Read, Show)

type ModedLit = Lit (Mode TextVar)

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
