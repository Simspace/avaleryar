{-# LANGUAGE TemplateHaskell #-}
module Language.Avaleryar (module Ava) where

import Language.Avaleryar.Parser     as Ava (fct, parseFile, parseText, qry, rls)
import Language.Avaleryar.PDP        as Ava (PDPConfig(..), PDPError(..), pdpConfig, pdpConfigText)
import Language.Avaleryar.PDP.Handle as Ava
    (PDPHandle, checkQuery, dumpDb, newHandle, retractAssertion, submitAssertion, submitFile, submitText,
    unsafeSubmitAssertion, unsafeSubmitFile, unsafeSubmitText)
import Language.Avaleryar.Semantics  as Ava (NativeDb, ToNative(..), compileRules, mkNativeDb, mkNativePred)
import Language.Avaleryar.Syntax     as Ava (Fact, Factual(..), Query, Rule, Valuable(..), fact, lit, query, val)

import TH
$evil
