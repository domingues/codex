
module Codex.Tester (
  oneOf,
  -- * module re-exports
  Meta, Code(..),
  lookupFromMeta,
  initBuildCache,
  module Codex.Tester.Monad,
  module Codex.Tester.Result,
  module Codex.Tester.Utils,
  module Codex.Tester.Limits,
  -- * generic stuff
  module Control.Monad,
  module Control.Monad.Trans,
  module System.FilePath,
  module System.Exit,
  module Data.Monoid,
  ) where

import           Codex.Types
import           Codex.Page (lookupFromMeta)
import           Text.Pandoc (Meta)
import           Codex.Tester.Monad
import           Codex.Tester.Limits
import           Codex.Tester.Result
import           Codex.Tester.Utils
import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Trans

import           System.FilePath
import           System.Exit
import           Data.Monoid


-- | Try testers in order, return the first one that suceedds.
-- This is just `asum` from Control.Applicative.Alternative
-- renamed for readability
oneOf :: [Tester a] -> Tester a
oneOf = foldr (<|>) empty

