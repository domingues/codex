
module Codex.Tester (
  Tester,
  testers,
  -- * module re-exports
  module Codex.Tester.Monad,
  module Codex.Tester.Result,
  module Codex.Tester.Utils,
  module Codex.Tester.Limits,
  -- * generic stuff
  module Control.Monad
  ) where

import           Codex.Types(Code)
import           Codex.Page (Page)
import           Codex.Tester.Monad
import           Codex.Tester.Limits
import           Codex.Tester.Result
import           Codex.Tester.Utils
import           Control.Applicative
import           Control.Monad 


-- | type synonym for an exercise tester
type Tester = FilePath -> Page -> Code -> Test Result

-- | combine a list of testers in sequence
testers :: [Tester] -> Tester
testers list path page code 
  = foldr (\t r -> t path page code <|> r) empty list
