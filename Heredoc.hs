module Heredoc (heredoc, fileAsString) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

heredoc :: QuasiQuoter
heredoc = QuasiQuoter (litE . stringL) (litP . stringL) (undefined) (undefined)

fileAsString :: QuasiQuoter
fileAsString = quoteFile heredoc
