{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import           Control.Exception.Safe
import           Control.Monad
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import           Test.HUnit

C.context C.cppCtx

C.include "<stdexcept>"

exceptionIsCaught :: Test
exceptionIsCaught = TestCase $ do
  result <- try [C.catchBlock|
    throw std::runtime_error("C++ error message");
    |]

  result @?= Left (C.ForeignException "C++ error message")

main :: IO ()
main = void $ runTestTT exceptionIsCaught