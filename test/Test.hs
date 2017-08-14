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
exceptionIsCaught = TestLabel "std::exceptions are caught" $ TestCase $ do
  result <- try [C.catchBlock|
    throw std::runtime_error("C++ error message");
    |]

  result @?= Left (C.ForeignException "C++ error message")

anythingIsCaught :: Test
anythingIsCaught = TestLabel "any other type is caught" $ TestCase $ do
  result <- try [C.catchBlock|
    throw 0xDEADBEEF;
    |]

  result @?= Left (C.ForeignException "")

main :: IO ()
main = void $ runTestTT (TestList [ exceptionIsCaught, anythingIsCaught ])