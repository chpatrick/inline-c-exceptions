{-# LANGUAGE TemplateHaskell #-}

module Language.C.Inline.Cpp.Exceptions
  ( ForeignException(..)
  , handleForeign
  , catchBlock
  , catchExp
  ) where

import           Control.Exception.Safe
import           Control.Monad
import qualified Language.C.Inline as C
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Foreign
import           Foreign.C

newtype ForeignException = ForeignException String
  deriving (Eq, Ord, Show)

instance Exception ForeignException

handleForeign :: (Ptr CString -> IO ()) -> IO ()
handleForeign cont =
  alloca $ \errPtrPtr -> do
    poke errPtrPtr nullPtr
    cont errPtrPtr `finally` do
      errPtr <- peek errPtrPtr
      when (errPtr /= nullPtr) $ do
        errMsg <- peekCString errPtr
        free errPtr
        throwM (ForeignException errMsg)

catchBlock :: QuasiQuoter
catchBlock = QuasiQuoter
  { quoteExp = \blockStr -> do
      errPtrVarName <- newName "haskellErrPtr"
      _ <- C.include "<exception>"
      _ <- C.include "<cstring>"
      _ <- C.include "<cstdlib>"
      let inlineCStr = unlines
            [ "void {"
            , "  try {"
            , blockStr
            , "  } catch (std::exception &e) {"
            , "    size_t whatLen = std::strlen(e.what()) + 1;"
            , "    char* message = static_cast<char*>(std::malloc(whatLen));"
            , "    std::memcpy(message, e.what(), whatLen);"
            , "    *$(const char** " ++ nameBase errPtrVarName ++") = message;"
            , "  } catch (...) {"
            , "    char* message = static_cast<char*>(std::malloc(1));"
            , "    *message = 0;"
            , "    *$(const char** " ++ nameBase errPtrVarName ++") = message;"
            , "  }"
            , "}"
            ]
      [e| handleForeign $ \ $(varP errPtrVarName) -> $(quoteExp C.exp inlineCStr) |]

  , quotePat = unsupported
  , quoteType = unsupported
  , quoteDec = unsupported
  } where
      unsupported _ = fail "Unsupported quasiquotation."

catchExp :: QuasiQuoter
catchExp = catchBlock { quoteExp = \str -> quoteExp catchBlock (str ++ ";") }