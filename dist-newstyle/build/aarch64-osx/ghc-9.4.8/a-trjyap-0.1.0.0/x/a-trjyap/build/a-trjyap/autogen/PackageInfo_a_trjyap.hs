{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_a_trjyap (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "a_trjyap"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Music Composer with Functional Patterns"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
