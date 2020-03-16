{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------

#include <netcode.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Netcode.IO where

import Foreign.Ptr         (FunPtr)
import Prelude             (IO)

--------------------------------------------------------------------------------

#ccall netcode_init, IO ()
#ccall netcode_term, IO ()