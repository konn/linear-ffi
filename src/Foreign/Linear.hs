{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Foreign.Linear (
  LinearPtr (),
  unsafeNewLinearPtr,
  unsafeConsume,
  unsafeConsume_,
  borrowAsForeignPtr,
) where

import qualified Control.Functor.Linear as Control
import qualified Data.Functor.Linear as Data
import qualified Data.IntMap.Strict as IM
import Foreign
import qualified Foreign.Concurrent as Conc
import Prelude.Linear
import System.IO.Linear (fromSystemIO, fromSystemIOU)
import qualified System.IO.Linear as LinIO
import System.IO.Resource.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import Prelude hiding (($), (.), (<*))
import qualified Prelude as P hiding ((<*))

newtype LinearPtr a = LinearPtr (Resource (Ptr a))

type ReleaseKey = Int

{-# ANN unsafeNewLinearPtr "HLint: ignore Avoid lambda" #-}
unsafeNewLinearPtr :: (Ptr a -> P.IO ()) -> P.IO (Ptr a) -> RIO (LinearPtr a)
unsafeNewLinearPtr delete acq = Control.do
  LinearPtr Control.<$> unsafeAcquire (fromSystemIOU acq) (\x -> fromSystemIO (delete x))

-- | Define non-standard consumer for a resource, other than 'release'.
unsafeConsume :: (Ptr a -> P.IO b) -> LinearPtr a %1 -> RIO (Ur b)
unsafeConsume sink (LinearPtr (UnsafeResource key ptr)) = Control.do
  RIO (P.const $ LinIO.mask_ $ fromSystemIOU (sink ptr))
    <* unsafeMarkAsReleased_ key

-- | Define non-standard consumer for a resource, other than 'release'.
unsafeConsume_ :: (Ptr a -> P.IO ()) -> LinearPtr a %1 -> RIO ()
unsafeConsume_ sink (LinearPtr (UnsafeResource key ptr)) = Control.do
  RIO $ P.const $ LinIO.mask_ $ fromSystemIO $ sink ptr
  unsafeMarkAsReleased_ key

borrowAsForeignPtr :: LinearPtr a %1 -> RIO (Ur (ForeignPtr a))
borrowAsForeignPtr (LinearPtr (UnsafeResource key ptr)) =
  unsafeWithRelease key \rel ->
    Unsafe.toLinear
      fromSystemIOU
      $ Unsafe.toLinear2
        Conc.newForeignPtr
        ptr
      $ Unsafe.toLinear LinIO.withLinearIO
      $ move Data.<$> rel

-- | Extracts the release function
unsafeWithRelease :: ReleaseKey -> (LinIO.IO () %1 -> LinIO.IO a) %1 -> RIO a
unsafeWithRelease key k = RIO \rrm -> Control.do
  Ur rel <- LinIO.mask_ Control.do
    Ur (ReleaseMap dic) <- LinIO.readIORef rrm
    LinIO.writeIORef rrm $ ReleaseMap $ IM.delete key dic
    Control.pure $ Ur $ dic IM.! key
  k rel

-- | Mark a linear point as released WITHOUT actually 'release'-ing or 'unsafeConsue'-ing.
unsafeMarkAsReleased_ :: ReleaseKey -> RIO ()
unsafeMarkAsReleased_ key = RIO \rrm -> LinIO.mask_ Control.do
  Ur (ReleaseMap dic) <- LinIO.readIORef rrm
  LinIO.writeIORef rrm $ ReleaseMap $ IM.delete key dic
