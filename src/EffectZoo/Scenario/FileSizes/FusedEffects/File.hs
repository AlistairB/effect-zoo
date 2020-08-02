{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators,
  KindSignatures, FlexibleInstances, MultiParamTypeClasses,
  UndecidableInstances, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module EffectZoo.Scenario.FileSizes.FusedEffects.File where

import GHC.Generics (Generic1)
import "fused-effects" Control.Algebra
import "fused-effects" Control.Effect.Sum
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified EffectZoo.Scenario.FileSizes.Shared
                                               as Shared
import Data.Kind (Type)
import Data.Functor (($>))

data File (m :: Type -> Type) k where
  TryFileSize :: FilePath -> File m (Maybe Int)

tryFileSize :: Has File sig m => FilePath -> m (Maybe Int)
tryFileSize = send . TryFileSize

newtype FileIOC m a = FileIOC
  { runFileIOC :: m a
  } deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (File :+: sig) (FileIOC m) where
  alg hdl sig ctx =
    case sig of
      (L (TryFileSize path)) -> FileIOC $ do
        msize <- liftIO (Shared.tryGetFileSize path)
        pure $ ctx $> msize
      (R other) -> FileIOC (alg (runFileIOC . hdl) other ctx)
