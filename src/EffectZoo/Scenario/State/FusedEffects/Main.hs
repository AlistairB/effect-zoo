module EffectZoo.Scenario.State.FusedEffects.Main where

import           Control.Effect
import           Control.Effect.State
import           Control.Effect.Void
import           EffectZoo.Scenario.State.FusedEffects.Program

countDown :: Int -> (Int, Int)
countDown initial = run (runState initial program)
