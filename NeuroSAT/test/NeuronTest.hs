{-# OPTIONS_GHC -F -pgmF htfpp #-}
module NeuronTest where

import Neuron

import Test.HUnit
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-} 
test_Timeserver :: Assertion
test_Timeserver = testX @?= 4