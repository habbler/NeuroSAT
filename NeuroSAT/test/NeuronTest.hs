{-# OPTIONS_GHC -F -pgmF htfpp #-}
module NeuronTest where

import Neuron

import Test.HUnit
import Test.Framework

test_Timeserver = testX @?= 3