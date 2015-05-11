module Neuron where

import NeuronTypes

import Control.Monad

import Control.Monad.State.Strict as SS

type NID_State v = State MaxNeuronID v

testX :: Integer
testX = 3


-- | Give each neuron a unique ID number
addNeuronID :: v -> NID_State (NeuronID, v)
addNeuronID v = SS.state (\(MaxNeuronID n) -> ((NeuronID n,v ), MaxNeuronID $! n+1)) 

testSS :: ([(NeuronID, Integer)], MaxNeuronID)
testSS = SS.runState op (MaxNeuronID 1) 
  where op = mapM (\z -> addNeuronID z) [4,3,2,1]

-- | Each neuron in the Winner Take All complex suppresses all the other ones.
configWTA :: [(NeuronID, NeuronDescriptor)] -> [(NeuronID, (NeuronID, SynapseBias))]
configWTA neurons = [ createSynapse n1 n2 | n1 <- neurons, n2 <- neurons, fst n1 /= fst n2]
 where createSynapse n1 n2 = (fst n1, (fst n2, -1))
 

 