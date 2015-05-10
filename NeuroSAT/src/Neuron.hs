module Neuron where

import NeuronTypes

import Control.Monad

import Control.Monad.State.Strict as SS

import qualified Data.Array as Arr 

testX :: Integer
testX = 3

-- | Give each neuron a unique ID number
addNeuronID :: v -> State Int (NeuronID, v)
addNeuronID v = SS.state (\n -> ((NeuronID n,v ), n+1)) 

testSS :: ([(NeuronID, Integer)], Int)
testSS = SS.runState op 1 
  where op = mapM (\z -> addNeuronID z) [4,3,2,1]

directions :: [Int]
directions = [1..4]



-- | Create an n by n grid of Neurons. 
createGrid :: Int -> State Int [[(NeuronID,NeuronDescriptor)]]
createGrid size = mapM makeNeuron [(x,y) | x <- [1..size], y <- [1..size]]
  where makeNeuron location = mapM (mkNDir location) directions
        mkNDir l d = addNeuronID $ NDirection (Location l) (Direction d)
 
-- | Store them in an array prefixed by the neuron identifier  
createGridIndex :: Int -> Arr.Array (Int, Int) [(NeuronID, NeuronDescriptor)]
createGridIndex size = Arr.listArray ((1,1),(size,size)) $ fst $ SS.runState (createGrid size) 1


testFirstCell :: [(NeuronID, NeuronDescriptor)]
testFirstCell = createGridIndex 3 Arr.! (1,1)

type NLocationArray = Arr.Array (Int,Int) [(NeuronID, NeuronDescriptor)]
     
-- | Only one direction can be active at a give location
configWTA_Loc :: NLocationArray -> [[(NeuronID, (NeuronID, SynapseBias))]]
configWTA_Loc arr = map configWTA (Arr.elems arr)

-- | Each neuron in the Winner Take All complex suppresses all the other ones.
configWTA :: [(NeuronID, NeuronDescriptor)] -> [(NeuronID, (NeuronID, SynapseBias))]
configWTA neurons = [ createSynapse n1 n2 | n1 <- neurons, n2 <- neurons, fst n1 /= fst n2]
 where createSynapse n1 n2 = (fst n1, (fst n2, -1))
 
testFirstCell2 :: [(NeuronID, (NeuronID, SynapseBias))]
testFirstCell2 = configWTA testFirstCell
 
 