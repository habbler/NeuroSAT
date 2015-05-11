module Grid where

import qualified Data.Array as Arr 


import Control.Monad.State.Strict as SS

import Neuron
import NeuronTypes




directions :: [Int]
directions = [1..4]



-- | Create an n by n grid of Neurons. 
createGrid :: Int -> NID_State [[(NeuronID,NeuronDescriptor)]]
createGrid size = mapM makeNeuron [(x,y) | x <- [1..size], y <- [1..size]]
  where makeNeuron location = mapM (mkNDir location) directions
        mkNDir l d = addNeuronID $ NDirection (Location l) (Direction d)
 
-- | Store them in an array prefixed by the neuron identifier  
createGridIndex :: Int -> Arr.Array (Int, Int) [(NeuronID, NeuronDescriptor)]
createGridIndex size = Arr.listArray ((1,1),(size,size)) $ fst $ SS.runState (createGrid size) (MaxNeuronID 1)


testFirstCell :: [(NeuronID, NeuronDescriptor)]
testFirstCell = createGridIndex 3 Arr.! (1,1)

type NLocationArray = Arr.Array (Int,Int) [(NeuronID, NeuronDescriptor)]
     
-- | Only one direction can be active at a give location
configWTA_Loc :: NLocationArray -> [[(NeuronID, (NeuronID, SynapseBias))]]
configWTA_Loc arr = map configWTA (Arr.elems arr)

testFirstCell2 :: [(NeuronID, (NeuronID, SynapseBias))]
testFirstCell2 = configWTA testFirstCell
 