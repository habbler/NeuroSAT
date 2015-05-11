module Grid where

import qualified Data.Array as Arr 


import Control.Monad.State.Strict as SS

import Neuron
import NeuronTypes

type Coord = (Int, Int)

directions :: [Int]
directions = [0..3]

-- | Find the coord in direction d from give coord
moveCoord :: Int -> Coord -> Direction -> Maybe Coord
moveCoord uBound (x,y) (Direction direction)  
                                = case direction of 
                                        0 -> cond (y < uBound) (x,y+1) -- N
                                        1 -> cond (x < uBound) (x+1,y) -- E
                                        2 -> cond (y > 0     ) (x,y-1) -- S  
                                        3 -> cond (x > 0     ) (x-1,y) -- W
     where cond c v = if c then Just v else Nothing

-- | Return neighbouring squares and the direction you need to go from the neighbouring square to get here     
getNeighbours :: Int -> Coord -> [(Coord, Int)]
getNeighbours uBound (x,y) = ifb (y < uBound) ((x,y+1),2) $
                             ifb (x < uBound) ((x+1,y),3) $
                             ifb (y > 0     ) ((x,y-1),0) $
                             ifb (x > 0     ) ((x-1,y),1) $
                             []
  where ifb cond result rest = if cond then result:rest else rest  
       

-- | The opposite direction.     
oppDirection :: Direction -> Direction
oppDirection (Direction dir) = Direction $ 
       if (dir == 4)  then 4   -- No direction
       else let newDir = dir + 2 in
              if (newDir > 3) then (newDir - 4) else newDir

-- | Create an n by n grid of Neurons. 
createGrid :: Int -> NID_State [[(NeuronID,NeuronDescriptor)]]
createGrid size = mapM makeNeuron [(x,y) | x <- [0..maxIndex], y <- [0..maxIndex]]
  where makeNeuron location = mapM (mkNDir location) directions
        mkNDir l d = addNeuronID $ NDirection (Location l) (Direction d)
        maxIndex = size - 1
 
-- | Store them in an array prefixed by the neuron identifier  
createGridIndex :: Int -> Arr.Array (Int, Int) [(NeuronID, NeuronDescriptor)]
createGridIndex size = Arr.listArray ((0,0),(maxIndex,maxIndex)) $ fst $ SS.runState (createGrid size) (MaxNeuronID 1)
  where maxIndex = size - 1


testFirstCell :: [(NeuronID, NeuronDescriptor)]
testFirstCell = createGridIndex 3 Arr.! (0,0)

type NLocationArray = Arr.Array (Int,Int) [(NeuronID, NeuronDescriptor)]
     
-- | Only one direction can be active at a give location
configWTA_Loc :: NLocationArray -> [[(NeuronID, (NeuronID, SynapseBias))]]
configWTA_Loc arr = map configWTA (Arr.elems arr)

testFirstCell2 :: [(NeuronID, (NeuronID, SynapseBias))]
testFirstCell2 = configWTA testFirstCell
 