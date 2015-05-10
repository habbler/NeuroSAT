module NeuronTypes where

import Data.IntMap as IMap
 
newtype NeuronID = NeuronID Int deriving (Eq,Show)

newtype Location = Location (Int, Int) deriving (Eq,Show)
newtype Direction = Direction Int deriving (Eq,Show)

-- | Use by the rules that decide how to connect Neurons together
data NeuronDescriptor = NDirection { location :: Location
                                   , direction :: Direction
                                   } deriving (Eq,Show)
                                   
data NeuronState = NeuronOn | NeuronOff

data NeuronType = 
        NSwitch -- ^ Type of neuron that sends a positive when it turns on, and a negative spike when it turns
                -- off.    

-- | Map from NeuronID's to their Descriptors                                   
type NDescriptors = IntMap NeuronDescriptor

-- | Map from NeuronID's to their current state
type NStates = IntMap NeuronState

data Synapse = Synapse { factor :: Float
                       , targetID :: NeuronID 
                       }

type SynapseBias = Float                       
type SynapseFactor = Float

-- | Map from the Source neuron to a list of destination neurons via the connecting synapse                       
type Synapses = IntMap [(SynapseFactor, NeuronID)]

-- | A signal from one neuron to its target.
type Spike = Float                                                          