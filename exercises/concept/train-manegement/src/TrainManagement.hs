module TrainManagement (Locomotive(..), Cargo(..), Wagon(..), Train(..)) where

data Locomotive = Locomotive deriving (Eq, Show)

data Cargo = Wheat | Corn | Barley | Coal | Iron deriving (Eq, Show)

data Wagon = Wagon Double Cargo deriving (Eq, Show)

data Train = Train Locomotive [Wagon] deriving (Eq, Show)
