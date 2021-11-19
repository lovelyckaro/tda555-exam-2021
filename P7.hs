module P7 where

data StandardSize
  = OneEightyTwo
  | OneEightySeven
  | OneNinetyTwo
  | OneNinetySeven
  | OneHundredTwo
  | OneHundredSeven
  deriving (Show)

data SkiSize = Custom Integer | Standard StandardSize
  deriving (Show)

data Binding = RaceBinding | SwitchBinding | NormalBinding
  deriving (Show)

data SkateBinding = SkateBinding
  deriving Show

type Price = Double
type Brand = String

data Ski
  = Skate SkateBinding SkiSize Brand Price
  | Classic Binding SkiSize Brand Price
  | Skin Binding SkiSize Brand Price
  deriving Show

type Shop = [Ski]

getClassics :: Shop -> [Ski]
getClassics = filter isClassic
  where isClassic Classic {} = True
        isClassic _ = False