# Instructions

Trelleborg is a small town in Sweden that ships a lot of cargo to the rest of the noridc countries from its port.
The cargo is transferred from the ships to the trains that will transport it the rest of the journey.

There has recently been a lot of the delays in the train depot due to mismanagement of the cargo.
Traffikverket (The Swedish Transport Administration) has decided to implement a new system to manage the cargo, and would like your help to model the data in the system.
The data includes different types of locomotives, cargo, and wagons.

## 1. Define a constructor for Locamotive

The system will require to reserv a locomotive for pulling each train.
Thereby a Locomotive is a type that has only one constructor, `Locomotive` is needed.

Define the `Locamotive` constructor of the type `Locomotive` that takes no arguments, it should derive the `Show` and `Eq` typeclass.

```haskell
Locomotive 
-- -> Locomotive 
```

## 2. Define constructors for Cargo

The cargo that is transported by the trains can be of different types.
Define the type `Cargo`, that has five different constructors: `Wheat`, `Corn`, `Barley`, `Coal`, and `Iron`, it should derive the `Show` and `Eq` typeclass.

```haskell
Barley
-- -> Barley
```

## 3. Define constructor for Wagon

The wagons are used to transport the cargo from the ships to the trains.
Define the `Wagon` constructor of the type `Wagon` that takes two arguments: a `Double` value representing the weight of the cargo and a `Cargo` value representing the type of cargo, it should derive the `Show` and `Eq` typeclass.

```haskell
Wagon 45.3 Wheat
-- -> Wagon 45.3 Wheat
```

## 4. Define constructor for Train

Define the `Train` constructor of the type `Train` that takes three arguments: a `Locomotive` value representing the locomotive that will pull the train, a list of `Wagon` values representing the wagons that will transport the cargo, and a list of `Cargo` values representing the cargo that will be transported, it should derive the `Show` and `Eq` typeclass.

```haskell
Train Locomotive [Wagon 45.3 Wheat, Wagon 30.2 Corn, Wagon 20.1 Barley]
-- -> Train Locomotive [Wagon 45.3 Wheat, Wagon 30.2 Corn, Wagon 20.1 Barley]
```
