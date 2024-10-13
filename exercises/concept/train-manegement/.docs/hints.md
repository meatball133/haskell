# Hints

## General

- All of the types need to derive the `Show` and `Eq` typeclasses.

## 1. Define a constructor for Locamotive

- The `Locomotive` type has only one constructor, `Locomotive`.

## 2. Define constructors for Cargo

- Define the type `Cargo` with five different constructors: `Wheat`, `Corn`, `Barley`, `Coal`, and `Iron`.

## 3. Define constructor for Wagon

- Define the `Wagon` constructor of the type `Wagon` that takes two arguments: a `Double` value representing the weight of the cargo and a `Cargo` value representing the type of cargo.

## 4. Define constructor for Train

- Define the `Train` constructor of the type `Train` that takes three arguments: a `Locomotive` value representing the locomotive that will pull the train, a list of `Wagon` values representing the wagons that will transport the cargo, and a list of `Cargo` values representing the cargo that will be transported.
