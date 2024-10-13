# Type Declartions

Haskell doesn't limit you to the built-in types, it does also allow you to deffine your very own types.
These are called Algebraic Data Types (ADT), and they can be used to define new types that can be used in your program.
To define your own data type you can use the `data` keyword followed by the name of the type.
The function defention should then be made of constructurs which themselfes are also functions, each constructor should be seperated by `|`.

```haskell
data Suit = Hearts | Clubs | Diamonds | Spades
```

This can be seen when checking the type of these constructors.

```haskell
ghci> :t Hearts
Hearts :: Suit
```

Then to construct a value of the type `Suit` you can use one of its constructors.

```haskell
hearts = Hearts
```

In the example above we have defined a new type called `Suit` with four constructors `Hearts`, `Clubs`, `Diamonds` and `Spades`.
But you may wonder at this point, what is the point of defining a new type then we can just use the built in-types, such as `String`?

The reason is mainly to make the code more readable and safer, since if you misspell a constructor the compiler will throw an error.

```haskell
data Suit = Hearts | Clubs | Diamonds | Spades

allSuitString = ["Hearts", "Clubs", "Dimonds", "Spades"]

allSuits = [Hearts, Clubs, Dimonds, Spades]
-- -> error: Not in scope: data constructor ‘Dimonds’
```

The constructors can also take arguments, for example if we want to represent a rank of a card we could assign a constructor that takes an `Int` as an argument.

```haskell
data Rank = Number Int | Jack | Queen | King | Ace

rankTwo = Number 2
:t rankTwo
-- -> rankTwo :: Rank
```

And in turn we can use the constructors to create values of the type `Card`.

```haskell
data Card = Card Rank Suit

card = Card (Number 2) Hearts
```

In the start we mentioned that the constructors are functions, this can also be seen when we check the type of the `Card` constructor, and we can then see it takes a `Rank` and a `Suit` as arguments.

```haskell
ghci> :t Card
Card :: Rank -> Suit -> Card
```

## Named fields

When defining a data type you can also give the fields names, this can make the code more readable and easier to understand.
This can be done by using the **record syntax**, where you define the fields of the type by using the `field :: Type` syntax.

You can then use the field names to create values of the type, this can make the code more readable and easier to understand.
What is worth pointing out is that the order doesn't matter when creating values of the type with named fields.

```haskell
data Person = Person { name :: String, age :: Int }

Person { age = 25, name = "Alice"}
-- -> Person {name = "Alice", age = 25}

-- You can still use the normal syntax to create values of the type
Person "Bob" 30
-- -> Person {name = "Bob", age = 30}
```

## Deriving Typeclasses

When defining a new type you can also derive typeclasses for it, this can be done by adding the `deriving` keyword followed by the typeclass you want to derive.
Typeclasses are a way to define a set of functions that a type must implement, for example the `Show` typeclass defines a function `show` that takes a value of the type and returns a `String`.
Another example is the `Eq` typeclass that defines two functions `==` and `/=` that takes two values of the type and returns a `Bool`.


```haskell
data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show, Eq)

hearts = Hearts
clubs = Clubs

hearts == clubs
-- -> False
```

By just deriving the `Show` typeclass so we can now use the `show` function to get a `String` representation of the value, without having to implement the function ourself.
But in later concepts we will explore how to implement custom instances of typeclasses.

## Type Synonyms

Type synonyms are a way to give an alredy exsisting type a different name, this can be useful to make the code more readable.
For example we could define a type synonym for a list of numbers.

```haskell
type Numbers = [Int]

numbers = [1, 2, 3] :: Numbers
```

## Recursive Types

Types can also be recursive, meaning that a type can contain values of the same type.
For example we could define a list type as follows.
This is most easiliest be thought as tree structures.

```haskell
data Tree = Leaf | Node Int Tree Tree

tree = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)
```
