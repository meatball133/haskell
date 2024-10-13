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
