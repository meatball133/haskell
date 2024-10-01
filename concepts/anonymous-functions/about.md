# About

[Anonymous functions][anon-fns] are commonly used throughout Haskell on their own, as return values, and as arguments in higher order functions such as `Enum.map/2`:

```haskell
map (\n -> n + 1) [1, 2, 3]
-- -> [2, 3, 4]
```

Functions in Haskell are treated as first class citizens:

- Named and anonymous functions can be assigned to variables.
- Named and anonymous functions can be passed around like data as arguments and return values.

Anonymous functions are created with the [`fn`][kernel-fn] keyword and invoked with a dot (`.`):

```haskell
function_variable :: (Int -> Int)
function_variable = \n -> n + 1

function_variable 1
-- -> 2
```


Anonymous functions in Elixir are [closures][closure]. They can access variables that are in scope when the function is defined. Variables assigned inside of an anonymous function are not accessible outside of it:

```elixir
y = 2

square = fn ->
  x = 3
  x * y
end

square.()
# => 6

x
# => ** (CompileError): undefined function x/0
```

[anon-fns]: https://hexdocs.pm/elixir/anonymous-functions.html
[kernel-fn]: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#fn/1
[kernel-capture]: https://hexdocs.pm/elixir/Kernel.SpecialForms.html#&/1
[capture]: https://dockyard.com/blog/2016/08/05/understand-capture-operator-in-elixir
[closure]: https://en.wikipedia.org/wiki/Closure_(computer_programming)