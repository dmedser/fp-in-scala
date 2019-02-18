Typeclasses 101 (homework)
--------------------------

1. Define your own typeclass `BoundedBand[A]`, with the following algebra:
    ```
    empty :: a
    combine :: (a, a) -> a    // I will be using + alias below
    ```
    and laws:
    ```
    forall a. a + empty == empty + a == a
    forall a b c. (a + b) + c == a + (b + c)
    forall a. a + a == a
    ```
    You can use and extend familiar typeclasses from Cats, e.g. `import cats.Monoid`.

1. Implement the following typeclass instances for `Map[K, V]` (if possible):

    1. `Monoid`
    
    1. `CommutativeMonoid`
    
    1. `BoundedBand`
    
    1. `BoundedSemilattice`

    Choose appropriate context bounds on `[K, V]`, if any, to make your instances as much useful as possible. Reuse code wherever possible.
    If you can't implement an instance, explain why.

1. Put your instances into one object and resolve the issue with ambiguous implicits.

1. Prove your instances are lawful. [HOWTO](https://typelevel.org/cats/typeclasses/lawtesting.html)