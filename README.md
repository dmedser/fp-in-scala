Functional Programming Club
---


### How to run:

Requirements: `Scala 2.12.7, SBT 1.2.6`

To run tests execute next command inside terminal: `sbt test`

### How to work with repo: 

Everyone can fork the repository and start working on exercises inside his own local repo. 
Its better to name the branch with your domain login. Like 'ruaXXX'.

I case if you want to challenge yourself you can make a pull-request with your changes and wait for main repo 
holders to review your changes. When review is completed your PR will be deleted so you can continue working on next exercises.

**Reviewers: ruabrat, ruaosv2, ruagus8, ruadvg1** 

### Lessons: 


#### Lesson 1 - Functions
Base FP blocks - functions will be covered during the Lesson.
We will meet pure functions which could be referential transparent as well. 

In addition we will understand how to perform cyclic operations without loops using recursion and will see 
how to apply tail recursion optimization in order to avoid StackOverflowException. 

#### Lesson 2 - Recursive Data Types
In computer programming languages, a recursive data type (also known as a recursively-defined, 
inductively-defined or inductive data type) is a data type for values that may contain other 
values of the same type. 

Data of recursive types are usually viewed as directed graphs.

###### Exercises: 
```scala
sealed trait MyLinkedList[+A] {

 // ...

 /**
    * Implement function returning a List consisting of all but the last element of a List
    */
  def init: List[A] = ???

  /**
    * Implement function that folds list starting from first element by applying f function
    */
  def foldLeft[B](z: B)(f: (B, A) => B): B = ???

  /**
    * Implement using foldLeft
    */
  def foldRight[B](z: B)(f: (A, B) => B): B = ???

  /**
    * Implement using foldRight
    *
    * @return length of list
    */
  def length: Int = ???

  /**
    *
    * Implement function returning the reverse of a list (given MyLinkedList(1,2,3) it returns MyLinkedList(3,2,1) )
    */
  def reverse: List[A] = ???

  /**
    * Write a function that concatenates a list of lists into a single list. Its runtime
    * should be linear in the total length of all lists. Try to use functions we have already
    * defined.
    *
    */
  def flatten: List[A] = ???
  
  /**
  * Implement flatMap function
  * For instance, MyLinkedList(1,2,3).flatMap(i => List(i,i)) should result in
  * List(1,1,2,2,3,3) .
  *
  */
  def flatMap[B](f: A => List[B]): List[B]
}
```

and implement pending tests inside `MyLinkedListSpec` file

#### Lesson 3 - ADT, Monads 

ADT: Sum types, product types
Monads: first introduction, calculation chaining, monad laws

#### Lesson 4 - Implicits  

`Implicits` is a powerful scala mechanism actively used to decrease an amount of boilerplate. 
There different type of implicits available as language feature: 
- Implicit function and class parameters 
- Implicit classes 
- Implicit conversions 

#### Lessons 5 & 6 - Higher Kinded Types & Type Classes 

See `./src/main/scala/lesson6/task.md`

#### Lessons 7 - Foldable 

See `./src/main/scala/lesson7/task.md`

#### Lessons 8 - Functors

1. Categories: definition
2. Monoid from the categorical POV
3. Scal: the category of types & functions
4. Functor: definition, laws
5. Functors in Scala
6. Co-/Contra-variant functors
7. Connection between type-variance and functors
8. Functor derivation: const, id, sum, prod, exp, comp

#### Lessons 9 - Functors (cont.)

1. Polymorphic functions as values.
2. Natural transformations.
3. Yoneda lemma. Yoneda/Coyoneda embedding.
4. Free objects. List as a free monoid. Coyoneda as a free functor.
5. Adjoint functors. Free -| Forgetful adjunction. 

#### Lessons 10 - Applicative Functor

1. Monoidal Category: definition
2. Monoidal Functor: definition, laws
3. Applicative Co-/Contra-variant functor via Monoidal in Scala
4. Applicative validator example
5. Applicative derivation: const, id, sum, prod, exp, comp

#### Lessons 11 - Traversable and Distributive Functors

See `./src/main/scala/lesson11/task.md`

#### Lessons 12 - Definitions of Monad

1. via Kleisli category
2. via Monoidal category
3. via Adjunctions

#### Lessons 13 - Particular Monads

1. Reader
2. Writer
3. State
4. Either, Try, Option

#### Lessons 14 - Particular Monads

1. Ior
2. Tree-like
3. Cont
4. Eval
5. Free monad

#### Lesson 15 - Intro to Final Tagless ####

1. Böhm-Berarducci encoding
2. Initial algebra
3. F-Algebra
4. Fixpoint types
5. Recursion Schemes (intro)
6. Ad hoc unions with TF
7. Expression problem (TF intro)

#### Lesson 16 - Effect composition via monad transformers

1. Ad-hoc effect composition
2. Implementing nested monad instances via traverse and distributive
3. ReaderT
4. WriterT
5. EitherT
6. StateT