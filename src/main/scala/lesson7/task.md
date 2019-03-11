Домашнее задание
----------------

### Часть 1.

В прошлый раз мы познакомились с тайпклассом Foldable и убедились, что лишь одной его функции достаточно, чтобы выразить все стратегии свертки.
```scala
import cats.Monoid
trait Foldable[F[_]] {
  def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B
}
```

Выразите функции `foldr` и `foldl` через `foldMap` и наоборот:
```scala
def foldr[A, B](fa: F[A])(z: B)(f: (A, B) => B): B
def foldl[A, B](fa: F[A])(z: B)(f: (B, A) => B): B
``` 
а также `foldr` и `foldl` друг через друга.

NB. На данном этапе не надо заботиться о стэкобезопасности и поддержке ленивых (бесконечных) структур.


### Часть 2.

Написать функции для тайпкласса `Foldable[F[_]]`:
```scala
def headOption[A](fa: F[A]): Option[A]
def lastOption[A](fa: F[A]): Option[A]
def length[A](fa: F[A]): Int
def exists[A](fa: F[A])(p: A => Boolean): Boolean
def forall[A](fa: F[A])(p: A => Boolean): Boolean
```

### Часть 3.

Написать функцию для частичной свертки первых N элементов:
```scala
def foldrN[A, B](fa: F[A])(n: Int)(z: B)(f: (A, B) => B): B
```

### Часть 4.

Строгие (strict) сигнатуры не подходят для частичной свертки ленивых (потенциально бесконечных) структур.

Рассмотрим другое, более ленивое, определение `Foldable`:
```scala
trait Foldable[F[_]] {
  def foldr[A, B](fa: F[A])(z: B)(f: (A, => B) => B): B
}
```

Реализовать инстанс для `Stream` и решить задачу №3 для бесконечного стрима, например `Stream.from(1)`. 


