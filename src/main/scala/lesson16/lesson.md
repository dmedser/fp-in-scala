# Лекция: трансформеры монад и MTL-подход

## План лекции

1. Эффекты
2. Монады и конкретные эффекты.
3. Композиция эффектов
3.1. Пример с DBEffect
3.2. Композиция произвольных эффектов
4. Трансформеры монад
4.1. ReaderT
4.2. WriterT
4.3. EitherT
4.4. StateT, ДЗ
5. Примеры использования
6. Другие трансформеры, ДЗ

## Эффекты

Напомним вкратце понятие эффекта в том виде, в каком оно нам пригодится в дальнейшем. Пусть у нас есть тип T и 
некоторый конструктор типов `F[_]`. Тогда, если тип `F[T]` обладает некоторыми дополнительными свойствами относительно 
изначального типа `T`, то `F` можно назвать эффектом, отвечающим за эти свойства.

Рассмотрим для пример `List[_]`. Если мы обернем тип `T` в `List`, получим тип `List[T]`. 
Для каждого элемента списка мы можем запускать какие-то вычисления и получать новые списки значений.
Композируя такие вычисления мы получаем эффект множества результирующих значений.

## Монады

На прошлых занятиях мы познакомились с монадами. Их также можно назвать эффектами или сказать, что определенные
типы монад выражают соответствующие типы эффектов. Напомним некоторые из них:

`Option` - эффект отсутствия значения. 
`Either` - эффект вычисления с ошибкой.
`Reader` - эффект зависимости вычислений от окружения.
`Writer` - эффект дополнительных данных на ряду с основными результатами.
`State`  - эффект вычисления с состоянием.
`IO`     - эффект вычислений, изменяющих состояние системы.

Если мы пишем цепочку вычислений с использованием одной монады, то используя методы map и flatMap, а также
for-comprehension, можно композировать несколько значений в эффекте. Рассмотрим пример с Option: 

```scala worksheet
val x = Option(5)
val y = None
val z = for {
  someX <- x
  someY <- y
} yield someX + someY
```

## Композиция эффектов

### Рассмотрим проблему на практическом примере

Но что, если нам не достаточно только эффекта отсутствия значения. Допустим мы хотим осуществлять доступ к
базе данных и при этом логгировать запросы в базу. Кроме этого при попытке доступа к базе мы можем получить
ошибку. Данные эффекты мы кодируем через:
* Writer для эффекта логгирования
* Either для эффекта вычисления с ошибкой

```scala worksheet
type DBEffect[T] = Writer[String, Either[DBException, T]]
def get(id: EntityId): DBEffect[Option[Entity]]
```

Допустим теперь, что мы хотим вернуть пару сущностей. Можно по аналогии попробовать написать код:
```scala worksheet
val x: DBEffect[Entity] = ???
val y: DBEffect[Entity] = ???
val pair: DBEffect[(Entity, Entity)]  = for {
  someX <- x
  someY <- y
} yield (someX, someY)
```

Однако такой код работать не будет. В лучшем случае нам придется раскрывать каждый из эффектов руками, 
что приведет к крайне громоздкому коду.

Конечно можно вывести инстанс монады руками.

Например, чтобы заставить DBEffect вести себя как монаду, можно определить его по другому:

```scala worksheet
case class DBEffect[T](value: Writer[String, Either[DBException, T]])

object DBEffect {
  implicit def monad[T]: Monad[DBEffect] = new Monad[DBEffect] {
    override def pure[A](x: A): DBEffect[A] = DBEffect(Writer(Right(x) -> ""))

    override def flatMap[A, B](fa: DBEffect[A])(f: A => DBEffect[B]): DBEffect[B] =
      DBEffect(Writer(fa.value.run match {
        case (Right(a), w) =>
          f(a).value.run match {
            case (res, w$) => (res, w ++ w$)
          }
        case (Left(e), w) => (Left(e), w)
      }))

  }
}
```

Но что, если бы DBEffect был определен не через Either, а, скажем, через IO. В этом случае нам бы пришлось написать
инстанс монады заново:

```scala worksheet
import cats.effect.IO

case class DBEffect[T](run: IO[Either[DBException, T]])

object DBEffect {
  implicit def monad[T]: Monad[DBEffect] = new Monad[DBEffect] {
    override def pure[A](x: A): DBEffect[A] = DBEffect(IO.pure(Right(x)))

    override def flatMap[A, B](fa: DBEffect[A])(f: A => DBEffect[B]): DBEffect[B] =
      DBEffect {
        fa.run.flatMap {
          case Left(e) => IO.pure(Left(e))
          case Right(a) => f(a).run
        }
      }
  }
}
```


### Композиция произвольных эффектов

Как можно заметить, код очень похож. В таком случае будет вполне естественно желание как-то этот код обобщить

Как мы можем помнить, если `F` и `G` - функторы, то, как мы видели на предыдущих занятиях,
`F[G[_]]` также является функтором.
```scala worksheet
case class Nested[F[_], G[_], A](value: F[G[A]])

def functorCompose[F[_], G[_]](implicit F: Functor[F], G: Functor[G]): Functor[λ[a => Nested[F, G, a]]] = 
  new Functor[λ[a => Nested[F, G, a]]] {
    def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] = Nested(fa.map((ga: G[A]) => ga.map(f)))
  }
```
Поэтому мы могли бы автоматически получить инстанс функтора для DBEffect и применять какие-то трансформации
к значению внутри функтора.

К сожалению скомпозировать монады таким же образом в общем случае нельзя. Т.е. мы не можем написать реализацию 
подобной функции:
```scala worksheet
def monadCompose[F[_], G[_]](implicit F: Monad[F], G: Monad[G]): Monad[λ[a => Nested[F, G, a]]] = ???
```

Однако написание такой функции возможно для некоторых частных случаев. Например, если наложить дополнительные 
ограничения на `G` с требованием наличия инстанса Traverse
или на `F` с требованием наличия инстанса Distributive
```scala worksheet
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import cats.{Monad, StackSafeMonad, Traverse}

implicit def nestedMonadWithTraverse[F[_] : Monad, G[_] : Monad : Traverse]: Monad[Nested[F, G, ?]] =
  new Monad[Nested[F, G, ?]] {
    override def pure[A](a: A): Nested[F, G, A] =
      Nested { Monad[F].pure(Monad[G].pure(a)) }

    // For example `λ[a => Writer[Option[a]]]`
    override def flatMap[A, B](fa: Nested[F, G, A])(f: A => Nested[F, G, B]): Nested[F, G, B] = Nested {
      fa.value // Nested[Writer, Option, A] => Writer[Option[A]]
        .map { ga => // Option[A]
          // traverse: Option[A] => (A => Writer[Option[A]]) => Writer[Option[Option[A]]
          ga.traverse(f(_).value)
        } // Writer[Writer[Option[Option[A]]
        .flatten
        .map(_.flatten)
    }
  }
 
implicit def nestedMonadWithDistributive[F[_] : Monad : Distributive, G[_] : Monad]: Monad[Nested[F, G, ?]] =
  new Monad[Nested[F, G, ?]] {
    override def pure[A](a: A): Nested[F, G, A] =
      Nested { Monad[F].pure(Monad[G].pure(a)) }

    // For example `λ[a => Function1[E, Option[a]]]` -- Reader
    override def flatMap[A, B](fa: Nested[F, G, A])(f: A => Nested[F, G, B]): Nested[F, G, B] = Nested {
      fa.value // Nested[Function1[E, ?], Option, A] => Function1[E, Option[A]]
        .map { ga => // Option[A]
          // distribute: Option[A] => (A => Function1[E, Option[A]]) => Function1[E, Option[Option[A]]
          ga.distribute(f(_).value)
        } // Function1[E, Function1[E, Option[Option[A]]]
        .flatten
        .map(_.flatten)
    }
  }
```

К сожалению подобный подход не даёт нам возможности скомпозировать два любых эффекта,
так например State не является ни Traverse ни Distributive.
В связи с этим был придуман более общий подход.

## Трансформеры монад

Основная идея трансформеров монад в дополнительной обёртке над монадой и реализации для этой обёртки
инстанса монады, насыщающего обёрнутую монаду дополнительным эффектом.

Давайте рассмотрим на конкретных примерах

### ReaderT

Эффект чтения из окружения. `Reader` мы на прошлых занятиях представили как обёртку над функцией из
типа окружения в тип результирующего значения -- `case class Reader[E, A](run: E => A)`.
Теперь мы хотим сделать `Reader` который станет обёрткой над неким `F`,
который уже является монадой со своим эффектом.
Естественно в таком случае мы должны добавить в определение ещё одну переменную типа -- `F[_]`.
`case class ReaderT[F[_], E, A](run: ???)`. Но что мы будем оборачивать?

Для этого стоит подумать о том, какой семантикой мы наделяем `ReaderT`.
Из кода выше мы видим, что `ReaderT` даёт нам возможность прочитать окружение и сконструировать
вычисления *с дополнительным эффектом `F`*. Данное описание естественным образом переводится на код
конкретного ЯП:

```scala worksheet
case class ReaderT[F[_], E, A](run: E => F[A])
//                                  ^^^^
//                   читаем из окружения
//                                       ^^^^
//                                       на его основе
//                                       конструируем вычисление с эффектом
```

Ещё нам нужны интерфейсные функции `Reader`: `ask` и `local`
```scala worksheet
def ask[F[_] : Monad, E] = ReaderT { e => Monad[F].pure(e) }
def local[F[_] : Monad, E, A](f: E => E)(fa: ReaderT[F, E, A]) = ReaderT { e => fa.run(f(e)) }
```

Нам нужно сделать `ReaderT` монадой:
```scala worksheet
implicit def readerTMonad[F[_] : Monad, E] = new Monad[ReaderT[F, E, ?]] {
  def pure[A](a: A): ReaderT[E, A] =
    // Reader { e => a } = Reader { e => identity(a) } -- просто заменим identity на pure
    ReaderT { e => Monad[F].pure(a) }

  def flatMap[A, B](fa: ReaderT[E, A])(f: A => ReaderT[E, B]): F[B] =
    // Reader { e => fa.run(e) |> (a => f(a).run(e)) } -- просто заменим |> на flatMap
    ReaderT { e => fa.run(e).flatMap(a => f(a).run(e)) }
}
```

Кроме этого нам нужен способ залифтить нечто типа `F[A]` в `ReaderT[F, E, A]`:
```scala worksheet
implicit def liftK[F, E]: F ~> ReaderT[F, E, ?] = new ~>[F, ReaderT[F, E, ?]] {
  def apply[A](fa: F[A]): ReaderT[F, E, A] = ReaderT { _ => fa }
}
```

Сделав такое определение мы теперь можем определить простой `Reader` таким образом:
```scala worksheet
type Reader[E, A] = ReaderT[Id, E, A]
```

Как видно из определений выше, конкретная монада `Reader` преобразуется в трансформер просто заменой
`|>` на `flatMap` и `identity` на `Monad[F].pure` в реализации `Monad`, подстановкой `F` в нужном месте
и описанием натурального преобразования из `F` в `ReaderT[F, E, ?]`.

### WriterT

`Writer` это эффект, семантика которого заключается в накоплении дополнительных данных в дополнение к результату.
Записываем мы его как `case class Writer[W, A](run: (A, W))`, где в определении монады `W` контекстуально ограничевается
классом типов `Monoid`, для комбинирования эффектов различных вычислений через функцию `combine` и выражение чистого
безэффектного вычисления через нейтральный элемент.

`WriterT` должен накапливать дополнительные данные в дополнение к эффекту `F`, для этого нам подходит форма вида
```scala worksheet
case class WriterT[F[_], W, A](run: F[(A, W)])
```

Основная интерфейсная функция для WriterT, это функция "записи в лог".
```scala worksheet
def tell[F[_] : Monad, W : Monoid](w: W) = WriterT(Monad[F].pure(() -> w))
```

WriterT монада:
```scala worksheet
implicit def writerTMonad[F[_] : Monad, W: Monoid]: Monad[WriterT[F, W, ?]] = new Monad[WriterT[F, W, ?]] {
  def pure[A](a: A): WriterT[F, W, A] =
    // Writer(a -> Monoid[W].empty)
    WriterT(Monad[F].pure(a -> Monoid[W].empty))

  def flatMap[A, B](fa: WriterT[F, W, A])(f: a => WriterT[F, W, B]): WriterT[F, W, B] = WriterT {
    // fa.run |> { case (a, w) =>
    //   f(a).run |> { case (b, w$) => b -> (w combine w$) }
    // }
    fa.run.flatMap { case (a, w) =>
      f(a).run.map { case (b, w$) => b -> (w combine w$) }
    }
  }
}
```

Лифтинг `F[A]` в `WriterT[F, W, A]`:
```scala worksheet
implicit def liftK[F[_] : Functor, W : Monoid]: F ~> WriterT[F, W, ?] =
  new FunctionK[F, WriterT[F, W, ?]] {
    def apply[A](fa: F[A]): WriterT[F, W, A] = WriterT { fa.map(_ -> Monoid[W].empty) }
  }
```

Теперь мы можем определить `Writer` как `WriterT` с зафиксированным параметром `F`:
```scala worksheet
type Writer[W, A] = WriterT[Id, W, A]
```

### EitherT

`EitherT` добавляет семантику вычислений завершающихся с ошибкой к эффекту `F`.

```scala worksheet
case class EitherT[F[_], E, A](run: F[Either[E, A]])
```

Интерфейсные функции:
```scala worksheet
def raiseError[F[_] : Monad, E, A](e: E): EitherT[F, E, A] = EitherT { Monad[F].pure(Left(e)) }
def handleErrorWith[F[_] : Monad, E, A](fa: EitherT[F, E, A])(f: E => EitherT[F, E, A]): EitherT[F, E, A] =
  fa.run.flatMap {
    case Left(e)  => f(e)
    case Right(a) => Monad[F].pure(Right(a))
  }
```

Монада:
```scala worksheet
implicit def eitherTMonad[F[_] : Monad, E] = new Monad[EitherT[F, E, ?]] {
  def pure[A](a: A): EitherT[F, E, A] =
    //                      Right(a)
    EitherT { Monad[F].pure(Right(a)) }

  def flatMap[A, B](fa: EitherT[F, E, A])(f: A => EitherT[F, E, B]): EitherT[F, E, B] = EitherT {
    // fa match {
    //   case Right(a) => f(a)
    //   case Left(e)  => Left(e)
    // }
    fa.run.flatMap {
      case Right(a) => f(a).run
      case Left(e)  => Monad[F].pure(Left(e))
    }
  }
}
```

Лифтинг:
```scala worksheet
implicit def liftK[F[_] : Functor, E]: F ~> EitherT[F, E, ?] = new FunctionK[F, EitherT[F, E, ?]] {
  def apply[A](fa: F[A]): EitherT[F, E, A] = EitherT { fa.map(Right(_)) }
}
```

### StateT

```scala worksheet
// case class State[S, A](run: S => (A, S))
case class StateT[F[_], S, A](run: ???)
```

Интерфейсные функции:
```scala worksheet
def get[F[_], S]: StateT[F, S, S] = ???
def set[F[_], S]: StateT[F, S, Unit] = ???
```

Инстанс монады:
```scala worksheet
implicit def stateTMonad[F[_] : Monad, S]: Monad[StateT[F, S, ?]] = new Monad[StateT[F, S, ?]] {
  def pure[A](a: A): StateT[F, S, A] =
    // State { s => (a, s) }
    ???

  def flatMap[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] =
    // State { s =>
    //   fa.run(s) |> { case (a, s$) => f(a).run(s$) }
    // }
    ???
}
```

Лифтинг:
```scala worksheet
implicit def liftK[F[_], S]: F ~> StateT[F, S, ?] = new FunctionK[F, StateT[F, S, ?]] {
  def apply[A](fa: F[A]): StateT[F, S, A] = ???
}
```

### Другие трансформеры

По такому же принципу выполнены многие другие трансформеры монад, которые можно найти в библиотеке Cats:

* OptionT -- ДЗ
* IorT -- ДЗ
* ContT

Семантика этих трансформеров соответствует семантике аналогичных нерасширяемых монад.