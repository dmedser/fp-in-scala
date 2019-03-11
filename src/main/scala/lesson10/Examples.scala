import lesson10.ApplicativeOps._
import cats.syntax.either._
import lesson10.Applikative

object Examples extends App {

  implicit val applikativeForOption: Applikative[Option] = new Applikative[Option] {
    def unit: Option[Unit] = Some(())
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
      (fa, fb) match {
        case (Some(a), Some(b)) => Some((a, b))
        case _ => None
      }
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  // Validated
  case class Validated[A](unwrap: Either[List[String], A]) extends AnyVal

  object Validated {
    implicit val applikativeValidated: Applikative[Validated] =
      new Applikative[Validated] {
        def unit: Validated[Unit] = Validated(Right(()))
        def product[A, B](fa: Validated[A], fb: Validated[B]): Validated[(A, B)] =
          (fa.unwrap, fb.unwrap) match {
            case (Right(a), Right(b)) => Validated(Right((a, b)))
            case (Left(as), Left(bs)) => Validated(Left(as ++ bs))
            case (Left(as), _) => Validated(Left(as))
            case (_, Left(bs)) => Validated(Left(bs))
          }
        def map[A, B](fa: Validated[A])(f: A => B): Validated[B] =
          Validated(fa.unwrap.map(f))
      }
  }

  def validateMaxLen(in: String, len: Int): Validated[String] =
    Validated {
      if (in.length >= len) in.asRight
      else List(s"length expected: >=$len, actual: ${in.length}").asLeft
    }

  def validateAge(in: Int): Validated[Int] =
    Validated {
      if (in >= 18 && in <= 65) in.asRight else List(s"invalid age").asLeft
    }

  case class Person(name: String, age: Int)

  def validatePerson(name: String, age: Int): Validated[Person] =
    (
      validateMaxLen(name, 5),
      validateAge(age)
    ).map2(Person.apply)

  println {
    validatePerson("Tony", 25)
  }
}