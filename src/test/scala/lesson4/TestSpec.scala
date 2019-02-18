package lesson4

import org.scalatest.{FreeSpec, MustMatchers}

import scala.language.implicitConversions

class TestSpec extends FreeSpec with MustMatchers {

  case class A(value: Int)
  case class B(value: Int)
  case class C(value: Int)

  case class CVS(values: List[String]) {
    def asString: String = values.mkString(",")
    def +(other: CVS): CVS = CVS(values ++ other.values)
  }

  trait AsCVS[T] {
    def toCVS(x: T): CVS
  }

  "implicit values" in {

    //В языке scala, в отличие от других языков, есть два скоупа. implicit и explicit.
    //Explicit scope: в него попадают все обычные функции и переменные, вроде val x = 5.
    //Implicit scope содержит функции и значения, перед которыми стоит слово implicit.
    //Любое implicit значение также является и explicit, наоборот не верно.
    implicit val x: A = A(1)
  }

  "implicit functions" in {

    //в отличие от implicit value объект инстанциируется при каждом вызове
    implicit def functionWithoutArguments: A = A(1)

    //особый случай, требует специального импорта, называется implicit conversion
    implicit def functionWithOneExplicitParameter(x: A): B = B(x.value)

    //странный вариант не имеющий практического применения
    implicit def functionWithNExplicitParameters(x: A, y: B): C = C(x.value + y.value)

    //полезный вариант, используется для построения цепочек имплиситов
    implicit def functionWithImplicitParameters(implicit x: A, y: B): C = C(x.value + y.value)

    //синтаксический сахар, называемый context bound, и эквивалентен функции ниже
    //функция implicitly будет объяснена далее
    def functionWithContextBound[T : AsCVS](x: T): CVS = implicitly[AsCVS[T]].toCVS(x)

    def functionWithExpandedContextBound[T](x: T)(implicit asCVS: AsCVS[T]): CVS = asCVS.toCVS(x)

    //надо заметить, слово implicit ставится вначале списка параметров и относится ко всем параметрам в списке.
    //список параметров со словом implicit обязан быть последним
  }

  "implicit resolution" - {

    //implicit параметры в функции ищутся компилятором в implicit scope и их не нужно подставлять вручную.
    //важный момент: поиск имплиситов ощущается в compile time
    "parameters" in {

      def printA(implicit x: A): Unit = println(s"x = $x")

      //можно не указывать параметр, если в implicit scope есть значение подходящего типа
      implicit val x: A = A(5)

      printA

      //с другой стороны можно указать значение явно
      printA(A(10))

      //в частности передать можно implicit value
      printA(x)
    }

    //можно строить цепочки имплиситов
    "implicit chains" in {

      implicit val x: A = A(5)

      implicit def toB(implicit x: A): B = {
        println(s"toB(x = $x)")
        B(x.value + 1)
      }

      implicit def toC(implicit x: B): C = {
        println(s"toC(x = $x)")
        C(x.value + 1)
      }

      def printC(implicit x: C): Unit = println(s"x = $x")

      printC
    }

    //Особый случай implicit conversion, требует специального импорта и как правило является нежелательной практикой.
    //Существует только один вариант применения этой возможности, который считается нормальным и называется
    //syntax extension, о чем далее
    "implicit conversion" in {

      implicit def aToB(x: A): B = {
        println(s"aToB(x = $x)")
        B(x.value)
      }

      val x: B = A(5)

      println(s"x = $x")
    }

    //implicitly - функция стандартной библиотеки. Выглядит она так
    //def implicitly[T](implicit e: T) = e
    //всё, что она делает, это ищет значение указанного типа в implicit scope
    "implicitly" in {
      implicit val x: A = A(1)
      println(implicitly[A])
    }

    //возможность, которая понадобится нам при объяснении тайп-классов
    "type classes related feature" in {

      implicit val aAsCVS: AsCVS[A] = new AsCVS[A] {
        override def toCVS(x: A): CVS = CVS(List(x.toString))
      }

      implicit val bAsCVS: AsCVS[B] = new AsCVS[B] {
        override def toCVS(x: B): CVS = CVS(List(x.toString))
      }

      def toCVS[T](x: T)(implicit asCVS: AsCVS[T]): CVS = asCVS.toCVS(x)

      //эквивалент функции выше
      def toCVS2[T : AsCVS](x: T): CVS = implicitly[AsCVS[T]].toCVS(x)

      val res: CVS = toCVS(A(1)) + toCVS(B(1))
      println(s"res = ${res.asString}")
    }
  }

  //с помощью implicit conversions можно "расширять" набор методов на каком-то классе
  "syntax extension" in {

    class IntExtensions(val x: Int) {
      def prettyPrint(): Unit = println(s"pretty int x = ~~~ $x ~~~")
    }

    implicit def toExtensions(x: Int): IntExtensions = new IntExtensions(x)

    5.prettyPrint()
  }

  //implicit class является синтаксическим сахаром для возможности syntax extension, описанной выше
  //пример ниже абсолютно эквивалентен тому, что выше
  "implicit classes" in {

    implicit class IntExtensions(val x: Int) {
      def prettyPrint(): Unit = println(s"pretty int x = ~~~ $x ~~~")
    }

    10.prettyPrint()
  }
}
