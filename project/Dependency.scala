import sbt._

object Dependency {

  object Version {
    val akka = "2.5.17"
    val akkaHttp = "10.1.5"
    val cats = "1.4.0"
    val circe = "0.9.3"
    val doobie = "0.5.3"
    val doobieNew = "0.6.0"
    val macwire = "2.3.1"
    val enumeratum = "1.5.13"
    val derevo = "0.5.1"
    val oauth2OidcSdk = "5.64.4"
    val opRabbit = "2.1.0"
    val monocle = "1.5.1-cats"
    val monix = "3.0.0-RC2"
  }

  val akka = "com.typesafe.akka" %% "akka-actor" % Version.akka
  val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % Version.akka

  val akkaStream = "com.typesafe.akka" %% "akka-stream" % Version.akka
  val akkaStreamTestkit = "com.typesafe.akka" %% "akka-stream-testkit" % Version.akka

  val akkaHttp = "com.typesafe.akka" %% "akka-http" % Version.akkaHttp
  val akkaHttpTestkit = "com.typesafe.akka" %% "akka-http-testkit" % Version.akkaHttp

  val akkaHttpCirce = "de.heikoseeberger" %% "akka-http-circe" % "1.20.1"
  val akkaHttpCors = "ch.megard" %% "akka-http-cors" % "0.3.0"

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"

  val catsCore = "org.typelevel" %% "cats-core" % Version.cats
  val catsFree = "org.typelevel" %% "cats-free" % Version.cats
  val alleyCatsCore = "org.typelevel" %% "alleycats-core" % Version.cats

  val catsEffect = "org.typelevel" %% "cats-effect" % "1.0.0"

  val catsMTLCore = "org.typelevel" %% "cats-mtl-core" % "0.4.0"

  val circeCore = "io.circe" %% "circe-core" % Version.circe
  val circeGeneric = "io.circe" %% "circe-generic" % Version.circe
  val circeGenericExtras = "io.circe" %% "circe-generic-extras" % Version.circe
  val circeParser = "io.circe" %% "circe-parser" % Version.circe
  val circeShapes = "io.circe" %% "circe-shapes" % Version.circe
  val circeJava8 = "io.circe" %% "circe-java8" % Version.circe

  val doobieCore = "org.tpolecat" %% "doobie-core" % Version.doobie
  val doobiePostgres = "org.tpolecat" %% "doobie-postgres" % Version.doobie
  val doobieHikari = "org.tpolecat" %% "doobie-hikari" % Version.doobie
  val doobieScalatest = "org.tpolecat" %% "doobie-scalatest" % Version.doobie

  val doobieCoreNew = "org.tpolecat" %% "doobie-core" % Version.doobieNew
  val doobiePostgresNew = "org.tpolecat" %% "doobie-postgres" % Version.doobieNew
  val doobieHikariNew = "org.tpolecat" %% "doobie-hikari" % Version.doobieNew
  val doobieScalatestNew = "org.tpolecat" %% "doobie-scalatest" % Version.doobieNew

  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"

  val apacheCommonCodecs = "commons-codec" % "commons-codec" % "1.11"

  val scalaConductrBundleLib = "com.typesafe.conductr" %% "scala-conductr-bundle-lib" % "1.9.0"

  val macwireMacros = "com.softwaremill.macwire" %% "macros" % Version.macwire
  val macwireUtil = "com.softwaremill.macwire" %% "util" % Version.macwire
  val macwireProxy = "com.softwaremill.macwire" %% "proxy" % Version.macwire

  val pureConfig = "com.github.pureconfig" %% "pureconfig" % "0.9.1"

  val fastParse = "com.lihaoyi" %% "fastparse" % "1.0.0"

  val enumeratum = "com.beachape" %% "enumeratum" % Version.enumeratum
  val enumeratumCirce = "com.beachape" %% "enumeratum-circe" % Version.enumeratum

  val authClientLib = "ru.raiffeisen.rbp" %% "rbp-authorization-client-lib" % "0.2.0"
  val typedSchema = "ru.tinkoff" %% "typed-schema" % "0.10.6"
  val derevoCirce = "org.manatki" %% "derevo-circe" % Version.derevo
  val derevoTschema = "org.manatki" %% "derevo-tschema" % Version.derevo
  val nimbus = "com.nimbusds" % "oauth2-oidc-sdk" % Version.oauth2OidcSdk

  val nimbusGen = "com.nimbusds" % "nimbus-jose-jwt" % "6.0.1"

  val flyway = "org.flywaydb" % "flyway-core" % "5.1.4"

  val jms = "com.ibm.mq" % "com.ibm.mq.allclient" % "9.0.5.0"
  val alpakkaJms = "com.lightbend.akka" %% "akka-stream-alpakka-jms" % "0.20"
  val scalaxb = "org.scalaxb" %% "scalaxb" % "1.7.0"
  val scalaxml = "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
  val scalaparser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

  val alpakka = "com.lightbend.akka" %% "akka-stream-alpakka-amqp" % "1.0-M1"
  
  val streamz =  "com.github.krasserm" %% "streamz-converter" % "0.10-M1"

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"

  val monocleCore = "com.github.julien-truffaut" %% "monocle-core" % Version.monocle
  val monocleMacro = "com.github.julien-truffaut" %%  "monocle-macro" % Version.monocle

  val monix = "io.monix" %% "monix" % Version.monix
}
