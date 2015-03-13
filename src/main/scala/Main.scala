import com.ning.http.client.Response
import dispatch._
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn
import scalaz.Free._

import scalaz._, Scalaz._

sealed trait HttpCommand[A]
case class Send(req: Req) extends HttpCommand[Response]

case class Error(cause: Throwable)

object Http {
  type Result[A] = EitherT[Future, Error, A]
  type Executor = HttpCommand ~> Result
}

case class HttpExecutor(http: Http)(implicit ec: ExecutionContext) extends Http.Executor {
  def apply[A](fa: HttpCommand[A]) = fa match {
    case Send(req) => EitherT.fromEither(http(req).either).leftMap(Error)
  }
}

object HttpCommands {
  def send[F[_]](req: Req)(implicit I: Inject[HttpCommand, F]): FreeC[F, Res] = Main.lift(Send(req))
}

sealed trait ConsoleCommand[A]
case class ReadLn(prompt: String) extends ConsoleCommand[String]

object Console {
  type Result[A] = EitherT[Future, Error, A]
  type Executor = ConsoleCommand ~> Result
}

object ConsoleExecutor extends Console.Executor {
  override def apply[A](fa: ConsoleCommand[A]): Console.Result[A] = fa match {
    case ReadLn(prompt) => {
      println(prompt)
      EitherT.right(Future.successful(StdIn.readLine()))
    }
  }
}

object ConsoleCommands {
  def readLn[F[_]](prompt: String)(implicit I: Inject[ConsoleCommand, F]): FreeC[F, String] = Main.lift(ReadLn(prompt))
}

object Main extends App {
  def lift[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): FreeC[G, A] = Free.liftFC(I.inj(fa))

  type App[A] = Coproduct[HttpCommand, ConsoleCommand, A]

  def prg[F[_]](implicit Ih: Inject[HttpCommand, F], Ic: Inject[ConsoleCommand, F]): FreeC[F, Int] = {
    for {
      path <- ConsoleCommands.readLn("Enter path")
      res <- HttpCommands.send(url("https://google.com") / path)
    } yield res.getStatusCode
  }

  val app: FreeC[App, Int] = prg[App]

  import ScalazExtensions._
  val exec = HttpExecutor(dispatch.Http)(scala.concurrent.ExecutionContext.Implicits.global) or ConsoleExecutor
  val run = Free.runFC(app)(exec).run

  run.onSuccess({
    case \/-(status) => println(status)
    case -\/(e) => println(e.cause)
  })
  Await.ready(run, 5.seconds)
}

object ScalazExtensions {
  implicit class NaturalTransformationExtentions[F[_], H[_]](f: F ~> H) {
    def or[G[_]](g: G ~> H): ({type cp[α]=Coproduct[F,G,α]})#cp ~> H = new NaturalTransformation[({type cp[α]=Coproduct[F,G,α]})#cp,H] {
      def apply[A](fa: Coproduct[F,G,A]): H[A] = fa.run match {
        case -\/(ff) ⇒ f(ff)
        case \/-(gg) ⇒ g(gg)
      }
    }
  }
}
