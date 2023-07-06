//> using dep org.typelevel:catbird-effect3_2.13:22.12.0
//> using options -Ykind-projector:underscores

import cats.effect.IOApp
import cats.effect.IO

import cats.effect.kernel.Ref
import cats.data.State
import cats.Monad

trait DoST[A] { self =>
  def apply[S]: ST[S, A]

  def run: A = self.apply[Unit](())._2
}

trait ST[S, A] private extends Function[S, (S, A)] {
  protected class PrimitiveVar[S, A](var value: A)
}

object ST {
  def ref[S, A](a: A): ST[S, Ref[ST[S, _], A]] = new ST[S, Ref[ST[S, _], A]] {

    override def apply(v1: S): (S, Ref[ST[S, _], A]) =
      val var_ = new PrimitiveVar(a)
      val p = new Ref[ST[S, _], A] {

        override def tryUpdate(f: A => A): ST[S, Boolean] =
          new ST[S, Boolean] {
            override def apply(v1: S): (S, Boolean) =
              val old = var_.value
              val new_ = f(old)
              var_.value = new_
              (v1, old == new_)
          }

        override def set(a: A): ST[S, Unit] =
          new ST[S, Unit] {
            override def apply(v1: S): (S, Unit) =
              var_.value = a
              (v1, ())
          }

        override def tryModify[B](f: A => (A, B)): ST[S, Option[B]] =
          new ST[S, Option[B]] {
            override def apply(v1: S): (S, Option[B]) =
              val old = var_.value
              val (new_, b) = f(old)
              var_.value = new_
              (v1, if old == new_ then None else Some(b))
          }

        override def update(f: A => A): ST[S, Unit] =
          new ST[S, Unit] {
            override def apply(v1: S): (S, Unit) =
              var_.value = f(var_.value)
              (v1, ())
          }

        override def tryModifyState[B](state: State[A, B]): ST[S, Option[B]] =
          new ST[S, Option[B]] {
            override def apply(v1: S): (S, Option[B]) =
              val old = var_.value
              val (new_, b) = state.run(old).value
              var_.value = new_
              (v1, if old == new_ then None else Some(b))
          }

        override def modify[B](f: A => (A, B)): ST[S, B] =
          new ST[S, B] {
            override def apply(v1: S): (S, B) =
              val old = var_.value
              val (new_, b) = f(old)
              var_.value = new_
              (v1, b)
          }

        override def access: ST[S, (A, A => ST[S, Boolean])] =
          new ST[S, (A, A => ST[S, Boolean])] {
            override def apply(v1: S): (S, (A, A => ST[S, Boolean])) =
              val old = var_.value
              (v1, (old, (a: A) => new ST[S, Boolean] {
                override def apply(v1: S): (S, Boolean) =
                  var_.value = a
                  (v1, old == a)
              }))
          }

        override def get: ST[S, A] =
          new ST[S, A] {
            override def apply(v1: S): (S, A) =
              (v1, var_.value)
          }

        override def modifyState[B](state: State[A, B]): ST[S, B] =
          new ST[S, B] {
            override def apply(v1: S): (S, B) =
              val old = var_.value
              val (new_, b) = state.run(old).value
              var_.value = new_
              (v1, b)
          }

      }
      (v1, p)
  }
}

def runST[A](f: DoST[A]): A = f.apply[Unit](())._2

import cats.syntax.all.*

implicit def monadI[S]: Monad[ST[S, _]] = new Monad[ST[S, _]] {

  override def flatMap[A, B](fa: ST[S, A])(f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    override def apply(v1: S): (S, B) =
      val (s, a) = fa(v1)
      f(a)(s)
  }

  override def tailRecM[A, B](a: A)(f: A => ST[S, Either[A, B]]): ST[S, B] =
    f(a).flatMap {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => pure(b)
    }

  override def pure[A](x: A): ST[S, A] = new ST[S, A] {
    override def apply(v1: S): (S, A) = (v1, x)
  }

}


val test = new DoST[Int] {
  override def apply[S]: ST[S, Int] = for {
      ref <- ST.ref[S, Int](0)
      _ <- ref.update(_ + 1)
      _ <- ref.update(_ + 1)
      _ <- ref.update(_ * 5)
      a <- ref.get
    } yield a
}.run

object Main extends IOApp.Simple:
  def run: IO[Unit] = 
    IO(println("Hello world!")) *> IO(println(test))