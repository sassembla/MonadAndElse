package com.kissaki

object FreeMonad {
  def main(arg: Array[String]): Unit = {
    // type class: Functor
    trait Functor[F[_]] {
      def map[A, B](m: F[A])(f: A => B): F[B]
    }

    // data Free f a = Pure a | Free (f (Free f a))
    sealed trait FreeM[S[+_], +A] {
      private case class FlatMap[S[+_], A, +B](a: FreeM[S, A], f: A => FreeM[S, B]) extends FreeM[S, B]

      def flatMap[B](f: A => FreeM[S, B]): FreeM[S, B] = this match {
        case a FlatMap g =>
          FlatMap(a, (x: Any) => g(x) flatMap f)
        case x => FlatMap(x, f)
      }
      def map[B](f: A => B): FreeM[S, B] = flatMap(a => Pure(f(a)))

      final def resume(implicit functor: Functor[S]): Either[S[FreeM[S, A]], A] =
        this match {
          case Pure(a) => Right(a)
          case Free(k) => Left(k)
          case a FlatMap f => a match {
            case Pure(a) => f(a).resume
            case Free(k) => Left(functor.map(k)(_ flatMap f))
            case b FlatMap g => b.flatMap((x: Any) => g(x) flatMap f).resume
          }
        }
    }

    case class Pure[S[+_], +A](a: A) extends FreeM[S, A]
    case class Free[S[+_], +A](k: S[FreeM[S, A]]) extends FreeM[S, A]

    // data CharIO a = GetCh (Char -> a) | PutCh Char a
    sealed trait CharIO[+A]
    case class GetCh[+A](f: Char => A) extends CharIO[A]
    case class PutCh[+A](c: Char, a: A) extends CharIO[A]

    // instance Functor CharIO where
    //    fmap f (GetCh g) = GetCh (f . g)
    //    fmap f (PutCh c x) = PutCh c (f x)

    implicit val charIOFunctor =
      new Functor[CharIO] {
        def map[A, B](a: CharIO[A])(f: A => B): CharIO[B] = a match {
          case GetCh(g) => GetCh(f compose g)
          case PutCh(c, x) => PutCh(c, f(x))
        }
      }

    // getCh :: Free CharIO Char
    // getCh = Free $ GetCh $ \ch -> Pure ch
    //
    // putCh :: Char -> Free CharIO ()
    // putCh ch = Free $ PutCh ch (Pure ())
    def getCh: FreeM[CharIO, Char] = Free(GetCh({ ch => Pure(ch) }))
    def putCh(ch: Char): FreeM[CharIO, Unit] = Free(PutCh(ch, Pure(())))

    // runStdIO :: Free CharIO a -> IO a
    // runStdIO (Pure a) = return a
    // runStdIO (Free (GetCh f)) = getChar >>= \ch -> runStdIO (f ch)
    // runStdIO (Free (PutCh ch cont)) = putChar ch >> runStdIO cont
    def runCharIO(free: FreeM[CharIO, Unit]): Unit =
      free.resume match {
        case Right(a) => a
        case Left(GetCh(f)) => runCharIO(f(readChar()))
        case Left(PutCh(ch, cont)) => print(ch); runCharIO(cont)
      }

    // mapM_
    def mapFreeM[S[+_], A](f: A => FreeM[S, Unit], s: Seq[A]): FreeM[S, Unit] = s.toList match {
      case x :: xs => xs.foldLeft(f(x)) { (m: FreeM[S, Unit], c: A) => m.flatMap { unit => f(c) } }
      case x: A => f(x)
    }

    //  ex0 = do
    //     mapM_ putCh "Hello, Haskeller! Please input a character:"
    //     ch <- getCh
    //     mapM_ putCh "The ordinal of the character is:"
    //     mapM_ putCh (show (ord ch))
    //     mapM_ putCh ".\n Thank you!\n" }

    val io: FreeM[CharIO, Unit] = for {
      _ <- mapFreeM(putCh, "Hello, Scala Programmer! Please input a character:")
      ch <- getCh
      _ <- mapFreeM(putCh, "The ordinal of the character is:")
      _ <- mapFreeM(putCh, ch.toInt.toString)
      _ <- mapFreeM(putCh, ".\n Thank you!\n")
    } yield ()

    runCharIO(io)

  }

}
