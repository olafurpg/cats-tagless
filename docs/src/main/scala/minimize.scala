package minimize
object app {
import cats.tagless._
import util.Try
import cats.tagless.implicits._
import cats.implicits._
import cats._
import cats.free.Free
import cats.arrow.FunctionK

@finalAlg
@autoFunctorK
@autoSemigroupalK
@autoProductNK
trait ExpressionAlg[F[_]] {
  def num(i: String): F[Float]
  def divide(dividend: Float, divisor: Float): F[Float]
}
import ExpressionAlg.autoDerive._
@finalAlg @autoFunctorK
trait Increment[F[_]] {
  def plusOne(i: Int): F[Int]
}
import Increment.autoDerive._

implicit object incTry extends Increment[Try] {
  def plusOne(i: Int) = Try(i + 1)
}

def program[F[_]: Monad: Increment](i: Int): F[Int] = for {
  j <- Increment[F].plusOne(i)
  z <- if (j < 10000) program[F](j) else Monad[F].pure(j)
} yield z


implicit def toFree[F[_]]: F ~> Free[F, *] = λ[F ~> Free[F, *]](t => Free.liftF(t))
program[Free[Try, *]](0).foldMap(FunctionK.id)
}
