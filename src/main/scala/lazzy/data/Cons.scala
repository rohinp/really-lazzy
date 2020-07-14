package lazzy.data
sealed trait Cons[+A]
object Cons {
  case object Empty extends Cons[Nothing]
  case class ConsList[A] private (h: () => A, t: () => Cons[A]) extends Cons[A]

  type Thunk[A] = () => A

  implicit def toThunk[A](a: =>A): Thunk[A]  = () => a

  //For things which are not values, implicit conversion doesn't works
  //Need a better syntax to achieve this
  def ==>[A](a: => A): () => A = () => a

  def apply[A](args: Thunk[A]*): Cons[A] = if(args.isEmpty) Empty else ConsList(args.head, () => Cons(args.tail: _*))

  def apply[A](a: => A, cons: => Cons[A] = Empty): Cons[A] = ConsList(a, cons)

  def head[A](consList: Cons[A]): Option[A] = consList match {
    case Empty      => None
    case ConsList(h, _) => Some(h())
  }

  def tail[A](consList: Cons[A]): Option[Cons[A]] =
    (consList: Cons[A]) match {
      case Empty      => None
      case ConsList(_, t) => Some(t())
    }

  def length[A](consList: Cons[A]): Int = {
    @scala.annotation.tailrec
    def loop(l: Cons[A], acc: Int): Int = l match {
      case Empty      => acc
      case ConsList(_, t) => loop(t(), acc + 1)
    }
    loop(consList, 0)
  }

  def eval[A](consList: Cons[A]): List[A] = consList match {
    case Empty      => Nil
    case ConsList(h, t) => h() :: eval(t())
  }

  def take[A](consList: Cons[A])(num: Int): Cons[A] = {
    @scala.annotation.tailrec
    def loop(l: Cons[A], counter: Int, acc: Cons[A]): Cons[A] =
      l match {
        case _ if counter == 0 => acc
        case Empty             => acc
        case ConsList(h, t)        => loop(t(), counter - 1, ConsList(h, acc))
      }
    loop(consList, num, Empty)
  }

  @scala.annotation.tailrec
  def foldl[A, B](f: B => (=> A) => B)(acc: B)(list: => Cons[A]): B =
    list match {
      case Empty      => acc
      case ConsList(h, t) => foldl(f)(f(acc)(h()))(t())
    }

  def foldr[A, B](f: (=> A) => B => B)(acc: B)(list: => Cons[A]): B =
    list match {
      case Empty      => acc
      case ConsList(h, t) => f(h())(foldr(f)(acc)(t()))
    }

  def map[A, B](f: => A => B)(cas: => Cons[A]): Cons[B] = cas match {
    case Empty      => Empty
    case ConsList(h, t) => ConsList[B](() => f(h()), () => map(f)(t()))
  }
}
