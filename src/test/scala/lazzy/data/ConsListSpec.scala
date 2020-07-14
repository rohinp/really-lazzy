package lazzy.data

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConsListSpec extends AnyWordSpec with Matchers{
  import Cons._
  "A cons list" when {
    "head" should {
      "get the head of a list" in {
        head(Cons(1 , 2 , ==>(???) )) shouldEqual Some(1)
      }
    }
  }
}