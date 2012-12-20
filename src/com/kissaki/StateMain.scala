/**
 * stateモナドについて理解したい。
 * このコードを段階的に書き出して、死霊化する。
 */

/**
  TとTStateのタプルを返す関数fを受け取るクラスMyState
*/
case class MyState[TState, +T](f: TState => (T, TState)) {
  println("MyStateに来てる")

  def apply(state: TState) = {
    println("applyに来てる")
    f(state)
  }

  def flatMap[U](rest: T => MyState[TState, U]): MyState[TState, U] = MyState((state: TState) => {
    println("flatMapに来てる")
    val (x, nextState) = f(state)
    rest(x)(nextState)
  })

  def myReturn[U](x: U): MyState[TState, U] = {
    println("myReturnに来てる")
    MyState((state: TState) => (x, state))
  }

  def map[U](f: T => U): MyState[TState, U] = {
    println("mapに来てる")
    flatMap((x: T) => myReturn(f(x)))
  }
}

class StateMain {
  println("getに来てる")
  def get[TState] = MyState((state: TState) => (state, state))
  println("putに来てる")
  def put[TState](state: TState) = MyState((_: TState) => ((), state))

  println("mainに来てる")
  def main(args: Array[String]) :Unit = {
    val resultF = {
      println("resultFに来てる")
      val ret = for {
        initVal <- get[Int]
        x = initVal + 1
        _ <- put(x * 2)
      } yield x

      println("retは "+ret)
      ret
    }
    println("プリントに来てる")
    println(resultF(0))
    // println(resultF(10))
    // println(resultF(10))
  }
}


val s = new StateMain
s.main(null)


