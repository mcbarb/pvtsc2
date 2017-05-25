/**
  * Created by marcelo on 5/11/17.
  */

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  print(lo + " ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo,streamRange(lo+1,hi))
}

object main extends App {
  streamRange(0,10)
}

