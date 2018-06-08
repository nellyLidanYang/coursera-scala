package streams

import common._

trait Solver extends GameDef {
  def done(b: Block): Boolean =  (b.b1 == goal && b.b2 == goal)

  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = (for {
    neigh <- b.legalNeighbors
  } yield (neigh._1, neigh._2 :: history)).toStream

  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = for {
    neigh <- neighbors
    if !(explored contains neigh._1)
  } yield neigh

  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = initial match {
    case Stream.Empty => Stream.Empty
    case (b, moves) #:: xs1 => {
      val neighbors = newNeighborsOnly(neighborsWithHistory(b, moves), explored + b)
      //for (n <- neighbors) if(n._1 == Block(Pos(4, 4), Pos(4, 4))) print(n)
      neighbors ++ from(xs1 ++ neighbors, explored + b) //bfs
    }
  }

//  {
//    println(explored)
//    val some = for {
//      init <- initial
//      neigh <- newNeighborsOnly(neighborsWithHistory(init._1, init._2), explored)
//    } yield neigh
//    println(some)
//    val more = initial #::: some
//    println(more)
//    more #::: from(some, explored ++ (some map (_._1)))
//  }

  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((startBlock, List())), Set(startBlock))

  lazy val pathsToGoal: Stream[(Block, List[Move])] = for {
    path <- pathsFromStart
    if (done(path._1))
  } yield path

  lazy val solution: List[Move] = {
    val tmp = pathsToGoal.take(1).toList
    if (tmp.length >= 1) tmp(0)._2.reverse
    else Nil
  }
}
