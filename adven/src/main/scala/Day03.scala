import scala.io.Source
import scala.collection.mutable.ListBuffer

package day03 {
  object Day03 {
    case class Coord(x: Int, y: Int) {
      def dist() : Int = Math.abs(x)+Math.abs(y)
      def +(c: Coord) : Coord = Coord(x+c.x, y+c.y)
    }
    case class CoordDist(c: Coord, d: Int)

    case class Instruct(dir: Char, len: Int) 
    object Instruct {
      def apply(str: String) : Instruct = Instruct(str.head, str.substring(1).toInt)
    }

    def addCoord(start: (Coord,List[Coord]) , ins: Instruct) : (Coord,List[Coord]) = {
      val prev = start._1
      val s = ins.dir match {
        case 'R' => for { i <- 1 to ins.len } yield prev + Coord(+i, 0)
        case 'L' => for { i <- 1 to ins.len } yield prev + Coord(-i, 0)
        case 'D' => for { i <- 1 to ins.len } yield prev + Coord( 0, -i)
        case 'U' => for { i <- 1 to ins.len } yield prev + Coord( 0, +i)
      }
      ( s.last , List.concat(s.toList ,start._2) )
    }

    def addCoordDist(start: (CoordDist,List[CoordDist]) , ins: Instruct) : (CoordDist,List[CoordDist]) = {
      val prevD = start._1.d
      val prevC = start._1.c
      val s = ins.dir match {
        case 'R' => for { i <- 1 to ins.len } yield CoordDist(prevC + Coord(i , 0)  , i+prevD)
        case 'L' => for { i <- 1 to ins.len } yield CoordDist(prevC + Coord(-i, 0)  , i+prevD)
        case 'D' => for { i <- 1 to ins.len } yield CoordDist(prevC + Coord(0 ,-i),   i+prevD)
        case 'U' => for { i <- 1 to ins.len } yield CoordDist(prevC + Coord(0 , i),   i+prevD)
      }
      ( s.last , List.concat(s.toList ,start._2) )
    }

    def getCoord(list: List[Instruct]) : Set[Coord] = {
      list.foldLeft(Coord(0,0), List[Coord]())(addCoord)._2.toSet
    }

    def getCoordDist(list: List[Instruct]) : List[CoordDist] = {
      list.foldLeft(CoordDist(Coord(0,0),0), List[CoordDist]())(addCoordDist)._2
    }

    def getShortest(listCoord: Set[Coord]) : Int = {
      val dists = for { c <- listCoord } yield c.dist
      dists.min
    }

    def getIntersect(wire1: Set[Coord],wire2: Set[Coord]): Set[Coord] = {
      for { 
        i <- wire1 
        if wire2.contains(i)}
      yield i
    }

    def part1(wire1: List[Instruct], wire2: List[Instruct]) : Int = 
      getShortest(getIntersect(getCoord(wire1), getCoord(wire2)))

    def part2(wire1: List[Instruct], wire2: List[Instruct]) : Int = {
      val inter = getIntersect(getCoord(wire1), getCoord(wire2))
      val coor1 = getCoordDist(wire1)
      val coor2 = getCoordDist(wire2)
      val ic1 = for { c <- coor1 if inter.contains(c.c) } yield c
      val ic2 = for { c <- coor2 if inter.contains(c.c) } yield c
      val res = for { c1 <- ic1; c2 <- ic2 if c1.c == c2.c } yield c1.d + c2.d
      res.min
    }

    def apply(file: String) = {
      println("Day 3")
      val lines = Source.fromFile(file).getLines.toList
      val wire1: List[Instruct] = for (cmd <- lines(0).split(',').toList) yield Instruct(cmd)
      val wire2: List[Instruct] = for (cmd <- lines(1).split(',').toList) yield Instruct(cmd)

      printf("Part1: %d\n",part1(wire1, wire2))
      printf("Part2: %d\n",part2(wire1, wire2))
    }
  }
}
