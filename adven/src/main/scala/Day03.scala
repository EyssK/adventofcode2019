import scala.io.Source
import scala.collection.mutable.ListBuffer

package day03 {
  object Day03 {
    case class Coord(x: Int, y: Int) {
      def dist() : Int = Math.abs(x)+Math.abs(y)
    }

    final val R = 'R'
    final val L = 'L'
    final val U = 'U'
    final val D = 'D'
    case class Instruct(dir: Char, len: Int) 
    object Instruct {
      def apply(str: String) : Instruct = Instruct(str.head, str.substring(1).toInt)
    }

    def addCoord(ins: Instruct, start: Coord) : List[Coord] = {
      val s = ins.dir match {
        case R => for { i <- 1 to ins.len } yield Coord(start.x+i,start.y)
        case L => for { i <- 1 to ins.len } yield Coord(start.x-i,start.y)
        case D => for { i <- 1 to ins.len } yield Coord(start.x,start.y-i)
        case U => for { i <- 1 to ins.len } yield Coord(start.x,start.y+i)
      }
      s.toList
    }
    def getCoord(list: List[Instruct]) : Set[Coord] = {
      var curCoor = Coord(0,0)
      var coords = new ListBuffer[Coord]()
      for { ins <- list }
      {
        coords.appendAll(addCoord(ins, curCoor))
        curCoor = coords.last
      }
      coords.toSet
    }
    def getShortest(listCoord: Set[Coord]) : Int = {
      val dists = for { c <- listCoord } yield c.dist
      dists.min
    }
    def getIntersect(wire1: Set[Coord],wire2: Set[Coord]): Set[Coord] = {
      println(wire1.size)
      println(wire2.size)
      for { 
        i <- wire1 
        if wire2.contains(i)}
      yield i
    }

    def part1(wire1: List[Instruct], wire2: List[Instruct]) : Int = 
      getShortest(getIntersect(getCoord(wire1), getCoord(wire2)))

    def apply(file: String) = {
      println("Day 3")
      val lines = Source.fromFile(file).getLines.toList
      val wire1: List[Instruct] = for (cmd <- lines(0).split(',').toList) yield Instruct(cmd)
      val wire2: List[Instruct] = for (cmd <- lines(1).split(',').toList) yield Instruct(cmd)

      println(wire1.head)
      println(wire2.head)

      printf("Part1: %d\n",part1(wire1, wire2))
      //printf("Part2: %d\n",part2(initList, 19690720))
    }
  }
}
