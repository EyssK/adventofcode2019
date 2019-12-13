import scala.io.Source

package day02 {
  object Day02 {
    object Program{
      var cur = 0
      var cont = true
      var data: List[Int] = List()
      def apply(init: List[Int], noun: Int, verb: Int) = {
        data = init.patch(1,List(noun,verb),2)
        cur = 0
        cont = true
      }
      def head: Int = data(0)
      def continue: Boolean = cont
      def execute  = {
        data(cur) match {
          case 1 => data = data.updated(data(cur+3), data(data(cur+1)) + data(data(cur+2)))
          case 2 => data = data.updated(data(cur+3), data(data(cur+1)) * data(data(cur+2)))
          case 99 => cont = false
          case _  => { 
            printf("Error execute: index %d equals %d\n", cur, data(cur))
            cont = false
          }
        }
        cur += 4
      }
    }

    def part1(init: List[Int], noun: Int, verb: Int): Int = {
      Program(init, noun, verb)
      do {
        Program.execute
      } while(Program.continue)
      Program.head
    }

    def part2(init: List[Int], goal: Int): Int = {
      val ans = for { 
        noun <- 0 to 100  
        verb <- 0 to 100
        if part1(init, noun, verb) == goal
      } yield ( noun , verb )
      val (x,y) = ans.head
      100 * x + y
    }

    def apply(file: String) = {
      println("Day 2")
      val lines = Source.fromFile(file).getLines.toList
      val initList = for (cmd <- lines.head.split(',').toList) yield cmd.toInt 

      printf("Part1: %d\n",part1(initList,12,2))
      printf("Part2: %d\n",part2(initList, 19690720))
    }
  }
}
