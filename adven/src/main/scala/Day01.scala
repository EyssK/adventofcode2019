import scala.io.Source

package day01 {
  object Day01 {
    def computeFuel(mass: Int): Int= List(mass/3-2, 0).max
    def step1(lines: List[Int]): Int = {
      val tot = for (module <- lines) 
        yield computeFuel(module)
      tot.sum
    }
    def computeFuel2(mass: Int): Int = {
      var tot: Int = 0
      var rem: Int = mass
      while(rem>0){
        rem = computeFuel(rem)
        tot += rem
      } 
      tot
    }
    def step2(lines: List[Int]): Int = {
      val tot = for (module <- lines) 
      yield computeFuel2(module)
      tot.sum  
    }
    def apply(file: String) = {
      println("Day 1")
      val lines = for (module <- Source.fromFile(file).getLines)
      yield module.toInt
      printf("Part1: %d\n" ,step1(lines.toList))
      val lines2 = for (module <- Source.fromFile(file).getLines)
      yield module.toInt
      printf("Part2: %d\n", step2(lines2.toList))
    }
  }
}
