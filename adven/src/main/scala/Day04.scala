
package day04 {
  object Day04 {

    def twoDigit(i: Int) : Boolean = {
      val s = i.toString
      val res = for {l <- s.init.zip(s.tail) if l._1 == l._2} yield true
      !res.isEmpty
    }
        
    def onlyTwoDigit(i: Int) : Boolean = {
      val s : String = "A" + i + "B"
      val res = for {l <- 1 to 5 if s(l) == s(l+1) && s(l) != s(l+2) && s(l-1) != s(l+1)} yield true
      !res.isEmpty
    }

    def neverDecre(i: Int) : Boolean = {
      val s = i.toString
      val res = for {l <- s.init.zip(s.tail) if l._1.toInt > l._2.toInt} yield true
      res.isEmpty
    }

    def part1(l: Int, h: Int) : Int = {
      val res = for { i <- l to h if twoDigit(i) && neverDecre(i) } 
      yield i
      res.length
    }

    def part2(l: Int, h: Int) : Int = {
      val res = for { i <- l to h if neverDecre(i) && onlyTwoDigit(i) } 
      yield i
      res.length
    }

    def apply() = {
      println("Day 4")

      val input = (272091,815432)

      printf("Part1: %d\n",part1(input._1, input._2))
      printf("Part2: %d\n",part2(input._1, input._2))
    }
  }
}
