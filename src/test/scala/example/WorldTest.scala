package example

import org.scalatest.{FlatSpec, MustMatchers}

class WorldTest extends FlatSpec with MustMatchers {

  "cells with less than 2 neighbours" should "die" in {
    World(Seq(Cell(0,0), Cell(0,1))).getNextGen mustBe World(Seq.empty)
  }

  "cells with neighbours" should "survive" in {
    World(Seq(Cell(0,0), Cell(1,1), Cell(2,2))).getNextGen mustBe World(Seq(Cell(1,1)))
  }

  "cell with 4 neighbours" should "die" in {
    World(Seq(Cell(0,0), Cell(-1, 0), Cell(0, -1), Cell(0,1), Cell(1, 0))).getNextGen mustBe
      World(Seq(Cell(1,-1), Cell(1,0), Cell(-1,0), Cell(-1,1), Cell(-1,-1), Cell(0,1), Cell(1,1), Cell(0,-1)))
  }

  "dead cell with 3 neighbours" should "be born" in {
    World(Seq(Cell(0,0), Cell(0,1), Cell(0,2))).getNextGen mustBe World(Seq(Cell(-1,1), Cell(0,1), Cell(1,1)))
  }

  "all cells with 2 neighbours" should "nothing will change" in {
    World(Seq(Cell(0,-1), Cell(0,1), Cell(-1,0), Cell(1, 0))).getNextGen mustBe World(Seq(Cell(1,0), Cell(-1,0), Cell(0,1), Cell(0,-1)))
  }

}
