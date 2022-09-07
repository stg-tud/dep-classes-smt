package dcc.entailment

import dcc.program.Empty
import dcc.syntax.{Id, Path}
import org.scalatest.funsuite.AnyFunSuite
import dcc.syntax.Implicit.StringToId
import dcc.syntax.Path.fromString

class TestPathEnumeration extends AnyFunSuite {
  private val vars: List[Id] = List("x", "y", "z")
  private val fields: List[Id] = List("f", "g", "h")

  private def enumPaths(depth: Int): List[Path] = {
    val limitEnc = new PathDepthLimitEncoding(Empty.program, debug = 0)

    limitEnc.enumeratePaths(vars, fields, depth)
  }

  test ("depth 0") {
    val paths = enumPaths(0)

    assert(paths.size == 3)
    assert(paths.contains(fromString("pth_x")))
    assert(paths.contains(fromString("pth_y")))
    assert(paths.contains(fromString("pth_z")))
  }

  test ("depth 1") {
    val paths = enumPaths(1)

    assert(paths.size == 3+9)

    // Round 0
    assert(paths.contains(fromString("pth_x")))
    assert(paths.contains(fromString("pth_y")))
    assert(paths.contains(fromString("pth_z")))

    // Round 1
    assert(paths.contains(fromString("pth_x.f")))
    assert(paths.contains(fromString("pth_x.g")))
    assert(paths.contains(fromString("pth_x.h")))

    assert(paths.contains(fromString("pth_y.f")))
    assert(paths.contains(fromString("pth_y.g")))
    assert(paths.contains(fromString("pth_y.h")))

    assert(paths.contains(fromString("pth_z.f")))
    assert(paths.contains(fromString("pth_z.g")))
    assert(paths.contains(fromString("pth_z.h")))
  }

  test ("depth 2") {
    val paths = enumPaths(2)

    assert(paths.size == 3+9+27)

    // Round 0
    assert(paths.contains(fromString("pth_x")))
    assert(paths.contains(fromString("pth_y")))
    assert(paths.contains(fromString("pth_z")))

    // Round 1
    assert(paths.contains(fromString("pth_x.f")))
    assert(paths.contains(fromString("pth_x.g")))
    assert(paths.contains(fromString("pth_x.h")))

    assert(paths.contains(fromString("pth_y.f")))
    assert(paths.contains(fromString("pth_y.g")))
    assert(paths.contains(fromString("pth_y.h")))

    assert(paths.contains(fromString("pth_z.f")))
    assert(paths.contains(fromString("pth_z.g")))
    assert(paths.contains(fromString("pth_z.h")))

    // Round 2
    assert(paths.contains(fromString("pth_x.f.f")))
    assert(paths.contains(fromString("pth_x.f.g")))
    assert(paths.contains(fromString("pth_x.f.h")))
    assert(paths.contains(fromString("pth_x.g.f")))
    assert(paths.contains(fromString("pth_x.g.g")))
    assert(paths.contains(fromString("pth_x.g.h")))
    assert(paths.contains(fromString("pth_x.h.f")))
    assert(paths.contains(fromString("pth_x.h.g")))
    assert(paths.contains(fromString("pth_x.h.h")))

    assert(paths.contains(fromString("pth_y.f.f")))
    assert(paths.contains(fromString("pth_y.f.g")))
    assert(paths.contains(fromString("pth_y.f.h")))
    assert(paths.contains(fromString("pth_y.g.f")))
    assert(paths.contains(fromString("pth_y.g.g")))
    assert(paths.contains(fromString("pth_y.g.h")))
    assert(paths.contains(fromString("pth_y.h.f")))
    assert(paths.contains(fromString("pth_y.h.g")))
    assert(paths.contains(fromString("pth_y.h.h")))

    assert(paths.contains(fromString("pth_z.f.f")))
    assert(paths.contains(fromString("pth_z.f.g")))
    assert(paths.contains(fromString("pth_z.f.h")))
    assert(paths.contains(fromString("pth_z.g.f")))
    assert(paths.contains(fromString("pth_z.g.g")))
    assert(paths.contains(fromString("pth_z.g.h")))
    assert(paths.contains(fromString("pth_z.h.f")))
    assert(paths.contains(fromString("pth_z.h.g")))
    assert(paths.contains(fromString("pth_z.h.h")))
  }

  test("depth = 3 to 9") {
    def expectedNumberOfElements(i: Int): Int = expectedGrowthPerRoundUntil(i, vars.size).sum

    def expectedGrowthPerRoundUntil(i: Int, base: Int): List[Int] = i match {
      case n if n < 0 => Nil
      case 0 => List(base)
      case _ => base :: expectedGrowthPerRoundUntil(i-1, base*fields.size)
    }

    for (i <- 0 to 9) {
      assert(enumPaths(i).size == expectedNumberOfElements(i))
    }
  }
}
