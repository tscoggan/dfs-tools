package test

import org.scalatest.FunSuite
import utils.StringUtils._

class StringUtilsTests extends FunSuite {
  
  test("splitCSV works without quotation marks") {
    val s = "a,b,c,d,e"
    assert(s.splitCSV().deep == Array("a","b","c","d","e").deep)
  }
  
  test("splitCSV works with quotation marks") {
    val s = "a,b,\"c,d\",e"
    assert(s.splitCSV().deep == Array("a","b","c,d","e").deep)
  }

  test("trimPrefix works") {
    assert(",p.BLAH".trimPrefix(",p.") == "BLAH")
  }
  
  test("trimPrefix works on missing prefix") {
    assert("BLAH".trimPrefix(",p.") == "BLAH")
  }
  
  test("trimSuffix works") {
    assert("BLAH,p.".trimSuffix(",p.") == "BLAH")
  }
  
  test("trimSuffix works on missing prefix") {
    assert("BLAH".trimSuffix(",p.") == "BLAH")
  }
  
  test("substringsBetween works") {
    val results = "ab(cd)e(f)gh".substringsBetween("(",")")
    assert(results == List("cd", "f"))
  }
  
  test("substringsBetween works for multi-character prefix & suffix") {
    val results = "ab(cd)e(f)ghab(cd)e(f)gh".substringsBetween("b(",")e")
    assert(results == List("cd", "cd"))
  }
  
  test("substringsBetween works for invalid prefix") {
    val results = "ab(cd)e(f)gh".substringsBetween("X",")")
    assert(results == Nil)
  }
  
  test("substringsBetween works for invalid suffix") {
    val results = "ab(cd)e(f)gh".substringsBetween("(","X")
    assert(results == Nil)
  }
  
  test("substringsBetween works for invalid prefix & suffix") {
    val results = "ab(cd)e(f)gh".substringsBetween("X","X")
    assert(results == Nil)
  }

}