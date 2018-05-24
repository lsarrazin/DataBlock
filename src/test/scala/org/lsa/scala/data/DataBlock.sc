package org.lsa.scala.data

object DataBlock {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val tpath = List("//", "/", "/toto", "/toto/titi", "toto", "toto/titi")
                                                  //> tpath  : List[String] = List(//, /, /toto, /toto/titi, toto, toto/titi)
  
  def nodePath(path: String): List[String] = {
    if (path.length == 0) Nil
    else if (((path.length == 1) && (path(0) == '/')) || (path == "//")) List("")
    else path.split('/').toList
  }                                               //> nodePath: (path: String)List[String]
  
  tpath.map(nodePath)                             //> res0: List[List[String]] = List(List(""), List(""), List("", toto), List("",
                                                  //|  toto, titi), List(toto), List(toto, titi))
  
}