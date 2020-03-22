//Name: Jacob Scriffiny
//File: implementation.scala

object implementation extends App {

  sealed trait LHeap { // max-oriented leftist heap
  // Order Invariant:
  //    for each node, the item is >= items of child nodes
  // Shape Invariant:
  //    for each node, the height of the left child >= height of the right child

  def height: Int
  def insert(x: Int): LHeap
  // and of course there would be more methods...
}

case object Empty extends LHeap {
  def height: Int = 0
  def insert(x: Int): LHeap = Node(x, Empty, Empty)
}

case class Node(item: Int, left: LHeap, right: LHeap) extends LHeap {
  // cache the height so don't need to recompute it
  private val h: Int = 1 + (left.height max right.height)

  def height: Int = h
  def insert(x: Int): LHeap = {
    if (x > item) Node(x,this,Empty)
    else {
      val temp = right.insert(x)
      if (temp.height > left.height) copy(left=temp,right=left) //Node(item,temp,left)
      else copy(right=temp) //Node(item,left,temp)
    }
  }
}

  //Tests
  val tests = Array[(LHeap,Int,LHeap)](
    (Empty,3,
      Node(3,Empty,Empty)),
    (Node(4,Empty,Empty),2,
      Node(4,Node(2,Empty,Empty),Empty)),
    (Node(3,Empty,Empty),5,
      Node(5,Node(3,Empty,Empty),Empty)),
    (Node(5,Node(4,Empty,Empty),Empty),3,
      Node(5,Node(4,Empty,Empty),Node(3,Empty,Empty))),
    (Node(8,Node(6,Empty,Empty),Node(5,Empty,Empty)),4,
      Node(8,Node(5,Node(4,Empty,Empty),Empty),Node(6,Empty,Empty))),
    (Node(7,Node(3,Empty,Empty),Node(1,Empty,Empty)),6,
      Node(7,Node(6,Node(1,Empty,Empty),Empty),Node(3,Empty,Empty))),
    (Node(7,Node(3,Empty,Empty),Node(1,Empty,Empty)),8,
      Node(8,Node(7,Node(3,Empty,Empty),Node(1,Empty,Empty)),Empty))
  )

///////////////////////////////////////////////////////////////////

  def parseLHeap(lheap: LHeap): String = lheap match {
    case Empty => "*"
    case Node(item,left,right) =>
      item.toString + " -> (" + parseLHeap(left) + ", " + parseLHeap(right) + ")"
  }

  implicit class ConsoleColorise(val str: String) extends AnyVal {
    import Console._
    def red    = s"$RED$str"
    def green  = s"$GREEN$str"
    def white  = s"$WHITE$str"
    def cyan   = s"$CYAN$str"
  }

  (1 to 50) foreach(_ => print("-"))
  println("\n#########   TESTING   #########")
  (1 to 50) foreach(_ => print("-"))
  print("\n")
  var testNum = 1
  for (test <- tests) {
    println(("Test #" + testNum).cyan)
    testNum += 1
    print("".white)
    println("LHeap: " + parseLHeap(test._1))
    println("x: " + test._2 + "\n")
    println("Expected: " + parseLHeap(test._3))
    println("Received: " + parseLHeap(test._1.insert(test._2)))
    if (test._1.insert(test._2) == test._3) println("PASSED".green)
    else println("**FAILED**".red)
    print("".white)
    print("\n")
    (1 to 50) foreach(_ => print("-"))
    print("\n")
  }
  val num = tests.count(test => test._1.insert(test._2) == test._3)
  println("#########   Passed " + num + "/" + tests.length + " tests   #########")
  (1 to 50) foreach(_ => print("-"))
  print("\n")
}
