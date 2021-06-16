
trait Queue {

  var front: Int = -1
  var rear: Int = -1
  var queue:List[Int] = List.empty

  def isEmpty: Boolean ={
    if(front == -1 && rear == -1) true else false
  }

  def enqueue(element: Int)

  def dequeue(): Unit ={
    "Now that we are writing the body for dequeue, we have to override it, otherwise it will show error"
  }

  def getQueue: List[Int] = queue

}

class DoubleQueue extends Queue
{
  def enqueue(element: Int): Unit = {
    if (isEmpty){
      front+=1
      rear+=1
    }
    else rear+=1

    val doubleQueue = 2 * element
    queue = queue ::: List(doubleQueue)
    println(getQueue)
  }

  override def dequeue(): Unit = {
    if(isEmpty) return
    else if(front==rear) {
      queue = queue.drop(1)
      front = -1
      rear = -1
    }
    else {
      queue = queue.drop(1)
      front+=1
    }
    println(getQueue)
  }
}

class SquareQueue extends Queue {
  def enqueue(element:Int): Unit = {
    if (isEmpty){
      front+=1
      rear+=1
    }
    else rear+=1

    val squareQueue = element * element
    queue = queue ::: List(squareQueue)
    println(getQueue)
  }

  override def dequeue(): Unit = {
    if(isEmpty) return
    else if(front==rear) {
      queue = queue.drop(1)
      front = -1
      rear = -1
    }
    else {
      queue = queue.drop(1)
      front+=1
    }
    println(getQueue)
  }
}

object Main{
  def main(args: Array[String]): Unit = {
    val objectDoubleQueue = new DoubleQueue
    val objectSquareQueue = new SquareQueue
    objectDoubleQueue.enqueue(1)
    objectDoubleQueue.enqueue(2)
    objectDoubleQueue.enqueue(3)
    objectDoubleQueue.dequeue()
    objectDoubleQueue.dequeue()
    objectDoubleQueue.dequeue()
    objectDoubleQueue.enqueue(3)
    objectDoubleQueue.dequeue()

    objectSquareQueue.enqueue(4)
    objectSquareQueue.enqueue(5)
    objectSquareQueue.enqueue(6)
    objectSquareQueue.dequeue()
  }
}
