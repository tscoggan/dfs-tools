package utils

import scala.collection.mutable.ArrayBuffer

class TopNArray[T <% Ordered[T]](maxSize: Int) {

  val array: ArrayBuffer[T] = new ArrayBuffer[T](maxSize)

  private var minElementIndex: Int = -1 // index of element with the smallest ordering value (i.e. the bottom element)

  def minElement: T = array(minElementIndex)

  def add(newElement: T): Unit = {
    if (array.size < maxSize) {
      // not full yet -> add the new element
      array += newElement
      minElementIndex = array.indexOf(array.min)
    } else if (newElement > minElement) {
      // full and new element is larger than minimum element -> replace the minimum element with the new element
      array.update(minElementIndex, newElement)
      minElementIndex = array.indexOf(array.min)
    }
  }
  
  def toList: List[T] = array.toList

}