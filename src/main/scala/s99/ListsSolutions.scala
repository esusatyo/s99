package s99

import Solutions._

trait ListsSolutions {

  def last[T](list: List[T]): T = list (list.size - 1)

  def penultimate[T](list: List[T]): T = list (list.size - 2)

  def nth[T](n: Int, list: List[T]): T = list (n)

  def length[T](list: List[T]): Int = list.size

  def reverse[T](list: List[T]): List[T] = list.reverse

  def isPalindrome[T](list: List[T]): Boolean = list match {
		case a if (a.size <= 1) 	=> true
		case a if (a(0) == last(a)) => isPalindrome(a.slice(1, a.size-1))
		case _ 						=> false
	}

  //Not working
  def flatten(list: List[Any]): List[Any] = list match {
		case Nil 			=> List()
		case (a :: tail)	=> a :: flatten(tail)
		case a 				=> flatten(a)
	}

  def compress[T](list: List[T]): List[T] = list match {
		case Nil						=> Nil
		case a :: b	:: tail if(a == b) 	=> compress(a :: tail) 
	    case a :: tail					=> a :: compress (tail)
	}

  def pack[T](list: List[T]): List[List[T]] = list match {
		case Nil		=> Nil
		case a :: tail 	=> ((a :: tail.takeWhile(_ == a)) :: pack(tail.dropWhile(_ == a)))
	}
		
  def encode[T](list: List[T]): List[(Int, T)] = 
	pack(list).map(l => (l.size, l.head))
		
  def encodeModified[T](list: List[T]): List[Any] = 
	pack(list).map(l => if(l.size == 1) l.head else (l.size, l.head))

  def decode[T](list: List[(Int, T)]): List[T] =
	list.map{case (s, a) => List.fill(s)(a)}.flatten

  def encodeDirect[T](list: List[T]): List[(Int, T)] = 	list match {
		case Nil		=> Nil
		case a :: tail 	=> (((tail.takeWhile(_ == a).size) + 1, a) :: encodeDirect(tail.dropWhile(_ == a)))
	}
	
  def duplicate[T](list: List[T]): List[T] = 
	list.flatMap(l => List.fill(2)(l))
	
  def duplicateN[T](n: Int, list: List[T]): List[T] = 
	list.flatMap(l => List.fill(n)(l))
	
  //Not working
  def drop[T](n: Int, list: List[T]): List[T] = ???

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = 
	list.splitAt(n)
	
  def slice[T](i: Int, j: Int, list: List[T]): List[T] = 
	list.slice(i, j)
	
  def rotate[T](n: Int, list: List[T]): List[T] = ???

  def removeAt[T](i: Int, list: List[T]): (List[T], T) = ???
  def insertAt[T](t: T, i: Int, list: List[T]): List[T] = ???
  def range[T](i: Int, j: Int): List[Int] = ???
  def randomSelect[T](n: Int, list: List[T]): List[T] = ???
  def lotto[T](i: Int, j: Int): List[Int] = ???
  def randomPermute[T](list: List[T]): List[T] = ???
  def combinations[T](n: Int, list: List[T]): List[List[T]] = ??? 
  def group3[T](list: List[T]): List[List[List[T]]] = ???
  def groups[T](ns: List[Int], list: List[T]): List[List[List[T]]] = ???
  def lsort[T](list: List[List[T]]): List[List[T]] = ???
  def lsortFreq[T](list: List[List[T]]): List[List[T]] = ???

}

