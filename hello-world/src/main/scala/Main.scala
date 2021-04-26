import scala.annotation.tailrec

import scala.io.StdIn.readLine
import javax.sql.rowset.RowSetWarning
import scala.language.postfixOps

object TreeTest extends App {
    case class Tree[A](
        value: A,
        leafA: Option[Tree[A]],
        leafB: Option[Tree[A]],
    ) { def map[B](f: (A => B)): Tree[B] = {
            new Tree( f(value)
                    , leafA.map(valA => valA.map(f))
                    , leafB.map(valB => valB.map(f)))
        }

        def find(predicate: A => Boolean): Option[A] = {

            if (predicate(value)) {
                Some(value)
            } else {
                (leafA, leafB) match {
                    case (Some(x), Some(y)) =>
                        x.find(predicate) match {
                            case Some(x) => Some(x)

                            case None => y.find(predicate)
                        }

                    case (None, Some(y)) => 
                        y.find(predicate) match {
                            case Some(y) => Some(y)

                            case None => None
                        }

                    case (Some(x), None) => 
                        x.find(predicate) match {
                            case Some(x) => Some(x)

                            case None => None
                        }

                    case (None, None) => None
                }
            }
        }
    }

    val some = new Tree("test",
        Some(new Tree("flamingo",
            Some(new Tree(
                "fsfsfs",
                Some(new Tree(
                    "da",
                    None,
                    None
                )),
                None
            )),
            Some(new Tree(
                "leafB",
                None,
                Some(new Tree(
                    "kreker",
                    None,
                    None
                ))
            ))
        )),
        None
    )

    // Using the functional programming approach
    def FunctionalBinarySearch(arr: Vector[Int], elemToSearch: Int): Int = {

        def BinarySearch(arr: Vector[Int], elemToSearch: Int, low: Int, high: Int): Int = { 

            // If element not found
            if (low > high)
                return -1

            // Getting middle index
            var middle = low + (high - low) / 2

            // Pattern matching
            arr match {

                // If element found , return the index
                case(arr: Vector[Int])
                    if (arr(middle) == elemToSearch) => middle

                // Call the function for the second half
                case(arr: Vector[Int])
                    if (arr(middle) < elemToSearch) => BinarySearch(arr,
                                                                    elemToSearch,
                                                                    middle + 1, high)

                // Call the function for the first half
                case(arr: Vector[Int]) 
                    if (arr(middle) > elemToSearch) => BinarySearch(arr,
                                                                    elemToSearch,
                                                                    low, middle - 1)
            }
        }

        // Calling the Binary Search function 
        BinarySearch(arr, elemToSearch, 0, arr.length - 1)
    }

    def startBinarySearch {

        def checkInput(s: String): Option[Int] = try {
            Some(s.toInt)
        } catch {
            case e: Exception => None
        }
    
        val getInput: Int = {
            print("Enter number: ")
            checkInput(readLine()) match {
                case Some(n) => n
                case None => {
                    println("Bad input, Int expected, set to default value -- 0")
                    0
                }
            }
        }
 
        // Calling the binary search function and
        // storing its result in index variable

        var index = FunctionalBinarySearch((-50 to 50).toVector, getInput)

        // If value not found
        if (index == -1)
        println("Element not found") 

        // Else print the index where  
        // the value is found
        else
        print("Element found at Index " + index)
    }
    // Start
    // startBinarySearch
    UnlimitedListWorks
}


object UnlimitedListWorks {

    sealed abstract class ListWorks[+A] extends Product with Serializable {
        def head(): A
        def length(): Int
        def tail(): ListWorks[A]
        def toString(): String
    }

    object ListWorks {
        final case class Cons[+A](value: A, link: ListWorks[A]) extends ListWorks[A] {
            def head(): A = { value }

            def tail(): ListWorks[A] = { link }

            def length(): Int = {

                if (Option(value) == Some(value)) {
                    1 + link.length
                } else {
                    0
                }
            }

            override
            def toString(): String = {

                "ListWorks(" ++ lwToStr(this) ++ ")"
            }
        }

        final case object Nil extends ListWorks[Nothing] {

            def head(): Nothing = {
                throw new RuntimeException("ListWorks should contain at least one element")
                ().asInstanceOf[Nothing]
            }

            def length(): Int = { 0 }

            def tail(): ListWorks[Nothing] = {
                ListWorks.Nil
            }

            override
            def toString(): String = {
                "ListWorks()"
            }
        }
    }

    def add[A](list: ListWorks[A], arg: A): ListWorks[A] = {

        if (list.length == 0) {

            Cons(arg, Nil)
        } else if (list.length == 1) {
            
            Cons(list.head, add(Nil, arg))
        } else {
            Cons(list.head, add(list.tail, arg))
        }
    }

    def lwToStr[A](data: ListWorks[A]): String = {

        data.head.toString ++ (
            if (data.tail != Nil) ", " ++ lwToStr(data.tail)
            else ""
        )
    }

    def Cons[A](value: A, link: ListWorks[A]): ListWorks[A] = {
        ListWorks.Cons(value, link)
    }

    def Nil[A](): ListWorks[A] = {
        ListWorks.Nil
    }

    def toListWorks[A](xs: List[A]): ListWorks[A] = {
        if (xs.length == 0) {
            Nil
        } else if (xs.length == 1) {
            Cons(xs.head, Nil)
        } else {
            Cons(xs.head, toListWorks(xs.tail))
        }
    }

    def toListWorks[A](xs: Array[A]): ListWorks[A] = {
        if (xs.length == 0) {
            Nil
        } else if (xs.length == 1) {
            Cons(xs.head, Nil)
        } else {
            Cons(xs.head, toListWorks(xs.tail))
        }
    }

    def toListWorks[A](xs: Vector[A]): ListWorks[A] = {
        if (xs.length == 0) {
            Nil
        } else if (xs.length == 1) {
            Cons(xs.head, Nil)
        } else {
            Cons(xs.head, toListWorks(xs.tail))
        }
    }

    def ListWorks[A](args: A*): ListWorks[A] = {
        toListWorks(args.toList)
    }

    def ListWorks[A](): ListWorks[A] = {
        Nil
    }

    def find[A](target: A, list: ListWorks[A]): Option[A] = {

        if (list.length > 0 && target == list.head) {
            Some(target)
        } else if (list.length != 0) {
            find(target, list.tail)
        } else {
            None
        }
    }

    def rm[A](target: A, list: ListWorks[A]): ListWorks[A] = {

        if (find(target, list) == None) {
            list
        } else {
            if (target == list.head) {
                if (list.length == 1) {
                    Nil
                } else {
                    Cons(list.tail.head, list.tail.tail)
                }
            } else if (list.length != 0) {
                Cons(list.head, rm(target, list.tail))
            } else {
                list
            }
        }
    }

    def smallest(list: ListWorks[Int], oldHead: Int): Int = {
        // find smallest number in LW
        
        val nsmhelp: Int = if (list.length != 0) list.head else oldHead
        val nsm: Int = if (oldHead <= nsmhelp) oldHead else nsmhelp


        if (list.length != 0) smallest(list.tail, nsm)
        else nsm
    }

    def sortListW (sortedList: ListWorks[Int], list: ListWorks[Int]): ListWorks[Int] = {

        if (list.length != 0) {

            sortListW(
                add(sortedList, smallest(list.tail, list.head)),
                rm(smallest(list.tail, list.head), list))
        } else {
            sortedList
        }
    }

    def sort (list: ListWorks[Int]): ListWorks[Int] = {
        // Starter
        if (list.length != 0 )
            sortListW(
                ListWorks(smallest(list.tail, list.head)),
                rm(smallest(list.tail, list.head), list))
        else list
    }

    val finalTest = ListWorks(1, 3, -2, 10, -2, 3, 1, 0, 4, -4)
}
