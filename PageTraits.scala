import scala.annotation.tailrec
import scala.collection.generic.Growable

//Weighted trait from PDF
trait Weighted[A] {
	 val items: Iterable[A]
	 
	 //See Wolfe's email about this, should not be completed here
	 val weightingFn: A => Double
	 
	 //TODO: Complete weights, totalWeight, and sumIf
	 def weights: Iterable[Double] = { for(item <- items) yield weightingFn(item) }
	 def totalWeight: Double = {
        @tailrec 
        def sum(xs: Iterable[Double], total: Double): Double = {
            xs match {
            case x :: tail => sum(tail, total + x)
            case Nil => total
            }
        }
        sum(weights, 0)
     }
	 def sumIf(p: A => Boolean): Double = { 
        var sum: Double = 0
        for(item <- items) {
            if(p(item)) {
                sum = sum + weightingFn(item)
            }
        }
        sum
     }
}

//Augmentable trait from PDF
trait Augmentable[A] {
	val items: scala.collection.mutable.Seq[A] with scala.collection.generic.Growable[A]

	//TODO: Complete ADD
	def add(newItem: A): Boolean = { 
        if(items.contains(newItem)) {
            false
        }
        else {
            items += newItem
            true
        }
    }
}