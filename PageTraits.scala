
//Weighted trait from PDF
trait Weighted[A] {
	 val items: Iterable[A]
	 
	 //See Wolfe's email about this
	 val weightingFn: A => Double
	 
	 //TODO: Complete weights, totalWeight, and sumIf
	 def weights: Iterable[Double] = { List(-1.0) }
	 def totalWeight: Double = { -1.0 }
	 def sumIf(p: A => Boolean): Double = { -1.0 }
}

//Augmentable trait from PDF
trait Augmentable[A] {
	val items: scala.collection.mutable.Seq[A] with
	scala.collection.generic.Growable[A]

	//TODO: Complete ADD
	def add(newItem: A): Boolean = { true }
}