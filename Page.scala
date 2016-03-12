
//ISSUE: Not sure if PageSummary is meant to be used as a member variable of Page
//			or if it should just be absorbed into the Page class.
//TODO: Should have a 'method url' and any additional contents we deem necessary.
class Page(val url: String) {
	
}


//ISSUE: IndexedPages needs to have a mutable list,  but that seems to conflict with the purpose of the Augmentable trait.
//			Does he just mean use scala.collection.mutable.Seq[Page]?
//TODO: Extend Iterable[Page], override iterator method, should use a mutable collection in the class to add pages
//TODO: define a method numContaining(word: String): Double that returns the number of pages 
//			that contain the given word as a whole word at least once (See doc for additional details)
class IndexedPages(val items: Iterable[Page]) extends Iterable[Page] {	
	override def iterator = items.iterator 
	
	def numContaining(word: String): Double = {
		-1.0
	}
	
	//TODO: Complete search
	def search(q: Query): SearchResults = {
		
		new SearchResults()
	}
}

//TODO: Create a class WeightedIndexedPages that extends IndexedPages and implements the Weighted trait.
class WeightedIndexedPages(override val items: Iterable[Page]) extends IndexedPages(items) with Weighted[Page] { //Not sure if Weighted type is correct here
	
	//TODO: Complete weightingFn
	val weightingFn = (x: Page) => -1.0
	
	//TODO: Complete numContaining 
	override def numContaining(word: String): Double = {
		-1.0
	}
	
	//TODO: Complete search
	override def search(q: Query): SearchResults = {
		val beforeWeights: SearchResults = super.search(q) //TODO: call superclass method [1 pts]
		val oldScores = beforeWeights.results.unzip._1
		val unnormScores = oldScores.zip(weights).map { (x) => (x._1 * x._2) }
		//Multiplying by weights may change the scale of the scores
		//Thus, newScores is unnormScores by the total of the unnormScores
		// (This is called "normalizing" the scores)
		val total = unnormScores.foldLeft(0.0) {_+_}
		val newScores = unnormScores.map { _ / total }
		// TODO: create and return adjusted SearchResults from newScores [4 pts]
		
		beforeWeights //THIS IS NOT CORRECT
	}
}

//TODO: "The easiest way to achieve such equality testing (equality testing of the Page based on the url)
//		  is to have a case class that takes the url as the sole argument of the primary constructor."
case class PageUrl(val url: String)

//My PageSummary class
class PageSummary( val url: String , val terms: List[String] ) {
	
	def fracMatching( term: String ) : Float = {
		if(terms.length > 0) {
			val matching = terms.filter(_.toLowerCase() == term.toLowerCase()).length.toFloat
			matching / terms.length.toFloat
		}
		else
			0f
	}
	def fracMatching( newTerms: List[String] ) : Float = {
		//println(terms)
		if(terms.length > 0) {
			newTerms match {
				case x :: tail =>
					terms.filter(_.toLowerCase() == x.toLowerCase()).length.toFloat / terms.length.toFloat + fracMatching(tail)
				case Nil =>
					0f
			}
		}
		else
			0f
	}
}