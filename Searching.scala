
//Define Query that takes in an Iterable[String]
class Query(items: Iterable[String]) { //Items are the query

}

// TODO: create a WeightedQuery class that inherits from Query and implements the Weighted Trait
class WeightedQuery( override val items: Iterable[String]) extends Query(items) with Weighted[String] {
	
	//TODO: Make this function correct
	val weightingFn = (x: String) => -1.0
	//You can pick a default weighting scheme for WeightedQuery, but it has to be different than uniform.
	//	That is, terms should have unequal weights
}

//ISSUE: Corrected
//TODO: Complete scoring class SearchResults, Decide on a scoring method: TF-IDF is one good option you can research.
class SearchResults() {
	//TODO: Complete results method, see spec for additional details
	def results(): Iterable[(Double,String)] = {
		//TODO: Iterable should be returned in decreasing order, so highest first. Hint: use SortWith method, see spec for example
		
		List((1.0,"test"))
	}
	
	//TODO: Complete printTop method, prints top n results on the console, with score printed first and url second
	def printTop(n: Int): Unit = {

	}
}