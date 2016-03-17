
//Define Query that takes in an Iterable[String]
class Query(val items: Seq[String]) { //Items are the query

}

// TODO: create a WeightedQuery class that inherits from Query and implements the Weighted Trait
class WeightedQuery(override val items: Seq[String]) extends Query(items) with Weighted[String] {
	
	//TODO: Make this function correct
	//val weightingFn = (x: String) => -1.0
    val weightingFn = (myItem: String) => {
		//((items.indexOf(myItem)+1)/items.length).toDouble
        myItem.length.toDouble
	}
	//You can pick a default weighting scheme for WeightedQuery, but it has to be different than uniform.
	//	That is, terms should have unequal weights
}

//ISSUE: Corrected
//TODO: Complete scoring class SearchResults, Decide on a scoring method: TF-IDF is one good option you can research.
class SearchResults(val items: Seq[(Double,String)]) {
	//TODO: Complete results method, see spec for additional details
	def results(): Iterable[(Double,String)] = {
		//TODO: Iterable should be returned in decreasing order, so highest first. Hint: use SortWith method, see spec for example
		items.sortWith(_._1>_._1)
	}
	
	//TODO: Complete printTop method, prints top n results on the console, with score printed first and url second
	def printTop(n: Int): Unit = {
        val sortedItems = items.sortWith(_._1>_._1)
        //println("Made it! length: " + sortedItems.length)
        //println(sortedItems)
        try {
            for (x <- 0 to n) {
                //println("X: "+x)
                if(x == sortedItems.length)
                    throw CompletedSearch
                //println("OH YEAH! "+x)
                println("Score: "+sortedItems(x)._1+", Url: "+sortedItems(x)._2)
                
            }
        }
        catch {
			case CompletedSearch => Unit
		}
        //println("I'm out!")
	}
}