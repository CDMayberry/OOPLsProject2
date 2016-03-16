class Query(myList: Iterable[String]){}

abstract class WeightedQuery(myList: Iterable[String]) extends Query(myList) with Weighted[String]{
	val weightingFn = (myItem: String) =>{
		val weight = myItem.length.toDouble
	}
}