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