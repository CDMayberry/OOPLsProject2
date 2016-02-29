import org.apache.http._
import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.client.utils._
import org.apache.http.message._
import org.apache.http.params._
import java.net.URL

object CompletedSearch extends Exception { }

object SearchEngine extends App {

	def isAlphanumeric( str: String ) : Boolean = { 
		val alphanumeric = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet
		str.forall(alphanumeric.contains(_)) 
	}
	
	def stripToAlphanumeric ( str: String ) : String = {

		//Alphanumeric character generation courtesy of StackOverflow
		val alphanumeric = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet

		//Filter out non-alphanumeric characters, then remove all unnecessary whitespace
		str.map( x => if (alphanumeric.contains(x)) x else " ")
			.mkString("").replaceAll("\\s+", " ").trim
	}
	
	def fetch( url: String ) : String = {
		val httpget = new HttpGet(url)
		//println(httpget.getURI)
		//Need to do try/catch block here
		try {
			val responseBody = new DefaultHttpClient().execute(httpget, new BasicResponseHandler())
			responseBody
		}
		catch {
			case e: org.apache.http.client.HttpResponseException =>
				println("Error "+e.getMessage)
				""
			case f: java.lang.IllegalStateException =>
				println("Error: "+f.getMessage)
				""
			case g: javax.net.ssl.SSLPeerUnverifiedException =>
				println("Error: "+g.getMessage)
				""
			case h: javax.net.ssl.SSLException =>
				println("Error: SSL Exception, full message suppressed") //h.getMessage
				""
		}
	}
		
	def getLinks( html : String , baseURL : String) : List[String] = {
		// See http://www.mkyong.com/regular-expressions/how-to-extract-html-links-with-regular-expression/ for explanation of regex
		val aTagRegex = """(?i)<a([^>]+)>(.+?)</a>""".r
		val urlRegex = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r
		
		val opts = for ( a <- aTagRegex findAllMatchIn html ) yield urlRegex.findFirstMatchIn(a.toString)
		
		val hrefs = opts collect { case Some(x) => x group 1 }
		
		// remove leading and trailing quotes, if any
		val cleaned = hrefs map { _.stripPrefix("\"").stripPrefix("\'").stripSuffix("\"").stripPrefix("\'") } filter { ! _.startsWith("javascript") } //"
		
		 //Use Java's URL class to parse the URL
		 //  and get the full URL string (including implied context)
		//println("Examining: "+baseURL)

		val contextURL = new java.net.URL(baseURL)

		def getURL(x: String) = {
			var result = ""
			try {
				result = new java.net.URL(contextURL, x).toString()
			}
			catch {
				case e: java.net.MalformedURLException => Unit
			}
			result
		}
        
		(cleaned map { getURL(_) } ).filter(_.length > 0).toList

	}
	
	def getTerms( html : String , fcn: String => Boolean ) : List[String] = {
		val terms = stripToAlphanumeric(html).split(" ").toList
		for(term <- terms; isTerm = fcn(term); if isTerm) yield term
	}

	def crawlAndIndex( url : String , pages : Int ) : List[PageSummary] = {

		var currentLinks : List[String] = List("")
		var spentLinks : List[String] = List("")
		var summaries = List[PageSummary]()

		var currentLink : String = url

		try {
			for (x <- 1 to pages) {
				//println("Loop: "+x+", Link: "+currentLink)
				val html = fetch(currentLink)

				val links = getLinks(html, currentLink)
				val terms = getTerms(html,(str : String) => if(str.length > 1) true else false)

				summaries = summaries:+ new PageSummary(currentLink,terms)

				//Compare if new link is already in the list or already used
				var newLinks = for (link <- links; if !currentLinks.contains(link) && !spentLinks.contains(link) && link != currentLink) yield link

				//Handles case where multiple copies of same link exist on one page
				newLinks = newLinks.distinct

				//Remake list using new links without the currently examined link
				currentLinks = currentLinks ::: newLinks diff List(currentLink)

				//Add current link to the read links
				spentLinks = currentLink :: spentLinks

				//get element of list
				currentLink = currentLinks.last
				if(currentLink == "" || currentLink == " " || currentLink == null || currentLink == Unit) {
					//If by some magic it runs out of links before selected number of pages.
					throw CompletedSearch
				}
			}
		}
		catch {
			case CompletedSearch => println("Ran out of links to search")
		}

		//println("Completed with length: " + currentLinks.length)

		summaries
	}
	
	def printBest(query : List[String], pages : List[PageSummary]) = {
		val scores = for(x <- pages) yield (x.url, x.fracMatching(query))
		for (x <- scores.sortBy(_._2).takeRight(5).reverse) println(x._1 + ": " + x._2.toString)
	}

	//var results = crawlAndIndex("http://google.com",5)
	//println(results.length)

//	printBest(List("The","html","div","span","and","or","will","script"),results)

//	for(result <- results) {
//		print("Url: "+result.url)
//		if(!result.terms.isEmpty)
//			println(" , Last term: "+result.terms.last)
//		else
//			println("")
//	}

}