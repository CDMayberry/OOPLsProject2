import org.apache.http._
import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.client.utils._
import org.apache.http.message._
import org.apache.http.params._
import java.net.URL

object SearchEngine extends App {
    
    def filterFcn(str: String): Boolean = {
        val commonHtml = Set("div","span","b","u","i","p","br","h1","h2","h3","h4","h5","h6","ol","li","ul","dl","dt","dd","hr","br","blockquote","href","src")
        if(str.length == 1 || commonHtml.contains(str)) {
            false
        }
        else {
            true
        }
    }
    

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
		
	def getLinks( html : String , baseURL : String) : List[PageUrl] = {
		// See http://www.mkyong.com/regular-expressions/how-to-extract-html-links-with-regular-expression/ for explanation of regex
		val aTagRegex = """(?i)<a([^>]+)>(.+?)</a>""".r
		val urlRegex = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r
		
		val opts = for ( a <- aTagRegex findAllMatchIn html ) yield urlRegex.findFirstMatchIn(a.toString)
		
		val hrefs = opts collect { case Some(x) => x group 1 }
		
		// remove leading and trailing quotes, if any
		val cleaned = hrefs map { _.stripPrefix("\"").stripPrefix("\'").stripSuffix("\"").stripPrefix("\'") } filter { ! _.startsWith("javascript") } //"
		
		 //Use Java's URL class to parse the URL
		 //  and get the full URL string (including implied context)
		val contextURL = new java.net.URL(baseURL)

		def getURL(x: String) = {
			var result = ""
			try {
				result = new java.net.URL(contextURL, x).toString()
			}
			catch {
				case e: java.net.MalformedURLException => Unit
			}
			new PageUrl(result)
		}
        
		(cleaned map { getURL(_) } ).filter(_.url.length > 0).toList

	}
	
	def getTerms( html : String , fcn: String => Boolean ) : List[String] = {
		val terms = stripToAlphanumeric(html).split(" ").toList
		for(term <- terms; isTerm = fcn(term); if isTerm) yield term
	}
    
	//TODO: Revise crawlAndIndex, mode should be "read" or "augment": for "read", no mixins are needed; 
	//		 for "augment", Augmentable should be mixed in to the returned object. If weight is true, 
	//		 then the returned object should be a WeightedIndexedPages. If weight is false, a plain IndexedPages is returned 	
	def crawlAndIndex(startUrl: String, maxPages: Int, mode: String ="read", weight: Boolean = true): IndexedPages = {
        
        var pages = scala.collection.mutable.ArrayBuffer[Page]()
        
        var currentLinks = List[PageUrl]()
		var spentLinks = List[PageUrl]()

		var currentLink = new PageUrl(startUrl)

		try {
			for (x <- 1 to maxPages) {
				val html = fetch(currentLink.url)
                
				val links = getLinks(html, currentLink.url)
				val terms = getTerms(html, filterFcn)

				pages = pages :+ new Page(currentLink.url,terms)

				//Compare if new link is already in the list or already used
				var newLinks : List[PageUrl] = for (link <- links; if !currentLinks.contains(link) && !spentLinks.contains(link) && link != currentLink) yield link

				//Handles case where multiple copies of same link exist on one page
				newLinks = newLinks.distinct

				//Remake list using new links without the currently examined link
				currentLinks = currentLinks ::: newLinks.filterNot({_ == currentLink})

				//Add current link to the read links
				spentLinks = currentLink :: spentLinks

				//get element of list
				currentLink = currentLinks.last
				if(currentLink.url == "" || currentLink.url == " " || currentLink == null) {
					//If by some magic it runs out of links before selected number of pages.
					throw CompletedSearch
				}
			}
		}
		catch {
			case CompletedSearch => println("Ran out of links to search")
		}
           
        if(mode == "read") { //Read-Only
            if(weight) {
                new WeightedIndexedPages(pages)
            }
            else {
                new IndexedPages(pages)
            }
        }
        else {              //Augmentable
            if(weight) {
                new WeightedIndexedPages(pages) with Augmentable[Page]
            }
            else {
                new IndexedPages(pages) with Augmentable[Page]
            }
        }
        
	}
	
	def printBest(query : List[String], pages : List[PageSummary]) = {
		val scores = for(x <- pages) yield (x.url, x.fracMatching(query))
		for (x <- scores.sortBy(_._2).takeRight(5).reverse) println(x._1 + ": " + x._2.toString)
	}
    
    def testFcn() = {
        val link = "https://en.wikipedia.org/wiki/Final_Fantasy_Type-0"
        val list = getTerms(fetch(link),filterFcn)
        var testPage = new Page(link,list)
        println("Test find: "+testPage.has("75"))
        var index = new IndexedPages(scala.collection.mutable.ArrayBuffer(testPage))
        index.search(new Query(List("String")))
        index.search(new WeightedQuery(List("String")))
    }

    testFcn
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