import com.thinkminimo.step.Step
import org.joda.time.{DateTime, Period}
import org.joda.time.format.PeriodFormatterBuilder
import scala.xml.Node
import java.util.Locale

case class Entry(title: String, url: String, var score: Int, date: DateTime)
{
    def toHtml = {
        val printer = new PeriodFormatterBuilder()
            .appendDays.appendSuffix(" day ", " days ")
            .appendHours.appendSuffix(" hour ", " hours ")
            .appendMinutes.appendSuffix(" minute ", " minutes ")
            .printZeroAlways()
            .appendSeconds.appendSuffix(" second", " seconds")
            .toPrinter
        var buf = new StringBuffer
        printer.printTo(buf, new Period(date, new DateTime), Locale.getDefault)
        <li>
            <a href={url}>{title}</a>
            <span style="size: -1; color: gray;">Posted {buf.toString} ago, {score} points</span>
            <a href={"/up/"+url}>Up</a>
            <a href={"/down/"+url}>Down</a>
        </li>
    }
}

class RedditClone extends Step
{
    var data = List(
        Entry("The Scala Programming Language", "http://www.scala-lang.org/", 1, new DateTime))

    before { contentType = "text/html" }
    
    def createHtml(title: String, content: Seq[Node]) = {
        <html>
            <head>
                <title>{title}</title>
            </head>
            <body>
                {content}
            </body>
        </html>
    }
    
    def renderLinks(sortFunc: (Entry, Entry) => Boolean) = 
       for (entry <- data.sort(sortFunc)) yield entry.toHtml
    
    get("/") {
        createHtml("Reddit.Scala",
            <h1>Reddit.Scala</h1>
            <h3>In slightly more than 100 lines of Scala</h3>
            <a href="/">Refresh</a>
            <span> &ndash; </span>
            <a href="/new">Add link</a>
            <h2>Highest ranking entries</h2>
            <ol>{ renderLinks {(e1, e2) => e1.score > e2.score} }</ol>
            <h2>Most recent submissions</h2>
            <ol>{ renderLinks {(e1, e2) => e1.date isAfter e2.date} }</ol>
        )
    }
    
    get("/new") {
        createHtml("Reddit.Scala: submit new link",
            <h1>Submit new link</h1>
            <span style="color:red">{ params("msg") }</span>
            <form action="/new" method="post">
                <div>URL: <input type="text" name="url" size="48" value="http://"/></div>
                <div>Title: <input type="text" name="title" size="48"/></div>
                <input type="submit" value="Add link"/>
            </form>
        )
    }

    def invalidUrl(url: String) =
        try { val foo = new java.net.URL(url); url isEmpty } catch { case _ => true }
    
    post("/new") {
        val title = params("title").trim
        val url = params("url").trim
        val target =
            if (title isEmpty) "/new?msg=Invalid Title"
            else if (invalidUrl(url)) "/new?msg=Invalid URL"
            else if (data.exists(entry => entry.url.equalsIgnoreCase(url)))
                "/new?msg=Link already submitted"
            else {
                data = Entry(title, url, 1, new DateTime) :: data
                "/"
            }
        redirect(target)
    }

    def rate(url: String, value: Int) = {
        data.synchronized {
            data.find(entry => entry.url.equalsIgnoreCase(url)) match {
                case Some(entry) => entry.score += value
                case None => ()
            }
        }
    }
    
    get("/up/:url") {
        rate(params(":url"), 1)
        redirect("/")
    }

    get("/down/:url") {
        rate(params(":url"), -1)
        redirect("/")
    }
}