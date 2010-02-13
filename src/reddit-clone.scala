import com.thinkminimo.step.Step
import org.joda.time.{DateTime, Period}
import org.joda.time.format.PeriodFormatterBuilder
import scala.collection.mutable.HashSet
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

case class User(name: String, password: String, email: String)

class RedditClone extends Step
{
    var data = List(
        Entry("The Scala Programming Language", "http://www.scala-lang.org/", 1, new DateTime))
        
    var registeredUsers = List(User("UlrichSG", "password", "ulrichsg@somewhere.de"))
    
    var onlineUsers = new HashSet[User]
    
    def currentUser = session("username") match {
        case Some(username) => registeredUsers.find(_.name == username)
        case None => None
    }

    before { contentType = "text/html" }
    
    def showLoginStatus = currentUser match {
        case Some(user) => <span>{user.name} <a href="/logout">(Log out)</a></span>
        case None => {
            <a href="/login">(Log in)</a>
            <span> &ndash; </span>
            <a href="/register">(Register)</a>
        }
    }
                    
    def createHtml(title: String, content: Seq[Node]) = {
        <html>
            <head>
                <title>{title}</title>
            </head>
            <body>
                <div style="float:right">{ showLoginStatus }</div>
                {content}
            </body>
        </html>
    }
    
    def renderLinks(sortFunc: (Entry, Entry) => Boolean) = 
       for (entry <- data.sort(sortFunc)) yield entry.toHtml
    
    get("/") {
        createHtml("Reddit.Scala",
            <h1>Reddit.Scala 2.0</h1>
            <h3>In slightly more than 200 lines of Scala</h3>
            <a href="/">Refresh</a>
            <span> &ndash; </span>
            <a href="/new">Add link</a>
            <h2>Highest ranking entries</h2>
            <ol>{ renderLinks {(e1, e2) => e1.score > e2.score} }</ol>
            <h2>Most recent submissions</h2>
            <ol>{ renderLinks {(e1, e2) => e1.date isAfter e2.date} }</ol>
        )
    }
    
    def textField(label: String, name: String, length: Int, default: String) =
        <tr>
            <td>{label}</td>
            <td><input type="text" name={name} length={length.toString} value={default}/></td>
        </tr>
        
    def submitButton(caption: String) =
        <tr>
            <td colspan="2"> <input type="submit" value={caption}/> </td>
        </tr>
    
    def form(method: String, url: String, content: Seq[Node]) =
        <form action={url} method={method}>
            <table cellspacing="5">
                {content}
            </table>
        </form>
    
    get("/login") {
        createHtml("Reddit.Scala: log in",
            <h1>Log in</h1>
            <span style="color:red">{ params("msg") }</span>
            <div>{ form("post", "/login", List(
                textField("Username:", "username", 25, ""),
                textField("Password:", "password", 25, ""),
                submitButton("Enter"))) }</div>
        )
    }
    
    post("/login") {
        val username = params("username").trim
        val password = params("password").trim
        registeredUsers.find(_.name == username) match {
            case Some(user) if user.password == password => {
                session("username") = username
                onlineUsers += user
                redirect("/")
            }
            case _ => redirect("/login?msg=Unknown username or wrong password")
        }
    }
    
    get("/logout") {
        currentUser match {
            case Some(user) => onlineUsers -= user
            case _ => ()
        }
        session.invalidate
        redirect("/")
    }
    
    get("/register") {
        createHtml("Reddit.Scala: register",
            <h1>Register</h1>
            <span style="color:red">{ params("msg") }</span>
            <div>{ form("post", "/register", List(
                textField("Username:", "username", 25, ""),
                textField("E-Mail:", "email", 25, ""),
                textField("Password:", "password", 25, ""),
                submitButton("Sign up"))) }</div>
        )
    }
    
    post("/register") {
        val username = params("username").trim
        val password = params("password").trim
        val email = params("email").trim
        val target =
            if (username isEmpty) "/register?msg=No name given!"
            else registeredUsers.find(u => u.name == username || u.email == email) match {
                case Some(_) => "/register?msg=Username and/or e-mail address already taken!"
                case None => {
                    registeredUsers = User(username, password, email) :: registeredUsers
                    "/"
                }
            }
        redirect(target)
    }
    
    get("/new") {
        createHtml("Reddit.Scala: submit new link",
            <h1>Submit new link</h1>
            <span style="color:red">{ params("msg") }</span>
            <div>{ form("post", "/new", List(
                textField("URL:", "url", 48, "http://"),
                textField("Title:", "title", 48, ""),
                submitButton("Add link"))) }</div>
        )
    }
    
    def invalidUrl(url: String) =
        try { val foo = new java.net.URL(url); url isEmpty } catch { case _ => true }
    
    post("/new") {
        val title = params("title").trim
        val url = params("url").trim
        val target =
            if (title isEmpty) "/new?msg=Invalid Title!"
            else if (invalidUrl(url)) "/new?msg=Invalid URL!"
            else if (data.exists{_.url.equalsIgnoreCase(url)})
                "/new?msg=Link already submitted!"
            else {
                data = Entry(title, url, 1, new DateTime) :: data
                "/"
            }
        redirect(target)
    }

    def rate(url: String, value: Int) =
        data.synchronized {
            data.find(_.url.equalsIgnoreCase(url)) match {
                case Some(entry) => entry.score += value
                case None => ()
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