package sysdes.formapp

import java.net.Socket

import scala.io.Source
import sysdes.formapp.server.{Handler, Interpolator, Server, UrlEncodedDecoder}

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)

}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Ok, Request, Response}

  override def handle(request: Request): Response = request match {
    case Request("GET", p, _, _, _) if (p.takeWhile(_ != '?') == "/") => index()
    case Request("POST", "/register-name", _, _, _) => nameForm()
    case Request("POST", "/register-gender", _, _, Some(body)) => genderForm(body)
    case Request("POST", "/register-message", _, _, Some(body)) => messageForm(body)
    case Request("POST", "/summary", _, _, Some(body)) => summary(body)
    case _ => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
  }

  def index(): Response = {
    val src = Source.fromFile("./html/index.html")
    val html = try src.mkString finally src.close()
    Ok(html)
  }

  def nameForm(): Response = {
    val src = Source.fromFile("./html/nameForm.html")
    val html = try src.mkString finally src.close()
    Ok(html)
  }

  def genderForm(body: String): Response = {
    val src = Source.fromFile("./html/genderForm.html")
    val html = try src.mkString finally src.close()
    val bodyMap = UrlEncodedDecoder.decode(body)
    Ok(Interpolator.interpolate(html, bodyMap))
  }

  def messageForm(body: String): Response = {
    val src = Source.fromFile("./html/messageForm.html")
    val html = try src.mkString finally src.close()
    val bodyMap = UrlEncodedDecoder.decode(body)
    Ok(Interpolator.interpolate(html, bodyMap))
  }

  def summary(body: String): Response = {
    val src = Source.fromFile("./html/summary.html")
    val html = try src.mkString finally src.close()
    val bodyMap = UrlEncodedDecoder.decode(body)
    Ok(Interpolator.interpolate(html, bodyMap))
  }
}
