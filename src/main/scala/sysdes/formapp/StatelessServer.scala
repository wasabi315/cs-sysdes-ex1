package sysdes.formapp

import java.net.Socket

import scala.io.Source
import sysdes.formapp.server.{Handler, Interpolator, Server, UrlEncodedDecoder}

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)

}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Ok, SeeOther, Request, Response}

  override def handle(request: Request): Response = request match {
    case Request("GET", "/", _, _, _) => index()
    case Request("GET", "/register", _, _, _) => startRegistration()
    case Request("POST", "/register/name", _, _, Some(body)) => registerName(body)
    case Request("POST", "/register/gender", _, _, Some(body)) => registerGender(body)
    case Request("POST", "/register/message", _, _, Some(body)) => registerMessage(body)
    case Request("POST", "/register/confirm", _, _, _) => confirm()
    case _ => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
  }

  def index(): Response = {
    val src = Source.fromFile("./html/index.html")
    val html = try src.mkString finally src.close()
    Ok(html)
  }

  def startRegistration(): Response = {
    val src = Source.fromFile("./html/nameForm.html")
    val html = try src.mkString finally src.close()
    Ok(html)
  }

  def registerName(body: String): Response = {
    val src = Source.fromFile("./html/genderForm.html")
    val html = try src.mkString finally src.close()
    val bodyMap = UrlEncodedDecoder.decode(body)
    Ok(Interpolator.interpolate(html, bodyMap))
  }

  def registerGender(body: String): Response = {
    val src = Source.fromFile("./html/messageForm.html")
    val html = try src.mkString finally src.close()
    val bodyMap = UrlEncodedDecoder.decode(body)
    Ok(Interpolator.interpolate(html, bodyMap))
  }

  def registerMessage(body: String): Response = {
    val src = Source.fromFile("./html/confirm.html")
    val html = try src.mkString finally src.close()
    val bodyMap = UrlEncodedDecoder.decode(body)
    Ok(Interpolator.interpolate(html, bodyMap))
  }

  def confirm(): Response = {
    val res = SeeOther()
    res.addHeader("Location", "/")
    res
  }
}
