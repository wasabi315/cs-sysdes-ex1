package sysdes.formapp

import java.net.Socket

import scala.io.Source
import sysdes.formapp.server._

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)

}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  override def handle(request: Request): Response = request match {
    case Request("GET", "/", _, _, _) => index()
    case Request("POST", "/register", _, _, body) => startRegistration(body.getOrElse(""))
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

  def startRegistration(body: String): Response = {
    val bodyMap = UrlEncodedDecoder.decode(body)
    val name = bodyMap.getOrElse("name", "")
    val gender = bodyMap.getOrElse("gender", "")
    val message = bodyMap.getOrElse("message", "")

    val src = Source.fromFile("./html/nameForm.html")
    val html = try src.mkString finally src.close()
    Ok(Interpolator.interpolate(html, Map(
      "name"    -> name,
      "gender"  -> gender,
      "message" -> message
    )))
  }

  def registerName(body: String): Response = {
    val bodyMap = UrlEncodedDecoder.decode(body)
    (for {
      name <- bodyMap.get("name")
    } yield {
      val gender = bodyMap.getOrElse("gender", "")
      val maleChecked = if (gender == "male") { "checked" } else { "" }
      val femaleChecked = if (gender == "female") { "checked "} else { "" }
      val message = bodyMap.getOrElse("message", "")

      val src = Source.fromFile("./html/genderForm.html")
      val html = try src.mkString finally src.close()
      Ok(Interpolator.interpolate(html, Map(
        "name"          -> name,
        "maleChecked"   -> maleChecked,
        "femaleChecked" -> femaleChecked,
        "message"       -> message
      )))
    }).getOrElse(redirectToStart())
  }

  def registerGender(body: String): Response = {
    val bodyMap = UrlEncodedDecoder.decode(body)
    (for {
      name   <- bodyMap.get("name")
      gender <- bodyMap.get("gender")
    } yield {
      val message = bodyMap.getOrElse("message", "")

      val src = Source.fromFile("./html/messageForm.html")
      val html = try src.mkString finally src.close()
      Ok(Interpolator.interpolate(html, Map(
        "name"    -> name,
        "gender"  -> gender,
        "message" -> message
      )))
    }).getOrElse(redirectToStart())
  }

  def registerMessage(body: String): Response = {
    val bodyMap = UrlEncodedDecoder.decode(body)
    (for {
      name    <- bodyMap.get("name")
      gender  <- bodyMap.get("gender")
      message <- bodyMap.get("message")
    } yield {
      val src = Source.fromFile("./html/confirm.html")
      val html = try src.mkString finally src.close()
      Ok(Interpolator.interpolate(html, Map(
        "name"    -> name,
        "gender"  -> gender,
        "message" -> message
      )))
    }).getOrElse(redirectToStart())
  }

  def confirm(): Response = {
    val res = SeeOther()
    res.addHeader("Location", "/")
    res
  }

  def redirectToStart(): Response = {
    val res = SeeOther()
    res.addHeader("Location", "/register")
    res
  }
}
