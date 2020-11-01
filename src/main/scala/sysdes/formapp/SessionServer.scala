package sysdes.formapp

import java.net.Socket
import java.util.UUID

import scala.collection.mutable.HashMap
import scala.io.Source
import scala.util.Try
import sysdes.formapp.server._

object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

class State(
  var name: Option[String] = None,
  var gender: Option[String] = None,
  var message: Option[String] = None
) {}

object SessionServerHandler {
  val states: HashMap[UUID, State] = HashMap()

  def createSession(): (UUID, State) = {
    val sessId = UUID.randomUUID()
    val state = new State()
    states.put(sessId, state)
    (sessId, state)
  }

  def getSession(request: Request): Option[(UUID, State)] = {
    for {
      cookie <- request.headers.get("Cookie")
      sessId <- Try(UUID.fromString(cookie.stripPrefix("session-id="))).toOption
      state  <- states.get(sessId)
    } yield {
      (sessId, state)
    }
  }
}


class SessionServerHandler(socket: Socket) extends Handler(socket) {

  def handle(request: Request): Response = {
    request match {
      case Request("GET", "/", _, _, _) => index()
      case Request("GET", "/register", _, _, _) => startRegistration(request)
      case Request("POST", "/register", _, _, _) => startRegistration(request)
      case Request("POST", "/register/name", _, _, _) => registerName(request)
      case Request("POST", "/register/gender", _, _, _) => registerGender(request)
      case Request("POST", "/register/message", _, _, _) => registerMessage(request)
      case Request("POST", "/register/confirm", _, _, _) => confirm(request)
      case _ => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
    }
  }

  def index(): Response = {
    val src = Source.fromFile("./html/index.html")
    val html = try src.mkString finally src.close()
    Ok(html)
  }

  def startRegistration(request: Request): Response = {
    val (sessId, state) = SessionServerHandler.getSession(request)
      .getOrElse(SessionServerHandler.createSession())

    val src = Source.fromFile("./html/nameForm.html")
    val html = try src.mkString finally src.close()
    val res = Ok(Interpolator.interpolate(html, Map(
      "name" -> state.name.getOrElse("")
    )))
    res.addHeader("Set-Cookie", s"session-id=${sessId}")
    res
  }

  def registerName(request: Request): Response = {
    (for {
      (_, state) <- SessionServerHandler.getSession(request)
      name <- {
        val bodyMap = UrlEncodedDecoder.decode(request.body.getOrElse(""))
        bodyMap.get("name").orElse(state.name)
      }
    } yield {
      state.name = Some(name)

      val src = Source.fromFile("./html/genderForm.html")
      val html = try src.mkString finally src.close()
      val maleChecked =
        if (state.gender.getOrElse("") == "male") { "checked" } else { "" }
      val femaleChecked =
        if (state.gender.getOrElse("") == "female") { "checked" } else { "" }
      Ok(Interpolator.interpolate(html, Map(
        "maleChecked"   -> maleChecked,
        "femaleChecked" -> femaleChecked
      )))
    }).getOrElse(redirectToStart())
  }

  def registerGender(request: Request): Response = {
    (for {
      (_, state) <- SessionServerHandler.getSession(request)
      gender <- {
        val bodyMap = UrlEncodedDecoder.decode(request.body.getOrElse(""))
        bodyMap.get("gender").orElse(state.gender)
      }
    } yield {
      state.gender = Some(gender)

      val src = Source.fromFile("./html/messageForm.html")
      val html = try src.mkString finally src.close()
      Ok(Interpolator.interpolate(html, Map(
        "message" -> state.message.getOrElse("")
      )))
    }).getOrElse(redirectToStart())
  }

  def registerMessage(request: Request): Response = {
    (for {
      (_, state) <- SessionServerHandler.getSession(request)
      message <- {
        val bodyMap = UrlEncodedDecoder.decode(request.body.getOrElse(""))
        bodyMap.get("message").orElse(state.message)
      }
    } yield {
      state.message = Some(message)

      val src = Source.fromFile("./html/confirm.html")
      val html = try src.mkString finally src.close()
      Ok(Interpolator.interpolate(html, Map(
        "name" -> state.name.get,
        "gender" -> state.gender.get,
        "message" -> state.message.get
      )))
    }).getOrElse(redirectToStart())
  }

  def confirm(request: Request): Response = {
    (for {
      _ <- SessionServerHandler.getSession(request)
    } yield {
      val res = SeeOther()
      res.addHeader("Location", "/")
      res
    }).getOrElse(redirectToStart())
  }

  def redirectToStart(): Response = {
    val res = SeeOther()
    res.addHeader("Location", "/register")
    res
  }
}
