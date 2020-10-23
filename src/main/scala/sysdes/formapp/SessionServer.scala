package sysdes.formapp

import java.net.Socket
import java.util.UUID
import scala.collection.mutable.HashMap
import scala.io.Source
import sysdes.formapp.server.{Handler, Interpolator, Server, UrlEncodedDecoder}

object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

object SessionServerHandler {
  val states: HashMap[UUID, State] = HashMap()
}

class State(var name: String = "", var gender: String = "", var message: String = "")

class SessionServerHandler(socket: Socket) extends Handler(socket) {
  import sysdes.formapp.server.{NotFound, Ok, BadRequest, SeeOther,Request, Response}

  def handle(request: Request): Response = {
    if (request.path.takeWhile(_ != '?') == "/") {
      return index()
    }

    if (!request.headers.contains("Cookie")) {
      val res = SeeOther()
      res.addHeader("Location", "/")
      return res
    }

    val sessId = UUID.fromString(request.headers("Cookie").stripPrefix("session-id="))
    if (!SessionServerHandler.states.contains(sessId)) {
      return BadRequest("400 Bad Request")
    }

    request match {
      case Request("POST", "/register-name", _, _, _) => nameForm()
      case Request("POST", "/register-gender", _, _, Some(body)) => genderForm(sessId, body)
      case Request("POST", "/register-message", _, _, Some(body)) => messageForm(sessId, body)
      case Request("POST", "/summary", _, _, Some(body)) => summary(sessId, body)
      case _                            => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
    }
  }

  def index(): Response = {
    val src = Source.fromFile("./html/index.html")
    val html = try src.mkString finally src.close()
    val res = Ok(html)

    val sessId = UUID.randomUUID()
    SessionServerHandler.states.put(sessId, new State)
    res.addHeader("Set-Cookie", s"session-id=${sessId}")

    res
  }

  def nameForm(): Response = {
    val src = Source.fromFile("./html/nameForm.html")
    val html = try src.mkString finally src.close()
    Ok(html)
  }

  def genderForm(sessId: UUID, body: String): Response = {
    val bodyMap = UrlEncodedDecoder.decode(body)
    SessionServerHandler.states(sessId).name = bodyMap("name")

    val src = Source.fromFile("./html/genderForm.html")
    val html = try src.mkString finally src.close()
    Ok(html)
  }

  def messageForm(sessId: UUID, body: String): Response = {
    val bodyMap = UrlEncodedDecoder.decode(body)
    SessionServerHandler.states(sessId).gender = bodyMap("gender")

    val src = Source.fromFile("./html/messageForm.html")
    val html = try src.mkString finally src.close()
    Ok(html)
  }

  def summary(sessId: UUID, body: String): Response = {
    val bodyMap = UrlEncodedDecoder.decode(body)
    val state = SessionServerHandler.states(sessId)
    state.message = bodyMap("message")

    val src = Source.fromFile("./html/summary.html")
    val html = try src.mkString finally src.close()
    Ok(Interpolator.interpolate(
      html,
      Map(
        "name" -> state.name,
        "gender" -> state.gender,
        "message" -> state.message,
      )
    ))
  }
}
