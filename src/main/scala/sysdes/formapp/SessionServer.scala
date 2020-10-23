package sysdes.formapp

import java.net.Socket
import sysdes.formapp.server.{Handler, Server}

object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

object SessionServerHandler {
  // インスタンス間で共有する内部状態に関する変数・関数はこの中に記述
}

class SessionServerHandler(socket: Socket) extends Handler(socket) {
  import sysdes.formapp.server.{NotFound, Ok, Request, Response}

  def handle(request: Request): Response = request match {
    case Request("GET", "/", _, _, _) => index()
    case _                            => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
  }

  def index(): Response = {
    Ok("""<html>
         |<body>
         |    <form action="/register-name" method="post">
         |        <input type="submit" value="start" />
         |    </form>
         |</body>
         |</html>""".stripMargin)
  }

}
