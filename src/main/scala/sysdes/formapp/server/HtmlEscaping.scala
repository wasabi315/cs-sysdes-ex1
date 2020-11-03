package sysdes.formapp.server

object HtmlEscaping {
  def escape(input: String): String =
    input
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("'", "&#039;")
      .replaceAll("\"", "&#034;")
}
