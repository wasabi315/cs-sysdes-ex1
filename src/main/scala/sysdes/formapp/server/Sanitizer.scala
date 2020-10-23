package sysdes.formapp.server

object Sanitizer {
  def sanitize(input: String): String =
    input
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("&", "&amp;")
      .replaceAll("'", "&#039;")
      .replaceAll("\"", "&#034;")
}
