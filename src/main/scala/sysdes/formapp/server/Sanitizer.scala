package sysdes.formapp.server

object Sanitizer {
  def sanitize(input: String): String =
    input
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("'", "&#039;")
      .replaceAll("\"", "&#034;")
}
