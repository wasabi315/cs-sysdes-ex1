package sysdes.formapp.server

import java.net.URLDecoder

import scala.collection.immutable.Map

object UrlEncodedDecoder {
  // x-www-form-urlencoded形式のリクエストボディをデコードしてMapにする
  def decode(body: String): Map[String, String] =
    body.split('&')
      .map(_.span(_ != '='))
      .map(kv => (kv._1, kv._2.stripPrefix("=")))
      .map(kv => (kv._1, URLDecoder.decode(kv._2, "UTF-8")))
      .toMap
}
