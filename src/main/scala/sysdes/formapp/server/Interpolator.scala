package sysdes.formapp.server

import scala.collection.immutable.Map

// htmlに変数を埋め込むテンプレートエンジンのようなもの
object Interpolator {
  private val re = """\{\{\s*([a-zA-Z][a-zA-Z0-9_]+)\s*}}""".r

  // テンプレート中の{{ 〇〇 }}という部分にMap内の対応する値を割り当てる
  // e.g.)
  //    interpolate("<span>{{ name }}</span>", Map("name" -> "John"))
  //      => "<span>John</span>"
  def interpolate(template: String, values: Map[String, String]): String =
    re.replaceAllIn(
      template,
      m => Sanitizer.sanitize(values.getOrElse(m.group(1), m.group(1)))
    )
}
