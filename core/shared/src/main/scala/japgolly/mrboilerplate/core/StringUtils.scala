package japgolly.mrboilerplate.core

import scala.annotation.tailrec

object StringUtils {

  implicit final class StringExt(private val self: String) extends AnyVal {

    def camelToSnakeCase: String = {
      // https://gist.github.com/sidharthkuruvila/3154845
      @tailrec
      def camel2SnakeRec(s: String, output: String, lastUppercase: Boolean): String =
        if (s.isEmpty)
          output
        else {
          val c = if (s.head.isUpper && !lastUppercase) "_" + s.head.toLower else s.head.toLower
          camel2SnakeRec(s.tail, output + c, s.head.isUpper && !lastUppercase)
        }
      if (self.forall(_.isUpper))
        self.map(_.toLower)
      else
        camel2SnakeRec(self, "", true)
    }

    /** `"foo_bar" --> "FooBar"` */
    def snakeToCamelCase: String = {
      // https://github.com/lift/framework/blob/master/core/util/src/main/scala/net/liftweb/util/StringHelpers.scala
      def loop(x : List[Char]): List[Char] = (x: @unchecked) match {
        case '_' :: '_' :: rest => loop('_' :: rest)
        case '_' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
        case '_' :: Nil => Nil
        case c :: rest => c :: loop(rest)
        case Nil => Nil
      }
      if (self == null)
        ""
      else
        loop('_' :: self.toList).mkString
    }

    /** `"foo_bar" --> "fooBar"` */
    def snakeToMethodCamelCase: String =
      self.snakeToCamelCase.withHeadLower

    /** Capitalize every "word" in the string. A word is either separated by spaces or underscores. */
    def toTitleCase: String = {
      // https://github.com/lift/framework/blob/master/core/util/src/main/scala/net/liftweb/util/StringHelpers.scala
      import java.lang.{StringBuilder => GoodSB}
      def capify(in: String, pos: Int, max: Int, lastLetter: Boolean, lastSymbol: Boolean, out: GoodSB): Unit = {
        if (pos >= max || pos >= in.length) return
        else {
          in.charAt(pos) match {
            case c if Character.isDigit(c) => out.append(c); capify(in, pos + 1, max, false, false, out)
            case c if Character.isLetter(c) => out.append(if (lastLetter) c else Character.toUpperCase(c)) ; capify(in, pos + 1, max, true, false, out)
            case c if (c == ' ' || c == '_') && !lastSymbol => out.append(c) ; capify(in, pos + 1, max, false, true, out)
            case _ => capify(in, pos + 1, max, false, true, out)
          }
        }
      }
      val tmp = ((self match {
        case null => ""
        case s => s
      }).trim match {
        case "" => "n/a"
        case s => s
      }).toLowerCase
      val sb = new GoodSB
      capify(tmp, 0, 250, false, false, sb)
      sb.toString
    }

    def withHeadLower: String =
      if (self.isEmpty) "" else self.substring(0, 1).toLowerCase + self.substring(1)

    def withHeadUpper: String =
      if (self.isEmpty) "" else self.substring(0, 1).toUpperCase + self.substring(1)

    def quoted: String =
      "\"" + self + "\"" // simple for now

    def withoutWildcards =
      self.replaceAll("""\[[\[\]_,\s]+\]""", "")
  }

}
