package sbt.errorssummary

/**
 * Format a set of ints into a concise textual description.
 *
 * Example:
 *    "2, 5, 7-14, 20"
 */
object ConciseIntSetFormat { // TODO Move into microlibs

  def apply(ints: TraversableOnce[Int], sep: String, rangeSep: String): String = {

    val comps = ints.toArray.sorted.foldRight(List.empty[Any])((i, cs) =>
      cs match {
        case (a: Int) :: (b: Int) :: t if i == a - 1 && a == b - 1 =>
          (i, b) :: t
        case (a: Int, b: Int)     :: t if i == a - 1 =>
          (i, b) :: t
        case _ =>
          i :: cs
      })

    val sb = new StringBuilder
    for (c <- comps) {
      if (sb.nonEmpty) sb.append(sep)
      c match {
        case (i: Int)         => sb append i
        case (a: Int, b: Int) => sb append a; sb append rangeSep; sb append b
        case _ => ???
      }
    }
    sb.toString
  }

  def short(ints: TraversableOnce[Int]): String =
    apply(ints, ",", "-")

  def spaced(ints: TraversableOnce[Int]): String =
    apply(ints, ", ", " - ")
}
