package sbt
package errorssummary

import xsbti.{Position, Reporter, Severity}

import java.io.File
import scala.Console._
import scala.compat.Platform.EOL

/**
 * A concise reporter that shows a summary of the lines with errors and warnings.
 *
 * @param logger The logger that will receive the output of the reporter.
 * @param base   The base prefix to remove from paths.
 * @param parent Another reporter that should also receive the messages.
 */
private class ConciseReporter(logger: Logger,
                              enableColors: Boolean,
                              base: String,
                              parent: Option[Reporter])
    extends Reporter {

  private val _problems = collection.mutable.ArrayBuffer.empty[Problem]

  override def reset(): Unit = {
    parent.foreach(_.reset())
    _problems.clear()
  }

  override def hasErrors(): Boolean =
    hasErrors(_problems)

  override def hasWarnings(): Boolean =
    hasWarnings(_problems)

  private def showFile(s: String): String =
    s.dropWhile(_ == '/')

  override def printSummary(): Unit = {
    parent.foreach(_.printSummary())

    _problems.foreach(logFull)

    val log: String => Unit =
      (line: String) =>
        if (hasErrors(_problems)) logger.error(line)
        else if (hasWarnings(_problems)) logger.warn(line)
        else logger.info(line)

    val problemsByFile = _problems.groupBy(i => showFile(i.position.pfile)).toArray

    if (problemsByFile.lengthCompare(1) > 0)
      problemsByFile.sortBy(_._1).foreach {
        case (file, inFile) =>
          /*
          def total(s: Severity, colour: String): String = {
            val n = inFile.count(_.severity == s)
            if (n == 0) "" else colored(colour, s" ($n)")
          }

//          val f = colored(UNDERLINED + YELLOW, file)
          val f = colored(UNDERLINED + GREEN, file)
//          val f = file
          val w = total(Severity.Warn, YELLOW)
          val e = total(Severity.Error, BOLD + RED)
          val d = colored(BOLD + BLUE, s"[${ConciseIntSetFormat.spaced(inFile.iterator.map(_.id))}]")
//          val d = colored(BOLD + BLUE, inFile.map(_.id).sorted.mkString("[", ",", "]"))
//          val details = inFile.sortBy(_.position.pline).map(showProblemLine).mkString(", ")

          log(s"$f:$w$e $d")
          */

          val hdrSize = _problems.groupBy(_.severity).valuesIterator.map(_.length.toString.length).max
          val hdrFmt = s"%${hdrSize}d"
          val hdrEmpty = " " * hdrSize

          def subset(s: Severity, colour: String): (String, String) = {
            val a = inFile.iterator.filter(_.severity == s).toArray
            if (a.isEmpty) (hdrEmpty, "") else {
              val count = colored(colour, hdrFmt.format(a.length))
              val details = colored(BOLD + BLUE, s"[${ConciseIntSetFormat.short(a.iterator.map(_.id))}]")
              val lines = colored(colour, s"[${ConciseIntSetFormat.short(a.iterator.map(_.position.pline))}]")
              (count, s"$details @ $lines")
            }
          }

//                    val f = colored(UNDERLINED + YELLOW, file)
//          val f = colored(UNDERLINED, file)
          //          val f = file
//          val (w1,w2) = subset(Severity.Warn, YELLOW)
//          val (e1,e2) = subset(Severity.Error, BOLD + RED)
          //          val d = colored(BOLD + BLUE, inFile.map(_.id).sorted.mkString("[", ",", "]"))
          //          val details = inFile.sortBy(_.position.pline).map(showProblemLine).mkString(", ")

          var first = true
          def qwe(x: (String, String)): Unit = {
            val (a, b) = x
            if (a != hdrEmpty) {
              val f = if (first) colored(UNDERLINED + YELLOW, file) else " " * file.length
              log(s"$a $f $b")
              first = false
            }
          }

          qwe(subset(Severity.Error, BOLD + RED))
          qwe(subset(Severity.Warn, YELLOW))
      }

    def reportTotal(s: Severity, colour: String, unit: String): Unit = {
      val n = _problems.count(_.severity == s)
      if (n != 0) {
        val units = if (n == 1) unit else unit + "s"
        log(colored(colour, s"$n $units found."))
        //        log(colored(BOLD + colour, n.toString) + colored(colour, s" $units found."))
      }
    }

    reportTotal(Severity.Warn, YELLOW_B + BLACK, "warning")
    reportTotal(Severity.Error, RED_B, "error")
  }

  override def problems(): Array[xsbti.Problem] =
    _problems.toArray

  override def log(pos: Position, msg: String, sev: Severity): Unit = {
    parent.foreach(_.log(pos, msg, sev))
    _problems += Problem(_problems.length + 1, sev, msg, pos)
  }

  override def comment(pos: Position, msg: String): Unit =
    parent.foreach(_.comment(pos, msg))

  /**
   * Log the full error message for `problem`.
   *
   * @param problem The problem to log.
   */
  private def logFull(problem: Problem): Unit = {
    val text = showText(problem)
    problem.severity match {
      case Severity.Error => logger.error(text)
      case Severity.Warn  => logger.warn(text)
      case Severity.Info  => logger.info(text)
    }
  }
  private def hasErrors(problems: Seq[Problem]): Boolean =
    problems.exists(_.severity == Severity.Error)

  private def hasWarnings(problems: Seq[Problem]): Boolean =
    problems.exists(_.severity == Severity.Warn)

  /**
   * Returns the absolute path of `file` with `base` stripped.
   *
   * @param file The file whose path to show.
   * @return The absolute path of `file` with `base` stripped.
   */
  private def showPath(file: File): String =
    Option(file).map(_.getAbsolutePath.stripPrefix(base)).getOrElse("Unknown")

  /**
   * Shows `str` with color `color`.
   *
   * @param color The color to use
   * @param str   The string to color.
   * @return The colored string.
   */
  private def colored(color: String, str: String): String =
    if (enableColors) s"${RESET}${color}${str}${RESET}"
    else str

  /**
   * Put a prefix `prefix` at the beginning of `paragraph`, indents all lines.
   *
   * @param prefix    The prefix to insert.
   * @param paragraph The block of text to prefix and indent.
   * @return The prefixed and indented paragraph.
   */
  private def prefixed(prefix: String, paragraph: String): String =
    augmentString(paragraph).lines
      .mkString(colored(BOLD + BLUE, prefix), EOL + " " * prefix.length, "")

  /**
   * Shows the full error message for `problem`.
   *
   * @param problem The problem to show
   * @return The full error message.
   */
  private def showText(problem: Problem): String = {
    val file = showFile(problem.position.pfile)
    val line = problem.position.pline
    val text =
      s"""${colored(UNDERLINED + BOLD + YELLOW, file)}:${colored(colorFor(problem),
                                                 line.toString)}:
         |${problem.message}
         |${problem.position.lineContent}
         |${problem.position.pointerSpace
           .map(sp => s"$sp^")
           .getOrElse("")}""".stripMargin
    val extraSpace = if (problem.severity == Severity.Warn) " " else ""
    prefixed(s"$extraSpace[${problem.id}] ", text)
  }

  /**
   * Retrieves the right color to use for `problem` based on Severity.
   *
   * @param problem The problem to show.
   * @return The ANSI string to set the right color.
   */
  private def colorFor(problem: Problem): String =
    problem.severity match {
      case Severity.Info  => BLUE
      case Severity.Error => BOLD + RED
      case Severity.Warn  => YELLOW
    }

//  /**
//   * Shows the line at which `problem` occured and the id of the problem.
//   *
//   * @param problem The problem to show
//   * @return A formatted string that shows the line of the problem and its id.
//   */
//  private def showProblemLine(problem: Problem): String = {
//    val color = colorFor(problem)
//    colored(color, problem.position.pline.toString) + colored(
//      BOLD + BLUE,
//      s" [${problem.id}]")
//  }

  implicit class MyPosition(position: Position) {
    def pfile: String = position.sourceFile.map(showPath).getOrElse("unknown")
    def pline: Int    = position.line.map(_.toInt).getOrElse(0)
  }

  private case class Problem(id: Int,
                             severity: Severity,
                             message: String,
                             position: Position)
      extends xsbti.Problem {
    override val category: String = ""
  }

}
