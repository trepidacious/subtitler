package org.rebeam.subtitler

import java.io.{File, PrintWriter}

import scala.io.Source

object SubtitlerApp extends App {

  val TimingLine = "(\\d\\d):(\\d\\d):(\\d\\d),(\\d\\d\\d)\\s-->\\s(\\d\\d):(\\d\\d):(\\d\\d),(\\d\\d\\d)".r

  def padTwo(s: String) = if (s.length < 2) "0" + s else s
  def padThree(s: String) = if (s.length == 1 ) "00" + s else if (s.length == 2) "0" + s else s

  def timeFormat(t: Int) = {
    val ms = t % 1000
    val as = t / 1000
    val s = as % 60
    val am = as / 60
    val m = am % 60
    val h = am / 60
    padTwo(h.toString) + ":" + padTwo(m.toString) + ":" + padTwo(s.toString) + "," + padThree(ms.toString)
  }

  def tolerantUTF8Source(file: File) = {
//    import java.nio.charset.CodingErrorAction
//    import scala.io.Codec
//
//    val codec = Codec("UTF-8")
//    codec.onMalformedInput(CodingErrorAction.REPLACE)
//    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
//
//    Source.fromFile(file)(codec)
    Source.fromFile(file)("ISO-8859-1")
  }

  def convert(file: File, outFile: File, factor: Double): Unit = {
    println(file)

    val writer = new PrintWriter(outFile, "UTF-8")

    def out(s: String): Unit = {
      writer.write(s + "\r\n")
      println(s)
    }

//    for (line <- Source.fromFile(file)("ISO-8859-1").getLines()) {
    for (line <- tolerantUTF8Source(file).getLines()) {
      line match {
        case TimingLine(h1, m1, s1, ms1, h2, m2, s2, ms2) => {
//          println("TimingLine(" + h1 + ", " + m1 + ", " + s1 + ", " + ms1 + ", " + h2 + ", " + m2 + ", " + s2 + ", " + ms2 + ")")

          val t1 = ms1.toInt + s1.toInt * 1000 + m1.toInt * 1000 * 60 + h1.toInt * 1000 * 60 * 60
          val t2 = ms2.toInt + s2.toInt * 1000 + m2.toInt * 1000 * 60 + h2.toInt * 1000 * 60 * 60
//          println(t1 + "ms to " + t2 + "ms")
//          println(line)
          val reformatted = timeFormat((t1 * factor).toInt) + " --> " + timeFormat((t2 * factor).toInt)
//          println(line)
          out(reformatted)
        }
        case _ => out(line)
      }
    }

    writer.close()

  }


  val home = System.getProperty("user.home")

  val dir = new File(home + "/Documents/s01")
  val outDir = new File(dir, "out")

  val files = dir.listFiles().filter(f => f.isFile && f.getName.endsWith(".srt"))

  files.foreach(f => convert(f, new File(outDir, f.getName), 23.976/25.0))

//  val dir = new File(Sys)

}
