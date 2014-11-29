#!/bin/sh
exec scala -feature -savecompiled $0 $@ 
!#

import java.io.File
import scala.io.Source
import scala.language.{implicitConversions, reflectiveCalls}
import java.nio.charset.CodingErrorAction
import scala.io.Codec
import scala.collection.mutable.Stack

implicit val codec = Codec("UTF-8")
codec.onMalformedInput(CodingErrorAction.REPLACE)
codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

implicit def toClosingSource(source: Source) = new {
  val lines = source.getLines
  var stillOpen = true
  def getLinesAndClose = new Iterator[String] {
    def hasNext = stillOpen && lines.hasNext
    def next = {
      val line = lines.next
      if (!lines.hasNext) { source.close() ; stillOpen = false }
      line
      }
    }
}

def findFiles(file: String): Seq[File] = {
  findFiles(Stack(new File(file)), Seq.empty)
}

def findFiles(files: Stack[File], result: Seq[File], hidden: Boolean = false): Seq[File] = {
  if (files.isEmpty) result else {
    val file = files.pop
    if (file.isFile) {
      findFiles(files, result :+ file)
    } else {
      file.listFiles.foreach { file =>
        val isScalaFile = file.isDirectory || (file.getName.endsWith(".scala"))
        val showHidden = hidden || (!file.isHidden)
        val include = showHidden && isScalaFile
        if (include) {
          files.push(file)
        }
      }
      findFiles(files, result)
    }
  }
}

val files = findFiles(".")

files.toList.foreach { file =>
  val lines = Source.fromFile(file).getLinesAndClose.toList

  val imports = lines.filter(_.startsWith("import")).zipWithIndex.flatMap { case (line, i) =>
    val last: Seq[String] = {
      val token = line.trim.split("\\.").last

      // handle grouped imports
      val tokens = if (token.startsWith("{")) {
        token.stripPrefix("{").stripSuffix("}")
            .split(",").map(_.trim).toSeq
      } else Seq(token)

      // handle aliases
      tokens.map { token =>
        if (token.contains("=>")) {
          token.split("=>").map(_.trim).last
        } else token
      }
    }

    /*if (last.exists(_ == "_")) {
      println(s"$file:$i WARN - found wildcard import")
    }*/

    last.filterNot(_ == "_")
  }

  val body = lines.filterNot(_.startsWith("import")).mkString("\n")
  val unused = imports.filterNot(body.contains)

  if (unused.nonEmpty) {
    val name = file.getAbsolutePath
    println(s"$name - ${unused.mkString(" ")}")
  }
}


