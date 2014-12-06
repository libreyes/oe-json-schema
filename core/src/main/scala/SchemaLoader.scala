package uk.org.openeyes.jsonschema.core

import java.net.URI
import org.json4s._
import java.io.{File,FileReader,Reader}

class SchemaLoader(parse: (JsonInput, Boolean) => JValue) {
  def loadFromDir(path: String): Map[String, JValue] = {
    val root = new File(path)
    if (!root.isDirectory) throw new IllegalArgumentException("Path '" + path + "' is not a directory")

    val rootLen = root.getAbsolutePath.length

    findFiles(root).map(f => {
      JField(f.getAbsolutePath.substring(rootLen + 1).replaceAll("""\.json$""", "").replaceAll("/", "."), parse(new java.io.FileReader(f), true))
    }).toMap
  }

  private def findFiles(base: File): Seq[File] = {
    Option[Array[File]](base.listFiles).getOrElse(Array()).partition(_.isDirectory) match {
      case (dirs, files) =>
        files.filter(_.getPath().matches(""".*\.json""")) ++
        dirs.filter(!_.getPath().matches("""\..*""")).flatMap(findFiles)
    }
  }
}
