package uk.org.openeyes.jsonschema.core

import java.net.URI
import org.json4s._
import java.io.{File,FileReader,Reader}

class SchemaLoader(parse: (JsonInput, Boolean) => JValue, extract: JValue => Schema) {
  def loadFromDir(path: String) = {
    val root = new File(path)
    if (!root.isDirectory) throw new IllegalArgumentException("Path '" + path + "' is not a directory")

    val rootLen = root.getAbsolutePath.length

    DraftV4Schema(
      generalValidation = GeneralValidation(
        definitions = Some(
          findFiles(root).map(f => {
            f.getAbsolutePath.substring(rootLen + 1).replaceAll("""\.json$""", "").replaceAll("/", ".") -> loadSchema(new java.io.FileReader(f))
          }).toMap
        )
      )
    )
  }

  private def findFiles(base: File): Seq[File] = {
    Option[Array[File]](base.listFiles).getOrElse(Array()).partition(_.isDirectory) match {
      case (dirs, files) =>
        files.filter(_.getPath().matches(""".*\.json""")) ++
        dirs.filter(!_.getPath().matches("""\..*""")).flatMap(findFiles)
    }
  }

  private def loadSchema(r: Reader) = {
    extract(Transform.transformRefs(parse(r, true), convertRef))
  }

  private def convertRef(uri: URI) = {
    new URI("#definitions/" + uri.getSchemeSpecificPart + Option(uri.getFragment).fold("")("/" + _))
  }
}
