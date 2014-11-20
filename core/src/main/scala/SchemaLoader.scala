package uk.org.openeyes.jsonschema.core

import java.io.{File,FileReader,Reader}

class SchemaLoader(deserialiser: (Reader => Schema)) {
  def loadFromDir(uriScheme: String, path: String) = {
    val root = new File(path)
    val rootLen = root.getAbsolutePath.length

    new SchemaSet(
      uriScheme,
      findFiles(root).map(f => {
        f.getAbsolutePath.substring(rootLen + 1).replaceAll("""\.json$""", "").replaceAll("/", ".") -> deserialiser(new java.io.FileReader(f))
      }).toMap
    )
  }

  private def findFiles(base: File): Seq[File] = {
    base.listFiles.partition(_.isDirectory) match {
      case (dirs, files) =>
        files.filter(_.getPath().matches(""".*\.json""")) ++
        dirs.filter(!_.getPath().matches("""\..*""")).flatMap(findFiles)
    }
  }
}
