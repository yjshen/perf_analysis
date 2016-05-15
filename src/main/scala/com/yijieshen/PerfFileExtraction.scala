package com.yijieshen

import java.io.{File, FileFilter}

object PerfFileExtraction {
  def main(args: Array[String]) {
    val srcFolder = "/Users/yijie/code/perf_result"
    val targetFolder = "/Users/yijie/code/perf_analysis"

    val root = new File(srcFolder)
    val target = new File(targetFolder)
    val appFolders = root.listFiles(new FileFilter {
      override def accept(pathname: File): Boolean = {
        pathname.getName.startsWith("app")
      }
    })

    val folerNames = appFolders.map(_.getName)
    val perfFiles = appFolders.flatMap(findPerfResult)

    folerNames.zip(perfFiles).foreach { case (name, file) =>
      file.renameTo(new File(target, name))
    }

  }

  def findPerfResult(appFolder: File): Array[File] = {
    val all = appFolder.listFiles()
    all.filter(f => !f.isDirectory && f.getName.startsWith("perf")) ++
      all.filter(_.isDirectory).flatMap(findPerfResult)
  }
}
