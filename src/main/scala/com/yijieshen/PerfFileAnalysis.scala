package com.yijieshen

import java.io.{File, FileFilter}

import scala.collection.mutable
import scala.io.Source

import org.sameersingh.scalaplot.XYData

object PerfFileAnalysis {
  val allMetricNames = Set(
      "instructions",
      "cache-references",
      "cache-misses",
      "branch-instructions",
      "branch-misses",
      "stalled-cycles-frontend",
      "stalled-cycles-backend",
      "ref-cycles",
      "cpu-clock",
      "task-clock",
      "page-faults",
      "context-switches",
      "cpu-migrations",
      "minor-faults",
      "major-faults",
      "alignment-faults",
      "emulation-faults",
      "L1-dcache-loads",
      "L1-dcache-load-misses",
      "L1-dcache-stores",
      "L1-dcache-store-misses",
      "L1-dcache-prefetches",
      "L1-dcache-prefetch-misses",
      "L1-icache-loads",
      "L1-icache-load-misses",
      "LLC-loads",
      "LLC-load-misses",
      "LLC-stores",
      "LLC-store-misses",
      "LLC-prefetches",
      "LLC-prefetch-misses",
      "dTLB-loads",
      "dTLB-load-misses",
      "dTLB-stores",
      "dTLB-store-misses",
      "iTLB-loads",
      "iTLB-load-misses",
      "branch-loads",
      "branch-load-misses")

  def main(args: Array[String]) {

    val nonEmptyFiles: Seq[File] = getSampleFiles()

    // extractSpeeds(nonEmptyFiles)

    extractExactNum(nonEmptyFiles)
  }

  def extractExactNum(nonEmptyFiles: Seq[File]): Unit = {
    val allMetrics = new mutable.HashMap[String, Array[Double]]()

    allMetricNames.foreach { metric =>
      allMetrics(metric) = new Array[Double](15)
    }

    nonEmptyFiles.zipWithIndex.foreach { case (f, i) =>
      for (line <- Source.fromFile(f).getLines()) {
        val metrics: Set[String] = allMetricNames.filter(line.contains(_))
        if (metrics.size > 0) {
          val metricName = metrics.maxBy(_.size)
          val h = line.trim.split(" ")(0)
          allMetrics.get(metricName).get.update(i, h.replace(",", "").toDouble)
        }
      }
    }

    allMetrics.foreach { case (s, v) =>
      draw("count", s, "", v)
    }
  }

  def extractSpeeds(nonEmptyFiles: Seq[File]): Unit = {
    val allMetrics = new mutable.HashMap[String, Array[Double]]()

    allMetricNames.foreach { metric =>
      allMetrics(metric) = new Array[Double](15)
    }

    val allMetricUnit = mutable.HashMap.empty[String, String]

    val doubleReg = """(\d+\.\d+)(.*)""".r

    nonEmptyFiles.zipWithIndex.foreach { case (f, i) =>
      for (line <- Source.fromFile(f).getLines()) {
        val metrics: Set[String] = allMetricNames.filter(line.contains(_))
        if (metrics.size > 0) {
          val metricName = metrics.maxBy(_.size)
          val h = line.substring(50)

          doubleReg.findFirstMatchIn(h) match {
            case Some(m) =>
              allMetrics.get(metricName).get.update(i, m.group(1).toDouble)
              allMetricUnit.put(metricName, m.group(2).split('[')(0).trim)
            case None => // do nothing
          }
        }
      }
    }

    allMetrics.foreach { case (s, v) =>
      draw("speed", s, allMetricUnit.getOrElse(s, ""), v)
    }
  }

  def getSampleFiles(): Seq[File] = {
    val dataFolder = "/Users/yijie/code/perf_analysis/perf_data"
    val root = new File(dataFolder)
    val nonEmptyFiles = collection.mutable.ArrayBuffer.empty[File]
    root.listFiles(new FileFilter {
      override def accept(f: File): Boolean = {
        f.getName.startsWith("app")
      }
    }).sliding(5, 5).foreach { case arr =>
      arr.length
      nonEmptyFiles += arr.filter(_.length() > 0).head
    }
    nonEmptyFiles
  }

  def draw(postfix: String, name: String, yName: String, arr: Array[Double]): Unit = {
    import org.sameersingh.scalaplot.Implicits._

    val x = (0 to 14).map(math.pow(2, _))

    val d: XYData = x -> Seq(Y(arr, label = ""))

    output(PDF("img/", name + s"_$postfix"),
      xyChart(d, name, x = Axis(label = "BatchSize", log = true, logbase = 2), y= Axis(label = yName), showLegend = true))

    println(name)
  }
}
