package config

import java.io.File

import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConversions._
/**
  * Created by sparr on 2017/4/8.
  */

object TestArgsParser {

  @Option(name="-r",usage="recursively run something")
  var recursive = ""

  @Option(name="-o",usage="output to this file",metaVar="OUTPUT")
  var out = new File(".")

  @Option(name="-n",usage="repeat <n> times\nusage can have new lines in it and also it can be verrrrrrrrrrrrrrrrrry long")
  var  num = -1

  val args0 = Array("-r", "192.168.1.1", "-o", "text", "-n", "99","-str","shit","-hah","aa")

  def main(args: Array[String]): Unit = {

    val parser = new CmdLineParser(this)
    try {
      parser.parseArgument(args0.toIterable)
      println(s"num = $num,out=${out.getAbsoluteFile},recursive=$recursive")
      print(num + 1 toString)
    } catch {
      case e:CmdLineException =>
        println(e.getMessage)
        parser.printUsage(System.err)
    }
  }
}
