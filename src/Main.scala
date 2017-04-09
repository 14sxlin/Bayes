import java.io.{File, FileNotFoundException}
import java.nio.charset.MalformedInputException

import algorithm.BayesAlgorithm
import config.Config
import input.DataSource
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Main extends App{


  @Option(name = "-p",usage = "指定配置文件",required = true)
  var propertiesFile:File= _

  @Option(name = "-i",usage = "指定输入属性的文件",required = true)
  var inputFile:File = _

  lazy val argsLocal = Array("-p", "resource/buyPC.properties","-i","resource/buyPC.input")


  override def main(args: Array[String]): Unit = {
    val cmdLineParser = new CmdLineParser(this)
    try {
      assert(cmdLineParser != null)
      cmdLineParser.parseArgument(args.toIterable)

      val inputs = ArrayBuffer[Array[String]]()
      for ( line <- Source.fromFile(inputFile).getLines()){
        inputs += line.split(",")
      }

      val dataModel = DataSource.fromFile(propertiesFile)

      for(input <- inputs)
      {
        val bayesAlgorithm = new BayesAlgorithm(dataModel,input)
        bayesAlgorithm.classify()
      }

    }catch {
      case e:CmdLineException =>
        println(e.getMessage)
        cmdLineParser.printUsage(System.err)
      case e:FileNotFoundException =>
        e.printStackTrace(System.out)
      case e:MalformedInputException =>
        println("数据模型的编码错误")
        e.printStackTrace(System.out)
    }

  }

}
