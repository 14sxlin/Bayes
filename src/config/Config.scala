package config

import java.io.{File, FileInputStream, FileOutputStream, InputStreamReader}
import java.util.Properties


object Config {

  private[this] var file:File = _

  def load(file: File,encoding:String="utf-8"): Properties = {
    this.file = file
    val properties = new Properties()
    properties.load(new InputStreamReader(new FileInputStream(file),encoding))
    properties
  }

  def store(properties: Properties): Unit ={
    properties.store(new FileOutputStream(file),"")
  }

  def main(args: Array[String]): Unit = {
    val ps = Array("a","b","c","d","e","f")
    val propertyFile = new File("src/buyPC.properties")
    val config = load(propertyFile)
    for(name <- ps){
      println(s"$name = '${config.getProperty(name)}'")
    }
    config.setProperty("a",(config.getProperty("a").toInt + 1).toString)
    println(s"a = '${config.getProperty("a")}'")
    config.store(new FileOutputStream(propertyFile),"hello")
  }
}
