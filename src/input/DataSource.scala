package input

import java.io.File

import config.Config

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object DataSource {

  var separator : String = _
  var dataPath : String = _
  var hasHeader: Boolean = _
  var headers : Array[String] = _
  var encoding: String = _
  var labelName:String = _
  var dispersed:Array[Boolean] = _
  var useLaplace:Boolean = _

  def initProperties(propertiesFile:File): Unit ={
    val config = Config.load(propertiesFile)
    separator = config.getProperty("separator")
    dataPath = config.getProperty("dataPath")
    hasHeader = config.getProperty("hasHeader").toBoolean
    encoding = config.getProperty("encoding")
    labelName = config.getProperty("labelName")
    headers = config.getProperty("headers").split(",")
    dispersed = config.getProperty("dispersed").split(",")
        .map( item => if (item == "1") true else false)
    useLaplace = config.getProperty("useLaplace").toBoolean
  }

  /**
    * @return 返回DataModel类的实例,表示数据
    */
  def fromFile(propertiesFile:File): DataModel = {
    initProperties(propertiesFile)
    val bufferSource = Source.fromFile(new File(dataPath),encoding)
    val headers = this.headers
    val types = dispersed
    val datas = ArrayBuffer[Array[String]]()
    val lines = bufferSource.getLines()
    var count = 0
    for(line <- lines){
      if(hasHeader && count==0){              // 读取列名字,默认第一行是列名字
      }
      else{
        datas += line.split(",")
      }
      count += 1
    }

    val model = new DataModel(
      headers.toArray,
      types.toArray,
      datas.toArray)
    model.labelName = labelName
    model.useLaplace = useLaplace
    model
  }

  def fromCSV(propertiesFile:File): DataModel = {
    fromFile(propertiesFile)
  }
}
