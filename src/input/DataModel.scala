package input

import scala.collection.immutable.IndexedSeq

/**
  * 用来表示输入模型的类
  *
  * @param colName 列名称
  * @param isDispersed 该列是否是离散型数据,否则为连续性数据
  * @param data     具体的数值
  */
class DataModel(val colName:Array[String],
                val isDispersed:Array[Boolean],
                val data:Array[Array[String]]) {

  var useLaplace = false            //拉普拉斯估计,防止出现条件概率为0的情况
  private[this] var correct = 0    //使用拉普拉斯估计对总数的校准
  private var _labelIndex = -1      //类标号的下标
  private var _labelName = ""      //类标号的名称

  def labelIndex_=(index:Int): Unit ={
    this._labelIndex = index
    this._labelName = colName(_labelIndex)
  }

  def labelIndex: Int = _labelIndex

  def labelName_=(name:String): Unit ={
    this._labelName = name
    this._labelIndex = colName.indexOf(name)
    assert(_labelIndex != -1,"没有指定分类列,请检查列名以及编码")
  }

  def labelName: String = _labelName

  def this(colName:Array[String],
           isDispersed:Array[Boolean],
           data:Array[Array[String]],
           labelName:String){
    this(colName,isDispersed,data)
    this._labelName = labelName

  }


  /**
    * 计算某个值在某一列出现的次数
    * @param colIndex 列的下标
    * @param value  寻找的值
    * @return
    */
  def countAttribute(colIndex:Int,value:String):Double={
    var count = 0
    for(i <- data.indices if data(i)(colIndex)==value)
      count += 1
    if (useLaplace)
    {
      correct = getAllDispersedValues(colIndex).length
      (count+1)*1.0
    }
    else
    {
      correct = 0
      count*1.0
    }
  }
  def countAttribute(colName:String,value:String):Double={
    countAttribute(this.colName.indexOf(colName),value)
  }

  def totalCount:Int = if(!useLaplace) data.length else data.length+correct

  /**
    * 获取数据表中某一列的值
    * @param columnIndex 列的下标
    * @return
    */
  def getColumnValues(columnIndex:Int): IndexedSeq[String] = {
    for(i <- data.indices)
      yield data(i)(columnIndex)
  }
  def getColumnValues(colName:String): IndexedSeq[String] = {
    getColumnValues(this.colName.indexOf(colName))
  }

  /**
    * 获取离散型变量的所有不同的取值
    * @param colIndex 列的下标
    */
  def getAllDispersedValues(colIndex:Int): IndexedSeq[String] = {
    assert(isDispersed(colIndex),s"${colName(colIndex)}必须是离散型的")
    val colValues = getColumnValues(colIndex)
    colValues.distinct
  }
  def getAllDispersedValues(colName:String): IndexedSeq[String] = {
    getAllDispersedValues(this.colName.indexOf(colName))
  }

  /**
    * 获取类标号的所有不同的值
    * @return
    */
  def labelValues(): IndexedSeq[String] ={
    getAllDispersedValues(_labelIndex)
  }

  /**
    * 获取第columnIndex列的所有类标号为labelValue的列的值
    * @param columnIndex 列的下标
    * @param labelValue 类的标号 要求类标号列必须是离散的
    * @return  返回值的数组
    */
  def getColumnValuesWithLabel(columnIndex:Int,labelValue:String): IndexedSeq[String]={
    assert(!isDispersed(columnIndex), s"类标号列:${colName(columnIndex)} 不是连续型变量")
    for(i <- data.indices if data(i)(_labelIndex)==labelValue)
      yield data(i)(columnIndex)
  }

  //下面的函数是针对离散型的变量

  /**
    * 求离散属性条件概率 P(attribute|label)
    * @param labelValue 分类值
    * @param attribute 属性值
    * @param attrColIndex 属性所在的列
    * @return
    */
  def getDispersedConditionProbability(labelValue:String,attrColIndex:Int,attribute:String):Double={
    assert(_labelIndex<data.length&&_labelIndex>=0,
      s"属性${colName(attrColIndex)}下标越界")       //保证下标有意义
    assert(attrColIndex>=0 && attrColIndex<data.length,
      s"属性${colName(attrColIndex)}下标越界")      //保证下标有意义
    assert(isDispersed(attrColIndex),s"${colName(attrColIndex)} 不是离散型")

    val labelCount = countAttribute(_labelIndex,labelValue)

    var attributeWithLabelCount = 0
    for(i<- data.indices                 // 计算某种类标号下属性值为attribute的个数
        if data(i)(attrColIndex)==attribute && data(i)(_labelIndex)==labelValue)
      attributeWithLabelCount += 1
    attributeWithLabelCount*1.0/labelCount
  }

  def getDispersedConditionProbability(labelValue:String,attrName:String,attrValue:String):Double={
    getDispersedConditionProbability(labelValue,colName.indexOf(attrName),attrValue)
  }


  // 下面的函数是针对连续型的变量

  /**
    * 求平均值
    * @param values 一组数值
    * @return
    */
  def average(values:IndexedSeq[String]): Double = {
    var sum = 0.0
    for(value <- values)
    {
      sum += value.toDouble
    }
    sum/values.length
  }

  /**
    * 求方差
    * @param values 一组数值
    */
  def variance(values:IndexedSeq[String]): Double ={
    val avg = average(values)
    var sum = 0.0
    for(value <- values)
      sum += Math.pow(Math.abs(value.toDouble - avg),2)
    sum/(values.length-1)
  }

  /**
    * 计算x在正态分布密度函数中的值
    * @param x 要被估计的值
    * @param avg 特定类型的条目的属性的平均值
    * @param variance 特定类型的条目的属性的方差
    */
  def normality(x:Double,avg:Double,variance:Double):Double={
    1/Math.sqrt(2*Math.PI*variance)*Math.exp(-Math.pow(x-avg,2)/(2*variance))
  }


  /**
    * 估计colIndex列的x在类标号为labelValue出现的情况下的概率
    * 也就是条件概率
    * @param colIndex 列的下标号
    * @param x 属性的值
    * @param labelValue 标签的值
    */
  def getConditionNormality(colIndex:Int,x:Double,labelValue:String):Double = {
    assert(!isDispersed(colIndex), s"${colName(colIndex)} 不是连续型变量")
    val values = getColumnValuesWithLabel(colIndex,labelValue)
    val avg = average(values)
    val vari = variance(values)
    normality(x,avg,vari)
  }

}
