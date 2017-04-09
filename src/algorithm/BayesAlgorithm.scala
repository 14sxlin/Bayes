package algorithm

import java.text.DecimalFormat

import input.DataModel

/**
  * Created by sparr on 2017/4/7.
  */
class BayesAlgorithm(val dataModel: DataModel,val input:Array[String]){

  /**
    * 针对输入进行分类
    */
  def classify(){
    val labelValues = dataModel.labelValues()
    val rate = new Array[Double](labelValues.length)
    for(i <- labelValues.indices){    //对每个不同的分类求条件概率
      println(s"----------${dataModel.labelName} = ${labelValues(i)}-------------")
      var multi = 1.0
      for(j <- input.indices){         //求每一列对应的条件概率
        if(dataModel.isDispersed(j)){   // 如果是离散的
          val p = dataModel.getDispersedConditionProbability(labelValues(i),j,input(j))
          multi *= p
          println(s"P(${dataModel.colName(j)}=${input(j)}" +
            s"|${dataModel.labelName}=${labelValues(i)})=$p")
        }
        else{                         // 如果是连续型的
          val p = dataModel.getConditionNormality(j,input(j).toDouble,labelValues(i))
          multi *= p
          println(s"P(${dataModel.colName(j)}=${input(j)}" +
            s"|${dataModel.labelName}=${labelValues(i)})=$p")
        }
      }
      println(s"P((${input.mkString(",")})" +
        s"|${dataModel.labelName}=${labelValues(i)})=$multi")
      rate(i) = multi * dataModel.countAttribute(dataModel.labelIndex,labelValues(i))/dataModel.totalCount
      println(s"P((${input.mkString(",")})" +
        s"|${dataModel.labelName}=${labelValues(i)})*P(${dataModel.labelName}=${labelValues(i)})=${rate(i)}")
    }
    println("--------------result-----------")
    val maxIndex = rate.indexOf(rate.max)
    var temp = ""
    for(i<-input.indices)
      temp += (dataModel.colName(i) + "=" + input(i))+"  "
    println(s"($temp) belongs to ${dataModel.labelName}" +
      s"=${labelValues(maxIndex)} ")
//     + s" ${new DecimalFormat("##.##%").format(rate(maxIndex))}")
  }
}
