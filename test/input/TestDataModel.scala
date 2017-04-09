package input

import java.io.File
import java.text.DecimalFormat

import org.scalatest.FunSuite

/**
  * Created by sparr on 2017/4/7.
  */
class TestDataModel extends FunSuite{

  val file = new File("resource/test.csv")
  println(file.getAbsoluteFile)
  val dataModel: DataModel = DataSource.fromCSV(file)
  dataModel.labelIndex = 0
  println(s"labelName = ${dataModel.labelName}")

  val file1 = new File("resource/buyPC.csv")
  val buyPcModel: DataModel = DataSource.fromCSV(file1)
  buyPcModel.labelName = "buy"
  println(s"labelName = ${buyPcModel.labelName}")

  test("getConditionProbability should work well"){
    val rate = dataModel.getDispersedConditionProbability("男",3,"12")
    println(s"rate = $rate")
    assert(rate==0.5)
    val rate1 = dataModel.getDispersedConditionProbability("女",3,"12")
    println(s"rate1 = $rate1")
  }


  test("getColumnValueWithLabel"){
    val values = dataModel.getColumnValuesWithLabel(1,"男")
    println(values)
    assert(values.length==4)
  }

  test("probability"){
//    println(dataModel.normality(2,0,2) )
//    println(1/Math.sqrt(2*Math.PI*2))
    assert(dataModel.normality(2,2,2) == 1/Math.sqrt(2*Math.PI*2))

  }

  test("in pc model getConditionProbability should work well"){
    val p_age_youth_buy_yes = buyPcModel.getDispersedConditionProbability("age","youth","yes")
    println(new DecimalFormat("0.00").format(p_age_youth_buy_yes))
    assert(new DecimalFormat("0.00").format(p_age_youth_buy_yes)=="0.22")

    val p_age_youth_buy_no = buyPcModel.getDispersedConditionProbability("age","youth","no")
    println(new DecimalFormat("0.00").format(p_age_youth_buy_no))
    assert(new DecimalFormat("0.00").format(p_age_youth_buy_no)=="0.60")

    val p_income_buy_yes = buyPcModel.getDispersedConditionProbability("income","medium","yes")
    println(new DecimalFormat("0.00").format(p_income_buy_yes))
    assert(new DecimalFormat("0.00").format(p_income_buy_yes)=="0.44")

    val p_income_buy_no = buyPcModel.getDispersedConditionProbability("income","medium","no")
    println(new DecimalFormat("0.00").format(p_income_buy_no))
    assert(new DecimalFormat("0.00").format(p_income_buy_no)=="0.40")

    val p_stu_buy_yes = buyPcModel.getDispersedConditionProbability("student","yes","yes")
    println(new DecimalFormat("0.00").format(p_stu_buy_yes))
    assert(new DecimalFormat("0.00").format(p_stu_buy_yes)=="0.67")

    val p_stu_no_buy_no = buyPcModel.getDispersedConditionProbability("student","yes","no")
    println(new DecimalFormat("0.00").format(p_stu_no_buy_no))
    assert(new DecimalFormat("0.00").format(p_stu_no_buy_no)=="0.20")

    val cre_fair_buy_yes =  buyPcModel.getDispersedConditionProbability("credit_rating","fair","yes")
    println(new DecimalFormat("0.00").format(cre_fair_buy_yes))
    assert(new DecimalFormat("0.00").format(cre_fair_buy_yes)=="0.67")

    val cre_fair_buy_no =  buyPcModel.getDispersedConditionProbability("credit_rating","fair","no")
    println(new DecimalFormat("0.00").format(cre_fair_buy_no))
    assert(new DecimalFormat("0.00").format(cre_fair_buy_no)=="0.40")

    val p_buy_yes = buyPcModel.countAttribute("buy","yes")/buyPcModel.totalCount
    println(s"buy_yes = $p_buy_yes")
    assert(new DecimalFormat("0.000").format(p_buy_yes)=="0.643")
    val p_buy_no = buyPcModel.countAttribute("buy","no")/buyPcModel.totalCount
    println(s"buy_no = $p_buy_no")
    assert(new DecimalFormat("0.000").format(p_buy_no)=="0.357")

  }

  test("all different dispersed values"){
    val stuDispersed = buyPcModel.getAllDispersedValues("student")
    println(stuDispersed.mkString(","))
    assert(stuDispersed(0)=="no" && stuDispersed(1)=="yes")

    val labelDispersed = buyPcModel.labelValues()
    println(labelDispersed.mkString(","))
    assert(labelDispersed(0)=="no" && labelDispersed(1)=="yes")
  }

  test("正态分布"){
    val avg = 5.855
    val variance = 0.035
    val result = dataModel.normality(6,avg,variance)
    println(result)
  }

}
