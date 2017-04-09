package algorithm

import java.io.File

import input.{DataModel, DataSource}
import org.scalatest.FunSuite


class TestBayesAlgorithm extends FunSuite{

  val file1 = new File("resource/buyPC.csv")
  val buyPcModel: DataModel = DataSource.fromCSV(file1)
  buyPcModel.labelName = "buy"

  val input = Array("youth","medium","yes","fair")
  val bayesAlgorithm = new BayesAlgorithm(buyPcModel,input)
  test("classify"){
    bayesAlgorithm.classify()
  }
}
