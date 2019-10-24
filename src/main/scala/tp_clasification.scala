import org.apache.spark
import org.apache.spark.SparkContext
import org.apache.spark.ml.regression.LinearRegression
import org.apache.spark.sql.{SQLContext, SparkSession}
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.feature.Normalizer
import org.apache.spark.mllib.regression.LinearRegressionWithSGD
object tp_clasification {
  //val training = spark.read.format("libsvm").load("data/mllib/sample_linear_regression_data.txt")
  def main(args: Array[String]): Unit = {

    //  val v=DenseMatrix(("hhhhhh","ggggg"),(4,5))
    // New 2.0.+ API: create SparkSession and use it for all purposes:
    val session = SparkSession.builder().appName("test").master("local").getOrCreate()
    val x=  session.read.load("D:/tp_regression/test.csv") // OK

   // val people = sqlContext.read.parquet("...")  // in Scala

  //  DataFrame people = sqlContext.read().parquet("...")  // in Java

    /* val lr = new LinearRegression()
       .setMaxIter(10)
       .setRegParam(0.3)
       .setElasticNetParam(0.8)*/

    // Fit the model
  //  val lrModel = lr.fit(x)

   // println(lrModel)

    /*// Old <= 1.6.* API: create SparkContext, then create a SQLContext for DataFrame API usage:
     val sc = new SparkContext("local", "test") // used for RDD operations only
     val sqlContext = new SQLContext(sc) // used for DataFrame / DataSet APIs
     sqlContext.read.load("/file") // OK
     //sc.read.load("/file") // NOT OK*/

  }

}
