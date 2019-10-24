package linalg.matrix

import java.util.Random

import breeze.linalg.{DenseMatrix, _}
import breeze.numerics._
import breeze.stats.mean
import breeze.stats.stddev
import breeze.stats.corrcoeff
import breeze.stats.variance
import breeze.linalg._
import eigSym.EigSym
import breeze.plot.{hist, scatter, _}
import breeze.stats.hist.Histogram
import com.lowagie.text.Image
import com.sun.prism.paint.Color
import org.apache.commons.math3.stat.correlation.Covariance
import org.jfree.data.statistics.HistogramDataset
import org.jfree.chart.{ChartFactory, ChartFrame}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
object demo {

  def main(args: Array[String]) {






    val mat = DenseMatrix((5.0,5.0,4.0,0.0,1.0,1.0), (4.0,3.0,3.0,2.0,2.0,1.0),(2.0,1.0,2.0,3.0,2.0,2.0), (5.0,3.0,5.0,3.0,4.0,3.0)
      ,(4.0,4.0,3.0,2.0,3.0,2.0),(2.0,0.0,1.0,3.0,1.0,1.0),
      (3.0,3.0,4.0,2.0,4.0,4.0),(1.0,2.0,1.0,4.0,3.0,3.0),(0.0,1.0,0.0,3.0,1.0,0.0),(2.0,0.0,1.0,3.0,1.0,0.0)
      ,(1.0,2.0,1.0,1.0,0.0,1.0),(4.0,2.0,4.0,2.0,1.0,2.0),(3.0,2.0,3.0,3.0,2.0,3.0)
      ,(1.0,0.0,0.0,3.0,2.0,2.0),(2.0,1.0,1.0,2.0,3.0,2.0))

    val lambda_s = DenseVector("lembda 1","lembda 2","lembda 3","lembda 4","lembda 5","lembda 6")

    val ligne = DenseVector("I1","I2","I3","I4","I5","I6","I7","I8","I9","I10","I11","I12","I13","I14","I15")
    val colone = DenseVector("CUB","PUZ","CAL","MEM","COM","VOC")

    //matrice de corelation
    val coeffmat=corrcoeff(mat)
    // println(corrcoeff(mat))

    //les valeur propre
    // val EigSym(lambda, evs) = eigSym(mat)
    val es = eigSym(coeffmat)
    val lambda = es.eigenvalues
    println("Les valeur Propre "+lambda)

    //val percent=(lambda/sum(lambda))
    val percent:DenseVector[Double] = (lambda*100.0 / sum(lambda))
    println("Les pourcentage  "+percent)
    //val percent_cmule:DenseVector[Double] = ;
    val percent_cmule:DenseVector[Double]= DenseVector.zeros[Double](percent.length);

    percent_cmule(0)=percent(0)
    for(i<-1 until percent.length) {
      percent_cmule(i)=  percent(i)+percent_cmule(i-1);
    }
    // les vecteur propre
    println("Les pourcentage cumulÃ© " +  percent_cmule)


    /* val f = Figure()
     val p2 = f.subplot(0)
     p2 += hist(lambda,100)

     p2.setYAxisDecimalTickUnits()
     p2.setYAxisDecimalTickUnits()

     p2.ylim(0,4)
     p2.xlim(0,4)
     p2.title = "A normal distribution"
     p2.xlabel="rr"*/

    val dataset= new DefaultCategoryDataset()

    for(i<-0 until lambda.length) {
      println(ligne(i))
      dataset.setValue(lambda(i), "",lambda_s(i) )
    }

    val frame=new ChartFrame("Histogramme lambda",
      ChartFactory.createBarChart("Histogramme Lambda test ","lambda yy",
        "lambda", dataset,PlotOrientation.VERTICAL,
        false,true,false))
    frame.pack()
    frame.setVisible(true)




    // ma trice centrer et reduite
    var moy = DenseMatrix.zeros[Double](mat.rows, mat.cols)
    val centrer_reduite = DenseMatrix.zeros[Double](mat.rows,mat.cols)
    for(i<-0 until mat.cols) {
      val moy1 = mean(mat(::, i))
      println("mprrr"+moy1)
      moy(::, i) := mat(::, i) - moy1  //ecart
      val ecart:Double=sqrt(sum(moy(::,i) :^ 2.0)/mat.rows.toDouble)
      centrer_reduite(::, i):= moy(::, i)/ecart
    }
    // println(centrer_reduite)

    //println(moy)




    //les  composants principales
    val evs = es.eigenvectors
    val n = centrer_reduite * evs
     println(n)


    //les contribution des individue
    val contribution = DenseMatrix.zeros[Double](mat.rows,mat.cols)
    val puiss = DenseMatrix.zeros[Double](mat.rows,mat.cols)

    puiss :=n:^ 2.0

    for(i<-0 until n.cols) {
      contribution(::,i) := puiss(::,i)/(n.rows*lambda(i))*100.0
    }
    println(contribution(0,4))



    //les qualite des individue
    val cosin = DenseMatrix.zeros[Double](mat.rows,mat.cols)
    for(i<-0 until n.rows) {
      cosin(i,::) := puiss(i,::)/(sum(puiss(i,::)))
    }
    println("qualite "+cosin)




    //graphe 1
    val f2 = Figure("pca")
    val p3 = f2.subplot(0)
    p3 += scatter(n(::, 5), n(::, 4), { _ => 0.1})
    p3.setXAxisDecimalTickUnits()


    //les  composants principales
    val n_v = DenseMatrix.zeros[Double](mat.cols,mat.cols)
    for(i<-0 until n.cols) {
      n_v := centrer_reduite.t * n / (mat.rows.toDouble * sqrt(lambda(i)))
    }
    //println(n_v(::,5))

    // contribution
    val puiss2 = DenseMatrix.zeros[Double](mat.cols,mat.cols)

    val cont_v = DenseMatrix.zeros[Double](mat.cols,mat.cols)
    for(i<-0 until n.cols) {
      puiss2(::,i):=n_v(::,i):^2.0

      cont_v(::,i) := puiss2(::,i)/lambda(i).toDouble
    }


    println("jjjjjj "+cont_v(::,5))


    //les qualite des individue
    val cosin_v= DenseMatrix.zeros[Double](mat.cols,mat.cols)
    val puiss3= DenseMatrix.zeros[Double](mat.cols,mat.cols)

    for(i<-0 until n_v.rows) {
      puiss3(::,i) :=n_v(::,i) :^ 2.0
      //   cosin_v(i,::) := puiss3(i,::)/(sum(puiss3(i,::)))
    }
    println(puiss3(::,5))

    val mat_c = DenseMatrix((0.0,0.0))
    //graphe 1
    val f3 = Figure("pca correlation")
    val p4 = f3.subplot(0)
    p4 += scatter(n_v(::, 5), n_v(::, 4), { _ => 0.03})

    p4 += scatter(mat_c(::,0), mat_c(::,1), { _=> 2.0})

    // p4.xlabel_=(ligne)
    //setsetYAxisDecimalTickUnits()
    p4.ylim(-1,1)
    p4.xlim(-1,1)

    val y = DenseVector.rand(20)
    val x = linspace(1, 10, 20)

    val fig = Figure("scatter plot")
    val plt = fig.subplot(0)
    plt += plot(n_v(::, 5), n_v(::, 4), '+', "blue",ligne(0))



    ///

    println(sum(lambda))



    val dataset1: XYSeriesCollection = new XYSeriesCollection


    var series3: XYSeries=null
    for(i<-1 until ligne.length) {
      series3 = new XYSeries(ligne(i))
      series3.add(n(i, 5), n(i, 4))
      dataset1.addSeries(series3)

    }

    val frame1=new ChartFrame("Histogramme lambda",
      ChartFactory.createScatterPlot("Histogramme Lambda test ","lambda yy",
        "lambda", dataset1,PlotOrientation.VERTICAL,
        true,true,true))
    frame1.pack()
    frame1.setVisible(true)
  }}




