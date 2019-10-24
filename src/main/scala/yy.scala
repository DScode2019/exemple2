import java.util.Random

import breeze.linalg
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

object yy {
  def main(args: Array[String]): Unit = {
 //   val mat = DenseMatrix((5,5,4,0,1,1), (4,3,3,2,2,1),(2,1,2,3,2,2), (5,3,5,3,4,3),(4,4,3,2,3,2),(2,0,1,3,1,1),(3,3,4,2,4,4)
    //  ,(1,2,1,4,3,3),(0,1,0,3,1,0),(2,0,1,3,1,0),(1,2,1,1,0,1),(4,2,4,2,1,2),(3,2,3,3,2,3),(1,0,0,3,2,2),(2,1,1,2,3,2))


    val mat =DenseMatrix((2.5,-3.0,0.5),(-3.0,5.0,-2.0),(0.5,-2.0,1.5))
   // val coef_cor=corrcoeff(convert(mat,Double))
    val es=eigSym(mat)
    val val_propre=es.eigenvalues
    print("rrrrrrr"+val_propre)


    val vect_propre=es.eigenvectors
    val mat_double=convert(mat,Double)
    val moyen=mean(mat_double(::,*))
    val ecart=stddev(mat_double(::,*))
   // val ligne = DenseVector("I1","I2","I3","I4","I5","I6","I7","I8","I9","I10","I11","I12","I13","I14","I15")

    val test=sum(vect_propre)

       println(test)
    val centre= mat_double(*,::)- moyen.inner
    val reduite=centre(*,::)/ecart.inner

 //   println(reduite)
//
    val composant_principale=reduite*vect_propre


    //graphe 1
    val f2 = Figure("pca")
    val p3 = f2.subplot(1,1,0)
    p3 += scatter(composant_principale(::, 5), composant_principale(::, 4), { _ => 0.1})
  //  p3.setXAxisDecimalTickUnits()


   val ligne = DenseVector("I1","I2","I3","I4","I5","I6","I7","I8","I9","I10","I11","I12","I13","I14","I15")
    val dataset1: XYSeriesCollection = new XYSeriesCollection
    var series3: XYSeries=null //la série prend chaque foi des valeurs différentes
    for(i<-1 until ligne.length) { // Parcourir tous les labels lignes pour créer les 15 séries(points)
      series3 = new XYSeries(ligne(i))  //Créer la série sous le nom de l’individu, il existe 15 séries
      // Ajouter les cordonnées des points qui forment le même groupe à la série. Dans notre cas, un point forme un seul groupe.
      series3.add(composant_principale(i, 5), composant_principale(i, 4))
      //Ajouter toutes les séries à l’ensemble de collections des données.
      dataset1.addSeries(series3)
    }
    // Même principe que celui du barChart
    val frame1=new ChartFrame("Histogramme lambda", ChartFactory.createScatterPlot("Histogramme Lambda test ","lambda yy", "lambda", dataset1,PlotOrientation.VERTICAL, true,false,false))
    //frame1.add(ChartFactory.createScatterPlot("Histogramme Lambda test ","lambda yy", "lambda", dataset1,PlotOrientation.VERTICAL, true,false,false))
    frame1.pack()
    frame1.setVisible(true)


    val contribution:DenseMatrix[Double]= new DenseMatrix(composant_principale.rows,composant_principale.cols)
    val puiss =composant_principale:^ 2.0
    for(i<-0 until composant_principale.cols) {
      contribution(::,i) := puiss(::,i)/(composant_principale.rows*val_propre(i))
    }
   // println(contribution(0,4))


val matr:DenseMatrix[Double]=composant_principale:^2.0
    val somme=sum(matr(*,::))
val qual=matr(*,::)/somme


    val qualite:DenseMatrix[Double]= new DenseMatrix(composant_principale.rows,composant_principale.cols)
    for(i<-0 until composant_principale.rows) {
      qualite(i,::) := puiss(i,::)/sum(puiss(i,::)).toDouble
    }


    var tt: DenseMatrix[Double]=reduite.t * composant_principale
    for(i<-0 until composant_principale.cols) {
  val x = tt(::,i)/1.0*sqrt(val_propre(i))
    }
    println("hhhhhhh" + tt)


    val comp_variable: DenseMatrix[Double] = new DenseMatrix(composant_principale.cols, composant_principale.cols)

    for(i<-0 until composant_principale.cols) {
    //  comp_variable(i,::):=matrice(i,::)/sqrt(val_propre(i))
    }


    //les  composants principales
    val n_v = DenseMatrix.zeros[Double](mat.cols,mat.cols)
    val matrice=  reduite.t  *     composant_principale

    for(i<-0 until composant_principale.cols) {
      //n_v :=coef_cor/ sqrt(val_propre(i))
    }
    println(n_v(::,5))

   val puiss2=n_v:^2.0
    val comp = DenseMatrix.zeros[Double](mat.cols,mat.cols)
    for(i<-0 until composant_principale.cols) {
     comp(::,i):=puiss2(::,i)/val_propre(i).toDouble


    }

    val cont_v = DenseMatrix.zeros[Double](mat.cols,mat.cols)

     val puiss22=n_v:^2.0

   val ii = puiss22(*,::)/val_propre



    println("hhhhhhh" + ii)
}



}
