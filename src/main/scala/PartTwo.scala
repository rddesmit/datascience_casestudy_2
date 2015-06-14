import scala.io.Source
import scala.util.Random

/**
 * Created by Rudie on 13-6-2015.
 */
object PartTwo extends App with GeneticAlgorithm[Seq[Double]] {

  val data = Source.fromFile(getClass.getResource("pregnancy.csv").toURI).getLines().toList.map(line =>
    line.split(";").map(_.toDouble).toSeq
  ).toSeq

  override def createIndividual(): Seq[Double] = (0 until 20).map(_ => (Random.nextDouble() * 2) - 1)

  override def crossover(parents: (Seq[Double], Seq[Double]), chance: Double): (Seq[Double], Seq[Double]) = {
    def getCrossover = {
      val splitIndex = Random nextInt 20

      val splittedParentOne = parents._1.splitAt(splitIndex)
      val splittedParentTwo = parents._2.splitAt(splitIndex)

      (splittedParentOne._1 ++ splittedParentTwo._2, splittedParentTwo._1 ++ splittedParentOne._2)
    }

    if (chance > (Random nextDouble)) getCrossover else parents
  }

  override def mutation(individual: Seq[Double], chance: Double): Seq[Double] = {
    def getMutation = {
      val mutationIndex = Random nextInt 20

      val splittedIndividual = individual.splitAt(mutationIndex)
      val mutatedValue = (Random.nextDouble() * 2) - 1
      (splittedIndividual._1 ++ (mutatedValue +: splittedIndividual._2)).take(20)
    }

    if (chance > (Random nextDouble)) getMutation else individual
  }

  override def computeFitness(individual: Seq[Double]): Double = {
    def sse(predictions: Seq[Double]) = {
      val pregnents = data.map(_.last).toSeq
      (0.0 /: pregnents.zip(predictions))((r, c) => r + Math.pow(c._1 - c._2, 2))
    }

    def predictions(indivudual: Seq[Double]) = {
      data.map { line =>
        (0.0 /: line.zip(individual))((r, c) => r + (c._1 * c._2))
      }
    }

    1 / sse(predictions(individual))
  }

  run(1, 0, elitism = false, 20, 1000).foreach(println)
}
