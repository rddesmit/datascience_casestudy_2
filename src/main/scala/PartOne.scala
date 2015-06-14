import scala.util.Random

/**
 * Created by Rudie on 26-5-2015.
 */
object PartOne extends App with GeneticAlgorithm[Seq[Int]] {

  override def createIndividual(): Seq[Int] = (0 until 20).map(_ => if (Random.nextBoolean()) 0 else 1)

  override def computeFitness(individual: Seq[Int]): Double = {
    individual.sum
  }

  override def crossover(parents: (Seq[Int], Seq[Int]), chance: Double): (Seq[Int], Seq[Int]) = {
    def getCrossover = {
      val splitIndex = Random nextInt 20

      val splittedParentOne = parents._1.splitAt(splitIndex)
      val splittedParentTwo = parents._2.splitAt(splitIndex)

      (splittedParentOne._1 ++ splittedParentTwo._2, splittedParentTwo._1 ++ splittedParentOne._2)
    }

    if (chance > (Random nextDouble)) getCrossover else parents
  }

  override def mutation(individual: Seq[Int], chance: Double): Seq[Int] = {
    def getMutation = {
      val mutationIndex = Random nextInt 20

      val splittedIndividual = individual.splitAt(mutationIndex)
      val mutatedValue = if (splittedIndividual._2.head == 0) 1 else 0
      (splittedIndividual._1 ++ (mutatedValue +: splittedIndividual._2)).take(20)
    }

    if (chance > (Random nextDouble)) getMutation else individual
  }

  run(0.8, 0.01, elitism = false, 20, 1000).foreach(result => println(computeFitness(result)))
}
