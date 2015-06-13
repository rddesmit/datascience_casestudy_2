import scala.util.Random

/**
 * Created by Rudie on 26-5-2015.
 */
object Main extends App{

  def createIndividual(): Seq[Int] = {
    (0 until 8).map(_ => if(Random.nextBoolean()) 0 else 1)
  }

  def computeFitness(individual: Seq[Int]): Double = {
    individual.sum
  }

  def selectTwoParents[T](population: Seq[(T, Double)]): () => (T, T) = {
    def nextRandom(): Double = {
      val totalFitness = population.map(_._2).sum
      Random.nextDouble() * totalFitness
    }

    def select(sum: Double, random: Double, population: Seq[(T, Double)]): T = {
      if((sum + population.head._2) >= random) population.head._1
      else select(sum + population.head._2, random, population.tail)
    }

    () => (select(0, nextRandom(), population), select(0, nextRandom(), population))
  }

  def crossover(parents: (Seq[Int], Seq[Int]), chance: Double): (Seq[Int], Seq[Int]) = {
    def getCrossover = {
      val splitIndex = Random nextInt 8

      val splittedParentOne = parents._1.splitAt(splitIndex)
      val splittedParentTwo = parents._2.splitAt(splitIndex)

      (splittedParentOne._1 ++ splittedParentTwo._2, splittedParentTwo._1 ++ splittedParentOne._2)
    }

    if(chance < (Random nextDouble)) getCrossover else parents
  }

  def mutation(individual: Seq[Int], chance: Double): Seq[Int] = {
    def getMutation = {
      val mutationIndex = Random nextInt 8

      val splittedIndividual = individual.splitAt(mutationIndex)
      val mutatedIndex = if (splittedIndividual._2.head == 0) 1 else 0
      (splittedIndividual._1 ++ (mutatedIndex +: splittedIndividual._2)).take(8)
    }

    if(chance < (Random nextDouble)) getMutation else individual
  }

  GeneticAlgorithm(0.8, 0.1, elitism = true, 6, 100).Run[Seq[Int]](createIndividual, computeFitness, selectTwoParents, crossover, mutation).foreach(println)
}
