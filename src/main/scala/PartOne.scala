import scala.util.Random

/**
 * Created by Rudie on 26-5-2015.
 */
object PartOne extends App with GeneticAlgorithm[Seq[Int]] with scalax.chart.module.Charting {

  override def createIndividual(): Seq[Int] = List.fill(20)(Random nextInt 2)

  override def computeFitness(individual: Seq[Int]): Double = individual.sum

  /** Roulette wheel selection */
  override def selectTwoParents(population: Seq[(Seq[Int], Double)]): () => (Seq[Int], Seq[Int]) = {

    // select a random value between 0 and the total fitness of the population
    def nextRandom = {
      val totalFitness = population.map(_._2).sum
      Random.nextDouble() * totalFitness
    }

    // loop through the list until the sum of the previous fitness's is >= than the random value
    def select(sum: Double, random: Double, population: Seq[(Seq[Int], Double)]): Seq[Int] = {
      if ((sum + population.head._2) >= random) population.head._1
      else select(sum + population.head._2, random, population.tail)
    }

    () => select(0, nextRandom, population) -> select(0, nextRandom, population)
  }

  /** Singe point crossover */
  override def crossover(parents: (Seq[Int], Seq[Int]), chance: Double): (Seq[Int], Seq[Int]) = {
    def getCrossover = {
      val splitIndex = Random nextInt 20

      val splittedParentOne = parents._1.splitAt(splitIndex)
      val splittedParentTwo = parents._2.splitAt(splitIndex)

      (splittedParentOne._1 ++ splittedParentTwo._2) -> (splittedParentTwo._1 ++ splittedParentOne._2)
    }

    if (chance > (Random nextDouble)) getCrossover
    else parents
  }

  override def mutation(individual: Seq[Int], chance: Double): Seq[Int] = {
    def getMutation = Random nextInt 2
    individual.map(x => if (chance > (Random nextDouble)) getMutation else x)
  }


  XYLineChart(Range.apply(0, 50, 1).par.map { i =>
    val result = run(crossoverRate = 0.85, mutationRate = 0.01, elitism = true, populationSize = 20, numGenerations = 100)
    i -> result.map(computeFitness).sum / result.size
  }).show("Average fitness")
}
