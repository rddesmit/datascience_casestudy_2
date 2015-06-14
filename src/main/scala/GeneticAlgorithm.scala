import scala.util.Random

/**
 * Created by Rudie on 13-6-2015.
 */
trait GeneticAlgorithm[T] {

  def createIndividual(): T

  def computeFitness(individual: T): Double

  def selectTwoParents(population: Seq[(T, Double)]): () => (T, T) = {
    val totalFitness = population.map(_._2).sum

    def nextRandom(): Double = {
      Random.nextDouble() * totalFitness
    }

    def select(sum: Double, random: Double, population: Seq[(T, Double)]): T = {
      if ((sum + population.head._2) >= random) population.head._1
      else select(sum + population.head._2, random, population.tail)
    }

    () => (select(0, nextRandom(), population), select(0, nextRandom(), population))
  }

  def crossover(parents: (T, T), chance: Double): (T, T)

  def mutation(individual: T, chance: Double): T

  def run(crossoverRate: Double, mutationRate: Double, elitism: Boolean, populationSize: Int, numGenerations: Int) =
    GeneticAlgorithmSolver(crossoverRate, mutationRate, elitism, populationSize, numGenerations)
      .run(createIndividual, computeFitness, selectTwoParents, crossover, mutation)
}
