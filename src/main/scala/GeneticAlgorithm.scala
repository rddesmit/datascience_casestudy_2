

/**
 * Created by Rudie on 13-6-2015.
 */
trait GeneticAlgorithm[Ind] {

  def createIndividual(): Ind

  def computeFitness(individual: Ind): Double

  def selectTwoParents(population: Seq[(Ind, Double)]): () => (Ind, Ind)

  def crossover(parents: (Ind, Ind), chance: Double): (Ind, Ind)

  def mutation(individual: Ind, chance: Double): Ind

  def run(crossoverRate: Double, mutationRate: Double, elitism: Boolean, populationSize: Int, numGenerations: Int) =
    GeneticAlgorithmSolver(crossoverRate, mutationRate, elitism, populationSize, numGenerations)
      .run(createIndividual, computeFitness, selectTwoParents, crossover, mutation)
}
