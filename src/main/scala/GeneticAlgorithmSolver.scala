

/**
 * Created by Rudie on 26-5-2015.
 */
class GeneticAlgorithmSolver(crossoverRate: Double, mutationRate: Double, elitism: Boolean, populationSize: Int, numGenerations: Int) {

  def run[T](createIndividual: () => T,
              computeFitness: T => Double,
              selectTwoParents: Seq[(T, Double)] => () => (T, T),
              crossover: ((T, T), Double) => (T, T),
              mutation: (T, Double) => T) = {

    //get fitnesses
    def getFitnesses(population: Seq[T]) = {
      population.map(individual => (individual, computeFitness(individual)))
    }

    //get elitism
    def getElitism(populationWithFitnesses: Seq[(T, Double)]): Option[T] = {
      if(elitism) Some(populationWithFitnesses.sortWith(_._2 > _._2).head._1)
      else None
    }

    //get offspring with change of crossover and mutation
    def getOffspring(parents: (T, T)) : Seq[T]= {
      val offspring = crossover(parents, crossoverRate)
      Seq(mutation(offspring._1, mutationRate), mutation(offspring._2, mutationRate))
    }

    def computeGeneration(generation: Int, population: Seq[T]): Seq[T] = {
      if(generation <= 0) population
      else {
        //calcualte fitnesses
        val populationWithFitnesses = getFitnesses(population)

        //get the offspring
        val getParents = selectTwoParents(populationWithFitnesses)
        val offsprings = (0 until populationSize)
          .map(_ => getParents())
          .flatMap(getOffspring)

        //apply elitism
        val nextGeneration = getElitism(populationWithFitnesses) match {
          case Some(individual) => individual +: offsprings
          case None => offsprings
        }

        computeGeneration(generation - 1, nextGeneration.take(populationSize))
      }
    }

    computeGeneration(numGenerations, (0 until populationSize).map(_ => createIndividual()))
  }
}

object GeneticAlgorithmSolver {
  def apply(crossoverRate: Double, mutationRate: Double, elitism: Boolean, populationSize: Int, numGenerations: Int) =
    new GeneticAlgorithmSolver(crossoverRate, mutationRate, elitism, populationSize, numGenerations)
}