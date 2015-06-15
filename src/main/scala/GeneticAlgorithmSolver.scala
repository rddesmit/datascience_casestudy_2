

/**
 * Created by Rudie on 26-5-2015.
 */
class GeneticAlgorithmSolver(crossoverRate: Double, mutationRate: Double, elitism: Boolean, populationSize: Int, numGenerations: Int) {

  def run[Ind](createIndividual: () => Ind,
               computeFitness: Ind => Double,
               selectTwoParents: Seq[(Ind, Double)] => () => (Ind, Ind),
               crossover: ((Ind, Ind), Double) => (Ind, Ind),
               mutation: (Ind, Double) => Ind) = {

    //get fitnesses
    def getFitnesses(population: Seq[Ind]) = {
      population.map(individual => (individual, computeFitness(individual)))
    }

    //get elitism
    def getElitism(populationWithFitnesses: Seq[(Ind, Double)]): Option[Ind] = {
      if(elitism) Some(populationWithFitnesses.sortWith(_._2 > _._2).head._1)
      else None
    }

    //get offspring with change of crossover and mutation
    def getOffspring(parents: (Ind, Ind)): Seq[Ind] = {
      val offspring = crossover(parents, crossoverRate)
      Seq(mutation(offspring._1, mutationRate), mutation(offspring._2, mutationRate))
    }

    def computeGeneration(generation: Int, population: Seq[Ind]): Seq[Ind] = {
      if(generation <= 0) population
      else {
        //calcualte fitnesses
        val populationWithFitnesses = getFitnesses(population)

        //get the offspring
        val getParents = selectTwoParents(populationWithFitnesses)
        val offsprings = List.fill(populationSize / 2)(getParents()).flatMap(getOffspring)

        //apply elitism
        val nextGeneration = getElitism(populationWithFitnesses) match {
          case Some(individual) => individual +: offsprings
          case None => offsprings
        }

        computeGeneration(generation - 1, nextGeneration.take(populationSize))
      }
    }

    computeGeneration(numGenerations, List.fill(populationSize)(createIndividual()))
  }
}

object GeneticAlgorithmSolver {
  def apply(crossoverRate: Double, mutationRate: Double, elitism: Boolean, populationSize: Int, numGenerations: Int) =
    new GeneticAlgorithmSolver(crossoverRate, mutationRate, elitism, populationSize, numGenerations)
}