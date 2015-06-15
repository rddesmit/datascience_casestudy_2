import scala.io.Source
import scala.util.Random

/**
 * Created by Rudie on 13-6-2015.
 */
object PartTwo extends App with GeneticAlgorithm[Seq[Double]] with scalax.chart.module.Charting {

  val data = Source.fromFile(getClass.getResource("pregnancy.csv").toURI).getLines().toList.map(line =>
    line.split(";").map(_.toDouble).toSeq
  )

  override def createIndividual(): Seq[Double] = List.fill(20)((Random.nextDouble() * 2) - 1)

  /** Ranked selection */
  override def selectTwoParents(population: Seq[(Seq[Double], Double)]): () => (Seq[Double], Seq[Double]) = {
    val rankedPopulation = population.sortWith(_._2 < _._2).zipWithIndex.map(x => x._1._1 -> x._2)

    // select a random value between 0 and the total fitness of the population
    def nextRandom = {
      val totalFitness = rankedPopulation.map(_._2).sum
      Random.nextDouble() * totalFitness
    }

    // loop through the list until the sum of the previous fitness's is >= than the random value
    def select(sum: Double, random: Double, population: Seq[(Seq[Double], Int)]): Seq[Double] = {
      if ((sum + population.head._2) >= random) population.head._1
      else select(sum + population.head._2, random, population.tail)
    }

    () => select(0, nextRandom, rankedPopulation) -> select(0, nextRandom, rankedPopulation)
  }

  /** Uniform crossover */
  override def crossover(parents: (Seq[Double], Seq[Double]), chance: Double): (Seq[Double], Seq[Double]) = {
    def getCrossover(parentOne: Seq[Double], parentTwo: Seq[Double]) = {
      parentOne.zip(parentTwo).map {
        case (one, two) => if (Random nextBoolean) one else two
      }
    }

    if (chance > (Random nextDouble)) getCrossover(parents._1, parents._2) -> getCrossover(parents._2, parents._1)
    else parents
  }

  override def mutation(individual: Seq[Double], chance: Double): Seq[Double] = {
    def getMutation = (Random.nextDouble() * 2) - 1
    individual.map(x => if (chance > (Random nextDouble)) getMutation else x)
  }

  override def computeFitness(individual: Seq[Double]): Double = {
    // compute the sse for a given individual
    def sse(individual: Seq[Double]) = data.par.map { line =>
      //compute the prediction(sum product) for each entry in the data set
      line.last -> (0.0 /: line.init.zip(individual))((r, c) => r + (c._1 * c._2))
    }.map {
      //compute the se for each entry in the data set
      case (pregnant, prediction) => Math.pow(pregnant - prediction, 2)
    }.sum

    1 / sse(individual)
  }

  XYLineChart(Range.apply(0, 10, 1).par.map { i =>
    val result = run(crossoverRate = 0.95, mutationRate = 0.0125, elitism = true, populationSize = 75, numGenerations = 250).map(1 / computeFitness(_))
    i -> result.sortWith(_ < _).head
  }).show("Best fitness")
}
