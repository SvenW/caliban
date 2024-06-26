package caliban.validation

import caliban._
import caliban.execution.NestedZQueryBenchmarkSchema
import caliban.introspection.Introspector
import caliban.parsing.{ Parser, VariablesCoercer }
import caliban.schema.RootType
import org.openjdk.jmh.annotations.{ Scope, _ }
import zio._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ValidationBenchmark {

  private val runtime = Runtime.default

  def run[A](zio: Task[A]): A                                      = Unsafe.unsafe(implicit u => runtime.unsafe.run(zio).getOrThrow())
  def toSchema[R](graphQL: GraphQL[R]): IO[CalibanError, RootType] =
    graphQL.validateRootSchema.map { schema =>
      RootType(
        schema.query.opType,
        schema.mutation.map(_.opType),
        schema.subscription.map(_.opType)
      )
    }

  import NestedZQueryBenchmarkSchema._

  val parsedSimpleQuery        = run(Parser.parseQuery(simpleQuery))
  val parsedMultifieldQuery    = run(Parser.parseQuery(multifieldQuery))
  val parsedDeepQuery          = run(Parser.parseQuery(deepQuery))
  val parsedDeepWithArgsQuery  = run(Parser.parseQuery(deepWithArgsQuery))
  val parsedIntrospectionQuery = run(Parser.parseQuery(ComplexQueryBenchmark.fullIntrospectionQuery))

  val simpleType = run(
    toSchema(graphQL[Any, SimpleRoot, Unit, Unit](RootResolver(NestedZQueryBenchmarkSchema.simple100Elements)))
  )

  val multifieldType =
    run(
      toSchema(
        graphQL[Any, MultifieldRoot, Unit, Unit](
          RootResolver(NestedZQueryBenchmarkSchema.multifield100Elements)
        )
      )
    )

  val deepType =
    run(
      toSchema(
        graphQL[Any, DeepRoot, Unit, Unit](
          RootResolver[DeepRoot](NestedZQueryBenchmarkSchema.deep100Elements)
        )
      )
    )

  val deepWithArgsType =
    run(
      toSchema(
        graphQL[Any, DeepWithArgsRoot, Unit, Unit](
          RootResolver[DeepWithArgsRoot](NestedZQueryBenchmarkSchema.deepWithArgs100Elements)
        )
      )
    )

  @Benchmark
  def simple(): Any = {
    val io = Validator.validate(parsedSimpleQuery, simpleType)
    run(io)
  }

  @Benchmark
  def multifield(): Any = {
    val io = Validator.validate(parsedMultifieldQuery, multifieldType)
    run(io)
  }

  @Benchmark
  def deep(): Any = {
    val io = Validator.validate(parsedDeepQuery, deepType)
    run(io)
  }

  @Benchmark
  def variableCoercer(): Any = {
    val io = VariablesCoercer.coerceVariables(deepArgs100Elements, parsedDeepWithArgsQuery, deepWithArgsType, false)
    run(io)
  }

  @Benchmark
  def introspection(): Any = {
    val io =
      Validator.validate(parsedIntrospectionQuery, Introspector.introspectionRootType)
    run(io)
  }

}
