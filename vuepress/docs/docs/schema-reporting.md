# Schema Reporting

The `caliban-reporting` module allows you to integrate with Apollo's [schema reporting protocol](https://github.com/apollographql/apollo-schema-reporting-preview-docs/blob/master/schema-reporting-protocol.md).
This enables your servers to automatically publish updated schemas on start up without involving any additional tooling.

You can enable the settings by providing the `ReportingDaemon` to your `Runtime` during setup.

```scala
// Define your GraphQL schema normally
val api: GraphQL[Any] = 
  graphQL(RootResolver(Queries(characters = List(Character("Amos"))))) 

// Define a SchemaReporter that will communicate with Apollo
val reporterL = SchemaReporter.fromDefaultConfig // Loads the access token from an environment variable called "APOLLO_KEY"
// Or load it from a configuration type
// val reporterL = ZLayer.service[ApolloConfig] >>> SchemaReporter.fromConfig[ApolloConfig](_.key)

// Define your graph references
val daemon: ZIO[Scope with ReportingDaemon, Unit] = 
  for {
    graph1 <- SchemaReportingRef.make(api, "my-graph@production")
    // For dynamic or possibly updating schemas you can provide a Ref and a transform function that will allow
    // you to push schema updates that occur at runtime.
    ref <- Ref.make[String]("schema { query: Query }\n type Query { hello: String! }")
    graph2 <- SchemaReportingRef.fromRef(ref, "dynamic-graph@production")(identity)
    _ <- ReportingDaemon.register(graph1)
    _ <- ReportingDaemon.register(graph2)
  } yield ()

// Now wire it up
ZIO.scoped(daemon).provide(reporterL, AsyncHttpClientZioBackend.layer(), ReportingDaemon.live)
```
