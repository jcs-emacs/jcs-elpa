This Emacs package provides completion for GraphQL file with your
GraphQL server, which host the GraphQL DSL and allow
introspection.

The type, fields, argument definition lookup process is relied on
the instrospection specified in `introspection.graphql` graphql
query file, which is used to get json response from the
server.  Once the JSON response is received, it builds a hashtable,
which is futher augmented to help faster definition lookup.

Read README for more details.
