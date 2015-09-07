context("Errors")

test_that("not failed if initial node not in dataset",{
  expect_error(dijkstra(graph, max(graph[i])+1))
})

