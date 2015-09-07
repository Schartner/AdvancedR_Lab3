context("return value")

i <- sample(1:6,1)
data(wiki_graph)
graph <- wiki_graph

test_that("check the return value that there is no names of the object",{
    expect_named(dijkstra(graph,i),NULL)
})

test_that("check the return value if it's a numeric vector",{
    expect_is(dijkstra(graph, i), "numeric")
})

test_that("check the return distance of initial node is 0",{
  expect_equal(dijkstra(graph,i)[i] == 0, TRUE)
})

test_that("Distances not plausible. Has to be either Infinity (not connected) or smaller than total sum of distances",{
  expect_equal(all(dijkstra(graph,i) < sum(graph[3]/2)|dijkstra(graph,i)==Inf), TRUE)
})