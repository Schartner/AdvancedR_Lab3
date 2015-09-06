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

#this test is not necessarily true for another dataset.  there could be a node not connected to inital. will write stopifnot
test_that("check the return value if it has an infinity",{
    expect_equal(any(dijkstra(graph,i) == Inf), FALSE)
})

test_that("check the return distance of initial node is 0",{
  expect_equal(dijkstra(graph,i)[i] == 0, TRUE)
})

test_that("The distance is higher than all possible connectiones combined. At least one connection used twice",{
  expect_equal(all(dijkstra(graph,i) < sum(graph[3])), TRUE)
})