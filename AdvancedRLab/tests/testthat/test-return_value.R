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

test_that("check the return value if it has an infinity",{
    expect_equal(any(dijkstra(graph,i) == Inf), FALSE)
})
