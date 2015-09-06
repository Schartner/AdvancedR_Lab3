context("greatest value")

test_that("the return value always be positive number",{
    a <- sample(1:100,1)
    b <- sample(1:100,1)
    
    expect_more_than(euclidian(a, -b),0)
    expect_more_than(euclidian(-a, b),0)
})