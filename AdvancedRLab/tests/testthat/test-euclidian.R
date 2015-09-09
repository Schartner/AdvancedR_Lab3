context("euclidian")

test_that("the return value always be positive number",{
    a <- sample(1:100,1)
    b <- sample(1:100,1)
    
    expect_more_than(euclidian(a, -b),0)
    expect_more_than(euclidian(-a, b),0)
})

test_that("when either of the number is zero, it give another number back",{
    a <- sample(1:100,1)
    b <- sample(1:100,1)
    
    expect_equal(euclidian(0,b), b)
    expect_equal(euclidian(a,0), a)
})

test_that("the return value is correct",{
    expect_equal(euclidian(100,1000),100)
    expect_equal(euclidian(123612, 13892347912),4)
})

