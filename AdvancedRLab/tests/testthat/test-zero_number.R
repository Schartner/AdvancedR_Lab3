context("zero number")

test_that("when either of the number is zero, it give another number back",{
    a <- sample(1:100,1)
    b <- sample(1:100,1)
    
    expect_equal(euclidian(0,b), b)
    expect_equal(euclidian(a,0), a)
})