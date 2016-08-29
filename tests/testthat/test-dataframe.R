context("dataframe")

dat <- data.frame(x = 1:2, y = 2:1)
sm <- sum(AdjustDataToReflectWeights(dat, 2:1))
# sm

test_that("Replicating data file with integer weights", {
    d = AdjustDataToReflectWeights(dat, 2:1)
    expect_that(nrow(d), equals(3))
    expect_that(sum(d), equals(9))
})

test_that("Creating bootrapped sample with weights", {
    # Small sample.
    d <- suppressWarnings(AdjustDataToReflectWeights(dat, (2:1) / 10))
    expect_that(nrow(d), equals(1))
    expect_that(sum(d), equals(3))
    expect_warning(AdjustDataToReflectWeights(dat, (2:1) / 10))

    # Moderate sample.
    d <- suppressWarnings(AdjustDataToReflectWeights(dat[rep(1:2, 50), ], runif(100)))
    expect_that(nrow(d), equals(50))
    expect_that(sum(d), equals(150))
    expect_warning(AdjustDataToReflectWeights(dat[rep(1:2, 50), ], runif(100)))
})

data(phone, package = "flipExampleData")
z <- data.frame(q23a_2 = phone$q23a,
     q23b_2 = phone$q23b,
     q23c_2 = phone$q23c,
     q23d_2 = phone$q23d,
     q23e_2 = phone$q23e,
     q23f_2 = phone$q23f)

    dat <- flipTransformations::AsNumeric(z, binary = TRUE, remove.first = TRUE)
dat[,1]


