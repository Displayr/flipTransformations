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


test_that("RemoveMissingLevelsFromFactors", {
    data(phone, package = "flipExampleData")
    phone <- phone[, 1:10]
    levels(phone$q2)[9] <- "Dog"
    phone1 <- suppressWarnings(RemoveMissingLevelsFromFactors(phone))
    expect_equal(nlevels(phone$q2) - 2, nlevels(phone1$q2))
    expect_equal(flipFormat::Labels(phone),flipFormat::Labels(phone1))
    expect_equal(sapply(phone1, class),sapply(phone1, class))
})

dat <- data.frame(x = 0:10, y = -5:5)
test_that("Standardization", {
    zscore <- StandardizeData(dat, "z-scores")
    expect_equal(unname(colMeans(zscore)), c(0, 0))
    expect_equal(unname(apply(zscore, 2, sd)), c(1, 1))
    range11 <- StandardizeData(dat, "Range [-1,1]")
    expect_equal(range11[, 1], 0.1 * (0:10))
    expect_equal(range11[, 2], 0.1 * (-5:5))
    range01 <- StandardizeData(dat, "Range [0,1]")
    expect_equal(range01[, 1], 0.1 * (0:10))
    expect_equal(range01[, 2], 0.1 * (0:10))
    max.mag <- StandardizeData(dat, "Maximum magnitude of 1")
    expect_equal(max.mag[, 1], 0.1 * (0:10))
    expect_equal(max.mag[, 2], 0.2 * (-5:5))
    expect_warning(mean1 <- StandardizeData(dat, "Mean of 1"), "mean of 0")
    expect_equal(mean1[, 1], 0.2 * (0:10))
    expect_equal(mean1[, 2], -5:5)
    sd1 <- StandardizeData(dat, "Standard deviation of 1")
    expect_equal(unname(apply(sd1, 2, sd)), c(1, 1))
    expect_warning(no.variation <- StandardizeData(data.frame(rep(1, 10)), "z-scores"), "no variation")
    expect_equal(no.variation[, 1], rep(0, 10))
})

