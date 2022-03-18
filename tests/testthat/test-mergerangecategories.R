context("MergeRangeCategories")

#Data Files
data(phone, package = "flipExampleData")
data(burger.brand.tracking, package = "flipExampleData")
data(cola, package = "flipExampleData")
data(ilock, package = "flipExampleData")

# Test cases
burger.income = burger.brand.tracking$C4
burger.age = burger.brand.tracking$S1
phone.age = phone$q4
cola.exercise.frequency = cola$Q28
cola.age = cola$Q3
ilock.income = ilock$Q7
ilock.income[ilock.income %in% c("Don’t know", "I refuse to answer this question")] = NA
ilock.income = droplevels(ilock.income)


test.cases = c("burger.income", "burger.age", "phone.age", "ilock.income")
error.cases = c("cola.exercise.frequency")
# Methods

# Even proportions
load("ranges.even.proportions.results.rda")
# ranges.even.proportions.results = list()
for (test.case in test.cases) {
    for (ncats in c("three", "five")) {
        if (ncats == "three") {
            num.categories = 3
        } else if (ncats == "five") {
            num.categories = 5
        }
        this.result = suppressWarnings(table(MergeRangeCategories(get0(test.case),
                                                 num.categories = num.categories,
                                                 method = "even.proportions")))
        test_that(paste0("Ranges even proportions:", paste0(c(test.case, ncats), collapse = ", ")), {
            expect_equal(this.result, ranges.even.proportions.results[[test.case]][[ncats]])
        })
        # ranges.even.proportions.results[[test.case]][[ncats]] = this.result
    }
}

# Even-width
load("ranges.even.ranges.results.rda")
# ranges.even.ranges.results = list()
for (test.case in test.cases) {
    if (test.case == "burger.income") {
        lower.bound = 0
        upper.bound = 200000
    } else if (test.case == "phone.age") {
        lower.bound = 13
        upper.bound = 85
    } else if (test.case == "ilock.income") {
        lower.bound = 2
        upper.bound = 250000
    } else {
        lower.bound = ""
        upper.bound = ""
    }
    for (ncats in c("three", "five")) {
        if (ncats == "three") {
            num.categories = 3
        } else if (ncats == "five") {
            num.categories = 5
        }
        this.result = suppressWarnings(table(MergeRangeCategories(get0(test.case),
                                                 num.categories = num.categories,
                                                 method = "even.ranges",
                                                 upper.bound = upper.bound,
                                                 lower.bound = lower.bound)))
        test_that(paste0("Ranges even proportions:", paste0(c(test.case, ncats), collapse = ", ")), {
            expect_equal(this.result, ranges.even.ranges.results[[test.case]][[ncats]])
        })
        # ranges.even.ranges.results[[test.case]][[ncats]] = this.result
    }
}




# Euro number convention
euro.income = burger.income
levels(euro.income) = c("Less than €20.000,00",
                        "€20.000,00 - €39.999,00",
                        "€40.000,00 - €59.999,00",
                        "€60.000,00 - €79.999,00",
                        "€80.000,00 - €99.999,00",
                        "€100.000,00 - €149.999,00",
                        "€150.000,00 or more",
                        "Prefer not to say")

# euro.results = list()
load("euro.results.rda")
# euro.results = list()
for (method in c("even.proportions", "even.ranges")) {
    this.result = table(suppressWarnings(MergeRangeCategories(euro.income,
                                       num.categories = 4,
                                       method = method,
                                       lower.bound = 0,
                                       upper.bound = 200000,
                                       grouping.mark = ".",
                                       decimals.mark = ",")))
    # euro.results[[method]] = this.result
    test_that(paste0("Euro results: ", method), {
        expect_equal(this.result, euro.results[[method]])
    })
}


# Missing data

test_that("Missing data preserved", {
    expect_equal(sum(is.na(MergeRangeCategories(ilock.income))), sum(is.na(ilock.income)))
})


# DS-3694 Not detecting labels properly, causing bad labels.
# Also tests that range detection still works when initial
# factor has levels out of order.
test.case.3964 = factor(rep(c("Less than 18 years",
                              "18 to 24 years",
                              "25 to 34 years",
                              "35 to 44 years",
                              "45 to 54 years",
                              "55 to 64 years",
                              "65 years or older"), each = 10))

load("result.2964.rda")
test_that("Labels identified correctly for DS-3964", {
    expect_equal(table(MergeRangeCategories(test.case.3964)), result.3964)
})

test_that("Interval phases being processed correctly", {
    expect_equal(identifyClosedOrOpenBoundariesFromText("less than"), "lower boundary open")
    expect_equal(identifyClosedOrOpenBoundariesFromText("more than"), "upper boundary open")
    expect_equal(identifyClosedOrOpenBoundariesFromText("and under"), "lower boundary closed")
    expect_equal(identifyClosedOrOpenBoundariesFromText("or more"), "upper boundary closed")
})

test_that("Ambiguous ranges not ignored DS-3693", {
    expect_equal(length(levels(MergeRangeCategories(cola.age, method = "even.ranges"))), 2)
})
