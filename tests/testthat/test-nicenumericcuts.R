context("NiceNumericCuts")

#Data Files
data(phone, package = "flipExampleData")
data(burger.brand.tracking, package = "flipExampleData")

# Test cases
# Phone count of SMS messages sent
counts.data = phone$q25
# Normally distributed values with large range
set.seed(1234)
normal.high.range = rnorm(200, mean = 100, sd = 20)

# Normally distibuted values with small range
set.seed(1234)
normal.small.range = rnorm(200, mean = 5, sd = 5)

# Beta distributed values, skewed left
set.seed(123)
beta.left = round(rbeta(725, 5, 1) * 50)

# Number of times dine in and burger shack
sparse.integers = burger.brand.tracking$nQ5c_1_1




# Test results to check
load("label.style.tests.rda")


# Test cosmetic properties
styles = c("tidy.labels", "inequality.notation", "interval.notation")
prefix = "$"
suffix = " AUD"
label.decimals = 2
for (style in styles) {
    test_that(paste0("Label styles and customization: ", style), {
        expect_equal(table(NiceNumericCuts(beta.left, 
                            label.style = style, 
                            number.prefix = "$", 
                            number.suffix = " AUD", 
                            label.decimals = 2)), label.style.tests[[style]])
    })
}


# Test algorithm solutions
test.cases = c("counts.data", "normal.high.range", "normal.small.range", "beta.left", "sparse.integers")

# Tidy intervals
load("tidy.intervals.results.rda")
for(open.or.closed in c("open", "closed")) {
    open.ends = open.or.closed == "open"
    for (ncats in c("six", "ten")) {
        if (ncats == "six") { 
            num.categories = 6
        } else if (ncats == "ten") {
            num.categories = 10
        }
        for (test.case in test.cases) {
            test_that(paste0("Tidy intervals results: ", paste0(c(open.or.closed, ncats, test.case), collapse = ", ")), {
                expect_equal(table(NiceNumericCuts(get0(test.case),
                                                     method = "tidy.intervals",
                                                     num.categories = num.categories,
                                                     label.decimals = 2,
                                                     open.ends = open.ends)), tidy.intervals.results[[open.or.closed]][[ncats]][[test.case]])
            })
        }

    }
}


# Percentiles
load("percentile.results.rda")
for (style in c("percentiles", "tidy.labels")) {
    for (percent.spec in c("single.value", "multiple.values")) {
        if (percent.spec == "single.value") {
            percents = 10
        } else if (percent.spec == "multiple.values") {
            percents = "25, 50, 75, 100"
        }
        for (side in c("right", "left")) {
            right = side == "right"
            for (test.case in test.cases) {
                this.result = suppressWarnings(table(NiceNumericCuts(get0(test.case),
                                              method = "percentiles",
                                              percents = percents,
                                              right = right,
                                              label.style = style)))
                this.result = this.result / sum(this.result) * 100
                test_that(paste0("Percentile results: ", paste0(c(test.case, percent.spec, side, style), collapse = ", ")), {
                    expect_equal(this.result, percentile.results[[test.case]][[percent.spec]][[side]][[style]])
                })   
            }
             
        }
    }
}

# Equal width
load("equal.width.results.rda")
for (test.case in test.cases) {
    if (test.case == "counts.data") {
        start = 0
        end = 200
        ncat = 5
    } else if (test.case == "normal.high.range") {
        start = 40
        end = 180
        ncat = 10
    } else if (test.case == "normal.small.range") {
        start = -10
        end = 25
        ncat = 7    
    } else if (test.case == "beta.left") {
        start = 15
        end = 50
        ncat = 5
    } else if (test.case == "sparse.integers") {
        start = 0
        end = 10
        ncat = 10
    }
    this.result = table(NiceNumericCuts(get0(test.case),
                                        method = "equal.width",
                                        num.categories = ncat,
                                        equal.intervals.start = start,
                                        equal.intervals.end = end,
                                        label.style = "interval.notation",
                                        label.decimals = 0))
    test_that(paste0("Equal width results: ", test.case), {
        expect_equal(this.result, equal.width.results[[test.case]]) 
    })
}

# Custom
load("custom.intervals.results.rda")
for (test.case in test.cases) {
    if (test.case == "counts.data") {
        breaks = "0, 10, 20, 50, 100, 200"
    } else if (test.case == "normal.high.range") {
        breaks = "40, 100, 180"
    } else if (test.case == "normal.small.range") {
        breaks = "-10,0,10,25"    
    } else if (test.case == "beta.left") {
        breaks = "15,20,25,30,50"
    } else if (test.case == "sparse.integers") {
        breaks = "0, 2, 4, 6, 8, 10"
    }
    this.result = table(NiceNumericCuts(get0(test.case),
                                        method = "custom",
                                        custom.breaks = breaks,
                                        label.decimals = 0,
                                        open.end = FALSE))
    test_that(paste0("Custom interval results: ", test.case), {
        expect_equal(this.result, custom.intervals.results[[test.case]]) 
    })
}


# Missing data
test_that("Missing values preserved", {
    this.result = NiceNumericCuts(counts.data)
    expect_equal(length(which(is.na(this.result))), length(which(is.na(counts.data))))
})

# Multiple variables
test_that("Multiple variables in data frame handled",{
    my.df = data.frame(counts.data, beta.left)
    this.result = NiceNumericCuts(my.df)
    expect_equal(dim(this.result), dim(my.df))
    expect_equal(levels(this.result[, 1]), levels(this.result[, 2]))
    expect_equal(length(which(is.na(this.result[, 1]))), length(which(is.na(this.result[, 1]))))
    expect_equal(length(which(is.na(this.result[, 2]))), length(which(is.na(this.result[, 2]))))
})
