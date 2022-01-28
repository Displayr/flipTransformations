context("NiceNumericCuts")

#Data Files
data(phone, package = "flipExampleData")
data(burger.brand.tracking, package = "flipExampleData")
data(ilock, package = "flipExampleData")


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

# Categories with single values
# Income relabeled with single values
fake.income = ilock$Q7
levels(fake.income) = c("$1,000",
                        "$1,000",
                        "$3,000",
                        "$4,000",
                        "$5,000",
                        "$6,000",
                        "$7,000",
                        "$8,000",
                        "$10,000",
                        "$12,500",
                        "$15,000",
                        "$17,500",
                        "$20,000",
                        "$22,500",
                        "$25,000",
                        "$30,000",
                        "$35,000",
                        "$40,000",
                        "$50,000",
                        "$60,000",
                        "$75,000",
                        "$90,000",
                        "$110,000",
                        "$130,000",
                        "$150,000",
                        "$200,000",
                        "Don’t know",
                        "I refuse to answer this question")

# Euro number convention
fake.euro.income = ilock$Q7
levels(fake.euro.income) = c("$1.000,00",
                        "$1.000,00",
                        "$3.000,00",
                        "$4.000,00",
                        "$5.000,00",
                        "$6.000,00",
                        "$7.000,00",
                        "$8.000,00",
                        "$10.000,00",
                        "$12,500",
                        "$15.000,00",
                        "$17,500",
                        "$20.000,00",
                        "$22,500",
                        "$25.000,00",
                        "$30.000,00",
                        "$35.000,00",
                        "$40.000,00",
                        "$50.000,00",
                        "$60.000,00",
                        "$75.000,00",
                        "$90.000,00",
                        "$110.000,00",
                        "$130.000,00",
                        "$150.000,00",
                        "$200.000,00",
                        "Don’t know",
                        "I refuse to answer this question")



# Test results to check
load("label.style.tests.rda")

# label.style.tests = list()
# Test cosmetic properties
styles = c("tidy.labels", "inequality.notation", "interval.notation")
prefix = "$"
suffix = " AUD"
label.decimals = 2
for (style in styles) {
    this.result = table(suppressWarnings(NiceNumericCuts(beta.left, 
                            label.style = style, 
                            number.prefix = "$", 
                            number.suffix = " AUD", 
                            label.decimals = 2)))
    test_that(paste0("Label styles and customization: ", style), {
        expect_equal(this.result, label.style.tests[[style]])
    })
    # label.style.tests[[style]] = this.result
}


# Test algorithm solutions
test.cases = c("counts.data", "normal.high.range", "normal.small.range", "beta.left", "sparse.integers")

# Tidy intervals
load("tidy.intervals.results.rda")
# tidy.intervals.results = list()
for(open.or.closed in c("open", "closed")) {
    open.ends = open.or.closed == "open"
    for (ncats in c("six", "ten")) {
        if (ncats == "six") { 
            num.categories = 6
        } else if (ncats == "ten") {
            num.categories = 10
        }
        for (test.case in test.cases) {
            test.name = paste0("Tidy intervals results: ", paste0(c(open.or.closed, ncats, test.case), collapse = ", "))
            this.result = table(suppressWarnings(NiceNumericCuts(get0(test.case),
                                                     method = "tidy.intervals",
                                                     num.categories = num.categories,
                                                     label.decimals = 2,
                                                     open.ends = open.ends)))

            test_that(test.name, {
                expect_equal(this.result, tidy.intervals.results[[open.or.closed]][[ncats]][[test.case]])
            })
            # tidy.intervals.results[[open.or.closed]][[ncats]][[test.case]] = this.result
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


# Euro numbers
load("euro.format.results.rda")
# euro.format.results = list()
for (data.type in c("factor", "character")) {
    test_that(paste0("European number convention: ", data.type), {
        test.data = fake.euro.income
        if (data.type == "character") {
            test.data = as.character(test.data)
        }
        this.result = suppressWarnings(table(NiceNumericCuts(test.data,
                                      num.categories = 5, 
                                      grouping.mark = ".", 
                                      decimals.mark = ",")))
        expect_equal(this.result, euro.format.results[[data.type]])
        # euro.format.results[[data.type]] = this.result 
    })
}



# Missing data
test_that("Missing values preserved", {
    this.result = suppressWarnings(NiceNumericCuts(counts.data))
    expect_equal(length(which(is.na(this.result))), length(which(is.na(counts.data))))
})

# Multiple variables
test_that("Multiple variables in data frame handled",{
    my.df = data.frame(counts.data, beta.left)
    this.result = suppressWarnings(NiceNumericCuts(my.df))
    expect_equal(dim(this.result), dim(my.df))
    expect_equal(levels(this.result[, 1]), levels(this.result[, 2]))
    expect_equal(length(which(is.na(this.result[, 1]))), length(which(is.na(this.result[, 1]))))
    expect_equal(length(which(is.na(this.result[, 2]))), length(which(is.na(this.result[, 2]))))
})
