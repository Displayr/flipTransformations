file <- system.file("extdata", "variable.sets.rda", package = "flipTransformations")
load(file)

test_that("Numeric Variable Set",
{
    out <- ScaleVariableSet(numeric, type = "standardize")
    expect_equal(out, scale(numeric), check.attributes = FALSE)

    out <- ScaleVariableSet(numeric, type = "center")
    expect_equal(out, scale(numeric, scale = FALSE), check.attributes = FALSE)

    out <- ScaleVariableSet(numeric, type = "unit")
    expect_null(dim(out))
    unit.nom <- (numeric - min(numeric))/diff(range(numeric))
    expect_equal(range(out), c(0, 1))
    expect_equal(out, unit.nom, check.attributes = FALSE)

    out <- ScaleVariableSet(numeric, type = "rank")
    ## numeric is an ID var which happens to be strictly increasing
    expect_equal(out, 1:800, check.attributes = FALSE)

    numeric[1:5] <- NA
    out <- ScaleVariableSet(numeric)
    expect_equal(out, scale(numeric), check.attributes = FALSE)

    expect_error(ScaleVariableSet(numeric, within.case = TRUE),
                 "only one variable")
})


test_that("Nominal Variable Set",
{
    nom.val <- numeric(length(nominal))
    vals <- attr(nominal, "values")
    for (l in levels(nominal))
        nom.val[nominal == l]  <- vals[l]
    nom.val2 <- vals[levels(nominal)[nominal]]
    expect_equal(nom.val, nom.val2, check.attributes = FALSE)

    out <- ScaleVariableSet(nominal, type = "standardize")
    expect_equal(out, scale(nom.val),
                 check.attributes = FALSE)

    out <- ScaleVariableSet(nominal, type = "center")
    expect_equal(out, scale(nom.val, scale = FALSE),
                 check.attributes = FALSE)

    out <- ScaleVariableSet(nominal, type = "unit")
    expect_equal(range(out), c(0, 1),
                 check.attributes = FALSE)
})


test_that("Ordinal Variable Set with NAs",
{
    ord.val <- rep(NA, length(ordinal.hide))
    vals <- attr(ordinal.hide, "values")
    for (l in levels(ordinal.hide))
        ord.val[ordinal.hide == l]  <- vals[l]
    ord.val2 <- vals[levels(ordinal.hide)[ordinal.hide]]
    expect_equal(ord.val, ord.val2, check.attributes = FALSE)

    out <- ScaleVariableSet(ordinal.hide, type = "standardize")
    expect_equal(out, scale(ord.val),
                 check.attributes = FALSE)

    out <- ScaleVariableSet(ordinal.hide, type = "center")
    expect_equal(out, scale(ord.val, scale = FALSE),
                 check.attributes = FALSE)

    out <- ScaleVariableSet(ordinal.hide, type = "unit")
    expect_equal(range(out, na.rm = TRUE), c(0, 1),
                 check.attributes = FALSE)

    out <- ScaleVariableSet(ordinal.hide, type = "rank")
    out.expect <- rank(ord.val, na.last = "keep", ties.method = "average")
    expect_equal(out, out.expect, check.attributes = FALSE)
})

test_that("Nominal - Multi Variable Set with NAs",
{
    nm.val <- matrix(NA, nrow(nominal.multi),
                     ncol(nominal.multi))
    vals <- attr(nominal.multi, "variablevalues")
    for (i in 1:ncol(nominal.multi))
        for (l in levels(nominal.multi[[i]]))
            nm.val[nominal.multi[[i]] == l, i]  <- vals[[i]][l]
    nm.val2 <- mapply(function(f, v) v[levels(f)[f]], nominal.multi, vals)
    expect_equal(nm.val, nm.val2, check.attributes = FALSE)

    out <- ScaleVariableSet(nominal.multi, type = "standardize")
    expect_equal(out, scale(nm.val),
                 check.attributes = FALSE)

    out <- ScaleVariableSet(nominal.multi, type = "center")
    expect_equal(out, scale(nm.val, scale = FALSE),
                 check.attributes = FALSE)

    out <- ScaleVariableSet(nominal.multi, type = "unit")
    expect_equal(range(out, na.rm = TRUE), c(0, 1),
                 check.attributes = FALSE)


    out <- ScaleVariableSet(nominal.multi, type = "standardize", within.case = TRUE)
    expect_equal(out, t(scale(t(nm.val))),
                 check.attributes = FALSE)

    nominal.multi[10, 5] <- nm.val[10, 5] <- NA
    out <- ScaleVariableSet(nominal.multi, type = "standardize", within.case = TRUE)
    expect_equal(out, t(scale(t(nm.val))),
                 check.attributes = FALSE)

    out <- ScaleVariableSet(nominal.multi, type = "rank", within.case = TRUE)
    out.expect <- t(apply(nm.val, 1, rank, na.last = "keep",
                          ties.method = "average"))
    expect_equal(colnames(out), colnames(nominal.multi))
    expect_equal(out, out.expect, check.attributes = FALSE)
})

test_that("Numeric - multi Ignores SUM column",
{
    out <- ScaleVariableSet(numeric.multi)
    expect_equal(out, scale(numeric.multi[, -ncol(numeric.multi)]),
                 check.attributes = FALSE)
    expect_equal(colnames(out), colnames(numeric.multi)[-ncol(numeric.multi)])
})


test_that("Numeric - grid",
{
    out <- ScaleVariableSet(numeric.grid, type = "center")
    ng.no.sum <- numeric.grid[, !grepl("SUM", colnames(numeric.grid))]
    expect_equal(out, scale(ng.no.sum, scale = FALSE), check.attributes = FALSE)
    expect_equal(colnames(out), colnames(ng.no.sum))
})

test_that("Nominal with merge, hide and changed value attr",
{
    ## Note: 1) 30 to 34 and 35 to 39 have been merged + renamed to 30 to 39
    ## 2) 50 to 54 has been hidden
    ## 3) value attribute for 65 or more changed to 65 from 77
    x <- nominal.merge.hide
    v <- attr(x, "values")
    cf <- attr(x, "codeframe")
    sv <- attr(x, "sourcevalues")
    out <- flipTransformations:::numbersFromCategoricalVariableSets(x)

    expect_is(out, "numeric")
    expect_equal(length(out), length(x))

    to <- table(out)
    tx <- table(x)
    expect_named(to, c("21", "27",
                       "34.5",  # ave. of underlyting values for 30 to 34 and 35 to 39
                       "42", "47", "52", "60",
                       "65"))  # value attr. diff from sourceval
    idx <- c(1:5, 8, 6, 7)
    expect_equal(as.numeric(to), as.numeric(tx[idx]))

    out.OtN <- OrderedToNumeric(x)
    expect_equal(out.OtN, out, check.attributes = FALSE)
    attr.to.rm <- eval(formals(flipU::CopyAttributes)$attr.to.not.copy)
    expect_equal(attributes(x)[!names(attributes(x)) %in% attr.to.rm],
                 attributes(out.OtN)[!names(attributes(out.OtN)) %in% attr.to.rm])

    out.AN <- AsNumeric(x, FALSE)
    expect_equal(out.AN, out, check.attributes = FALSE)
    expect_equal(attributes(x)[!names(attributes(x)) %in% attr.to.rm],
                 attributes(out.AN)[!names(attributes(out.AN)) %in% attr.to.rm])

})

test_that("PickOneMulti with merge, hide, NET",
{
    ## Note: 1) variablevalues don't match variablesourcevalues:
    ##  Hate = -3, Love = 3
    ## 2) Hate has been hidden from codeframe
    ## 3) Neither like nor dislike has been merged to "NOT positives" (see cf below)
    ## 4) codeframe contains an extra NET "Like + Love NET"
    x <- super.pick.one.multi
    v <- attr(x, "variablevalues")
    cf <- attr(x, "codeframe")
    sv <- attr(x, "variablesourcevalues")
    out <- flipTransformations:::numbersFromCategoricalVariableSets(x)

    expect_is(out, "matrix")
    expect_equal(colnames(out), names(x))

    ## 0,-1 averaged to -.5
    ## -2 in sourcevals mapped to -3, 2 in sourcevals to 3
    for (i in seq_along(x))
    {
        ti <- table(out[, i])
        expect_named(ti, c("-3", "-0.5", "1", "3"))
        ## hidden cf Hate is at end of levels of factor from R (Core wontfix)
        xi <- x[[i]]
        idx <- c("Hate", levels(xi)[-length(levels(xi))])
        expect_equal(as.numeric(ti), as.numeric(table(xi)[idx]))
    }

    out.AN <- AsNumeric(x, binary = FALSE)
    expect_equal(out.AN, as.data.frame(out), check.attributes = FALSE)
    attr.to.rm <- eval(formals(flipU::CopyAttributes)$attr.to.not.copy)
    expect_equal(attributes(x)[!names(attributes(x)) %in% attr.to.rm],
                 attributes(out.AN)[!names(attributes(out.AN)) %in% attr.to.rm])
})

test_that("DS-3898 Duplicate factor levels produce wrong answer",
{
    # Pick One - Multi Variable set where the category labels are duplicated
    test.case <- structure(list(X = structure(c(NA, 4L, 3L, 4L, 3L, 3L, NA, 4L, 
4L, 3L, 2L, 2L, 1L, 3L, 3L, 5L, 3L, 4L, 4L, 5L), levels = c("A", 
"B", "C", "B", "A"), class = c("ordered", "factor")), Y = structure(c(NA, 
3L, 5L, 5L, 4L, 3L, NA, 4L, 1L, 3L, 4L, 2L, 3L, 4L, 3L, 4L, 4L, 
4L, 4L, 5L), levels = c("A", "B", "C", "B", "A"), class = c("ordered", 
"factor"))), row.names = c(302L, 273L, 103L, 43L, 951L, 698L, 
2L, 600L, 999L, 54L, 688L, 592L, 522L, 617L, 586L, 833L, 941L, 
357L, 139L, 74L), questiontype = "PickOneMulti", question = "Question", dataset = "Final(Panel)-BrandHook - T2 Growth - [A-41336]_22Aug2022.sav", span = list(
    rows = structure(list(c("Label 1", "Label 2", "Label 3", 
    "Label 4", "Label 5", "Label 6", "Label 7", "Label 8", "Label 9", 
    "Label 10", "Label 11", "Label 12")), class = "data.frame", names = "", row.names = c(NA, 
    12L)), columns = structure(list(c("Missing data", "A", "B", 
    "C", "B", "A")), class = "data.frame", names = "", row.names = c(NA, 
    6L))), values = c(A = 1, B = 2, C = 3, B = 4, A = 5), sourcevalues = c(A = 1, 
B = 2, C = 3, B = 4, A = 5), variablevalues = list(c(A = 1, B = 2, 
C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5), c(A = 1, 
B = 2, C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5
), c(A = 1, B = 2, C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, 
B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5), c(A = 1, 
B = 2, C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5
), c(A = 1, B = 2, C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, 
B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5)), variablesourcevalues = list(
    c(A = 1, B = 2, C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, 
    B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5), c(A = 1, 
    B = 2, C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, 
    A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5), c(A = 1, B = 2, 
    C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5
    ), c(A = 1, B = 2, C = 3, B = 4, A = 5), c(A = 1, B = 2, 
    C = 3, B = 4, A = 5), c(A = 1, B = 2, C = 3, B = 4, A = 5
    ), c(A = 1, B = 2, C = 3, B = 4, A = 5)), codeframe = list(
    A = 1, B = 2, C = 3, B = 4, A = 5, NET = c(1, 2, 3, 4, 5)), secondarycodeframe = list(
    `Label 1` = 0L, `Label 2` = 1L, `Label 3` = 2L, `Label 4` = 3L, 
    `Label 5` = 4L, `Label 6` = 5L, `Label 7` = 6L, `Label 8` = 7L, 
    `Label 9` = 8L, `Label 10` = 9L, `Label 11` = 10L, `Label 12` = 11L), transposed = TRUE, class = "data.frame")

    expect_error(ScaleVariableSet(test.case, type = 'standardize', within.case = TRUE))
})
