load("variable.sets.rda")

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

    numeric[1:5] <- NA
    out <- ScaleVariableSet(numeric, type = "unit")
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

})

test_that("Numeric - grid",
{
    out <- ScaleVariableSet(numeric.grid)
    expect_equal(out, scale(out), check.attributes = FALSE)
    expect_equal(colnames(out), colnames(numeric.grid))
})

