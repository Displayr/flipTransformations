#' Scale a Displayr Variable Set
#'
#' Given a Displayr Variable Set or Q Question, performs a specified
#' scaling on the data; first converting any categorical variables to
#' numeric as necessary.
#' @param data A Displayr Variable Set or Q Question: A numeric or
#'     factor vector, or a data.frame containing either all numeric
#'     columns or all factor columns.
#' @param type Character string specifying the type of scaling to
#'     perform; one of \code{"standardize"} (the default),
#'     \code{"center"}, \code{"rank"}, or \code{"unit"}.
#' @param within.case Logical; should the scaling be performed within
#'     case (rows) or variables (columns). An error is thrown if
#'     \code{TRUE} and \code{data} contains only one variable.
#' @return A numeric vector if \code{data} contains only a single
#'     variable or a data.frame containing the scaled values.
#' @seealso \code{\link{scale}}, \code{\link{numbersFromCategoricalVariableSets}}
#' @examples
#' x <- structure(c(1, 2, 3, 4, 5), questiontype = "Number",
#'                dataset = "dat", name = "x", label = "x", question = "x")
#'
#' ScaleVariableSet(x, type = "unit")
#' @export
ScaleVariableSet <- function(
                             data,
                             type = c("standardize", "center", "unit", "rank"),
                             within.case = FALSE)
{
    type <- match.arg(type)

    if (within.case && NCOL(data) == 1L)
        stop(shQuote("data"), "contains only one variable, scaling within case is ",
             "not meaningful.")

    is.categorical <- is.factor(data) || (is.data.frame(data) && is.factor(data[[1]]))
    if (is.categorical)
    {
        data.with.values <- numbersFromCategoricalVariableSets(data)
        if (NCOL(data.with.values) == 1L)
            data.with.values <- as.matrix(data.with.values)
    }else
    {
        qt <- attr(data, "questiontype")
        if (!is.null(qt) && grepl("Number[MG]", qt))
            data <- removeSUMColumns(data)
        data.with.values <- as.matrix(data)
    }


    if (within.case)
        data.with.values <- t(data.with.values)

    if (!within.case) {
        variances = apply(data.with.values, 2, var, na.rm = TRUE)
        if (any(is.na(variances) | variances == 0))
            warning("One or more of the input variable(s) has no variation.")
    }

    if (type == "unit")
        out <- apply(data.with.values, 2,
                       function(x)
                       {
                           min.x <- min(x, na.rm = TRUE)
                           max.x <- max(x, na.rm = TRUE)
                           return((x - min.x)/(max.x - min.x))
                       })
    else if (type == "rank")
        out <- apply(data.with.values, 2, rank, na.last = "keep",
                     ties.method = "average")
    else
        out <- scale(data.with.values, scale = type != "center")

    if (within.case)
        out <- t(out)
    else
        out <- drop(out)

    return(out)
}

#' Replace factor levels with values attribute
#' @param x data.frame
#' @return numeric matrix with same dimensions as
#' @noRd
replaceFactorsWithValues <- function(x)
{
    vv <- attr(x, "variablevalues")
    if (is.list(vv))  # multi-variable variable set
    {
        if (length(vv) != ncol(x))
            stop("Invalid variable set provided; the length of the ",
                 dQuote("variablevalues"), " attribute does not match ",
                 " the number of variables in the variable set.")
        out <- mapply(function(fact, vals) vals[levels(fact)[fact]], x, vv,
                      SIMPLIFY = TRUE)
        return(out)
    }  # else single variable variable set
    v <- attr(x, "values")
    if (!is.null(v))
        return(as.matrix(v[levels(x)[x]]))
    ## else
    return(as.matrix(as.numeric(x)))
}

#' @importFrom flipU CopyAttributes
#' @noRd
removeSUMColumns <- function(df)
{
    if (NCOL(df) == 1) # No SUM column if 
        return(df)
    qt <- attr(df, "questiontype")
    if (qt == "NumberMulti"){
        return(df[, !colnames(df) %in% "SUM"])
    }else  # NumberGrid
        return(df[, !grepl("^SUM, |, SUM$", colnames(df))])
}

#' Get numbers from attributtes of Pick One and Pick One - Multi questions
#'
#'  Extracts the codeframe and value attributes from the (R
#'  data.frame/factor representation of) Displayr's Categorical
#'  Variable Sets (PickOne and PickOneMulti questions in Q) and
#'  applies them to the input factors/data.frame to convert it to its
#'  underlying values.
#' @param x A variable set from Q/Displayr (an R factor or data.frame
#'     with special attributes).
#' @return For data.frame (vector) \code{x}, A matrix (vector) of
#'     numeric values with number of rows (elements) equal to
#'     \code{NROW(x)}.
#' @details levels(x) are always present in the names of the codeframe
#'     attribute, \emph{unless} codes are hidden. The codeframe
#'     contains sourcevalues for each code/level, which need to be
#'     mapped from the sourcevalues attribute (original values as read
#'     in with the data set) to the (possibly user-modified) values
#'     attribute.
#'
#'     A level that is the result of a merge will contain multiple
#'     source values for that element in the codeframe. These are
#'     mapped to the corresponding underlying values in the values
#'     attribute and averaged.
#'
#' @keywords internal
numbersFromCategoricalVariableSets <- function(x)
{
    vv <- attr(x, "variablevalues")
    duplicate.levels <- vapply(vv, FUN = function(y) anyDuplicated(names(y)), FUN.VALUE = numeric(1))
    if (!all(duplicate.levels == 0))
        stop("The variable(s) that you have selected have duplicate category labels. ",
             "You should edit the category labels to remove duplicates before using this feature.")
    if (is.list(vv))  # multi-variable variable set
    {
        if (length(vv) != ncol(x))  # probably not needed
            stop("Invalid variable set provided; the length of the ",
                 dQuote("variablevalues"), " attribute does not match ",
                 " the number of variables in the variable set.")

        ## out <- mapply(function(fact, vals) vals[levels(fact)[fact]], x, vv,
        ##               SIMPLIFY = TRUE)
        sv <- attr(x, "variablesourcevalues")
        cf <- attr(x, "codeframe")

        return(mapply(numbersFromCategoricalQVariable, x, vv, sv,
                      MoreArgs = list(code.frame = cf),
                      SIMPLIFY = TRUE))
    }
    ## else single variable variable set
    return(numbersFromCategoricalQVariable(x,
                                           attr(x, "values"),
                                           attr(x, "sourcevalues"),
                                           attr(x, "codeframe")))
}


numbersFromCategoricalQVariable <- function(x, value.attr, source.vals, code.frame)
{
    if (is.null(value.attr))
        return(as.numeric(x))

    if (is.null(code.frame) || is.null(source.vals))
        return(value.attr[levels(x)[x]])

    level.sv <- lapply(levels(x),
              function(l)
              {
                  if (l %in% names(code.frame))
                      return(code.frame[[l]])
                  else  # hidden code
                      return(source.vals[[l]])
              })
    names(level.sv) <- levels(x)

    sv.codes <- lapply(level.sv,
                                   function(val) names(source.vals[which(source.vals %in% val)]))
    v.codes <- lapply(sv.codes, function(val) value.attr[val])
    ## take average of values for merges
    level.vals <- vapply(v.codes, mean, numeric(1L))
    out <- level.vals[x]
    return(unname(out))
}
