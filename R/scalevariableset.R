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
#'     \code{"center"}, or \code{"unit"}.
#' @param within.case Logical; should the scaling be performed with
#'     case (rows) or variables (columns). An error is through if
#'     \code{TRUE} and \code{data} contains only one variable.
#' @return A numeric vector if \code{data} contains only a single
#'     variable or a data.frame containing the scaled values.
#' @seealso \code{\link{scale}}
#' @examples
#' ScaleVariableSet(1:5, type = "unit")
#' @export
ScaleVariableSet <- function(
                             data,
                             type = c("standardize", "center", "unit"),
                             within.case = FALSE)
{
    type <- match.arg(type)

    if (within.case && NCOL(data) == 1L)
        stop(shQuote("data"), "contains only one variable, scaling within case is ",
             "not meaningful.")

    is.categorical <- is.factor(data) || (is.data.frame(data) && is.factor(data[[1]]))
    if (is.categorical)
        data.with.values <- replaceFactorsWithValues(data)
    else
    {
        if (grepl("Number[MG]", attr(data, "questiontype")))
            data <- removeSUMColumns(data)
        data.with.values <- as.matrix(data)
    }
        ## may need to drop NET
        ## if (NCOL(data.with.values) > 1)
        ## data.with.values <- data.with.values[, !grepl("^NET$|^SUM$", colnames(data.with.values))]


    if (within.case)
        data.with.values <- t(data.with.values)

    if (type == "unit")
        out <- apply(data.with.values, 2,
                       function(x)
                       {
                           min.x <- min(x, na.rm = TRUE)
                           max.x <- max(x, na.rm = TRUE)
                           return((x - min.x)/(max.x - min.x))
                       })
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
    qt <- attr(df, "questiontype")
    if (qt == "NumberMulti"){
        return(df[, !colnames(df) %in% "SUM"])
    }else  # NumberGrid
        return(df[, !grepl("^SUM, |, SUM$", colnames(df))])
}
