#' \code{AsNumeric}
#' @description Coerces a \code{\link{data.frame}} or \code{\link{vector}} that may contain
#' factors into a \code{\link{vector}} or \code{\link{data.frame}} that does not contain factors.
#' @param x The \code{\link{data.frame}} or \code{\link{vector}}.
#' @param binary If \code{TRUE}, unordered factors are represented as dummy variables.
#' Otherwise, they are represented as sequential integers.
#' @param name Used if \code{binary} is \code{TRUE} to construct variable names. This parameter is
#' ignored if x is a \code{\link{data.frame}} or a \code{\link{list}}.
#' @param remove.first Remove the first binary variable.
#' @details Characters are treated as factors.
#' @importFrom flipFormat RemoveParentName
#' @export
AsNumeric <- function(x, binary = TRUE, name = NULL, remove.first = FALSE)
{
    UseMethod("AsNumeric")
}

#' @importFrom flipFormat RemoveParentName Names
#' @importFrom flipTime AsDateTime
#' @export
AsNumeric.default <- function(x, binary = TRUE, name = NULL, remove.first = FALSE)
{
    if (is.numeric(x))
        return(x)

    if (is.character(x))
    {
        x.tmp <- AsDateTime(x, on.parse.failure = "silent")
        if (!any(is.na(x.tmp)))
            x <- x.tmp
    }
    if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt"))
        return(as.numeric(x))
    if (is.null(name))
    {
        names <- Names(x)
        name <- RemoveParentName(names)
    }
    if (is.character(x))
        x <- Factor(x, levels = unique(x))
    else if (!is.vector(x) & !is.factor(x))
        stop("'AsNumeric' is only applicable to vectors, factors, and data.frames.")
    if (!is.factor(x))
        return(x)
    if (length(x) == 1) # avoid errors with factor of length 1
        return(1)
    FactorToNumeric(x, binary & !is.ordered(x), name = name, remove.first = remove.first)
}

#' @export
AsNumeric.data.frame <- function(x, binary = TRUE, name = NULL, remove.first = FALSE)
{
    new.x = asNumericList(x, binary = binary, remove.first = remove.first, return.data.frame = TRUE)
    row.names(new.x) = row.names(x)
    new.x
}

#' @export
AsNumeric.list <- function(x, binary = TRUE, name = NULL, remove.first = FALSE)
{
    if (!is.null(name))
        warning("'name' parameter is not used when 'x' is a list.")
    asNumericList(x, binary = binary, remove.first = remove.first, return.data.frame = FALSE)
}

#' \code{OneHot}
#'
#' Produces a numeric \code{\link{matrix}} of binary predictor variables and a numeric \code{\link{vector}}
#' of an outcome variable from an input \code{\link{data.frame}}.
#' With the exception of \code{outcome}, all \code{\link{factor}} variables of \code{data} are
#' converted to one binary column per factor level.  Numeric variables are unchanged.
#' @param data A \code{\link{data.frame}}.
#' @param outcome Optionally, the name of a variable in \code{data} to be converted to a numeric vector.
#'
#' @export
OneHot <- function(data, outcome = NULL)
{
    # convert predictor data to numeric matrix with factors converted to multiple binary columns
    X <- AsNumeric(data[, !names(data) %in% outcome, drop = FALSE])
    for (i in 1:ncol(X))
        if (!is.null(attr(X[, i], "label")))
            colnames(X)[i] <- attr(X[, i], "label")
    X <- matrix(apply(X, 2, as.numeric), ncol = ncol(X))

    # convert outcome variable to numeric vector (encoding from 0 to nlevels(outcome)-1)
    y <- NULL
    outcome.levels <- NULL
    if (!is.null(outcome) && outcome %in% names(data))
        if (is.factor(data[, outcome]))
        {
            outcome.levels <- levels(data[, outcome])
            y <- as.numeric(data[, outcome]) - 1
        }
        else
            y <- data[, outcome]

    return(list(X = X, y = y, outcome.levels = outcome.levels))
}

#' Convert user pasted data to numeric
#'
#' Tries to convert character data to numeric including
#' converting entries with a '%' sign to numeric format.
#' @param nrow Optional dimensions of matrix to return if \code{drop} is false.
#' @param ncol Optional dimnsions of matrix to return if \code{drop} is false.
#' @param drop If true (default), a vector will always be returned
#' @noRd
asNumeric <- function(t, nrow = 1, ncol = 1, drop = FALSE, warn = FALSE)
{
    v <- as.vector(t)
    out <- asNumericVector(v)
    is.percentage <- isTRUE(attr(out, "statistic") == "%")

    # If could not convert
    if (any(is.na(out) & !isMissing(v)))
    {
        if (warn)
            warning("The entered data could not be interpreted.", call. = FALSE)
        if (!drop)
            return(t)
        return(v)
    }

    if (!drop)
        out <- matrix(out, nrow, ncol)
    if (is.percentage)
        attr(out, "statistic") <- "%"
    out
}


isMissing <- function(t)
{
    return(grepl("^[[:blank:]]*(|-|\\.|N/A|NA|NaN|[M|m]issing|[I|i]nvalid)?[[:blank:]]*$", t))
}

asNumericVector <- function(t)
{
    v <- gsub(",", "", TrimWhitespace(as.vector(t)))
    v <- gsub("^\\$", "", v)
    result <- suppressWarnings(as.numeric(v))

    # Convert percentages
    ind <- is.na(result) & grepl("%$", v)
    if (any(ind))
        result[ind] <- suppressWarnings(as.numeric(gsub("%$", "", v[ind]))) / 100
    if (all(ind | isMissing(v)))
        attr(result, "statistic") <- "%"

    # Convert parentheses to negative numbers
    patt <- "^\\(\\$?[0-9.]+)$"
    ind <- is.na(result) & (regexpr(patt, v) > 0)
    if (any(ind))
        result[ind] <- -1 * as.numeric(gsub("[()$]", "", v[ind]))

    # Convert parentheses to negative percentages
    patt <- "^\\([0-9.]+%)$"
    ind <- is.na(result) & (regexpr(patt, v) > 0)
    if (any(ind))
        result[ind] <- -0.01 * as.numeric(gsub("[()%]", "", v[ind]))


    return(result)
}

isTextNumeric <- function(t, allow.missing = FALSE)
{
    v <- suppressWarnings(asNumericVector(t))
    if (allow.missing)
        return(all(!is.na(v) | t == "" | isMissing(t)))
    else
        return(all(!is.na(v) | t == ""))
}

