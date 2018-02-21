#' Parse data from dataEntry R GUI control
#'
#' Takes a raw character matrix returned by dataEntry R GUI control
#' and attempts to parse it to something more friendly such as a
#' numeric matrix.
#' @param raw.matrix Character matrix
#' @param warn Whether to show warnings
#' @param want.data.frame logical; should a \code{data.frame} be returned instead
#' of a matrix or vector? If the input matrix returns a text matrix, this converted
#' to a dataframe
#' @param want.factors logical; should a text variable be converted to a factor?
#' Ignored if \code{want.data.frame} is \code{FALSE}
#' @param want.col.names logical; should the first row be interpretted as column names?
#' Ignored if \code{want.data.frame} is \code{FALSE}
#' @param want.row.names logical; should the first colulm be interpretted as row names?
#' Ignored if \code{want.data.frame} is \code{FALSE}
#' @param us.format logical; should the U.S. convention be used when parsing dates?
#' Ignored if \code{want.data.frame} is \code{FALSE}
#' @return if \code{want.data.frame == FALSE}, numeric vector or matrix, possibly
#' with an attribute \code{"statistic"} if row and column names are present; otherwise,
#' a \code{data.frame}
#' @details First removes empty rows and columns.  If every entry of
#'     the resulting matrix (or vector) is numeric, a matrix without
#'     row or column names is returned.  If the matrix has row and
#'     column names and a value in the \code{[1,1]} entry, that entry
#'     will be returned as an attribute in the output called
#'     \code{"statistic"}.
#' @note If characters are present in the entire first row (including
#'     the \code{[1,1]} entry), but the first column has all numeric
#'     entries aside from that \code{[1,1]} entry, then the resulting matrix
#' will have column names, but no row names.  And similarly for the case when
#' the first row is all numeric, but the \code{[1,1]} entry is not, the output
#' matrix will have row names, but no column names.
#'     to extract both row and column names from the resulting matrix
#' @export
ParseUserEnteredTable <- function(raw.matrix,
                                  warn = TRUE,
                                  want.data.frame = FALSE,
                                  want.factors = TRUE,
                                  want.col.names = TRUE,
                                  want.row.names = FALSE,
                                  us.format = NULL)
{
    if (all(raw.matrix == ""))
        stop("no data has been entered")

    m <- removeEmptyRowsAndColumns(raw.matrix, !want.data.frame)
    res <- NULL
    if (!isTRUE(want.data.frame)) # including NULL
        res <- parseAsVectorOrMatrix(m, warn && length(dim(m)) != 2)

    # Try parsing as dataframe if output was a character matrix
    if (isTRUE(want.data.frame) || (is.character(res) && length(dim(m)) == 2))
    {
        if (!is.null(res))
        {
            want.col.names <- attr(res, "col.names.given")
            want.row.names <- attr(res, "row.names.given")
        }
        res <- ParseAsDataFrame(m, warn, want.factors, want.col.names, want.row.names, us.format)
    }
    attr(res, "row.names.given") <- NULL
    attr(res, "col.names.given") <- NULL
    return(res)
}

#' Convert user pasted data to numeric
#'
#' Tries to convert character data to numeric including
#' converting entries with a '%' sign to numeric format.
#' @param nrow Optional dimensions of matrix to return if \code{drop} is false.
#' @param ncol Optional dimnsions of matrix to return if \code{drop} is false.
#' @param drop If true (default), a vector will always be returned
#' @noRd
asNumeric <- function(t, nrow = 1, ncol = 1, drop = TRUE, warn = FALSE)
{
    v <- as.vector(t)
    missing.idx <- v == "" | isMissing(v)
    out <- NA * numeric(length(v))
    v.non.miss <- v[!missing.idx]
    v.non.miss <- gsub(",", "", v.non.miss)  # remove commas, e.g. '1,000'
    out.non.miss <- suppressWarnings(as.numeric(v.non.miss))

    ## deal with possible use of percentages
    ind <- is.na(out.non.miss) & grepl("%$", v.non.miss)
    out.non.miss[ind] <- suppressWarnings(as.numeric(sub("%$", "", v.non.miss[ind])))

    if (any(is.na(out.non.miss)))  # couldn't all be converted to numeric or missing/NA
    {
        if (!drop)
            v <- matrix(v, nrow, ncol)
        if (warn)
            warning("The entered data could not be interpreted.", call. = FALSE)

        return(v)
    }
    # out[missing.idx] <- NA
    out[!missing.idx] <- out.non.miss
    if (!drop)
        out <- matrix(out, nrow, ncol)
    if (all(ind))
        attr(out, "statistic") <- "%"
    out
}

isNumericOrPercent <- function(t)
{
    v <- as.vector(t)
    v <- gsub(",", "", v)
    all(v == "" | isMissing(v) | !is.na(suppressWarnings(as.numeric(sub("%$", "", v)))))
}

isMissing <- function(t)
{
    return(grepl("^[[:blank:]]*(|-|\\.|N/A|NA|NaN|[M|m]issing|[I|i]nvalid)?[[:blank:]]*$", t))
}


#' Check For Titles in a User-Entered Table
#'
#' Searches the first row and column
#' in a character matrix to see if they each have a single
#' non-empty entry, and if so returns them in a vector
#' @return NULL if no titles found; otherwise, a length-2
#' vector containing the titles
#' @noRd
#' @keywords internal
getTableTitles <- function(m)
{
    if (NROW(m) <= 2 || NCOL(m) <= 2 || m[1, 1] != "")
        return(NULL)

    row.idx <- m[, 1] != ""
    if (sum(row.idx) != 1L)
        return(NULL)
    col.idx <- m[1, ] != ""
    if (sum(row.idx) != 1L)
        return(NULL)
    c(m[row.idx, 1L], m[1L, col.idx])
}

#' Convert a user-entered/pasted table to a Numeric Matrix
#'
#' Process a user pasted table when the user has not specified
#' that the data is raw data
#' @noRd
#' @keywords internal
parseAsVectorOrMatrix <- function(m, warn = FALSE)
{
    n.row <- NROW(m)
    n.col <- NCOL(m)

    if (n.row == 1 || n.col == 1)
    {
        vm <- drop(m)
        first.entry.chars <- !isNumericOrPercent(vm[1])
        if (!first.entry.chars || n.row == n.col)  ## unnamed row or column vector
            return(asNumeric(m, warn = warn))

        out <- asNumeric(vm[-1], warn = warn)
        if (is.numeric(out))
        {
            attr(out, "name") <- vm[1]
            return(out)
        } else
            return(m)
    }

    ## check for titles; if found, extract, and process submatrix
    if (length(titles <- getTableTitles(m))){
        m <- m[-1, -1]
        n.row <- n.row - 1
        n.col <- n.col - 1
    }

    first.entry.chars <- !isNumericOrPercent(m[1, 1])
    idx <- if (first.entry.chars) -1
           else seq_len(n.row)
    row.names.given <- !isNumericOrPercent(m[idx, 1])
    idx <- if (first.entry.chars) -1
           else seq_len(n.col)
    col.names.given <- !isNumericOrPercent(m[1, idx])

    if ((row.names.given && col.names.given) ||
        (row.names.given && !first.entry.chars) ||
        (col.names.given && !first.entry.chars))
    {
        out <- asNumeric(m[2:n.row, 2:n.col], n.row - 1, n.col - 1, drop = FALSE)
        if (first.entry.chars)
            attr(out, "statistic") <- m[1, 1]
        rownames(out) <- m[-1, 1]
        colnames(out) <- m[1, -1]

    } else if (row.names.given)
    {
        # somewhat ambiguous case, named 1,1 entry, but otherwise all numbers in first row
        out <- asNumeric(m[, 2:n.col], n.row, n.col - 1, drop = FALSE)
        rownames(out) <- m[, 1]
        out <- drop(out)
    } else if (col.names.given)
    {
        # somewhat ambiguous case, named 1,1 entry, but otherwise all numbers in first column
        out <- asNumeric(m[2:n.row, ], n.row - 1, n.col, drop = FALSE)
        colnames(out) <- m[1, ]
        out <- drop(out)
    }
    else
        out <- asNumeric(m, n.row, n.col, drop = FALSE)

    if (!is.null(attr(out, "statistic")) && grepl("%$", attr(out, "statistic")))
        out <- out/100
    if (length(titles))
        attr(out, "row.column.names") <- titles

    if (warn && is.character(out))
        warning("The entered data could not be interpreted.")

    # Save state of row/column names for ParseUserEnteredData
    attr(out, "row.names.given") <- row.names.given
    attr(out, "col.names.given") <- col.names.given

    # Vectors with attributes cannot be printed because of RS-3402
    # This is now fixed in Q 5.2.7+, but we retain support for older versions
    # by converting to a matrix if necessary
    if (!is.null(attr(out, "statistic")) && (is.null(dim(out)) || length(dim(out)) == 1))
    {
        tmp <- attr(out, "statistic")
        out <- as.matrix(out)
        attr(out, "statistic") <- tmp
    }
    out
}
