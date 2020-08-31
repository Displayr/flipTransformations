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

    m <- removeEmptyRowsAndColumns(raw.matrix, drop = FALSE)
    m <- extractTableTitle(m)
    m.title <- attr(m, "title")
    if (!is.null(m.title))
        m <- removeEmptyRowsAndColumns(m, !want.data.frame)
    m <- extractRowColumnTitles(m)
    row.col.title <- attr(m, "row.column.names")
    res <- NULL
    if (!isTRUE(want.data.frame)) # including NULL
        res <- parseAsVectorOrMatrix(m, FALSE)

    # Try parsing as dataframe if output was a character matrix
    if (isTRUE(want.data.frame) || is.character(res) && NCOL(res) > 1)
    {
        if (!is.null(res))
        {
            want.col.names <- isTRUE(attr(res, "col.names.given"))
            want.row.names <- isTRUE(attr(res, "row.names.given"))
        }
        res <- ParseAsDataFrame(as.matrix(m), warn, want.factors, want.col.names, want.row.names, us.format)
    }
    attr(res, "row.names.given") <- NULL
    attr(res, "col.names.given") <- NULL
    attr(res, "row.column.names") <- row.col.title
    attr(res, "title") <- m.title
    return(res)
}

extractTableTitle <- function(x)
{
    if (NCOL(x) == 1)
        return(x)

    # We require that titles are in the first cell of the first row which is otherwise empty
    # Note that entries in the 1-2 position are assumed to be column titles
    entries.in.first.row <- which(nchar(x[1,]) > 0)
    if (length(entries.in.first.row) == 1 && entries.in.first.row == 1)
    {
        title <- x[1,entries.in.first.row]
        x <- x[-1, , drop = FALSE]
        attr(x, "title") <- title
        return(x)
    }
    return(x)
}

#' Check For Titles in a User-Entered Table
#'
#' Searches the first row and column
#' in a character matrix to see if they each have a single
#' non-empty entry, and if so returns them in a vector
#' @return matrix with row and column title (if found) moved
#'   into the "row.column.names" attribute
#' @noRd
#' @keywords internal
extractRowColumnTitles <- function(m)
{
    if (NROW(m) <= 2 || NCOL(m) <= 2 || m[1, 1] != "")
        return(m)

    row.idx <- m[, 1] != ""
    col.idx <- m[1, ] != ""
    if (sum(row.idx) != 1L && sum(col.idx) != 1L)
        return(m)

    row.title <- if (sum(row.idx) == 1L) m[row.idx, 1L] else ""
    col.title <- if (sum(col.idx) == 1L) m[1L, col.idx] else ""
    m <- m[(1+(sum(col.idx)==1L)):nrow(m), (1+(sum(row.idx)==1L)):ncol(m), drop = FALSE]
    attr(m, "row.column.names") <- c(row.title, col.title)
    return(m)
}

#' Convert a user-entered/pasted table to a Numeric Matrix
#'
#' Process a user pasted table when the user has not specified
#' that the data is raw data
#' @noRd
#' @keywords internal
#' @importFrom flipTime IsDateTime
#' @importFrom flipU TrimWhitespace
parseAsVectorOrMatrix <- function(m, warn = FALSE)
{
    n.row <- NROW(m)
    n.col <- NCOL(m)

    if (n.row == 1 || n.col == 1)
    {
        dim.given <- if (n.row == 1) "row.names.given"
                     else            "col.names.given"

        vm <- drop(m)
        first.entry.chars <- !isTextNumeric(vm[1], allow.missing = TRUE)
        if (!first.entry.chars || n.row == n.col)  ## unnamed row or column vector
            return(asNumeric(m, warn = warn, NROW(m), NCOL(m)))

        # Check if entries are dates - note we retain them as characters
        # But we want to know if there is a column heading
        out <- asNumeric(vm[-1], drop = TRUE, warn = warn)
        if (!is.numeric(out))
        {
            if (IsDateTime(vm[1]) == IsDateTime(vm[-1]))
                return(m)
            else
            {
                out <- vm[-1]
                attr(out, "name") <- vm[1]
                attr(out, dim.given) <- TRUE
                return(out)
            }
        }

        attr(out, "name") <- vm[1]
        attr(out, dim.given) <- TRUE
        return(out)
    }

    data.attribute <- NULL
    row.names.given <- isPossibleLabel(m[-1,1])
    col.names.given <- isPossibleLabel(m[1,-1])

    # If input data is text, we assume there no row labels unless statistic is supplied
    if (any(!isTextNumeric(m[-1,-1], allow.missing = TRUE)))
        row.names.given <- isQStatistic(m[1,1])

    # Handling ambiguous row/column labels
    if (is.na(row.names.given) || is.na(col.names.given))
    {
        # if either is not a label then the 1,1 position cannot be a statistic
        if (isTRUE(!row.names.given))
            col.names.given <- isTRUE(isPossibleLabel(m[1,]))
        else if (isTRUE(!col.names.given))
            row.names.given <- isTRUE(isPossibleLabel(m[,1]))
        else
        {
            # We only need to check whether the 1,1 entry is a statistic
            # if BOTH row and column labels might be present

            if (isQStatistic(m[1,1]))
            {
                row.names.given <- TRUE
                col.names.given <- TRUE
            }
            else if (isTRUE(row.names.given))
                col.names.given <- FALSE
            else if (isTRUE(col.names.given))
                row.names.given <- FALSE
            else
            {
                if (nchar(m[1,1]) > 0 && !isTextNumeric(m[1,1]))
                {
                    # at least one of them is a label
                    # but which one (or both) is unclear
                    row.names.given <- FALSE
                    col.names.given <- TRUE
                }
                else
                {
                    row.names.given <- FALSE
                    col.names.given <- FALSE
                }
            }
        }
    }

    if (row.names.given && col.names.given)
    {
        out <- asNumeric(m[2:n.row, 2:n.col, drop = FALSE], n.row-1, n.col-1)
        if (nchar(m[1,1]) > 0)
            data.attribute <- m[1, 1]
        rownames(out) <- TrimWhitespace(m[-1, 1])
        colnames(out) <- TrimWhitespace(m[1, -1])

    } else if (row.names.given)
    {
        out <- asNumeric(m[, 2:n.col, drop = FALSE], n.row, n.col - 1)
        rownames(out) <- TrimWhitespace(m[, 1])
    } else if (col.names.given)
    {
        out <- asNumeric(m[2:n.row, , drop = FALSE], n.row - 1, n.col)
        colnames(out) <- m[1, ]
    }
    else
        out <- asNumeric(m, n.row, n.col)

    if (any(grepl("%", data.attribute)) && !isTRUE(attr(out, "statistic") == "%"))
        out <- out/100
    if (!is.null(data.attribute))
        attr(out, "statistic") <- data.attribute
    if (warn && is.character(out))
        warning("The entered data could not be interpreted.")

    # Save state of row/column names for ParseAsDataFrame
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


isQStatistic <- function(x)
{
    statistic.list <- c("", "%", "% Column Responses", "% Column Share",
        "% Excluding NaN", "% Responses", "% Row Responses", "% Row Share",
        " %Share", "% Total Responses", "% Total Share", "5th Percentile",
        "25th Percentile", "75th Percentile", "95th Percentile",
        "Average", "Base n", "Base Population", "Coefficient", "Column %",
        "Column Comparisons", "Column n", "Column Names",
        "Column Population", "Columns Compared", "Column Standard Error",
        "Column Standard Error of Mean", "Corrected p", "Correlation",
        "Cumulative %", "d.f", "Effective n", "Effective Base n",
        "Expected Average", "Expected %", "Expected Correlation",
        "Expected n", "Index", "Interquartile Range", "Labels",
        "Lower Confidence Interval", "Lower Confidence Interval %",
        "Maximum", "Median", "Minimum", "Missing n", "Mode",
        "Multiple Comparison Adjustment", "n", "n Observations",
        "Not duplicate", "Observation", "p", "Population", "Probability %",
        "Range", "Residual", "Residual %", "Row %", "Row Population",
        "Row n", "Standard Deviation", "Standard Error", "Sum", "Text",
        "Text With No Blanks", "Total %", "Trimmed Average", "t-Statistic",
        "Unique Text", "Upper Confidence Interval",
        "Upper Confident Interval %", "Values", "z-Statistic")
    return(x %in% statistic.list)
}

# Returns a logical indicating whether the vector of text might be a label
# Returns NA if they are all whole integers or can be parsed as a date
isPossibleLabel <- function(t)
{
    v <- asNumericVector(t)
    has.numeric.and.missing <- any(isMissing(t)) && any(!is.na(v)) && all(!is.na(v) | isMissing(t))
    if (has.numeric.and.missing) # all missing could potentially be a valid label
        return(FALSE)
    if (any(is.na(v)))
        return(TRUE)

    # Additional checks to see if the numeric text can also be read as a label
    if (any(nchar(t) == 0))
        return(FALSE)
    if (IsDateTime(t))
        return (NA)
    if (any(v != as.integer(v)))
        return (FALSE)
    return(NA)
}
