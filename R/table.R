
#' \code{RemoveRowsAndOrColumns}
#' @description Removes rows or columns from the table.
#' @param x The data that is being analyzed
#' @param row.names.to.remove A vector or comma-separated string containing the row labels to remove.
#' @param column.names.to.remove A vector or comma-separated string containing the column labels to remove.
#' @param split Delimiter to split string on.
#' @details Trailing spaces are removed and lower/upper case is ignored.
#' @importFrom flipFormat ConvertCommaSeparatedStringToVector
#' @importFrom flipU CopyAttributes
#' @export
RemoveRowsAndOrColumns <- function(x,
                                   row.names.to.remove = c("NET", "Total", "SUM"),
                                   column.names.to.remove = c("NET", "Total", "SUM"),
                                   split = ",")
{
    if (is.null(row.names.to.remove) && is.null(column.names.to.remove))
        return(x)
    ind <- RetainedRowsAndOrColumns(x = x, row.names.to.remove = row.names.to.remove,
                                column.names.to.remove = column.names.to.remove,
                                split = split)

    if (length(ind[[1]]) == 0 || length(ind[[2]]) == 0)
        stop ("Removing rows/columns gives empty input matrix\n")

    CopyAttributes(x[ind[[1]], ind[[2]], drop = FALSE], x)
}

#' \code{RetainedRowsAndOrColumns}
#' @description Produces a list of 2 vectors, indices of rows and columns from a table.
#' @param x The data that is being analyzed.
#' @param row.names.to.remove A vector or comma-separated string containing the row labels to remove.
#' @param column.names.to.remove A vector or comma-separated string containing the column labels to remove.
#' @param split Delimiter to split string on.
#' @details Trailing spaces are removed and lower/upper case is ignored.
#' @export
RetainedRowsAndOrColumns <- function(x,
                                   row.names.to.remove = c("NET", "Total", "SUM"),
                                   column.names.to.remove = c("NET", "Total", "SUM"),
                                   split = ",")
{
    ind <- list(retained.rows = 1:nrow(x), retained.cols = 1:ncol(x))
    for (i in 1:2)
    {
        tmpname <- switch(i, rownames(x), colnames(x))
        tmpstring <- switch(i, row.names.to.remove, column.names.to.remove)

        if (!is.null(tmpname) && !is.null(tmpstring))
        {
            tmpname <- tolower(trimws(tmpname))

            # No splitting if empty delimiter is given (vector is expected)
            if (split != "")
                tmpstring <- ConvertCommaSeparatedStringToVector(tmpstring, split)
            tmpstring <- tolower(tmpstring)
            ind[[i]] <- which(!tmpname %in% tmpstring)
        }
    }
    return(ind)
}
