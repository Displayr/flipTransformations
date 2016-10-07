
#' \code{RemoveRowsAndOrColumns}
#' @description Removes rows or columns from the table.
#' @param x The data that is being analyzed
#' @param row.names.to.remove A vector or comma-separated string containing the row labels to remove, if row names are defined. Trailing spaces are removed and lower/upper case is ignored
#' @param column.names.to.remove A vector or comma-separated string containing the column labels to remove, if column names are defined
#' @export
RemoveRowsAndOrColumns <- function(x,
                                   row.names.to.remove = c("NET", "Total", "SUM"),
                                   column.names.to.remove = c("NET", "Total", "SUM"))
{
    ind <- list(1:nrow(x), 1:ncol(x))
    for (i in 1:2)
    {
        tmpname <- switch(i, rownames(x), colnames(x))
        tmpstring <- switch(i, row.names.to.remove, column.names.to.remove)

        if (!is.null(tmpname) && !is.null(tmpstring))
        {
            tmpname <- tolower(trimws(tmpname))
            tmpstring <- unlist(strsplit(tmpstring, split=","))
            tmpstring <- tolower(trimws(tmpstring))
            ind[[i]] <- which(!tmpname %in% tmpstring)
        }
    }
    x[ind[[1]], ind[[2]], drop = FALSE]
}
