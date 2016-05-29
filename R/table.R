
#' \code{RemoveRowsAndOrColumns}
#' @description Removes rows or columns from the table.
#' @param x The data that is being analyzed
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#' @export
RemoveRowsAndOrColumns <- function(x,
                                   row.names.to.remove = c("NET", "Total", "SUM"),
                                   column.names.to.remove = c("NET", "Total", "SUM"))
{
    x[!rownames(x) %in% row.names.to.remove, !colnames(x) %in% column.names.to.remove, drop = FALSE]
}
