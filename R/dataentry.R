#' \code{ParseEnteredData}
#' @description Takes a raw character matrix returned by dataEntry R GUI control
#' and attempts to parse it to something more friendly such as a numeric matrix.
#' @param raw.matrix Character matrix
#' @export
ParseEnteredData <- function(raw.matrix)
{
    m <- raw.matrix

    if (all(m == ""))
        stop("No data has been entered.")

    # Remove first few rows and columns if they are empty
    start.row <- 1
    for (i in 1:nrow(m))
        if (all(m[i, ] == ""))
            start.row <- i + 1
        else
            break
    start.col <- 1
    for (i in 1:ncol(m))
        if (all(m[, i] == ""))
            start.col <- i + 1
    else
        break
    m <- m[start.row:nrow(m), start.col:ncol(m)]

    n.row <- nrow(m)
    n.col <- ncol(m)

    if (isTextNumeric(m)) {
        if (is.vector(m))
            as.numeric(m) # numeric vector, without labels
        else
            matrix(as.numeric(m), nrow = n.row) # numeric matrix, without labels
    } else if (is.vector(m)) # character vector
        m
    else if (n.col == 2 && isTextNumeric(m[, 2])) # numeric vector with labels
        structure(as.numeric(m[, 2]), names = m[, 1])
    else if (isTextNumeric(m[2:n.row, 2:n.col])) # numeric matrix with labels
    {
        numeric.m <- matrix(as.numeric(m[2:n.row, 2:n.col, drop = FALSE]), nrow = n.row - 1)
        colnames(numeric.m) <- m[1, 2:n.col]
        rownames(numeric.m) <- m[2:n.col, 1]
        numeric.m
    }
    else if (isNumericMatrixWithLabelsAndTitles(m)) # numeric matrix with row and column labels and titles
    {
        numeric.m <- matrix(as.numeric(m[3:n.row, 3:n.col, drop = FALSE]), nrow = n.row - 2)
        colnames(numeric.m) <- m[2, 3:n.col]
        rownames(numeric.m) <- m[3:n.col, 2]
        attr(numeric.m, "row.column.names") <- c(m[3, 1], m[1, 3])
        numeric.m
    }
    else # character matrix
        m
}

isTextNumeric <- function(t)
{
    all(!is.na(suppressWarnings(as.numeric(t))) | t == "")
}

isNumericMatrixWithLabelsAndTitles <- function(m)
{
    n.row <- nrow(m)
    n.col <- ncol(m)
    result <- n.row >= 3 && n.col >= 3 && m[3, 1] != "" && m[1, 3] != "" && all(m[1:2, 1:2] == "") && isTextNumeric(m[3:n.row, 3:n.col])
    if (n.row > 3)
        result <- result && m[4:n.row, 1] == ""
    if (n.col > 3)
        result <- result && m[4:n.col, 1] == ""
    result
}
