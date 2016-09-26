#' \code{AdjustDataToReflectWeights}
#'
#' Creates a new \code{\link{data.frame}} to reflect weights.
#'
#' @param data A \code{\link{data.frame}}.
#' @param weights The sampling or replication weights.
#' @param seed The seed used in random number generation.
#' @details In situations where an algorithm does not accomodate weights, this
#' function modifies the \code{\link{data.frame}} by either: (A) stretching it
#' out, where the the weights are integers, or (B) resampling to create a new
#' bootstrapped \code{\link{data.frame}}, where the \code{weights} are
#' proportional to the probability of selection. When creating the bootstrap
#' sample, the sample size is whichever is greatest of the rounded sum and 1.
#'# Inspired by Zelig, 13-11-15.

#' @export
AdjustDataToReflectWeights <- function(data, weights, seed = 123)
{
    set.seed(seed)
    n <- nrow(data)
    all.weights.are.integers <- all(weights %% 1 == 0)
    if (all.weights.are.integers) # All weights are integers
    {   # Reproducing cases according to the values of the weights.
        replicants <- rep(seq_len(n), weights)
    }
    else
    {   # Creating bootstrapped data file by resampling.
        warning("Weights have been applied, but the algorithm you have selected ",
            "is only able to use integer valued weights. ",
            "A bootstrapped version of the dataset was constructed using the ",
            "weights as sample probabilities.")
        sum.weights <- max(round(sum(weights)), 1)
        replicants <- sample.int(n, size = sum.weights,
            replace = TRUE, prob = weights / sum.weights)
    }

    return(data[replicants, ])
}


#' RemoveMissingLevelsFromFactors
#' @param data A \code{data.frame}.
#' @export
RemoveMissingLevelsFromFactors <- function(data)
{
    for (i in seq_along(data))
    {
        v <- data[, i]
        if (is.ordered(v))
            data[, i] <- Ordered(v)
        else if (is.factor(v))
            data[, i] <- Factor(v)
    }
    return(data)
}



#' StandardizeData
#' @param data A \code{data.frame} or \code{matrix}.
#' @param method The standardization method. Takes values \code{"z-scores"}, \code{"Range [-1,1]"},
#'  \code{"Range [0,1]"}, \code{"Maximum magnitude of 1"}, \code{"Mean of 1"} and \code{"Standard deviation of 1"}.
#' @param no.variation If \code{"ignore"}, the absence of variation is ignored. Other options are \code{"warn"} and \code{"stop"}.
#' @param no.variation.value The value to assign to data where there is no variance.
#' @importFrom stats sd
#' @export
StandardizeData <- function(data, method, no.variation = "warn", no.variation.value = 0)
{
    sd.0 <- apply(result, 2, sd) == 0
    if (no.variation != "ignore")
    {
        if (any(sd.0))
        {
            vars <- paste("The following variables have no variation:", paste0(names(data)[which(no.variation)], collapse = ", "))
            if (no.variation == "stop")
                stop(vars)
            else
                warning(paste(vars,"Values that could not be transformed have been set to", no.variation.value))
        }
    }
    result <- switch(method,
                     "z-scores" = scale(data),
                     "Smallest is 0" = data - apply(data, 2, min, na.rm = TRUE),
                     "Range [0,1]" = {df <- StandardizeData(data, "Smallest is 0")
                                      df / apply(df, 2, max, na.rm = TRUE)}
                     "Range [-1,1]" = StandardizeData(data, "Range [0,1]") * 2 - 1)
    if (is.na(no.variation.value) & any(sd.0))
        result[, sd.0][!is.na(result[, sd.0])] <- no.variation.value


    if (nchar(var.names.warning) > 0) {
        if (require.variation)
            warning(paste("The values for", var.names.warning,
                          "have been set to zero as they have no variation and cannot be standardized using the method:", method))
        else if (method == "Mean of 1")
            warning(paste("The values for", var.names.warning,
                          "have a mean of 0 and they cannot be standardized to have a mean of 1 by scaling."))
    }
    result

}



oldStandardizeData <- function(data, method)
{
    require.variation <- method == "z-scores" ||
                         method == "Range [-1,1]" ||
                         method == "Range [0,1]" ||
                         method == "Standard deviation of 1"

    result <- data
    var.names.warning <- ""
    for (i in 1:ncol(result)) {
        vec <- result[, i]
        min.val <- min(vec)
        max.val <- max(vec)
        if (max.val == min.val && require.variation) {
            result[, i] <- rep(0, length(vec))
            if (nchar(var.names.warning) == 0)
                var.names.warning <- colnames(data)[i]
            else if (nchar(var.names.warning) > 100)
                var.names.warning <- paste0(var.names.warning, "...")
            else
                var.names.warning <- paste0(var.names.warning, ", ", colnames(data)[i])
        } else {
            if (method == "z-scores") {
                result[, i] <- scale(vec)
            } else if (method == "Range [-1,1]") {
                result[, i] <- vec / (max.val - min.val)
            } else if (method == "Range [0,1]") {
                result[, i] <- (vec - min.val) / (max.val - min.val)
            } else if (method == "Maximum magnitude of 1") {
                max.mag <- max(abs(vec))
                if (max.mag > 0)
                    result[, i] <- vec / max(abs(vec))
            } else if (method == "Mean of 1") {
                mean.vec <- mean(vec)
                if (mean.vec != 0)
                    result[, i] <- vec / mean(vec)
                else {
                    if (nchar(var.names.warning) == 0)
                        var.names.warning <- colnames(data)[i]
                    else if (nchar(var.names.warning) > 100)
                        var.names.warning <- paste0(var.names.warning, "...")
                    else
                        var.names.warning <- paste0(var.names.warning, ", ", colnames(data)[i])
                }
            } else if (method == "Standard deviation of 1") {
                result[, i] <- vec / sd(vec)
            }
        }
    }
    if (nchar(var.names.warning) > 0) {
        if (require.variation)
            warning(paste("The values for", var.names.warning,
                          "have been set to zero as they have no variation and cannot be standardized using the method:", method))
        else if (method == "Mean of 1")
            warning(paste("The values for", var.names.warning,
                          "have a mean of 0 and they cannot be standardized to have a mean of 1 by scaling."))
    }
    result
}
