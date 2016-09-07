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
