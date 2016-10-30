#' \code{AdjustDataToReflectWeights}
#'
#' Creates a new \code{\link{data.frame}} to reflect weights.
#'
#' @param data A \code{\link{data.frame}}.
#' @param weights The sampling or replication weights.
#' @param seed The seed used in random number generation. If \code{NULL}, the seed is not set.
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
#'
#' @param data A \code{data.frame} or \code{matrix}.
#' @param method The standardization method. Takes values \code{"z-scores"},
#'  \code{"Mean centered"},\code{"Range [-1,1]"}, \code{"Range [0,1]"},
#'  and \code{"Standard deviation of 1"}.
#' @param no.variation If \code{"ignore"}, the absence of variation is
#' ignored. Other options are \code{"warn"} and \code{"stop"}.
#' @param no.variation.value The value to assign to data where there
#' is no variance, if the method requires variation.
#' @param mean.zero If the method is \code{"Mean of 1"} and the mean
#' is 0. Options are \code{"ignore"}, \code{"warn"} and \code{"stop"}.
#' @details Mean of 1 multiples by a constant to set the mean to 1,
#' whereas \code{"Mean centered"} subtracts a constant such that each
#' variable has a mean of 0.
#' @importFrom stats sd
#' @export
StandardizeData <- function(data, method, no.variation = "warn", no.variation.value = 0, mean.zero = "warn")
{
    require.variation <- method == "z-scores" ||
                         method == "Range [-1,1]" ||
                         method == "Range [0,1]" ||
                         method == "Standard deviation of 1"
    if (require.variation)
    {
        sd.0 <- apply(data, 2, sd) == 0
        if (no.variation != "ignore" && any(sd.0))
        {
            vars <- paste("There is no variation in the values of", collapseNames(colnames(data)[sd.0]))
            if (no.variation == "stop")
                stop(vars)
            else
                warning(paste0(vars, ". Values that could not be transformed have been set to ", no.variation.value))
        }
    }
    else if (method == "Mean of 1")
    {
        mean.0 <- apply(data, 2, mean) == 0
        if (mean.zero != "ignore" && any(mean.0))
        {
            vars <- paste("The values for", collapseNames(colnames(data)[mean.0]), "have a mean of 0.")
            if (mean.zero == "stop")
                stop(vars)
            else
                warning(paste(vars, "They have not been standardized."))
        }
    }
    result <- switch(method,
                     "z-scores" = scale(data, center = TRUE, scale = TRUE),
                     "Mean centered" = scale(data, center = TRUE, scale = FALSE),
                     "Range [-1,1]" = apply(data, 2, function(x) x / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))),
                     "Range [0,1]" = apply(data, 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))),
                     "Mean of 1" = apply(data, 2, function(x) x / mean(x, na.rm = TRUE)),
                     "Standard deviation of 1" = apply(data, 2, function(x) x / sd(x, na.rm = TRUE)))
    rownames(result) <- rownames(data)
    colnames(result) <- colnames(data)
    if (require.variation && any(sd.0))
        result[, sd.0] <- no.variation.value
    else if (method == "Mean of 1" && any(mean.0))
        result[, mean.0] <- data[, mean.0]
    result
}

collapseNames <- function(names, max.names = 10)
{
    if (length(names) > max.names)
        paste0(paste0(names[1:max.names], collapse = ", "), ", ...")
    else
        paste0(names, collapse = ", ")
}
