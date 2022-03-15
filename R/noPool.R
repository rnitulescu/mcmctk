
## Generic function
noPool <- function(x, clusterID=NULL, mu=NULL, se=NULL, num=NULL, denom=NULL) {
    ## Error checking
    if ( length(intersect(class(x), c("data.frame", "data.table"))) == 0) {
        stop("Error: 'x' is not a data.frame nor a data.table.")
    } else if (!( (!is.null(mu) & !is.null(se)) | (!is.null(num) & !is.null(denom)) )) {
        stop("Error: Please supply either 'mu' and 'se' or 'num' and 'denom'.")
    } else if ( !is.null(denom) ) {
        if ( any(x[[denom]] == 0) ) {
            stop("Error: 'denom' cannot be zero.")
        } else {
            UseMethod("noPool")
        }
    } else if (is.null(clusterID)) {
        stop("Error: 'clusterID' not provided.")
    } else {
        UseMethod("noPool")
    }
}

## No default function implementation yet
noPool.default <- function(x, ...) {
    stop("Error: Invalid class.")
}

## Normally distributed variables
noPool.normal <- function(x, clusterID, mu, se) {
    ## Standardize data format
    x[["clusterID"]] <- x[[clusterID]]
    x[["mu"]] <- x[[mu]]
    x[["se"]] <- x[[se]]
    ## Compute lower and upper confidence limits
    x[["lcl95"]] <- x[[mu]] - (qnorm(1-(0.05/2)) * x[[se]])
    x[["ucl95"]] <- x[[mu]] + (qnorm(1-(0.05/2)) * x[[se]])
    ## Return input object
    x <- x[order(x[["clusterID"]]),]
    return( x[c("clusterID", "mu", "se", "lcl95", "ucl95")] )
}

## Pearson-Clopper
clopper.pearson <- Vectorize(function(n, m) {
    as.numeric(binom.test(n, m, conf.level=(1 - 0.05))[["conf.int"]])
})

## Binomial/Bernoulli distributed variables
noPool.binomial <- function(x, clusterID, num, denom) {
    ## Standardize data format
    x[["clusterID"]] <- x[[clusterID]]
    x[["num"]] <- x[[num]]
    x[["denom"]] <- x[[denom]]
    ## Compute lower and upper confidence limits
    limits <- clopper.pearson(x[[num]], x[[denom]])
    x[["proportion"]] <- x[[num]] / x[[denom]]
    x[["lcl95"]] <- limits[1,]
    x[["ucl95"]] <- limits[2,]
    ## Return input object
    x <- x[order(x[["clusterID"]]),]
    return( x[c("clusterID", "num", "denom", "proportion", "lcl95", "ucl95")] )
}

