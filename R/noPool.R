
## Generic function
noPool <- function(x, mu=NULL, se=NULL, num=NULL, denom=NULL) {
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
    } else {
        UseMethod("noPool")
    }
}

## No default function implementation yet
noPool.default <- function(x, ...) {
    stop("Error: Invalid class.")
}

## Normally distributed variables
noPool.normal <- function(x, mu, se) {
    ## Compute lower and upper confidence limits
    x[["lcl95"]] <- x[[mu]] - (qnorm(1-(0.05/2)) * x[[se]])
    x[["ucl95"]] <- x[[mu]] + (qnorm(1-(0.05/2)) * x[[se]])
    ## Return input object
    return( x )
}

## Pearson-Clopper
clopper.pearson <- Vectorize(function(n, m) {
    as.numeric(binom.test(n, m, conf.level=(1 - 0.05))[["conf.int"]])
})

## Binomial/Bernoulli distributed variables
noPool.binomial <- function(x, num, denom) {
    ## Compute lower and upper confidence limits
    limits <- clopper.pearson(x[[num]], x[[denom]])
    x[["lcl95"]] <- limits[1,]
    x[["ucl95"]] <- limits[2,]
    ## Return input object
    return( x )
}

