samples_to_hdis <- function(samples) {
    n_pix <- ncol(samples)
    n_vars <- n_pix / 2
    lo <- vector(length = n_vars)
    hi <- vector(length = n_vars)
    for (n in 1:n_vars) {
        a <- mean(samples[,n])
        h <- hdi(samples[,n+n_vars], credMass = 0.9)
        lo[n] <- h[1] / a
        hi[n] <- h[2] / a
    }
    hdis <- cbind(lo, hi)
}