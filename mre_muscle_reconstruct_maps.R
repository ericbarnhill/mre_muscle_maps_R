reconstruct_maps <- function(data_tall, csim, hdis, means) {
    ROPE = 50;
    max_x <- max(data_tall$x)
    max_y <- max(data_tall$y)
    hi_map <- matrix(nrow=max_x, ncol=max_y)
    lo_map <- matrix(nrow=max_x, ncol=max_y)
    cred_map <- matrix(nrow=max_x, ncol=max_y)
    
    nPix <- 4070
    for (n in 1:nPix) {
        x <- data_tall$x[n]
        y <- data_tall$y[n]
        post_lo <- hdis[n,1]
        post_hi <- hdis[n,2]
        lo <- exp(means[n] + post_lo)
        hi <- exp(means[n] + post_hi)
        lo_map[x,y] <-lo
        hi_map[x,y] <-hi
        if ((sign(post_lo) == sign(post_hi))) {
            cred_map[x,y] <- exp(means[n] + means[n+nPix])
        } else {
            cred_map[x,y] = 0
        }
    }
    maps <- list(hi_map=hi_map, lo_map=lo_map, cred_map=cred_map)
}