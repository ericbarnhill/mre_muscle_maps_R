print_histogram_comparison <- function(data_tall, sample_hist_on, sample_hist_off) {
    par(mar=c(2,2,2,2))
    XLIMS = c(0, 4000)
    BREAKS = seq(0, 4000, 40)
    par(mfrow=c(3,2))
    orig_hist_off <- data_tall$value[which(data_tall$cond_id == 
                                               "Pre" & data_tall$musc_id == "VI")];
    orig_hist_on <- data_tall$value[which(data_tall$cond_id == 
                                              "Post" & data_tall$musc_id == "VI")];
    o1 <- hist(orig_hist_off, main="Orig Pre", breaks=BREAKS, xlim = XLIMS, xlab=NULL, ylab=NULL)
    o2 <- hist(orig_hist_on,  main="Orig Post", breaks=BREAKS, xlim = XLIMS, xlab=NULL, ylab=NULL)
    s1 <- hist(sample_hist_off, main="Samples Pre", breaks=BREAKS, xlim = XLIMS, xlab=NULL, ylab=NULL)
    barplot(o2$counts[1:40] - o1$counts, main="Diff orig")
    barplot(s2$counts - s1$counts[1:37], main="Diff samples")
}