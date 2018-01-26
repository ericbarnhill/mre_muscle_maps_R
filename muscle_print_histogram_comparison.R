print_histogram_comparison <- function(data_tall, sample_hist_on, sample_hist_off) {
    XLIMS = c(0, 4000)
    par(mfrow=c(2,2))
    hist(data_tall$value[which(data_tall$cond_id == "Pre" & data_tall$musc_id == "VL")], 
         main="Orig Pre", breaks=32, xlim = XLIMS, xlab=NULL, ylab=NULL)
    hist(data_tall$value[which(data_tall$cond_id == "Post" & data_tall$musc_id == "VL")],
         main="Orig Post", breaks=32, xlim = XLIMS, xlab=NULL, ylab=NULL)
    hist(sample_hist_off, main="Samples Pre", breaks=32, xlim = XLIMS, xlab=NULL, ylab=NULL)
    hist(sample_hist_on, main="Samples Post", breaks=32, xlim = XLIMS, xlab=NULL, ylab=NULL)
}