require(tidyr)
require(ggplot2)

muscle_predict_original_means <- function(orig_data, csim, byMusc) {
    orig_means <- get_orig_means(orig_data, byMusc)
    
    pre_means  <- orig_means[which(orig_means$cond_id == "Pre"),]
    post_means  <- orig_means[which(orig_means$cond_id == "Post"),]
    pix_in_both <- intersect(pre_means$pix_id, post_means$pix_id)
    pre <- pre_means[which(pre_means$pix_id %in% pix_in_both),]
    post <- post_means[which(post_means$pix_id %in% pix_in_both),]
    levs_in_both <- levels(orig_means$pix_id) %in% pix_in_both
    levs_for_sims <- rep(levs_in_both, 2)
    sim_col_means <- colMeans(csim)
    sims_in_both <- unname(sim_col_means[levs_for_sims])
    
    musc_levs <- levels(orig_data$musc_id)
    nMusc <- length(musc_levs)
    #N <- length(musc_ids) edited to remove null column
    if (byMusc) {
        pred_means <- vector(length=nMusc*2)
        for (n in 1:nMusc) {
            beta_off_ind <- n
            beta_on_ind <- n + nMusc
            delta_off_ind <- n + 2*nMusc
            delta_on_ind <- n + 3*nMusc
            shape_off <- sim_col_means[delta_off_ind]
            shape_on <- shape_off + sim_col_means[delta_on_ind]
            pred_off <- sim_col_means[beta_off_ind]
            pred_on <- pred_off + sim_col_means[beta_on_ind]
            rate_off <- shape_off / exp(pred_off)
            rate_on <- shape_on / exp(pred_on)
            samples_off <- rgamma(10000, shape_off, rate_off)
            samples_on <- rgamma(10000, shape_on, rate_on)
            # save out sample historgrams for markdown
            if (n == 10) {
                sample_hist_off <- samples_off
                sample_hist_on <- samples_on
                save(list=c("sample_hist_off", "sample_hist_on"), file = "sample_histograms.RData")
            }
            mean_off <- mean(samples_off)
            mean_on <- mean(samples_on)
            pred_means[n] <- mean_off
            pred_means[n + nMusc] <- mean_on
        }
    } else { #map by pixel
        nPix <- length(sims_in_both)
        pred_means <- vector(length=nPix*2)
        for (n in 1:nPix) {
            pix_id <- orig_data$pix_id[n]
            #pix_musc_id <- orig_data$musc_id[which(
            #    orig_data$pix_id == lev
            #)]
            #musc_ind <- musc_inds[which(
             #   musc_ind == pix_musc_id
            #)]
            beta0_ind <- n
            beta1_on_ind <- n + nPix
            #delta_off_ind <- n + 2*nPix
            #delta_on_ind <- n + 3*nPix
            #shape_off <- sim_col_means[delta_off_ind]
            #shape_on <- shape_off + sim_col_means[delta_on_ind]
            #pred_off <- sim_col_means[beta_off_ind]
            #pred_on <- pred_off + sim_col_means[beta_on_ind]
            #rate_off <- shape_off / exp(pred_off)
            #rate_on <- shape_on / exp(pred_on)
            #samples_off <- rnorm(10000, shape_off, rate_off)
            #samples_on <- rnorm(10000, shape_on, rate_on)
            #mean_off <- mean(samples_off)
            #mean_on <- mean(samples_on)
            pred_means[n] <- exp(sim_col_means[beta0_ind])
            pred_means[n + nPix] <- exp(sim_col_means[beta0_ind + beta1_on_ind])
        }
    }
    pred_means <- pred_means[-c(13,26)]
    means <- data.frame(orig_means, pred_means)

    #<- pre_means[which(pre$cond_id %in% pix_in_both)]
    
    return(means)
}

get_orig_means<- function (orig_data, byMusc) {
    if (byMusc) {
        data_cropped = orig_data[,c("musc_id",  "cond_id", "value")]
        orig_means <- aggregate(value ~ musc_id + cond_id, orig_data, mean)
    } else {
        data_cropped = orig_data[,c("pix_id",  "cond_id", "value")]
        orig_means <- aggregate(value ~ pix_id + cond_id, orig_data, mean)
    }
    colnames(orig_means)[3] <- "orig_means"
    return(orig_means)
}

plot_predictive_means_comparison <- function(means) {
    means_tall <- gather(means, "type", "value", 3:4)
    plt <- ggplot(means_tall) +
        geom_line(aes(x=musc_id, y=value, group=type, col=type)) +
        facet_grid(. ~ cond_id) +
        theme(legend.position = "bottom")
    print(plt)
}