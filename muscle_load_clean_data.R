
# some globals
proj_path="/home/ericbarnhill/Documents/code/R/mre_muscle_maps_R/"
data_path="data/"


load_clean_data <- function(path, ds) {
    require(plyr)
    require(magrittr)
    data_tall <- NULL
    s_dir <- dir(path)
    for (s in s_dir) {
        if (substr(s, 1, 1) == "P") {
            m_dir <- dir(file.path(path, s))
            for (m in m_dir) {
                m_file <- read.csv(file.path(path, s, m), header = TRUE)
                # rep global data
                m_file[,3:(ncol(m_file)-3)] <- apply(m_file[1,3:(ncol(m_file)-3)], 2, function(x) {rep(x, nrow(m_file))}) 
                # downsample
                m_file <- m_file[which((m_file$X %% ds == 0) & (m_file$Y %% ds == 0)),]
                # adjust values
                m_file <- m_file %>% mutate(X = X/ds, Y = Y/ds, pix_id = (448/ds)*X + Y)
                if (is.na(max(m_file$subj_id))) {
                    print(paste("Max ID:",  max(m_file$subj_id)))
                }
                if (is.null(data_tall)) {
                    data_tall <- as.data.frame(m_file)
                } else {
                    data_tall <- rbind(data_tall, m_file)
                }
            }
        }
    }
    # clean the data
    data_tall <- data_tall[which(data_tall$Value > 0),]
    data_tall <- data_tall[,-c(2,7)]
    colnames(data_tall)[6:8] <- c("x", "y", "value")
    data_tall[,1:5] <-lapply(data_tall[,1:5], factor)
    return(data_tall)
}

pix_id_to_xy <- function(ci, data_tall) {
    pix_names <- rownames(ci)[nrow(ci)/2+2:nrow(ci)]
    pix_ids <- vector(length = nrow(ci)-1)
    x <- vector(length = nrow(ci)-1)
    y <- vector(length = nrow(ci)-1)
    ci_lo <- vector(length = nrow(ci)-1)
    ci_hi <- vector(length = nrow(ci)-1)
    for (ind in seq_along(pix_names)) {
        ind_adj <- nrow(ci)/2+ind
        pix_name <- pix_names[ind];
        pix_ids[ind] <- substr(pix_name,7,nchar(pix_name)-11)
        orig_row <- head(data_tall[which(
            data_tall$pix_id == pix_ids[ind]
        ),], n=1)
        if (dim(orig_row)[1] > 0) {
            x[ind] <- orig_row$x
            y[ind] <- orig_row$y
            ci_lo[ind] <- ci[ind_adj,1]
            ci_hi[ind] <- ci[ind_adj,2]
        }
    }
    ci_df <- data.frame(x,y,ci_lo,ci_hi)
}

xy_to_matrix <- function(ci_df) {
    lo_map <- matrix(nrow=112, ncol=112)
    hi_map <- matrix(nrow=112, ncol=112)
    for (n in 1:nrow(ci_df)) {
        x <- ci_df$x[n]
        y <- ci_df$y[n]
        lo_map[x,y] <- ci_df$ci_lo[n]
        hi_map[x,y] <- ci_df$ci_hi[n]
    }
    maps <- list(lo_map=lo_map, hi_map=hi_map)
}