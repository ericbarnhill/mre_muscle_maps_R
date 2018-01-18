path="data/"


load_clean_data <- function(path) {
    data_tall <- NULL
    s_dir <- dir(path)
    for (s in s_dir) {
        if (substr(s, 1, 1) == "P") {
            m_dir <- dir(file.path(path, s))
            for (m in m_dir) {
                m_file <- read.csv(file.path(path, s, m), header = TRUE)
                # downsample
                m_file <- m_file[which((m_file$X %% 4 == 0) & (m_file$Y %% 4 == 0)),]                
                if (is.null(data_tall)) {
                    data_tall <- as.data.frame(m_file)
                } else {
                    data_tall <- rbind(data_tall, m_file)
                }
            }
        }
    }
    # clean the data
    data_tall <- data_tall[which((data_tall$cond_id == "Pre") | (data_tall$cond_id == "Post")),]
    data_tall$cond_id <- factor(data_tall$cond_id, levels(data_tall$cond_id)[c(3,1)])
    data_tall$pix_id <- as.factor(data_tall$pix_id) 
    data_tall <- data_tall[which(data_tall$Value > 0),]
    return(data_tall)
}