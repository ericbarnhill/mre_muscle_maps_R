test_levels <- function(x,y) {
    for (lev in levels(x$pix_id)) {
        match = y[which(lev== y$pix_id),]
        
        if (is.na(match)) {
            print(lev)
        } else {
           #match %>% dim $%>% print
            print(dim(match)[2])
        }
    }
}

test_ag <- function(x) {
    tally = 0
    for (lev in levels(x$pix_id)) {
        match = x[which(lev== x$pix_id),]
        
        if (is.na(match)) {
            print(paste("na match:", lev))
        } else if (dim(match)[1] < 2) {
            print("not a double match")
        } else {
            tally = tally + 1
        }
    }
    print(tally)
}