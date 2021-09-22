poem <- paste("Inside me is a skeleton, of this I have no doubt,",
"now it’s got my flesh on, but it’s waiting to get out.")
poem_words <- strsplit(poem, " ")[[1]]

# Ex 1
poem_without_fullstops <- gsub(",|\\.", "", poem_words)

tabulate(nchar(poem_words))

# 2
comma_period_position <- grep(",|\\.", poem_words)
cat(poem_words)
poem_words[comma_period_position] <- paste(poem_words[comma_period_position], "\n", sep = " ")
cat(poem_words)

#3
set.seed(0);
y <-rt(100,df=4)
hist(y)
y_mean <- mean(y)
y_sd <- sd(y)

close_y <- abs(y_mean -y) <= 2*y_sd
y[close_y]
close_y_mean <- mean(close_y)
close_y_std <- sd(close_y)


super_close_ys <- rm_k_times_sd_dist_from_mean(y, k=1)
#4
rm_k_times_sd_dist_from_mean <- function(y, k=2) {
    y_mean <- mean(y)
    y_sd <- sd(y)

    close_y <- abs(y_mean -y) <= k*y_sd
    y[close_y]
    close_y_mean <- mean(close_y)
    close_y_mean
}

new_y_mean <- rm_k_times_sd_dist_from_mean(y,k=0)
