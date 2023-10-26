library(dplyr)
library(ggplot2)


tracking_data <- readRDS("temp/tracking_data.rds")

count(tracking_data, track_sess_id)


sess_data <- filter(tracking_data, track_sess_id == 126)


table(sess_data$type)
