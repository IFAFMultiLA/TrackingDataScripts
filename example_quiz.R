library(dplyr)
library(jsonlite)

sess <- read.csv('data/2023-06-12_133039_476bc20dbf/tracking_sessions.csv')
events <- read.csv('data/2023-06-12_133039_476bc20dbf/tracking_events.csv')

quizdata <- left_join(sess, events, by = c('track_sess_id')) %>%
    filter(event_type == "learnr_event_question_submission")

head(quizdata)

events_parsed <- lapply(quizdata$event_value, fromJSON)

quizdata$quiz_label <- sapply(events_parsed, function(quizevent) { quizevent$label })
quizdata$answ_correct <- sapply(events_parsed, function(quizevent) { quizevent$correct })
