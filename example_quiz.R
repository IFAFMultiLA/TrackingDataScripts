library(dplyr)
library(jsonlite)
library(ggplot2)


sess <- read.csv('data/tracking_sessions.csv')
events <- read.csv('data/tracking_events.csv')

quizdata <- left_join(sess, events, by = c('track_sess_id')) |>
    filter(event_type == "learnr_event_question_submission")

head(quizdata)

events_parsed <- lapply(quizdata$event_value, fromJSON)

quizdata$quiz_label <- sapply(events_parsed, function(quizevent) { quizevent$label })
quizdata$answ_correct <- sapply(events_parsed, function(quizevent) { quizevent$correct })

quizdata <- select(quizdata, track_sess_id, track_sess_start, track_sess_end, event_time, quiz_label, answ_correct) |>
    mutate(quiz_label = as.factor(quiz_label),
           track_sess_start = as.POSIXct(gsub("T", " ", track_sess_start)),
           track_sess_end = as.POSIXct(gsub("T", " ", track_sess_end)),
           event_time = as.POSIXct(gsub("T", " ", event_time)))

head(quizdata)

quiztries <- group_by(quizdata, track_sess_id, quiz_label) |>
    arrange(track_sess_id, quiz_label, event_time) |>
    mutate(quiz_try = row_number()) |>
    filter(answ_correct) |>
    select(-answ_correct) |>
    ungroup()

head(quiztries)

tries_per_label <- select(quiztries, quiz_label, quiz_try) |>
    group_by(quiz_label) |>
    summarise(n = n(),
              mean_tries = mean(quiz_try),
              median_tries = median(quiz_try),
              sd_tries = sd(quiz_try),
              se_tries = sd(quiz_try) / sqrt(n)) |>
    ungroup()


head(tries_per_label)

ggplot(tries_per_label, aes(quiz_label, mean_tries)) +
    geom_pointrange(aes(ymin = mean_tries - sd_tries, ymax = mean_tries + sd_tries)) +
    theme_minimal() +
    labs(title = "Anzahl Versuche bis zur Lösung pro Quiz", x = "Quiz", y = "Versuche")
