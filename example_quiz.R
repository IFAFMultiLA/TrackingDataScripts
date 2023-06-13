library(dplyr)
library(jsonlite)
library(ggplot2)

# load CSVs for data from a single application session

sess <- read.csv('data/tracking_sessions.csv')
events <- read.csv('data/tracking_events.csv')

# join tracking session and event data by tracking session ID and filter for only "question submission" events

quizdata <- left_join(sess, events, by = c('track_sess_id')) |>
    filter(event_type == "learnr_event_question_submission")

head(quizdata)

# parse "event_value" JSON text to extract quiz label and and whether the quiz was solved correctly
events_parsed <- lapply(quizdata$event_value, fromJSON)

quizdata$quiz_label <- sapply(events_parsed, function(quizevent) { quizevent$label })
quizdata$answ_correct <- sapply(events_parsed, function(quizevent) { quizevent$correct })

# select relevant columns, transform quiz_label to factor and times to proper type
quizdata <- select(quizdata, track_sess_id, track_sess_start, track_sess_end,
                   event_time, quiz_label, answ_correct) |>
    mutate(quiz_label = as.factor(quiz_label),
           track_sess_start = as.POSIXct(gsub("T", " ", track_sess_start)),
           track_sess_end = ifelse(track_sess_end == "", NA, track_sess_end),
           event_time = as.POSIXct(gsub("T", " ", event_time))) |>
    mutate(track_sess_end = as.POSIXct(gsub("T", " ", track_sess_end)))

head(quizdata)

# find out the number of tries until a quiz in a given tracking session was solved (if ever)
quiztries <- group_by(quizdata, track_sess_id, quiz_label) |>
    arrange(track_sess_id, quiz_label, event_time) |>
    mutate(quiz_try = row_number()) |>
    filter(answ_correct) |>
    select(-answ_correct) |>
    ungroup()

head(quiztries)

#ggplot(quiztries, aes(quiz_label, quiz_try)) +
#    geom_boxplot()

# calculate some summary statistics per quiz label
tries_per_label <- select(quiztries, quiz_label, quiz_try) |>
    group_by(quiz_label) |>
    summarise(n = n(),
              mean_tries = mean(quiz_try),
              median_tries = median(quiz_try),
              sd_tries = sd(quiz_try),
              se_tries = sd(quiz_try) / sqrt(n)) |>
    ungroup()

tries_per_label

# plot these summary statistics
ggplot(tries_per_label, aes(quiz_label, mean_tries)) +
    geom_pointrange(aes(ymin = mean_tries - sd_tries, ymax = mean_tries + sd_tries)) +
    theme_minimal() +
    labs(title = "Anzahl Versuche bis zur Lösung pro Quiz", x = "Quiz", y = "Versuche")
