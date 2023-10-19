options(digits.secs = 4)

library(dplyr)
library(jsonlite)

# load CSVs for data from a single application session

sess <- read.csv('data/tracking_sessions.csv')
events <- read.csv('data/tracking_events.csv')

# counts per event type
table(events$event_type)

# join tracking session and event data by tracking session ID and filter for only "mouse" events

mousedata <- left_join(sess, events, by = c('track_sess_id')) |>
    filter(event_type == "mouse") |>
    mutate(track_sess_start = as.POSIXct(gsub("T", " ", track_sess_start)),
           track_sess_end = as.POSIXct(gsub("T", " ", ifelse(track_sess_end == "", NA, track_sess_end))),
           event_time = as.POSIXct(gsub("T", " ", event_time))) |>
    select(-c(app_sess_code, event_type))

#events_parsed <- lapply(mousedata$event_value, fromJSON)
#mousedata$event_value <- NULL

# count mouse event chunks per tracking session
group_by(mousedata, user_app_sess_code, track_sess_id, track_sess_start, track_sess_end) |>
    count() |>
    ungroup() |>
    arrange(desc(track_sess_start))


# function to parse a single mouse tracking frame passed as list `frame` as collected by mus.js
# returns a dataframe with a single row for the row
parse_mousetracking_frame <- function(frame) {
    frame <- sapply(frame, function(x) { ifelse(is.null(x), NA_character_, x) })

    if (!(frame[1] %in% c("w", "s", "m", "c", "i"))) {
        return(NULL)
    }

    res <- data.frame(
        timestamp = as.integer(frame[length(frame)]),  # always at last position
        type = NA_character_,   # one of "window", "scroll", "mouse", "click", "input"
        coord1 = NA_real_,      # for dimensions such as window size, this means width, otherwise this means x position
        coord2 = NA_real_,      # for dimensions such as window size, this means height, otherwise this means y position
        xpath = NA_character_,  # XPath to HTML element
        css = NA_character_,    # CSS selector to HTML element
        value = NA_character_   # input value for type "input"
    )

    if (frame[1] != "i")  {
        coord <- as.numeric(frame[2:3])
        res$coord1 <- coord[1]
        res$coord2 <- coord[2]
    }


    if (frame[1] == "w") {              # window size change
        res$type <- "window"
    } else if (frame[1] == "s") {       # scroll position change
        res$type <- "scroll"
    } else if (frame[1] == "m") {       # mouse position change
        res$type <- "mouse"
        res$xpath <- frame[4]
        if (length(frame) > 5) {   # new format with CSS selector
            res$css <- frame[5]
        }
    } else if (frame[1] == "c") {       # mouse click
        res$type <- "click"
        res$xpath <- frame[4]
        if (length(frame) > 5) {   # new format with CSS selector
            res$css <- frame[5]
        }
    } else {                            # key input "i"
        stopifnot(frame[1] == "i")
        res$type <- "input"
        res$xpath <- frame[2]

        if (length(frame) > 4) {   # new format with CSS selector
            res$css <- frame[3]
            res$value <- trimws(frame[4])
        } else {
            res$value <- trimws(frame[3])
        }
    }

    res
}


# extract all tracking data for all events of a single tracking session given as dataframe `tracking_sess_data`
# parses the mus.js tracking data frames that are given as JSON for each event and turns them into a dataframe
extract_tracking_data <- function(tracking_sess_data, tracking_sess_id) {
    tracking_sess_events <- lapply(tracking_sess_data$event_value, fromJSON, simplifyVector = FALSE)

    event_id <- 1
    frames_per_event <- lapply(tracking_sess_events,  function(event) {
        frames_df <- bind_rows(lapply(event$frames, parse_mousetracking_frame))
        frames_df$event_id <- event_id
        event_id <<- event_id + 1
        frames_df
    })
    frames_per_event <- bind_rows(frames_per_event) |>
        arrange(timestamp) |>
        select(event_id, everything())

    track_data <- data.frame(
        event_id = 1:nrow(tracking_sess_data),
        event_time = tracking_sess_data$event_time #,
        #time_elapsed = sapply(tracking_sess_events, function(event) { event$timeElapsed })
    ) |> left_join(frames_per_event, by = "event_id") |>
        mutate(event_time = event_time + timestamp/1000) |>
        select(-c(event_id, timestamp))
    if (all(order(track_data$event_time) != 1:nrow(track_data))) {
        warning(sprintf("order of events may be wrong for data in tracking session #%d", tracking_sess_id))
    }
    track_data
}

tracking_data <- group_by(mousedata, user_app_sess_code, track_sess_id, track_sess_start, track_sess_end) |>
    arrange(event_time) |>
    group_modify(extract_tracking_data)

saveRDS(tracking_data, "temp/tracking_data.rds")
