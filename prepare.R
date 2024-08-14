# Data preparation script.
#
# Transforms the raw tracking data for all application sessions given in folder `data/raw` to a dataframes that can
# be used for analyses. The output dataframes will be stored under `data/prepared`.
#
# Author: Markus Konrad <markus.konrad@htw-berlin.de>
# Date: Nov./Dec. 2023
#

options(digits.secs = 4)

library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)


FILTER_START_DATE <- ymd("2023-11-06")


# ---- function definitions ----


# function to extract data not related to mouse tracking; parses the JSON string of each `event_value` in a given
# dataframe `row` and returns a dataframe with a single row of parsed data
extract_other_tracking_data <- function(row, row_index) {
    if (is.null(row$event_value) || trimws(row$event_value) == "") {
        v <- list()
    } else {
        v <- fromJSON(row$event_value, simplifyVector = FALSE)
    }

    res <- data.frame(
        type = row$event_type,         # one of "chapter", "device_info_update", "input_change", "ex_result",
                                       # "ex_submit", "question_submit"
        chapter_index = NA_integer_,   # content chapter index
        chapter_id = NA_character_,    # content chapter ID
        ex_label = NA_character_,   # exercise/question label
        ex_id = NA_character_,      # exercise ID
        ex_event = NA_character_,   # exercise event: result or submitted
        ex_output = NA_character_,  # exercise output
        ex_correct = NA,            # exercise/question result correct for checked results?
        xpath = NA_character_,  # XPath to HTML element
        css = NA_character_,    # CSS selector to HTML element
        value = NA_character_,  # input value for type "input"
        coord1 = NA_real_,      # for dimensions such as window size, this means width
        coord2 = NA_real_       # for dimensions such as window size, this means height
    )

    if (row$event_type == "chapter") {
        res$chapter_index <- v$value$chapter_index
        res$chapter_id <- v$value$chapter_id
    } else if (row$event_type == "device_info_update") {
        type <- character()
        coord1 <- numeric()
        coord2 <- numeric()

        if (!is.null(v$window_size)) {
            type <- c(type, "window")
            coord1 <- c(coord1, v$window_size[[1]])
            coord2 <- c(coord2, v$window_size[[2]])
        }

        if (!is.null(v$main_content_viewsize)) {
            type <- c(type, "main_content_viewsize")
            coord1 <- c(coord1, v$main_content_viewsize[[1]])
            coord2 <- c(coord2, v$main_content_viewsize[[2]])
        }

        if (!is.null(v$main_content_scrollsize)) {
            type <- c(type, "main_content_scrollsize")
            coord1 <- c(coord1, v$main_content_scrollsize[[1]])
            coord2 <- c(coord2, v$main_content_scrollsize[[2]])
        }

        res <- res[rep(1, length(type)),]
        res$type <- type
        res$coord1 <- coord1
        res$coord2 <- coord2
    } else if (row$event_type == "input_change") {
        res$xpath <- v$xpath
        res$css <- paste0('#', v$id)
        res$value <- v$value
    } else if (row$event_type %in% c("learnr_event_exercise_result", "learnr_event_exercise_submitted")) {
        res$type <- ifelse(row$event_type == "learnr_event_exercise_result", "ex_result", "ex_submit")
        res$ex_label <- v$label
        res$ex_id <- v$id
        res$ex_event <- ifelse(row$event_type == "learnr_event_exercise_result", "result", "submitted")
        res$value <- v$code

        if (!is.null(v$output)) {
            res$ex_output <- paste(trimws(v$output), collapse = "\n")
        }

        if (!is.null(v$checked) && v$checked && !is.null(v$feedback$correct) && length(v$feedback$correct) == 1) {
            res$ex_correct <- v$feedback$correct
        }
    } else if (row$event_type == "learnr_event_question_submission") {
        res$type <- "question_submit"
        res$ex_label <- v$label

        if (!is.null(v$correct)) {
            res$ex_correct <- v$correct
        }

        if (length(v$answer) > 1) {
            res$value <- paste0("[", paste(paste0('"', v$answer, '"'), collapse = " ; "), "]")
        } else {
            res$value <- v$answer
        }
    } else if (row$event_type == "visibility_change") {
        res$value <- v$state
    } else if (row$event_type == "summary_shown") {
        res$value <- "summary_shown"
    } else if (row$event_type == "summary_topic_added") {
        res$css <- paste0('#', v$id)
        res$value <- v$key
    }  else {
        warning(paste("unknown event type:", row$event_type))
    }

    res
}


# function to parse a single mouse tracking frame passed as list `frame` as collected by mus.js;
# returns a dataframe with a single row of parsed data
parse_mousetracking_frame <- function(frame) {
    frame <- sapply(frame, function(x) { ifelse(is.null(x), NA_character_, x) })

    if (!(frame[1] %in% c("w", "s", "S", "m", "c", "i"))) {
        warning(paste("unknown frame type:", frame[1]))
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
    } else if (frame[1] == "s") {       # window scroll position change
        res$type <- "scroll"
    } else if (frame[1] == "S") {       # main content scroll position change
        res$type <- "contentscroll"
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


# function to extract all tracking data for all events of a single tracking session given as dataframe
# `tracking_sess_data`; parses the mus.js tracking data frames that are given as JSON for each event and turns them
# into a dataframe.
extract_mousetracking_data <- function(tracking_sess_data, tracking_sess_id) {
    tracking_sess_data$chunk_id <- 1:nrow(tracking_sess_data)
    tracking_sess_events <- lapply(tracking_sess_data$chunk_id, function(chunk_id) {
        parsed <- fromJSON(tracking_sess_data[chunk_id, ]$event_value, simplifyVector = FALSE)
        parsed$chunk_id <- chunk_id
        parsed
    })

    frames_per_event <- lapply(tracking_sess_events,  function(event) {
        # filter frames beforehand: don't take frames with "attribute change" events (noted as frame type "a")
        take_frames <- sapply(event$frames, function(f) {
            f[[1]] != "a"
        })

        filtered_frames <- event$frames[take_frames]

        if (length(filtered_frames) > 0) {
            # parse the filtered frames
            frames_df <- bind_rows(lapply(filtered_frames, parse_mousetracking_frame))

            # newer event data (since Nov. 28 2023) has a "startedAtISODate" value as time reference for the event
            # frame timestamps
            if (!is.null(event$startedAtISODate) && is.character(event$startedAtISODate)) {
                frames_df$mouse_tracking_starttime <- ymd_hms(event$startedAtISODate)
            } else {
                frames_df$mouse_tracking_starttime <- NA
            }
            frames_df$chunk_id <- event$chunk_id
            return(frames_df)
        } else {
            return(data.frame(chunk_id = numeric()))
        }
    })

    frames_per_event <- bind_rows(frames_per_event) |>
        arrange(timestamp)

    filled <- filter(frames_per_event, type == "mouse") |>
        fill(xpath, css)

    frames_per_event <- bind_rows(filter(frames_per_event, type != "mouse"), filled) |>
        arrange(timestamp)

    # older event data (before Nov. 28 2023) has no "startedAtISODate" value, hence we must use the first event time
    # as time reference for the mouse events
    mouse_event_start <- min(tracking_sess_data$event_time) -
        filter(frames_per_event, type == "mouse") |> pull(timestamp) |> min() / 1000

    track_data <- select(tracking_sess_data, chunk_id, event_time) |>
        inner_join(frames_per_event, by = "chunk_id") |>
        mutate(event_time = as.POSIXct(ifelse(is.na(mouse_tracking_starttime),
                                       mouse_event_start + timestamp/1000,
                                       mouse_tracking_starttime + timestamp/1000), tz = "UTC")) |>
        select(-c(chunk_id, timestamp, mouse_tracking_starttime)) |>
        arrange(event_time)

    stopifnot(nrow(track_data) == nrow(frames_per_event))
    stopifnot(sum(is.na(track_data$event_time)) == 0)

    if (all(order(track_data$event_time) != 1:nrow(track_data))) {
        warning(sprintf("order of events may be wrong for data in tracking session #%d", tracking_sess_id))
    }

    track_data
}


# function to load CSVs for data from a single application session identified by `app_sess_id`
load_app_sess_data <- function(app_sess_id) {
    datadir <- paste0("data/raw/", app_sess_id)
    sess <- read.csv(paste0(datadir, '/tracking_sessions.csv')) |>
        filter(!is.na(track_sess_id))   # filter for those that allowed tracking
    events <- read.csv(paste0(datadir, '/tracking_events.csv'))

    # counts per event type
    print(table(events$event_type))

    # generate device information from JSON in `track_sess_device_info`
    sess_device_info <- bind_rows(lapply(sess$track_sess_device_info, function(jsonstr) {
        res <- data.frame(
            user_agent = NA_character_,
            form_factor = NA_character_,
            client_ip = NA_character_,
            initial_win_width = NA_real_,
            initial_win_height = NA_real_,
            initial_contentview_width = NA_real_,
            initial_contentview_height = NA_real_,
            initial_contentscroll_width = NA_real_,
            initial_contentscroll_height = NA_real_
        )
        if (nchar(jsonstr) > 0) {
            parsed <- fromJSON(jsonstr)
            res$user_agent <- parsed$user_agent
            res$form_factor <- parsed$form_factor
            res$client_ip <- parsed$client_ip
            res$initial_win_width <- parsed$window_size[1]
            res$initial_win_height <- parsed$window_size[2]
            res$initial_contentview_width <- parsed$main_content_viewsize[1]
            res$initial_contentview_height <- parsed$main_content_viewsize[2]
            res$initial_contentscroll_width <- parsed$main_content_scrollsize[1]
            res$initial_contentscroll_height <- parsed$main_content_scrollsize[2]
        }

        res
    }))

    sess <- bind_cols(sess, sess_device_info) |>
        mutate(form_factor = factor(form_factor, levels = c("desktop", "tablet", "phone"))) |>
        select(-track_sess_device_info)

    print(summary(sess))

    # ---- parse data that is not related to mouse tracking ----

    # join tracking session and event data by tracking session ID and filter for all but the "mouse" events
    nonmousedata <- left_join(sess, events, by = c('track_sess_id')) |>
        filter(event_type != "mouse") |>
        mutate(track_sess_start = ymd_hms(track_sess_start),
               track_sess_end = ymd_hms(ifelse(track_sess_end == "", NA, track_sess_end)),
               event_time = ymd_hms(event_time)) |>
        select(-app_sess_code) |>
        arrange(event_time) |>
        filter(track_sess_start >= FILTER_START_DATE, event_time >= FILTER_START_DATE)

    nonmousedata <- mutate(nonmousedata, tmp_id = 1:nrow(nonmousedata))

    parsed_nonmousedata <- group_by(nonmousedata, tmp_id) |>
        group_modify(extract_other_tracking_data)

    nonmousedata_complete <- select(nonmousedata, -c(event_type, event_value)) |>
        inner_join(parsed_nonmousedata, by = "tmp_id") |>
        select(-tmp_id)

    rm(nonmousedata, parsed_nonmousedata)

    # ---- parse mouse tracking data ----

    # join tracking session and event data by tracking session ID and filter for only "mouse" events
    mousedata <- left_join(sess, events, by = c('track_sess_id')) |>
        filter(event_type == "mouse") |>
        mutate(track_sess_start = ymd_hms(track_sess_start),
               track_sess_end = ymd_hms(ifelse(track_sess_end == "", NA, track_sess_end)),
               event_time = ymd_hms(event_time)) |>
        select(-c(app_sess_code, event_type)) |>
        arrange(event_time) |>
        filter(track_sess_start >= FILTER_START_DATE, event_time >= FILTER_START_DATE)

    # count mouse event chunks per tracking session
    group_by(mousedata, user_app_sess_code, track_sess_id, track_sess_start, track_sess_end) |>
        count() |>
        ungroup() |>
        arrange(desc(track_sess_start)) |>
        print()

    # apply the parsing -- this takes some time
    mousetracking_complete <- group_by(mousedata, user_app_sess_code, user_app_sess_user_id,
                                       track_sess_id, track_sess_start, track_sess_end,
                                       user_agent, form_factor, client_ip,
                                       initial_win_width, initial_win_height,
                                       initial_contentview_width, initial_contentview_height,
                                       initial_contentscroll_width, initial_contentscroll_height) |>
        arrange(event_time) |>
        group_modify(extract_mousetracking_data) |>
        arrange(event_time) |>    # individual events may be out of order otherwise
        ungroup()

    rm(mousedata)

    # ---- combine the data ----

    initial_chapt_id <- filter(nonmousedata_complete, chapter_index == 0) |>
        distinct(chapter_id) |>
        pull(chapter_id)

    # combine the data, re-arrange by tracking session and event time, fill down chapter data
    final <- bind_rows(nonmousedata_complete, mousetracking_complete) |>
        arrange(user_app_sess_code, track_sess_id, event_time) |>
        group_by(track_sess_id) |>
        mutate(chapter_index = ifelse(row_number() == 1 & type != "chapter" & is.na(chapter_index),
                                      0, chapter_index),
               chapter_id = ifelse(row_number() == 1 & type != "chapter" & is.na(chapter_id),
                                   initial_chapt_id, chapter_id)) |>
        fill(chapter_index, chapter_id) |>
        mutate(win_width = ifelse(row_number() == 1, initial_win_width, NA_real_),
               win_height = ifelse(row_number() == 1, initial_win_height, NA_real_),
               contentview_width = ifelse(row_number() == 1, initial_contentview_width, NA_real_),
               contentview_height = ifelse(row_number() == 1, initial_contentview_height, NA_real_),
               contentscroll_width = ifelse(row_number() == 1, initial_contentscroll_width, NA_real_),
               contentscroll_height = ifelse(row_number() == 1, initial_contentscroll_height, NA_real_),
               track_sess_end = as.POSIXct(ifelse(is.na(track_sess_end) & row_number() == n(),   # last event is end of
                                           event_time, track_sess_end)), tz = "UTC") |>          # tracking session
        fill(track_sess_end, .direction = "up") |>
        ungroup() |>
        mutate(user_app_sess_code = as.factor(user_app_sess_code),
               chapter_id = as.factor(chapter_id),
               ex_label = as.factor(ex_label),
               win_width = ifelse(type == "window", coord1, win_width),
               win_height = ifelse(type == "window", coord2, win_height),
               contentview_width = ifelse(type == "main_content_viewsize", coord1, contentview_width),
               contentview_height = ifelse(type == "main_content_viewsize", coord2, contentview_height),
               contentscroll_width = ifelse(type == "main_content_scrollsize", coord1, contentscroll_width),
               contentscroll_height = ifelse(type == "main_content_scrollsize", coord2, contentscroll_height),
               win_scroll_x = ifelse(type == "scroll", coord1, NA_real_),
               win_scroll_y = ifelse(type == "scroll", coord2, NA_real_),
               content_scroll_x = ifelse(type == "contentscroll", coord1, NA_real_),
               content_scroll_y = ifelse(type == "contentscroll", coord2, NA_real_)) |>
        group_by(track_sess_id) |>
        fill(user_agent, form_factor,
             initial_win_width, initial_win_height, win_width, win_height,
             initial_contentview_width, initial_contentview_height, contentview_width, contentview_height,
             initial_contentscroll_width, initial_contentscroll_height, contentscroll_width, contentscroll_height) |>
        fill(win_scroll_x, win_scroll_y, content_scroll_x, content_scroll_y, .direction = "downup") |>
        ungroup() |>
        filter(!(type %in% c("main_content_scrollsize", "main_content_viewsize", "window"))) |>
        mutate(type = as.factor(type))

    print(summary(final))

    #filter(final, track_sess_id == max(final$track_sess_id)) |> View()

    track_sess_metadata <- distinct(final, user_app_sess_code, track_sess_id, track_sess_start, track_sess_end) |>
        mutate(duration = track_sess_end - track_sess_start)
    print(track_sess_metadata)

    # number of events per session
    group_by(final, user_app_sess_code, track_sess_id) |>
        count() |>
        print()

    # number of events per type
    group_by(final, type) |>
        count() |>
        print()

    # number of events per session and type
    group_by(final, user_app_sess_code, track_sess_id, type) |>
        count() |>
        print()

    final
}



# ---- load data from all application sessions ----

for (app_sess_id in list.dirs("data/raw", full.names = FALSE, recursive = FALSE)) {
    dir <- file.path("data", "prepared")
    if (!dir.exists(dir)) dir.create(dir)
    outputfile <- paste0(dir, "/", app_sess_id, "_tracking_data.rds")

    if (file.exists(outputfile)) {
        print(paste("prepared app session data already exists for app session", app_sess_id, "– skipping"))
    } else {
        print(paste("loading data for app session", app_sess_id))
        final <- load_app_sess_data(app_sess_id) |>
            select(-c(user_agent, client_ip))

        # save as RDS
        print(paste("storing prepared data to", outputfile))
        saveRDS(final, outputfile)
    }

    cat("----\n\n")
}
