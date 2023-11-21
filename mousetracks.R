library(dplyr)
library(ggplot2)
library(patchwork)


tracking_data <- readRDS("data/prepared/8d97149cd2_tracking_data.rds")
tracking_data

count(tracking_data, track_sess_id)

track_sessions <- distinct(tracking_data, track_sess_id, track_sess_start, track_sess_end) |>
    filter(track_sess_start >= "2023-11-14") |>
    arrange(track_sess_start)

ggplot(track_sessions, aes(y = ordered(track_sess_id))) +
    geom_linerange(aes(xmin = track_sess_start, xmax = track_sess_end)) +
    geom_point(aes(x = track_sess_start)) +
    geom_point(aes(x = track_sess_end)) +
    labs(title = "Tracking session start and end times", x = "Date", y = "Tracking session ID") +
    theme_minimal()


track_sessions$duration <- track_sessions$track_sess_end - track_sessions$track_sess_start
track_sessions

durations_h <- track_sessions$duration |> as.double(units = "hours")
ggplot(data.frame(duration_hours = durations_h), aes(duration_hours)) +
    geom_histogram() +
    labs(title = "Histogram of tracking session durations", x = "Duration in hours", y = "Frequency") +
    theme_minimal()

median(track_sessions$duration) |> as.double(units = "hours")

max(tracking_data$track_sess_id)

sess_data <- filter(tracking_data, track_sess_id == 195) |>
    select(-c(user_app_sess_code, user_app_sess_user_id, track_sess_id))

table(sess_data$type)

sess_mouse_tracks <- group_by(sess_data, chapter_index, chapter_id) |>
    mutate(chapt_content_width = max(win_width) - max(contentview_width) + max(contentscroll_width),
           chapt_content_height = max(win_height) - max(contentview_height) + max(contentscroll_height),
           time = as.double(event_time - min(event_time)), units = "secs") |>
    ungroup() |>
    filter(type %in% c("mouse", "click")) |>
    mutate(mouse_x = (coord1 + content_scroll_x) / chapt_content_width,
           mouse_y = (coord2 + content_scroll_y) / chapt_content_height) |>
    select(chapter_index, chapter_id, time, type, mouse_x, mouse_y,
           chapt_content_width, chapt_content_height)

sess_mouse_tracks
summary(sess_mouse_tracks)

trackplots_per_chapt <- lapply(sort(unique(sess_mouse_tracks$chapter_index)), function (chapt_index) {
    chapt_data <- filter(sess_mouse_tracks, chapter_index == chapt_index)
    chapt_id <- sub("section-", "", unique(chapt_data$chapter_id), fixed = TRUE)

    ggplot() +
        geom_path(aes(x = mouse_x, y = mouse_y, color = time),
                  data = filter(chapt_data, type == "mouse"),
                  alpha = 0.5) +
        geom_point(aes(x = mouse_x, y = mouse_y, color = time),
                   data = filter(chapt_data, type == "click")) +
        scale_x_continuous(limits = c(0, 1)) +
        scale_y_reverse(limits = c(1, 0)) +
        scale_color_continuous(name = "time in sec.") +
        coord_fixed() +
        labs(title = sprintf("Chapter %d", chapt_index+1), subtitle = chapt_id, x = "x", y = "y") +
        theme_minimal()
})

wrap_plots(trackplots_per_chapt)
