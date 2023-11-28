source("analysis_helpers.R")

inspect_track_sess_id <- 345

tracking_data <- readRDS("data/prepared/476bc20dbf_tracking_data.rds") |>
    filter(track_sess_id == inspect_track_sess_id)

unique(tracking_data$track_sess_start)
unique(tracking_data$track_sess_end)
min(tracking_data$event_time)
max(tracking_data$event_time)

select(tracking_data, event_time, type, chapter_index) |> View()

sess_data <- filter(tracking_data, track_sess_id == inspect_track_sess_id) |>
    select(-c(user_app_sess_code, user_app_sess_user_id, track_sess_id)) |>
    mutate(chapter_changes = cumsum(c(0, abs(diff(chapter_index)))))

count(sess_data, chapter_changes)
filter(sess_data, chapter_changes == 2) |> View()

mouse_tracks_data <- mouse_tracks_for_tracking_sess(tracking_data, inspect_track_sess_id)
form_factor <- mouse_tracks_data$form_factor
mouse_tracks_data <- mouse_tracks_data$tracks

plot_mouse_tracks_for_tracking_session(mouse_tracks_data, inspect_track_sess_id, form_factor,
                                       x_limits = c(-0.1, 1.1), y_limits = c(1.1, -0.1))
