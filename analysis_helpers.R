theme_set(theme_bw())

tracking_sess_times <- function(tracking_data) {
    distinct(tracking_data, track_sess_id, track_sess_start, track_sess_end) |>
        arrange(track_sess_start) |>
        mutate(duration = track_sess_end - track_sess_start)
}

plot_tracking_sess_durations <- function(track_sess_times) {
    ggplot(track_sess_times, aes(y = ordered(track_sess_id))) +
        geom_linerange(aes(xmin = track_sess_start, xmax = track_sess_end)) +
        geom_point(aes(x = track_sess_start)) +
        geom_point(aes(x = track_sess_end)) +
        labs(title = "Tracking session start and end times", x = "Date", y = "Tracking session ID")
}

plot_tracking_sess_durations_hist <- function(track_sess_times, ...) {
    durations_h <- track_sess_times$duration |> as.double(units = "hours")
    ggplot(data.frame(duration_hours = durations_h), aes(duration_hours)) +
        geom_histogram(...) +
        labs(title = "Histogram of tracking session durations", x = "Duration in hours", y = "Frequency")
}

plot_event_type_counts <- function(tracking_data) {
    type_counts <- count(tracking_data, type)
    p <- ggplot(type_counts, aes(x = type, y = n)) +
        geom_col() +
        scale_y_log10() +
        labs(title = "Number of events per type", x = "event type", y = "Frequency on log10 scale")
    list(plot = p, table = type_counts)
}

plot_event_type_counts_per_tracking_sess <- function(tracking_data) {
    type_counts <- count(tracking_data, track_sess_id, type)
    p <- ggplot(type_counts, aes(x = type, y = n)) +
        geom_col() +
        scale_y_log10() +
        labs(title = "Number of events per type", x = "event type", y = "Frequency on log10 scale") +
        facet_wrap(vars(track_sess_id)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    list(plot = p, table = type_counts)
}




