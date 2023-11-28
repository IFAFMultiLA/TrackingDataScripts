library(plyr)
library(dplyr)
library(bcpa)
library(tidyr)
library(ggplot2)
library(patchwork)


theme_set(theme_bw())

tracking_sess_times <- function(tracking_data) {
    distinct(tracking_data, track_sess_id, track_sess_start, track_sess_end) |>
        arrange(track_sess_start) |>
        mutate(duration = track_sess_end - track_sess_start)
}

question_submit_data <- function(tracking_data) {
    filter(tracking_data, type == "question_submit") |>
        select(track_sess_id, event_time, ex_label, ex_correct, value)
}

exercise_result_data <- function(tracking_data) {
    filter(tracking_data, type  == "ex_result") |>
        select(track_sess_id, event_time, type, starts_with("ex_"), value) |>
        mutate(ex_correct = ifelse(is.na(ex_correct), FALSE, ex_correct))
}

question_or_exercise_submit_tries <- function(quest_or_ex_data) {
    select(quest_or_ex_data, -value) |>
        group_by(track_sess_id, ex_label) |>
        arrange(track_sess_id, ex_label, event_time) |>
        mutate(try = row_number()) |>
        filter(row_number() == n()) |>
        ungroup()
}

prop_correct_in_ith_try <- function(quest_or_ex_data) {
    max_tries <- select(quest_or_ex_data, -value) |>
        group_by(track_sess_id, ex_label) |>
        count() |>
        ungroup() |>
        pull(n) |>
        max()

    lapply(1:max_tries, function(which_try) {
        select(quest_or_ex_data, -value) |>
            group_by(track_sess_id, ex_label) |>
            arrange(track_sess_id, ex_label, event_time) |>
            mutate(try = row_number(), max_try = max(try)) |>
            filter(try == min(max_try, which_try)) |>
            ungroup() |>
            group_by(ex_label) |>
            summarise(prop_correct = mean(ex_correct)) |>
            ungroup() |>
            mutate(try = which_try)
    }) |> bind_rows() |>
        group_by(ex_label) |>
        mutate(lag_correct = lag(prop_correct)) |>
        filter(is.na(lag_correct) | lag_correct < 1) |>
        ungroup() |>
        arrange(ex_label, try) |>
        select(-lag_correct)

}

mouse_tracks_for_tracking_sess <- function(tracking_data, tracking_sess_id) {
    sess_data <- filter(tracking_data, track_sess_id == tracking_sess_id) |>
        select(-c(user_app_sess_code, user_app_sess_user_id, track_sess_id)) |>
        mutate(chapter_changes = cumsum(c(0, abs(diff(chapter_index)))))

    form_factor <- unique(sess_data$form_factor)
    stopifnot(length(form_factor) == 1)

    tracks <- group_by(sess_data, chapter_changes) |>
        mutate(chapt_content_width = max(win_width) - max(contentview_width) + max(contentscroll_width),
               chapt_content_height = max(win_height) - max(contentview_height) + max(contentscroll_height),
               time = as.double(event_time - min(event_time)), units = "secs") |>
        ungroup() |>
        filter(type %in% c("mouse", "click")) |>
        mutate(mouse_x = (coord1 + content_scroll_x) / chapt_content_width,
               mouse_y = (coord2 + content_scroll_y) / chapt_content_height) |>
        select(chapter_changes, chapter_index, chapter_id, time, type, mouse_x, mouse_y,
               chapt_content_width, chapt_content_height)

    list(tracks = tracks, form_factor = form_factor)
}

mouse_tracks_features <- function(mouse_tracks_data) {
    track <- MakeTrack(mouse_tracks_data$mouse_x, mouse_tracks_data$mouse_y, mouse_tracks_data$time / 60)
    track_vt <- GetVT(track, units = "min", skiplast = FALSE)

    t_steps <- 0:ceiling(max(track_vt$T.end))
    track_vt$t_step <- cut(track_vt$T.end, t_steps, labels = FALSE, ordered_result = TRUE)

    group_by(track_vt, t_step) |>
        summarise(mean_t_step_V = mean(V)) |>
        filter(!is.na(mean_t_step_V) & is.finite(mean_t_step_V)) |>
        ungroup()
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
        labs(title = "Histogram of tracking session durations", x = "Duration in hours", y = "frequency")
}

plot_event_type_counts <- function(tracking_data) {
    type_counts <- count(tracking_data, type)
    p <- ggplot(type_counts, aes(x = type, y = n)) +
        geom_col() +
        scale_y_log10() +
        labs(title = "Number of events per type", x = "event type", y = "frequency on log10 scale")
    list(plot = p, table = type_counts)
}

plot_event_type_counts_per_tracking_sess <- function(tracking_data) {
    type_counts <- count(tracking_data, track_sess_id, type)
    p <- ggplot(type_counts, aes(x = type, y = n)) +
        geom_col() +
        scale_y_log10() +
        labs(title = "Number of events per type", x = "event type", y = "frequency on log10 scale") +
        facet_wrap(vars(track_sess_id)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    list(plot = p, table = type_counts)
}

plot_question_prop_correct <- function(quest_data) {
    prop_per_question <- group_by(quest_data, ex_label) |>
        summarise(n = n(),
                  prop_correct = mean(ex_correct),
                  sd_correct = sqrt(prop_correct*(1-prop_correct)/n))
    p <- ggplot(prop_per_question, aes(y = ex_label)) +
        geom_linerange(aes(xmin = prop_correct-sd_correct, xmax = prop_correct+sd_correct)) +
        geom_point(aes(x = prop_correct)) +
        scale_y_discrete(limits = rev) +
        labs(title = "Proportion of correct answers per question",
             y = "question label",
             x = "proportion with standard deviations")
    list(plot = p, table = prop_per_question)
}

plot_exercise_prop_correct <- function(ex_data) {
    prop_per_question <- group_by(ex_data, ex_label) |>
        summarise(n = n(),
                  prop_correct = mean(ex_correct),
                  sd_correct = sqrt(prop_correct*(1-prop_correct)/n))
    p <- ggplot(prop_per_question, aes(y = ex_label)) +
        geom_linerange(aes(xmin = prop_correct-sd_correct, xmax = prop_correct+sd_correct)) +
        geom_point(aes(x = prop_correct)) +
        scale_y_discrete(limits = rev) +
        labs(title = "Proportion of correct answers per code exercise",
             y = "exercise label",
             x = "proportion with standard deviations")
    list(plot = p, table = prop_per_question)
}

plot_question_n_tries <- function(quiz_tries) {
    ggplot(quiz_tries, aes(y = ex_label, x = try)) +
        geom_boxplot() +
        geom_jitter(height = 0.25) +
        scale_y_discrete(limits = rev) +
        labs(title = "Number of tries per question", y = "question label", x = "number of tries")
}

plot_exercise_n_tries <- function(ex_tries) {
    ggplot(ex_tries, aes(y = ex_label, x = try)) +
        geom_boxplot() +
        geom_jitter(height = 0.25) +
        scale_y_discrete(limits = rev) +
        labs(title = "Number of tries per exercise", y = "exercise label", x = "number of tries")
}

plot_mouse_tracks_for_chapter <- function(chapt_change_number, mouse_tracks_data,
                                          x_limits = c(0, 1),
                                          y_limits= c(1, 0)) {
    chapt_data <- filter(mouse_tracks_data, chapter_changes == chapt_change_number)
    chapt_index <- unique(chapt_data$chapter_index)
    chapt_id <- sub("section-", "", unique(chapt_data$chapter_id), fixed = TRUE)

    ggplot() +
        geom_path(aes(x = mouse_x, y = mouse_y, color = time),
                  data = filter(chapt_data, type == "mouse"),
                  alpha = 0.5) +
        geom_point(aes(x = mouse_x, y = mouse_y, color = time),
                   data = filter(chapt_data, type == "click")) +
        scale_x_continuous(limits = x_limits) +
        scale_y_reverse(limits = y_limits) +
        scale_color_continuous(name = "time in sec.") +
        coord_fixed() +
        labs(title = sprintf("%d: Chapter %d", chapt_change_number+1, chapt_index+1),
             subtitle = chapt_id, x = "x", y = "y") +
        theme_minimal()
}

plot_mouse_tracks_for_tracking_session <- function(mouse_tracks_data, track_sess_id, form_factor,
                                                   x_limits = c(0, 1),
                                                   y_limits= c(1, 0)) {
    trackplots_per_chapt <- lapply(sort(unique(mouse_tracks_data$chapter_changes)),
                                   plot_mouse_tracks_for_chapter, mouse_tracks_data, x_limits, y_limits)

    wrap_plots(trackplots_per_chapt) +
        plot_annotation(title = sprintf("Tracking session #%d (%s)", track_sess_id, form_factor))
}

plot_prop_correct_per_try <- function(prop_correct_per_try, title) {
    ggplot(prop_correct_per_try, aes(x = try, y = prop_correct, color = ex_label)) +
        geom_line() +
        scale_x_continuous(breaks = 1:max(prop_correct_per_try$try)) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_color_discrete(name = "Label") +
        labs(title = title, x = "Try", y = "Proportion of correct submissions")
}

plot_mouse_velocity_heatmap <- function(tracks_features_per_track_sess) {
    ggplot(tracks_features_per_track_sess, aes(x = t_step,
                                               y = as.factor(sprintf("%d (%s)", track_sess_id, form_factor)))) +
        geom_tile(aes(fill = mean_t_step_V)) +
        scale_fill_binned(name = "mean velocity\nin pixels/sec", n.breaks = 7) +
        labs(title = "Heatmap of movement velocity per minute in each tracking session",
             x = "Minute after tracking session start",
             y = "Tracking session ID")
}



