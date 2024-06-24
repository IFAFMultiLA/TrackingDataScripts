# Data analysis helper functions.
#
# Several helper functions used for tracking data analyses.
#
# Author: Markus Konrad <markus.konrad@htw-berlin.de>
# Date: Nov./Dec. 2023
#

library(plyr)
library(dplyr)
library(bcpa)
library(tidyr)
library(ggplot2)
library(patchwork)
library(lubridate)


theme_set(theme_bw())  # set default theme

# ----- data preparation helper functions -----

# Load all tracking data from several application sessions identified in vector `app_sessions`. Optionally assign
# each application session to a group passed via `groups`. If given, `groups` must has the same length as
# `app_sessions`.
load_app_sessions_tracking_data <- function(app_sessions, groups = NULL, shorten_user_code = TRUE) {
    stopifnot(is.null(groups) || length(app_sessions) == length(groups))

    tracking_data <- lapply(app_sessions, function(sess) {
        sess_data <- readRDS(sprintf("data/prepared/%s_tracking_data.rds", sess)) |>
            mutate(app_session = sess)

        if (is.null(groups)) {
            sess_data <- mutate(sess_data, group = "")
        } else {
            sess_data <- mutate(sess_data, group = groups[which(app_sessions == sess)])
        }

        sess_data
    }) |>
        bind_rows() |>
        mutate(app_session = as.factor(app_session))


    if (shorten_user_code) {
        tracking_data$user_code <- substr(tracking_data$user_app_sess_code, 0, 16)

        stopifnot(nrow(distinct(tracking_data, user_app_sess_code)) == nrow(distinct(tracking_data, user_code)))
    } else {
        tracking_data$user_code <- tracking_data$user_app_sess_code
    }


    mutate(tracking_data, group = as.factor(group)) |>
        select(app_session, group, user_code, everything())
}

# Get start date, end date and duration for each tracking session in `tracking_data`.
tracking_sess_times <- function(tracking_data) {
    distinct(tracking_data, user_code, track_sess_id, track_sess_start, track_sess_end) |>
        arrange(track_sess_start) |>
        mutate(duration = track_sess_end - track_sess_start)

}

# Get all tracking data related to question submissions in `tracking_data`.
question_submit_data <- function(tracking_data) {
    filter(tracking_data, type == "question_submit", !startsWith(as.character(ex_label), "survey_")) |>
        select(user_code, group, event_time, ex_label, ex_correct, value)
}

# Get all survey data from `tracking_data` as wide table with answers in columns per user and tracking session ID.
survey_data <- function(tracking_data) {
    all_numeric_or_NA <- function (x) {
        all(is.na(x) | grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$", x))
    }

    filter(tracking_data, type == "question_submit", !is.na(ex_label)) |>
        mutate(ex_label = as.character(ex_label)) |>
        filter(startsWith(ex_label, 'survey_')) |>
        select(app_session, group, user_app_sess_code, track_sess_id, ex_label, value) |>
        mutate(item = substr(ex_label, nchar('survey_') + 1, nchar(ex_label))) |>
        select(-ex_label) |>
        pivot_wider(names_from = item) |>
        mutate(across(where(all_numeric_or_NA), as.numeric))
}

# Get all tracking data related to code exercises in `tracking_data`.
exercise_result_data <- function(tracking_data) {
    # in step 3, `ex_correct` is transformed so that exercises where there was no result submitted (i.e. NA) are counted
    # as "not correct"
    filter(tracking_data, type  == "ex_result") |>
        select(group, user_code, event_time, type, starts_with("ex_"), value) |>
        mutate(ex_correct = ifelse(is.na(ex_correct), FALSE, ex_correct))
}

# Calculate number of question submission or code submission tries as variable `try` until the user was correct or has
# given up as. Pass `quest_or_ex_data` as question or code submission data returned from the functions
# `question_submit_data()` or `exercise_result_data()`.
question_or_exercise_submit_tries <- function(quest_or_ex_data) {
    select(quest_or_ex_data, -value) |>
        group_by(user_code, ex_label) |>    # calculate per tracking session and exercise
        arrange(user_code, ex_label, event_time) |>
        mutate(try = row_number()) |>           # current try equals the row number after sorting by time
        filter(row_number() == n()) |>          # select only the last row per group to get the final try count
        ungroup()
}

# Calculate the proportion of correct answers per exercise and try as variable `prop_correct`. Pass `quest_or_ex_data`
# as question or code submission data returned from the functions `question_submit_data()` or `exercise_result_data()`.
prop_correct_in_ith_try <- function(quest_or_ex_data) {
    # get the maximum number of tries in total
    max_tries <- select(quest_or_ex_data, -value) |>
        group_by(group, user_code, ex_label) |>
        count() |>
        ungroup() |>
        pull(n) |>
        max()

    # for each number of tries in range [1, `max_tries`], calculate the proportion of correct answers per exercise
    lapply(1:max_tries, function(which_try) {
        select(quest_or_ex_data, -value) |>
            group_by(group, user_code, ex_label) |>                # for each exercise in each tracking session ...
            arrange(group, user_code, ex_label, event_time) |>
            mutate(try = row_number(), max_try = max(try)) |>   # ... get each try and the overall number of tries and
            filter(try == min(max_try, which_try)) |>      # ... filter for the current try or the last try as fallback;
            ungroup() |>
            group_by(group, ex_label) |>                   # then calculate the proportion of correct answers for ...
            summarise(prop_correct = mean(ex_correct), .groups = "keep") |>  # ... each exercise
            ungroup() |>
            mutate(try = which_try)
    }) |> bind_rows() |>                                   # concatenate to single dataframe
        group_by(group, ex_label) |>
        mutate(lag_correct = lag(prop_correct)) |>         # keep only observations where prev. try was first try or
        filter(is.na(lag_correct) | lag_correct < 1) |>    # was not 100% correct, yet
        ungroup() |>
        arrange(ex_label, try) |>
        select(-lag_correct)

}

# Generate mouse tracking data from tracking dataframe `tracking_data` for a single tracking session given as
# `tracking_sess_id`. The mouse tracks are calculated per chapter view identified via variable `chapter_changes`.
# Returns a list with the mouse tracking data as "$tracks" and the form factor of the device used
# during the tracking session as "$form_factor".
#
# The mouse tracking data contains the following variables:
# - `chapter_changes`: number of times the chapter was changed previously
# - `chapter_index`: index number of the current chapter
# - `chapter_id`: ID of the current chapter
# - `time`: time in seconds measured since the start of the current chapter view (i.e. always starts at 0 per
#           group `chapter_changes`)
# - `type`: either "mouse" or "click", the former describing movement
# - `mouse_x` and `mouse_y`: mouse pointer coordinates in range [0, 1] relative to `chapt_content_width` and
#   `chapt_content_height`
# - `chapt_content_width` and `chapt_content_height`: content dimensions for the current chapter view
#
mouse_tracks_for_tracking_sess <- function(tracking_data, tracking_sess_id) {
    # filter for the tracking session and generate `chapter_changes` as number of the previous switches between chapters
    sess_data <- filter(tracking_data, track_sess_id == tracking_sess_id) |>
        select(-c(user_app_sess_code, user_app_sess_user_id, track_sess_id)) |>
        mutate(chapter_changes = cumsum(c(0, abs(diff(chapter_index)))))

    form_factor <- unique(sess_data$form_factor)
    stopifnot(length(form_factor) == 1)

    #substitute NAs with zeros in content_scroll_x/y (in case of no scroll it is filled with NAs)
    sess_data$content_scroll_x[is.na(sess_data$content_scroll_x)]<-0
    sess_data$content_scroll_y[is.na(sess_data$content_scroll_y)]<-0

    # generate the mouse tracks data: filter for the mouse-related events; group by chapter changes; set the content
    # bounds for the current chapter view; set the time as time since the start of the current chapter view;
    # calculate the mouse coordinates as normalized coordinates relative to the content bounds so they are in
    # [0, 1] range
    tracks <- filter(sess_data, type %in% c("mouse", "click")) |>
        group_by(chapter_changes) |>
        mutate(chapt_content_width = max(win_width) - max(contentview_width) + max(contentscroll_width),
               chapt_content_height = max(max(win_height) + max(contentscroll_height), (coord2 + content_scroll_y)),
               time = as.double(event_time - min(event_time)), units = "secs") |>
        ungroup() |>
        mutate(mouse_x = (coord1 + content_scroll_x) / chapt_content_width,
               mouse_y = (coord2 + content_scroll_y) / chapt_content_height) |>
        select(chapter_changes, chapter_index, chapter_id, time, type, mouse_x, mouse_y,
               chapt_content_width, chapt_content_height)

    list(tracks = tracks, form_factor = form_factor)
}

# Calculate features for mouse tracks given in `mouse_tracks_data` (as returned from
# `mouse_tracks_for_tracking_sess()`). Current only feature is mean step velocity per step in pixels per second.
# See `bcpa::GetVT()` for details.
mouse_tracks_features <- function(mouse_tracks_data) {
    # generate the track
    track <- MakeTrack(mouse_tracks_data$mouse_x, mouse_tracks_data$mouse_y, mouse_tracks_data$time / 60)
    # calculate the features
    track_vt <- GetVT(track, units = "min", skiplast = FALSE)

    # generate discrete time steps: each step is one minute
    t_steps <- -1:ceiling(max(track_vt$T.end))
    track_vt$t_step <- cut(track_vt$T.end, t_steps, labels = FALSE, ordered_result = TRUE) - 1

    # calculate the mean velocity per time step
    group_by(track_vt, t_step) |>
        summarise(mean_t_step_V = mean(V)) |>
        filter(!is.na(mean_t_step_V) & is.finite(mean_t_step_V)) |>
        ungroup()
}

# ----- plotting functions -----

# Plot tracking session durations given in `track_sess_times` (as returned from `tracking_sess_times()`).
plot_tracking_sess_durations <- function(track_sess_times, by_user_code = FALSE) {
    if (by_user_code) {
        ggplot(track_sess_times, aes(y = user_code)) +
            geom_linerange(aes(xmin = track_sess_start, xmax = track_sess_end), alpha = 0.5) +
            geom_point(aes(x = track_sess_start), alpha = 0.5) +
            geom_point(aes(x = track_sess_end), alpha = 0.5) +
            labs(title = "Tracking session start and end times", x = "Date", y = "User ID")
    } else {
        ggplot(track_sess_times, aes(y = ordered(track_sess_id), color = user_code)) +
            geom_linerange(aes(xmin = track_sess_start, xmax = track_sess_end)) +
            geom_point(aes(x = track_sess_start)) +
            geom_point(aes(x = track_sess_end)) +
            labs(title = "Tracking session start and end times", x = "Date", y = "Tracking session ID")
    }
}

# Plot tracking session durations as histogram given by `track_sess_times` (as returned from `tracking_sess_times()`).
# Arguments in `...` are passed to `geom_histogram()`.
plot_tracking_sess_durations_hist <- function(track_sess_times, ...) {
    durations_h <- track_sess_times$duration |> as.double(units = "hours")
    ggplot(data.frame(duration_hours = durations_h), aes(duration_hours)) +
        geom_histogram(...) +
        labs(title = "Histogram of tracking session durations", x = "Duration in hours", y = "frequency")
}

# Make bar plot of event type counts in `tracking_data` on log10 scale. `tracking_data` is data as prepared in
# `prepare.R`.
plot_event_type_counts <- function(tracking_data) {
    type_counts <- count(tracking_data, type)
    p <- ggplot(type_counts, aes(x = type, y = n)) +
        geom_col() +
        scale_y_log10() +
        labs(title = "Number of events per type", x = "event type", y = "frequency on log10 scale") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    list(plot = p, table = type_counts)
}

# Make bar plot of event type counts *per user* on log10 scale. `tracking_data` is data as prepared in
# `prepare.R`.
plot_event_type_counts_per_user <- function(tracking_data) {
    type_counts <- count(tracking_data, user_code, type)
    p <- ggplot(type_counts, aes(x = type, y = n)) +
        geom_col() +
        scale_y_log10() +
        labs(title = "Number of events per type", x = "event type", y = "frequency on log10 scale") +
        facet_wrap(vars(user_code)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    list(plot = p, table = type_counts)
}

# Plot proportion of correct answers per question. `quest_data` is data as returned from `question_submit_data()`.
plot_question_prop_correct <- function(quest_data, per_group = FALSE) {
    grp_compare <- length(levels(quest_data$group)) > 1

    if (grp_compare) {
        if (per_group) {
            d <- group_by(quest_data, group)
        } else {
            d <- group_by(quest_data, group, ex_label)
        }
    } else {
        d <- group_by(quest_data, ex_label)
    }

    prop_per_question <- summarise(d,
                                   n = n(),
                                   prop_correct = mean(ex_correct),
                                   sd_correct = sqrt(prop_correct*(1-prop_correct)/n)) |>
        ungroup()

    if(grp_compare) {
        if (per_group) {
            mapping <- aes(y = group, color = group)
        } else {
            prop_per_question <- group_by(prop_per_question, group) |>
                arrange(ex_label) |>
                mutate(ex_label_pos = 1:n()) |>
                ungroup() |>
                mutate(ex_label_pos = ex_label_pos + (as.integer(group) * 2 - 3) * 0.12)

            mapping <- aes(y = ex_label_pos, color = group)
        }
    } else {
        mapping <- aes(y = ex_label)
    }

    p <- ggplot(prop_per_question, mapping) +
        geom_linerange(aes(xmin = prop_correct-sd_correct, xmax = prop_correct+sd_correct)) +
        geom_point(aes(x = prop_correct)) +
        geom_text(aes(x = prop_correct, label = paste("n =", n)), nudge_x = 0.05, nudge_y = 0.15, size = 3) +
        labs(title = "Proportion of correct answers per question",
             y = "question label",
             x = "proportion with standard deviations")

    if (grp_compare && !per_group) {
        lbls <- sort(unique(as.character(prop_per_question$ex_label)))
        p <- p + scale_y_continuous(breaks = 1:length(lbls), labels = lbls)
    }

    list(plot = p, table = prop_per_question)
}

# Plot proportion of correct answers per coding exercise `ex_data` is data as returned from `exercise_result_data()`.
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

# Plot number of tries per question as box plot. `quiz_tries` is data as returned from
# `question_or_exercise_submit_tries()`.
plot_question_n_tries <- function(quiz_tries, per_group = FALSE) {
    grp_compare <- length(levels(quiz_tries$group)) > 1

    if (grp_compare) {
        if (per_group) {
            d <- group_by(quiz_tries, group)
        } else {
            d <- group_by(quiz_tries, group, ex_label)
        }
    } else {
        d <- group_by(quiz_tries, ex_label)
    }

    n_tries <- summarise(d,
                         n = n(),
                         median_tries = median(try),
                         tries_lwr = quantile(try, 0.25),
                         tries_upr = quantile(try, 0.75)) |>
        ungroup()

    if(grp_compare) {
        if (per_group) {
            mapping <- aes(y = group, color = group)
        } else {
            n_tries <- group_by(n_tries, group) |>
                arrange(ex_label) |>
                mutate(ex_label_pos = 1:n()) |>
                ungroup() |>
                mutate(ex_label_pos = ex_label_pos + (as.integer(group) * 2 - 3) * 0.12)

            mapping <- aes(y = ex_label_pos, color = group)
        }
    } else {
        mapping <- aes(y = ex_label)
    }

    p <- ggplot(n_tries, mapping) +
        geom_linerange(aes(xmin = tries_lwr, xmax = tries_upr)) +
        geom_point(aes(x = median_tries)) +
        geom_text(aes(x = median_tries, label = paste("n =", n)), nudge_x = 0.05, nudge_y = 0.15, size = 3) +
        labs(title = "Number of tries per question",
             y = "question label",
             x = "25th, 50th and 75th percentile")

    if (grp_compare && !per_group) {
        lbls <- sort(unique(as.character(n_tries$ex_label)))
        p <- p + scale_y_continuous(breaks = 1:length(lbls), labels = lbls)
    }

    list(plot = p, table = n_tries)
}

# Plot the proportion of correct answers per question after the last try
plot_prop_correct_after_last_try <- function(quiz_tries) {
    group_by(quiz_tries, group, ex_label) |>
        summarise(prop_correct = mean(ex_correct)) |>
        ggplot(aes(x = ex_label, y = prop_correct, fill = group)) +
            geom_col(position = position_dodge()) +
            labs(title = "Proportion of correct answers after the last try",
                 x = "question label", y = "proportion") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# Plot number of tries per coding exercise as box plot. `ex_tries` is data as returned from
# `question_or_exercise_submit_tries()`.
plot_exercise_n_tries <- function(ex_tries) {
    ggplot(ex_tries, aes(y = ex_label, x = try)) +
        geom_boxplot() +
        geom_jitter(height = 0.25) +
        scale_y_discrete(limits = rev) +
        facet_wrap(vars(group)) +
        labs(title = "Number of tries per exercise", y = "exercise label", x = "number of tries")
}

# Plot mouse tracks for a single chapter view `chapt_change_number`. `mouse_tracks_data` is data as returned from
# `mouse_tracks_for_tracking_sess()` but filtered for a specific tracking session.
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

# Plot mouse tracks for a single tracking session identified by `track_sess_id`.
# `mouse_tracks_data` is data as returned from `mouse_tracks_for_tracking_sess()`. `form_factor` is the device form
# factor of the device used in the tracking session (to be displayed in the plot title).
plot_mouse_tracks_for_tracking_session <- function(mouse_tracks_data, track_sess_id, form_factor,
                                                   x_limits = c(0, 1),
                                                   y_limits= c(1, 0)) {
    trackplots_per_chapt <- lapply(sort(unique(mouse_tracks_data$chapter_changes)),
                                   plot_mouse_tracks_for_chapter, mouse_tracks_data, x_limits, y_limits)

    wrap_plots(trackplots_per_chapt) +
        plot_annotation(title = sprintf("Tracking session #%d (%s)", track_sess_id, form_factor))
}

# Plot proportion of correct answers/submissions per try. `prop_correct_per_try` is data as returned from
# `prop_correct_in_ith_try()`. `title` is plot title.
plot_prop_correct_per_try <- function(prop_correct_per_try, title) {
    ggplot(prop_correct_per_try, aes(x = try, y = prop_correct, color = ex_label)) +
        geom_line() +
        scale_x_continuous(breaks = 1:max(prop_correct_per_try$try)) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_color_discrete(name = "Label") +
        facet_wrap(vars(group)) +
        labs(title = title, x = "Try", y = "Proportion of correct submissions")
}

# Plot heatmap of mouse velocities with tracking sessions on y-axis and time steps on x-axis.
# `tracks_features_per_track_sess` is a dataframe with the following variables:
#
# - `t_step`: time step
# - `track_sess_id`: tracking session ID
# - `form_factor`: device form factor
# - `mean_t_step_V`: mean velocity in pixels per sec. for each time step
plot_mouse_velocity_heatmap <- function(tracks_features_per_track_sess) {
    ggplot(tracks_features_per_track_sess, aes(x = t_step,
                                               y = as.factor(sprintf("%d (%s)", track_sess_id, form_factor)))) +
        geom_tile(aes(fill = mean_t_step_V)) +
        scale_fill_binned(name = "mean velocity\nin pixels/sec", n.breaks = 7) +
        labs(title = "Heatmap of movement velocity per minute in each tracking session",
             x = "Minute after tracking session start",
             y = "Tracking session ID")
}

# Make boxplots for all numerical survey items from survey data given by `survey`.
plot_survey_numerical_items <- function(survey, per_group = TRUE, questions = NULL, scale_labels = NULL,
                                        xlab = "score on 5-point Likert scale", ylab = NULL,
                                        title = "Survey results") {
    numdata <- select(survey, -c(mehrere_tabs, kommentar)) |>
        pivot_longer(schwierigkeit:zufriedenheit, names_to = "item") |>
        filter(!is.na(value))

    if (per_group) {
        grouped_data <- group_by(numdata, group, item)
        aes_mapping <- aes(x = value, y = item, color = group)
    } else {
        grouped_data <- group_by(numdata, group, item)
        aes_mapping <- aes(x = value, y = item)
    }

    summdata <- summarise(grouped_data, mean = mean(value), .groups = "keep") |>
        pivot_wider(names_from = item, values_from = mean)

    p <- ggplot(numdata, aes_mapping) +
        geom_boxplot(position = position_dodge2(padding = 0.3, reverse = TRUE), outliers = FALSE) +
        geom_jitter(width = 0.1, height = 0.3, alpha = 0.25) +
        labs(title = title,
             x = xlab,
             y = ylab)

    if (!is.null(questions)) {
        item_labels <- unlist(questions, use.names = FALSE)[order(names(questions))]
        p <- p + scale_y_discrete(limits = rev, labels = rev(item_labels))
    } else {
        p <- p + scale_y_discrete(limits = rev)
    }

    if (!is.null(scale_labels)) {
        scale_labels_vec <- unlist(scale_labels[order(names(scale_labels))], use.names = FALSE)
        scale_left <- rev(scale_labels_vec[seq(1, length(scale_labels_vec), 2)])
        scale_right <- rev(scale_labels_vec[seq(2, length(scale_labels_vec), 2)])
        p <- p + annotate("label", x = 0, y = 1:length(scale_labels), label = scale_left, size = 2.5, hjust = 0) +
            annotate("label", x = 6, y = 1:length(scale_labels), label = scale_right, size = 2.5, hjust = 1) +
            scale_x_continuous(limits = c(0, 6), breaks = 1:5)
    } else {
        p <- p + scale_x_discrete(limits = 1:5)
    }

    list(data = summdata, plot = p)
}

