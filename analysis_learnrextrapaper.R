# Analysis effect of summary pane (with/without) on amount of scrolling
#
#
# Author: Markus Konrad <markus.konrad@htw-berlin.de>
# Date: Aug 2024
#
source("analysis_helpers.R")

tracking_data <- load_app_sessions_tracking_data(c("36263541b9", "e47a7d46bf"), c("treat", "ctrl"))

# summer semester 2024 valid times
starttime <- ymd_hm("2024-05-15 11:55", tz = "UTC")
endtime <- ymd_hm("2024-05-15 15:00", tz = "UTC")

tracking_data <- filter(tracking_data, track_sess_start >= starttime, event_time >= starttime, event_time <= endtime) |>
    group_by(track_sess_id) |>
    mutate(track_sess_end = min(track_sess_end, max(event_time))) |>
    ungroup()

tracking_data <- filter(tracking_data, !(user_code %in% c("13d48be01652b32e", "1e8e8090e4bc1355", "9cd0199df2050778")))

track_sess_times <- tracking_sess_times(tracking_data)

plot_tracking_sess_durations(track_sess_times)

scrolldata <- select(tracking_data, group, user_code, track_sess_start, track_sess_end, type, event_time,
                     y = coord2) |>
    filter(type == "contentscroll") |>
    select(-type) |>
    group_by(group, user_code) |>
    arrange(event_time) |>
    mutate(delta = c(NA, diff(y)),
           user_start = min(event_time),
           user_end = max(event_time),
           user_duration_sec = as.numeric(as.duration(user_end - user_start))) |>
    ungroup() |>
    filter(!is.na(delta) & abs(delta) > 0 & abs(delta) < 100) |>
    select(group, user_code, user_start, user_end, user_duration_sec, delta)

head(scrolldata)

hist(scrolldata$user_duration_sec)
hist(scrolldata$delta)

scrollaggreg <- select(scrolldata, -c(user_start, user_end)) |>
    group_by(group, user_code, user_duration_sec) |>
    summarise(total_pixels = sum(abs(delta)),
              n_events = n()) |>
    ungroup() |>
    mutate(
        pixels_per_min = total_pixels / user_duration_sec * 60,
        n_events_per_min = n_events / user_duration_sec * 60
    )

n_ctrl_treat <- count(scrollaggreg, group) |> arrange(group) |> pull(n)

plt <- ggplot(scrollaggreg, aes(x = group, y = pixels_per_min)) +
    geom_boxplot() +
    scale_x_discrete(labels = sprintf(c("without summary panel\nN=%d", "with summary panel\nN=%d"), n_ctrl_treat)) +
    labs(title = "",   # title = "Mean scrolling distance"
         x = "", y = "Pixels / minute")
plt
ggsave("figures/summarypanel_scrolldist.png", plt, width = 1000, height = 1200, units = "px")

ggplot(scrollaggreg, aes(x = group, y = n_events_per_min)) +
    geom_boxplot()

t.test(pixels_per_min ~ group, scrollaggreg, alternative = "greater")

t.test(n_events_per_min ~ group, scrollaggreg, alternative = "greater")

levels(tracking_data$type)

# "attention spans" for all users in chapter 1: times where the page was visible to the user

attention_spans_chapt1 <- select(tracking_data, group, user_code, event_time, type, chapter_index, value) |>
    filter(chapter_index == 0) |>   # only use chapter 1!
    select(-chapter_index) |>
    mutate(visible = ifelse(type == "visibility_change", value == "visible", NA)) |>
    group_by(user_code) |>
    mutate(visible = ifelse(row_number() == 1, TRUE, visible)) |>
    ungroup() |>
    fill(visible) |>
    #filter(!(type %in% c("contentscroll", "mouse"))) |>
    group_by(user_code) |>
    mutate(visibility_change = c(1, diff(visible))) |>
    ungroup() |>
    filter(visibility_change >= 0) |>
    group_by(user_code) |>
    mutate(att_index = cumsum(visibility_change)) |>
    ungroup() |>
    group_by(group, user_code, att_index) |>
    summarise(start = min(event_time),
              end = max(event_time),
              duration = end - start) |>
    ungroup()

attention_spans_chapt1

ggplot(attention_spans_chapt1) +
    geom_linerange(aes(xmin = start, xmax = end, y = user_code, color = group)) +
    scale_x_datetime()

time_chapt1 <- group_by(attention_spans_chapt1, group, user_code) |>
    summarise(duration_minutes = sum(as.numeric(as.duration(duration))) / 60)

plt <- ggplot(time_chapt1, aes(x = group, y = duration_minutes)) +
    geom_boxplot() +
    scale_x_discrete(labels = sprintf(c("without summary panel\nN=%d", "with summary panel\nN=%d"), n_ctrl_treat)) +
    labs(title = "",   # title = "Time spent in the first chapter"
         x = "", y = "Duration in minutes")
plt
ggsave("figures/summarypanel_duration.png", plt, width = 1000, height = 1200, units = "px")

t.test(duration_minutes ~ group, time_chapt1, alternative = "greater")


attention_spans_chapt2 <- select(tracking_data, group, user_code, event_time, type, chapter_index, value) |>
    filter(chapter_index == 2) |>   # only use chapter 2!
    select(-chapter_index) |>
    mutate(visible = ifelse(type == "visibility_change", value == "visible", NA)) |>
    group_by(user_code) |>
    mutate(visible = ifelse(row_number() == 1, TRUE, visible)) |>
    ungroup() |>
    fill(visible) |>
    #filter(!(type %in% c("contentscroll", "mouse"))) |>
    group_by(user_code) |>
    mutate(visibility_change = c(1, diff(visible))) |>
    ungroup() |>
    filter(visibility_change >= 0) |>
    group_by(user_code) |>
    mutate(att_index = cumsum(visibility_change)) |>
    ungroup() |>
    group_by(group, user_code, att_index) |>
    summarise(start = min(event_time),
              end = max(event_time),
              duration = end - start) |>
    ungroup()

attention_spans_chapt2

ggplot(attention_spans_chapt2) +
    geom_linerange(aes(xmin = start, xmax = end, y = user_code, color = group)) +
    scale_x_datetime()

time_chapt2 <- group_by(attention_spans_chapt2, group, user_code) |>
    summarise(duration_minutes = sum(as.numeric(as.duration(duration))) / 60)

ggplot(time_chapt2, aes(x = group, y = duration_minutes)) +
    geom_boxplot() +
    scale_x_discrete(labels = sprintf(c("without summary panel\nN=%d", "with summary panel\nN=%d"), n_ctrl_treat)) +
    labs(title = "",   # title = "Time spent in the second chapter"
         x = "", y = "Duration in minutes")


attention_spans <- select(tracking_data, group, user_code, event_time, type, value) |>
    mutate(visible = ifelse(type == "visibility_change", value == "visible", NA)) |>
    group_by(user_code) |>
    mutate(visible = ifelse(row_number() == 1, TRUE, visible)) |>
    ungroup() |>
    fill(visible) |>
    #filter(!(type %in% c("contentscroll", "mouse"))) |>
    group_by(user_code) |>
    mutate(visibility_change = c(1, diff(visible))) |>
    ungroup() |>
    filter(visibility_change >= 0) |>
    group_by(user_code) |>
    mutate(att_index = cumsum(visibility_change)) |>
    ungroup() |>
    group_by(group, user_code, att_index) |>
    summarise(start = min(event_time),
              end = max(event_time),
              duration = end - start) |>
    ungroup()

attention_spans

ggplot(attention_spans) +
    geom_linerange(aes(xmin = start, xmax = end, y = user_code, color = group)) +
    scale_x_datetime()

time_taken <- group_by(attention_spans, group, user_code) |>
    summarise(duration_minutes = sum(as.numeric(as.duration(duration))) / 60)

ggplot(time_taken, aes(x = group, y = duration_minutes)) +
    geom_boxplot() +
    scale_x_discrete(labels = sprintf(c("without summary panel\nN=%d", "with summary panel\nN=%d"), n_ctrl_treat)) +
    labs(title = "",   # title = "Time spent in the first chapter"
         x = "", y = "Duration in minutes")

t.test(duration_minutes ~ group, time_taken, alternative = "greater")
