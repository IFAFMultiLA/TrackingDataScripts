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

ggplot(scrollaggreg, aes(x = group, y = pixels_per_min)) +
    geom_boxplot() +
    scale_x_discrete(labels = c("without summary panel", "with summary panel")) +
    labs(title = "Mean scrolling distance",
         x = "", y = "Pixels / minute")

ggplot(scrollaggreg, aes(x = group, y = n_events_per_min)) +
    geom_boxplot()

t.test(pixels_per_min ~ group, scrollaggreg, alternative = "greater")

t.test(n_events_per_min ~ group, scrollaggreg, alternative = "greater")
