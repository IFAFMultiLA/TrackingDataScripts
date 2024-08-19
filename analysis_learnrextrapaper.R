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


survey <- survey_data(tracking_data)

survey_questions <- list(
    bedienung_ctrl = "Without summary panel: The UI of this app is  ...",
    bedienung_treat = "With summary panel: The UI of this app is  ...",
    praxisbeispiel = "I find clinical tests as practical example ...",
    verwendung_uebung = "In class, we should ...",
    verwendung_zuhause = "At home, I would ...",
    schwierigkeit = "I find the complexity of the topic ...",
    zufriedenheit = "All in all, I find this app ..."

)
survey_scale <- list(
    bedienung_ctrl = c("hard to understand and use", "easy to understand and use"),
    bedienung_treat = c("hard to understand and use", "easy to understand and use"),
    praxisbeispiel = c("uninteresting", "interesting"),
    verwendung_uebung = c("never use such apps", "always use such apps"),
    verwendung_zuhause = c("never use such apps", "always use such apps"),
    schwierigkeit = c("very difficult", "very easy"),
    zufriedenheit = c("very bad", "very good")
)

numdata <- mutate(survey,
                  bedienung_treat = ifelse(group == "treat", bedienung, NA),
                  bedienung_ctrl = ifelse(group == "ctrl", bedienung, NA)) |>
    select(-c(group, bedienung, mehrere_tabs, kommentar)) |>
    pivot_longer(schwierigkeit:bedienung_ctrl, names_to = "item") |>
    filter(!is.na(value), item %in% names(survey_questions)) |>
    mutate(item = factor(item, levels = names(survey_questions)))

summdata <- summarise(numdata, n = n(), .by = c(item, value)) |>
    mutate(value = as.integer(value)) |>
    arrange(item, value)

items <- levels(numdata$item)

plts <- lapply(1:length(items), function(i) {
    it <- items[i]
    itemdata <- filter(summdata, item == it)
    max_y <- max(summdata$n)
    p <- ggplot(itemdata, aes(x = value, y = n)) +
        geom_col(fill = "gray") +
        geom_vline(aes(xintercept = median(value))) +
        scale_x_continuous(limits = c(0.5, 5.5)) +
        scale_y_continuous(limits = c(0,  max_y)) +
        labs(title = survey_questions[[it]],
             #subtitle = sprintf("N=%d", sum(itemdata$n)),
             x = "", y = "") +
        annotate("label", x = c(0.5, 5.5), y = max_y, label = survey_scale[[it]],
                 hjust = c(0, 1), vjust = 1, size = 2.5) +
        annotation_custom(
            grob = grid::textGrob(sprintf("N=%d", sum(itemdata$n)), hjust = 1, gp = grid::gpar(cex = 0.75)),
            xmin = 5.5, xmax = 5.5, ymin = max_y + 3, ymax = max_y + 3) +
        coord_cartesian(clip = "off") +
        theme(panel.background = element_rect(fill = "white", color = NA),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.line.x = element_line(color = "darkgray"),
              plot.title = element_text(size = 10),
              axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 7))

    if (i < length(items)) {
        p <- p + theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       plot.subtitle = element_text(hjust = 1))
    }

    # if (i > 1) {
    #     p <- p + theme(axis.text.y = element_blank(),
    #                    axis.ticks.y = element_blank())
    # }

    p
})

p <- wrap_plots(plts, ncol = 1)
p

ggsave("figures/survey_results.png", p, width = 1000, height = 1000, units = "px", scale = 2)

