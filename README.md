# R scripts for preparing and analyzing data collected with the MultiLA web API

Markus Konrad [markus.konrad\@htw-berlin.de](mailto:markus.konrad@htw-berlin.de){.email}, January 2024

## Data preparation

You can download collected tracking data from the MultiLA administration interface on the *Export* page. Download data for a specific application session (processing all data at once is not reasonable and not supported) and unzip it. The unzipped CSV files with the collected data should then be placed under `data/raw/<application_session_id>/`. It should contain four CSV files: `app_sessions.csv`, `tracking_events.csv`, `tracking_sessions.csv` and `user_feedback.csv`.

Next, run `prepare.R`. This will transform the raw data for all application sessions in `data/raw` to a flat data frame which is in detail described in the next section. The resulting data will be located at `data/prepared/<application_session_id>_tracking_data.rds`.

## Codebook for prepared tracking events data

The prepared data will be stored under `data/prepared/<application_session_id>_tracking_data.rds`. It contains a dataframe of tracked events with the following variables:

-   `user_app_sess_code`: user application session code (session code for an individual anonymous or registered user interacting with a specific application session) -- factor
-   `user_app_sess_user_id`: user ID for registered users; no further data on individual users is provided in this dataset -- integer for registered users or NA for anonymous users
-   `track_sess_id`: tracking session ID (ID indicating a continuous interaction of a user with the application session on a single device) -- integer
-   `track_sess_start`: start of the tracking session (first visit of a user on this device for this application session) -- POSIXct time
-   `track_sess_end`: end of the tracking session (user closes the browser window of logs out) -- POSIXct time
-   `user_agent`: "user agent"" string from the browser -- character string
-   `client_ip`: client IP address (if it could be determined)
-   `form_factor`: factor; `"desktop"`, `"tablet"` or `"phone"`
-   `initial_win_width`: initial client browser window width; numeric
-   `initial_win_height`: initial client browser window height; numeric
-   `initial_contentview_width`: initial content view width; numeric
-   `initial_contentview_height`: initial content view height; numeric
-   `initial_contentscroll_width`: initial content scroll area width; numeric
-   `initial_contentscroll_height`: initial content scroll area height; numeric
-   `event_time`: time the event took place; POSIXct time with millisecond accuracy
-   `type`: event type; factor
-   `chapter_index`: content chapter index; integer
-   `chapter_id`: content chapter ID label; factor
-   `ex_label`: question or exercise label; factor
-   `ex_id`: question or exercise ID; character string
-   `ex_event`: learnr code exercise event type; character string (`"result"` or `"submitted"`)
-   `ex_output`: learnr code exercise R output; character string
-   `ex_correct`: learnr code exercise result assessment -- correct or uncorrect; logical
-   `xpath`: XPath to HTML element related to the event (if any); character string
-   `css`: CSS selector to HTML element related to the event (if any); character string
-   `value`: value related to the event; for input events this is the value that was entered; for code exercises this is the code the user submitted; for question exercises, this is the submitted answer (multiple answers are formatted as `[answer1, answer2, ... answern]`); character string
-   `coord1`: X coordinate for spatial events like mouse moves or clicks; numeric
-   `coord2`: Y coordinate for spatial events like mouse moves or clicks; numeric
-   `win_width`: current client browser window width at the time when the event occurred; numeric
-   `win_height`: current client browser window height at the time when the event occurred; numeric
-   `contentview_width`: current content view width at the time when the event occurred; numeric
-   `contentview_height`: current content view height at the time when the event occurred; numeric
-   `win_scroll_x`: current client browser window scroll X position; numeric
-   `win_scroll_y`: current client browser window scroll Y position; numeric
-   `content_scroll_x`: current content area scroll X position; numeric
-   `content_scroll_y`: current content area scroll Y position; numeric

## Data analysis

All data analyses are performed in the RMarkdown files in this repository. Common data analysis helper functions are stored in `analysis_helpers.R`. Analysis documents and scripts:

-   `analysis_stosta_wise2324.Rmd`: preliminary analysis for the data collected on 15 students of the course "Stochastik und induktive Statistik" (computer science & economics master, HTW Berlin) that were using the *TestgenauigkeitBayes* learning application
-   `inspect_mousetracks.R`: small script for trying out analysis of mouse track data
