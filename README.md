# R scripts for preparing and analyzing data collected with the MultiLA web API

Markus Konrad <markus.konrad@htw-berlin.de>, January 2024


## Installation

- install [renv](https://rstudio.github.io/renv/index.html) if you haven't yet
- then install all R dependencies via `renv::restore()`

## Data preparation

You can download collected tracking data from the MultiLA administration interface on the *Export* page. Download data for a specific application session (processing all data at once is not reasonable and not supported) and unzip it. The unzipped CSV files with the collected data should then be placed under `data/raw/<application_session_id>/`. It should contain four CSV files: `app_sessions.csv`, `tracking_events.csv`, `tracking_sessions.csv` and `user_feedback.csv`.

Next, run `prepare.R`. This will transform the raw data for all application sessions in `data/raw` to a flat data frame which is in detail described in the next section. The resulting data will be located at `data/prepared/<application_session_id>_tracking_data.rds`.

## Codebook for prepared tracking events data

See `data/prepared/codebook_prepared_data.pdf` in this repository.

