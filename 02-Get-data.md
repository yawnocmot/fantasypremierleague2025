# Get data
Tom Conway

## Get data

### Get data from FPL API and structure into separate files.

Get data from FPL API - keep separate so don’t re-run it.

``` r
url <- "https://fantasy.premierleague.com/api/bootstrap-static/"
response <- GET(url)

fpl_data <- fromJSON(content(response, "text"))
```

    No encoding supplied: defaulting to UTF-8.

Next - take out the different tables and give good names. Make the data
frames into

``` r
# Chips - gives chips with names, quantity, start and end, and chip types
# Data structure is almost normal, but lots associated with the assistant manager chip is not.
# Add assistant manager to the list of things to work out later.
fpl_chips <- tibble(fpl_data$chips)

# Events - gives details of each game week.
# Key columns appear to be the deadline, whether the week has passed, is current, or is next, 
# and details of which players have been most chosen or transferred in.

fpl_events <- tibble(fpl_data$events)

# Game_settings - a list containing the parameters for the game. 
# Can capture as might be helpful to reference these if things change in different seasons (e.g. squad value)

fpl_game_settings <- fpl_data$game_settings

# Game_config - a further list with some additional rules (broken into other lists).
# Contains useful parameters like the number of points for different events

fpl_game_config <- fpl_data$game_config

# Phases - the different phases that scores get summarised on, with their start and end weeks
# Whole season plus months.

fpl_phases <- tibble(fpl_data$phases)

# Teams - lists teams with a unique code, whether they have played this week, and some indications of their overall strength.

fpl_teams <- tibble(fpl_data$teams)

fpl_teams |> write_csv("processed_data/fpl_teams.csv")

# Total_players - how many players are currently in the game - updates in real time.

fpl_total_players <- fpl_data$total_players

# Element_stats - a list of the different stats that players have associated with them.
# Each has a display name and a data column name

fpl_element_stats <- tibble(fpl_data$element_stats)

# Element_types - a list of the 4 different player types, plus managers, and their attributes.

fpl_element_types <- tibble(fpl_data$element_types)

# Elements - the big table of players - have as a tibble. Over 100 variables about each player. This will be the key source of data.

fpl_elements <- tibble(fpl_data$elements)

fpl_elements |> write_csv("processed_data/fpl_elements.csv")

# Fixtures - this is also available from the API, the future fixtures.

fpl_fixtures <- tibble(fromJSON("https://fantasy.premierleague.com/api/fixtures/"))

fpl_fixtures |> write_csv("processed_data/fpl_fixtures.csv")
```

fpl

- Find data of results by team - get data from Openfootball API in
  Github. Openfootball has the full list of scores in table format, but
  with some errors. It also depends on when someone updates their
  repository on github, which slows down the process. Can I find a
  better place to get live premier league scores from?

``` r
url2 <- "https://raw.githubusercontent.com/openfootball/football.json/refs/heads/master/2024-25/en.1.json"
response2 <-GET(url2)

open_football_data <- fromJSON(content(response2,"text"))

open_football_data_2 <- open_football_data$matches |>
  unnest_wider(score, names_sep = "_")|>
  unnest_wider(score_ht, names_sep = "_")|>
  unnest_wider(score_ht_1, names_sep = "_")|>
  unnest_wider(score_ft, names_sep = "_")|>
  unnest_wider(score_ft_1, names_sep = "_")

# Also get data from the earlier years

open_football_year <- function(url) {
  response <- GET(url)
  open_football_data <- fromJSON(content(response,"text"))
  open_football_data <- open_football_data$matches |>
  unnest_wider(score, names_sep = "_")|>
  unnest_wider(score_ht, names_sep = "_")|>
  unnest_wider(score_ht_1, names_sep = "_")|>
  unnest_wider(score_ft, names_sep = "_")|>
  unnest_wider(score_ft_1, names_sep = "_")
  open_football_data
}

open_football_2023_24 <- open_football_year("https://github.com/openfootball/football.json/raw/refs/heads/master/2023-24/en.1.json")
open_football_2022_23 <- open_football_year("https://github.com/openfootball/football.json/raw/refs/heads/master/2022-23/en.1.json")
open_football_2021_22 <- open_football_year("https://github.com/openfootball/football.json/raw/refs/heads/master/2021-22/en.1.json")

open_football_21_24 <- rbind(
  open_football_2021_22 |> mutate(
    match_number = row_number(date),
    season_end = 2022),
  open_football_2022_23 |> mutate(
    match_number = row_number(date),
    season_end = 2023),
  open_football_2023_24 |> mutate(
    match_number = row_number(date),
    season_end = 2024)
)

open_football_21_24 |> write_csv("processed_data/open_football_21_24.csv")

# and also championship data for information on promoted teams

open_football_championship_2024_25 <- open_football_year("https://github.com/openfootball/football.json/raw/refs/heads/master/2024-25/en.2.json")
open_football_championship_2023_24 <- open_football_year("https://github.com/openfootball/football.json/raw/refs/heads/master/2023-24/en.2.json")
open_football_championship_2022_23 <- open_football_year("https://github.com/openfootball/football.json/raw/refs/heads/master/2022-23/en.2.json")|>
  select(!c(score_et,score_p))
open_football_championship_2021_22 <- open_football_year("https://github.com/openfootball/football.json/raw/refs/heads/master/2021-22/en.2.json")|>
  select(!c(score_et,score_p))

open_football_champ_21_25 <- rbind(
  open_football_championship_2021_22 |> mutate(
    match_number = row_number(date),
    season_end = 2022)|>
    select(-stage),
  open_football_championship_2022_23 |> mutate(
    match_number = row_number(date),
    season_end = 2023)|>
    select(-stage),
  open_football_championship_2023_24 |> mutate(
    match_number = row_number(date),
    season_end = 2024)|>
    select(-stage),
  open_football_championship_2024_25 |> mutate(
    match_number = row_number(date),
    season_end = 2025)|>
    select(-score_et)
)

open_football_champ_21_25 |> write_csv("processed_data/open_football_champ_21_25.csv")
```

- Openfootball seems unreliable - here is another JSON API from
  fixturedownload.com

``` r
url3 <- "https://fixturedownload.com/feed/json/epl-2024"
response3 <- GET(url3)

fixture_download_data <- tibble(fromJSON(content(response3, "text")))

write_csv(fixture_download_data,"processed_data/fixture_download_data.csv")
```

- Next - revert to using worldfootball R - this all doesn’t add anything
  yet, so delete it. We will go back and get past goals scored

- Projections to bring in?
