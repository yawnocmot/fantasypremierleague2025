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

# Total_players - how many players are currently in the game - updates in real time.

fpl_total_players <- fpl_data$total_players

# Element_stats - a list of the different stats that players have associated with them.
# Each has a display name and a data column name

fpl_element_stats <- tibble(fpl_data$element_stats)

# Element_types - a list of the 4 different player types, plus managers, and their attributes.

fpl_element_types <- tibble(fpl_data$element_types)

# Elements - the big table of players - have as a tibble. Over 100 variables about each player. This will be the key source of data.

fpl_elements <- tibble(fpl_data$elements)
```

fpl

- Find data of results by team

- Other team or player data?

- Get Opta projections

### Noting later data cleaning tasks

- Deadline date needs to be in a time format

- 
