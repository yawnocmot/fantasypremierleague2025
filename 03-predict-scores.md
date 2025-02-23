# Predict scores
Tom Conway

## Predict the scores of matches based on the teams involved

- Set up what libraries are needed, and name the precursors - requires
  running of get-data first.

- Set up a file with well coded data for team fixtures and scores

``` r
# Want to have team names and numbers from fpl_teams
# id changes season to season, but code is consistent through seasons. the elements file has both. Also want full name and short name
# Need it twice to join both home and away

fpl_team_id_home <- fpl_teams |>
  select(home_code = code,
         HomeTeam = name,
         home_short_name = short_name)

fpl_team_id_away <- fpl_teams |>
  select(away_code = code,
         AwayTeam = name,
         away_short_name = short_name)

# Join these names with fixture_down_load data for this season's fixtures by the HomeTeam and AwayTeam names.

fixture_data <- fixture_download_data |>
  left_join(fpl_team_id_home)|>
  left_join(fpl_team_id_away)|>
  select(!c(Location,Group)) |>
  janitor::clean_names()|>
  # turn date into a datetime
  mutate(date_utc = ymd_hms(date_utc))|>
  mutate(season_end = 2025)
```

    Joining with `by = join_by(HomeTeam)`
    Joining with `by = join_by(AwayTeam)`

``` r
# next - consider the open_football data - just premier league here.
# We want to get the data in the same format as fixture_data


open_football_21_24 <- open_football_21_24|>
  select(match_number,
         round_number = round,
         date,
         time,
         home_team = team1,
         away_team = team2,
         home_team_score = score_ft_1_1,
         away_team_score = score_ft_1_2,
         season_end
         ) |>
  # turn date and time into date time
  mutate(date_utc = ymd_hms(str_c(date," ",time)),
         .keep = "unused",
         .after = round_number)|>
  # turn round_number into a number
  mutate(round_number = as.numeric(str_extract(round_number,"\\d+")))

  # next we need to change the names of the teams - using a lookup table?

names_open_football <- open_football_21_24|>
  group_by(home_team)|>
  summarise()

names_fixtures <- fixture_data |>
  group_by(home_team) |>
  summarise()

# Create a function to change things.

names_open_football_to_fixtures <- function(names_data){
  # FC after each one
  str_replace_all({{names_data}},
                  c(" FC" = "",
                    # AFC before Bournemouth
                    "AFC " = "",
                    # & Hove Albion after Brighton
                    " & Hove Albion" = "",
                    # Wolves vs Wolverhampton Wanderers
                    "Wolverhampton Wanderers" = "Wolves",
                    # City after Leicester
                    "Leicester City" = "Leicester",
                    # Spurs vs Tottenham Hotspur
                    "Tottenham Hotspur" = "Spurs",
                    # Manchester vs Man
                    "Manchester" = "Man",
                    # Utd vs United for Manchester
                    "Man United" = "Man Utd",
                    # No United after Newcastle or West Ham
                    " United" = "",
                    # Nottingham Forest to Nott'm Forest
                    "Nottingham" = "Nott'm")
  )
}

# Check it works
names_open_football <- names_open_football |>
  mutate(home_team = names_open_football_to_fixtures(home_team))

# It does, apply to the main open_football dataset

open_football_21_24 <- open_football_21_24 |>
  mutate(home_team = names_open_football_to_fixtures(home_team),
         away_team = names_open_football_to_fixtures(away_team)
  )

# Then add short names and codes

name_finder <- fixture_data |>
  select(home_team, home_code, home_short_name)|>
  group_by(home_team, home_code, home_short_name) |>
  summarise()|>
  rename(team = home_team,
         code = home_code,
         short_name = home_short_name)|>
  # create dummy codes for those not here - can come back and change if promoted
  rbind(tribble(~team, ~code, ~short_name,
               "Burnley", 99, "BUR",
               "Leeds", 98, "LEE",
               "Luton Town", 97, "LUT",
               "Norwich City", 96, "NOR",
               "Sheffield", 95, "SHU",
               "Watford", 94, "WAT",
               ))
```

    `summarise()` has grouped output by 'home_team', 'home_code'. You can override
    using the `.groups` argument.

``` r
open_football_21_24 <- open_football_21_24 |>
  left_join(name_finder, by = join_by(home_team == team))|>
  rename(home_code = code,
         home_short_name = short_name)|>
  left_join(name_finder, by = join_by(away_team == team))|>
  rename(away_code = code,
         away_short_name = short_name)|>
  relocate(season_end,.after = away_short_name)

# Now should all be the same, can we rbind?

all_fixtures <- rbind(fixture_data, open_football_21_24)

all_fixtures
```

    # A tibble: 1,520 × 12
       match_number round_number date_utc            home_team     away_team     
              <dbl>        <dbl> <dttm>              <chr>         <chr>         
     1            1            1 2024-08-16 19:00:00 Man Utd       Fulham        
     2            2            1 2024-08-17 11:30:00 Ipswich       Liverpool     
     3            3            1 2024-08-17 14:00:00 Arsenal       Wolves        
     4            4            1 2024-08-17 14:00:00 Everton       Brighton      
     5            5            1 2024-08-17 14:00:00 Newcastle     Southampton   
     6            6            1 2024-08-17 14:00:00 Nott'm Forest Bournemouth   
     7            7            1 2024-08-17 16:30:00 West Ham      Aston Villa   
     8            8            1 2024-08-18 13:00:00 Brentford     Crystal Palace
     9            9            1 2024-08-18 15:30:00 Chelsea       Man City      
    10           10            1 2024-08-19 19:00:00 Leicester     Spurs         
    # ℹ 1,510 more rows
    # ℹ 7 more variables: home_team_score <dbl>, away_team_score <dbl>,
    #   home_code <dbl>, home_short_name <chr>, away_code <dbl>,
    #   away_short_name <chr>, season_end <dbl>

NEXT STEP IS TO GO BACK AND SAVE ALL DATA AS FILES EXPLICITLY THAT WILL
BE NEEDED

t

- Set up a poisson model based on each team having an attack score and a
  defence score

- Plus home advantage

- Plus changes over time

- Create some visuals for demonstrating teams’ ability and how it
  changes over time.
