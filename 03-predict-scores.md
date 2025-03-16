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

- Set up a poisson model based on each team having an attack score and a
  defence score

``` r
# Very simple model says score ~ Poiss(attacker + defender + home/away)
# So actually need to reformat the data again.
# Need each line to be attacker, defender, home/away, score
# create functions to pivot from score per row to match per row and vice versa

score_per_row <- function(data) {
  data|> pivot_longer(
    cols = c(home_team_score, away_team_score),
    names_to = "home_or_away"
  )|>
  mutate(
    attacker_name = if_else(home_or_away == "home_team_score",
                            home_team,
                            away_team),
    defender_name = if_else(home_or_away == "home_team_score",
                            away_team,
                            home_team),
    attacker_code = if_else(home_or_away == "home_team_score",
                            home_code,
                            away_code),
    defender_code = if_else(home_or_away == "home_team_score",
                            away_code,
                            home_code),
    attacker_short_name = if_else(home_or_away == "home_team_score",
                                  home_short_name,
                                  away_short_name),
    defender_short_name = if_else(home_or_away == "home_team_score",
                                  away_short_name,
                                  home_short_name),
    home_or_away = if_else(home_or_away == "home_team_score",
                          "home",
                          "away")
  )
}


match_per_row <- function(data, score){
  data |>
  mutate(
    home_team = if_else(home_or_away == "home",
                        attacker_name,
                        defender_name),
    away_team = if_else(home_or_away == "home",
                        defender_name,
                        attacker_name),
    home_code = if_else(home_or_away == "home",
                        attacker_code,
                        defender_code),
    away_code = if_else(home_or_away == "home",
                        defender_code,
                        attacker_code),
    home_short_name = if_else(home_or_away == "home",
                              attacker_short_name,
                              defender_short_name),
    away_short_name = if_else(home_or_away == "home",
                              defender_short_name,
                              attacker_short_name),
  ) |>
    select(season_end, round_number, match_number, date_utc, home_team, away_team,
           home_or_away, {{score}}, home_code, away_code, home_short_name, away_short_name
           )  |>
  pivot_wider(names_from = home_or_away,
              values_from = {{score}})

}

fixtures_poisson_format <- all_fixtures|>
  score_per_row() |>
  select(season_end, round_number, match_number, date_utc,
         attacker_short_name, defender_short_name, home_or_away, score = value,
         attacker_name, defender_name, attacker_code, defender_code)

# Want to separate into past and future - regress on the past, predict the future

fixtures_poisson_past <- fixtures_poisson_format |>
  filter(!is.na(score))

fixtures_poisson_future <- fixtures_poisson_format |>
  filter(is.na(score))

# Now, create a basic poisson regression model on the past fixtures
# Use short_name for understandability

poisson_model <- poisson_reg()|>
  set_engine("glm")

poisson_recipe <- recipe(score ~ attacker_short_name + 
                                 defender_short_name + 
                                 home_or_away,
                         data = fixtures_poisson_past)

poisson_workflow <- workflow() %>%
  add_recipe(poisson_recipe) %>%
  add_model(poisson_model)

poisson_fit <- poisson_workflow |>
  fit(data = fixtures_poisson_past)

# This gives a list of coefficients for different scores.
# From this we can identify a league table for how many goals each would be expected to score against each other.

# Score 19% more goals at home
```

- Second model - weighted by time

``` r
# Choose an arbitrary decay parameter - 0.005 means last week's game has 3% less influence than today's game, last year's game has 16% of the influence.

decay_parameter <- 0.005

# Next version of model - use a weighted iteration.
# Need to include weights
fixtures_poisson_past <- fixtures_poisson_past |>
  mutate(days_ago = (today() %--% date_utc)/days(1),
         weight = exp(decay_parameter*days_ago))|>
  arrange(desc(days_ago))

# Now fit a weighted poisson model - this still the same as above

poisson_weighted_recipe <- recipe(score ~ attacker_short_name + defender_short_name + home_or_away,
                           data = fixtures_poisson_past)

poisson_weighted_model <- poisson_reg() |>
  set_engine("glm")

poisson_weighted_workflow <- workflow() |>
  add_model(poisson_model)|>
  add_recipe(poisson_weighted_recipe)

poisson_weighted_fit <- 
  fit(poisson_weighted_workflow, data = fixtures_poisson_past)

weighted_fit <- glm(score ~ attacker_short_name + 
            defender_short_name + 
            home_or_away,
    data = fixtures_poisson_past,
    family = "poisson",
    weights = weight)
```

- Create a table to demonstrate the quality of teams in relation to the
  model - weighted poisson data feels more right.

``` r
# Have everyone play everyone and see total goal difference. Use this to create a team ranking.

all_2025_fixtures <- fixtures_poisson_format |>
   filter(season_end==2025)|>
   select(!score)

predicted_all_fixtures <- all_2025_fixtures |>
  bind_cols(predict(weighted_fit, newdata = all_2025_fixtures, type = "response")) |>
  rename(.pred = ...12)
```

    New names:
    • `` -> `...12`

``` r
offensive_rank <- predicted_all_fixtures|>
  group_by(attacker_short_name)|>
  rename(short_name = attacker_short_name)|>
  summarise(offence_score = sum(.pred) /38)|>
  arrange(desc(offence_score))

defensive_rank <- predicted_all_fixtures|>
  group_by(defender_short_name)|>
  rename(short_name = defender_short_name)|>
  summarise(defence_score = sum(.pred) /38)|>
  arrange(defence_score)

total_rank <- full_join(offensive_rank, defensive_rank) |>
  mutate(advantage_score = offence_score - defence_score)|>
  arrange(desc(advantage_score))|>
  left_join(name_finder)|>
  relocate(team)
```

    Joining with `by = join_by(short_name)`
    Joining with `by = join_by(short_name)`

``` r
total_rank
```

    # A tibble: 20 × 6
       team           short_name offence_score defence_score advantage_score  code
       <chr>          <chr>              <dbl>         <dbl>           <dbl> <dbl>
     1 Liverpool      LIV                2.32          1.02          1.30       14
     2 Arsenal        ARS                1.96          0.814         1.14        3
     3 Man City       MCI                2.08          1.19          0.892      43
     4 Newcastle      NEW                1.85          1.44          0.411       4
     5 Chelsea        CHE                1.81          1.43          0.376       8
     6 Spurs          TOT                1.90          1.54          0.360       6
     7 Nott'm Forest  NFO                1.71          1.36          0.353      17
     8 Bournemouth    BOU                1.61          1.33          0.282      91
     9 Crystal Palace CRY                1.44          1.23          0.211      31
    10 Brentford      BRE                1.62          1.50          0.127      94
    11 Brighton       BHA                1.60          1.49          0.112      36
    12 Fulham         FUL                1.44          1.44         -0.00418    54
    13 Everton        EVE                1.14          1.21         -0.0706     11
    14 Aston Villa    AVL                1.48          1.56         -0.0801      7
    15 Man Utd        MUN                1.35          1.49         -0.142       1
    16 Wolves         WOL                1.32          1.80         -0.479      39
    17 West Ham       WHU                1.21          1.69         -0.481      21
    18 Ipswich        IPS                0.986         2.20         -1.21       40
    19 Leicester      LEI                0.815         2.22         -1.41       13
    20 Southampton    SOU                0.754         2.45         -1.70       20

``` r
# next thing is to predict a league table
# Need to have each row be a match

# Want a function that can calculate the win, lose and draw probabilities from two poisson variables
# The skellam functions do this

# Want it to be for the future
predicted_future_fixtures <- fixtures_poisson_future |>
  select(!score)|>
  bind_cols(predict(weighted_fit, newdata = fixtures_poisson_future, type = "response")) |>
  rename(.pred = ...12)
```

    New names:
    • `` -> `...12`

``` r
predict_future_rows <- predicted_future_fixtures|>
  match_per_row(.pred)|>
  mutate(prob_home_win = 1-pskellam(0,home,away),
          prob_draw = dskellam(0,home,away),
          prob_away_win = pskellam(-1,home,away)
  )

# We want to combine the past scores with the predicted rows to create a table

past_rows <- fixtures_poisson_past|>
  match_per_row(score) |>
  filter(season_end == 2025)|>
  mutate(prob_home_win = as.numeric(home > away),
          prob_draw = as.numeric(home == away),
          prob_away_win = as.numeric(away > home)
  )

predict_match_rows <- bind_rows(past_rows, predict_future_rows)

# next format this as a league table

home_record <- summarise(predict_match_rows,
                         .by = home_short_name,
                         home_wins = sum(prob_home_win),
                         home_draws = sum(prob_draw),
                         home_losses = sum(prob_away_win)
)|>
  rename(short_name = home_short_name)


away_record <- summarise(predict_match_rows,
                         .by = away_short_name,
                         away_wins = sum(prob_away_win),
                         away_draws = sum(prob_draw),
                         away_losses = sum(prob_home_win)
)|>
  rename(short_name = away_short_name)

pred_future_table <- left_join(home_record, away_record, by = join_by(short_name))|>
  mutate(
    wins = home_wins + away_wins,
    draws = home_draws + away_draws,
    losses = home_losses + away_losses,
    points = 3*wins + draws,
    .keep = "unused"
  )|>
  arrange(desc(points))


# Then display this alongside 'now' ranks, as the final summary table

display_table <- left_join(pred_future_table, total_rank, by = join_by(short_name))|>
  select(team, short_name, offence_score, defence_score, points, wins, draws, losses) |>
  mutate(across(where(is.numeric), ~ format(round(.,1), nsmall = 1)))

display_table
```

    # A tibble: 20 × 8
       team         short_name offence_score defence_score points wins  draws losses
       <chr>        <chr>      <chr>         <chr>         <chr>  <chr> <chr> <chr> 
     1 Liverpool    LIV        2.3           1.0           88.7   "26.… " 8.… " 2.6"
     2 Arsenal      ARS        2.0           0.8           75.8   "21.… "12.… " 4.7"
     3 Nott'm Fore… NFO        1.7           1.4           69.0   "20.… " 8.… " 9.6"
     4 Man City     MCI        2.1           1.2           66.8   "19.… " 7.… "10.6"
     5 Newcastle    NEW        1.9           1.4           63.8   "18.… " 7.… "12.0"
     6 Chelsea      CHE        1.8           1.4           63.6   "18.… " 9.… "10.7"
     7 Brighton     BHA        1.6           1.5           60.2   "15.… "13.… " 9.3"
     8 Bournemouth  BOU        1.6           1.3           58.5   "16.… "10.… "11.8"
     9 Aston Villa  AVL        1.5           1.6           56.6   "15.… "11.… "11.8"
    10 Fulham       FUL        1.4           1.4           54.3   "14.… "11.… "12.4"
    11 Brentford    BRE        1.6           1.5           53.7   "15.… " 7.… "15.4"
    12 Crystal Pal… CRY        1.4           1.2           52.5   "13.… "11.… "12.9"
    13 Spurs        TOT        1.9           1.5           50.0   "14.… " 6.… "17.3"
    14 Man Utd      MUN        1.3           1.5           46.8   "12.… " 9.… "16.1"
    15 Everton      EVE        1.1           1.2           45.0   " 9.… "15.… "12.8"
    16 West Ham     WHU        1.2           1.7           44.5   "11.… " 9.… "17.2"
    17 Wolves       WOL        1.3           1.8           37.0   "10.… " 7.… "21.0"
    18 Ipswich      IPS        1.0           2.2           23.7   " 4.… " 9.… "23.6"
    19 Leicester    LEI        0.8           2.2           23.5   " 5.… " 6.… "25.6"
    20 Southampton  SOU        0.8           2.5           13.6   " 3.… " 4.… "30.4"

- Calculate for future games the chances of clean sheets, and save as
  CSV.

  ``` r
  predict_future_rows <- predict_future_rows|>
    mutate(
      prob_home_clean_sheet = dpois(0,away),
      prob_away_clean_sheet = dpois(0,home)
      )

  write_csv(predict_future_rows,"processed_data/predict_future_rows.csv")
  ```
