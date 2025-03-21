# FPL model plan
Tom Conway

## Plan - what do I need to do and status of each

## Start as a GitHub project?

- Explore GitHub and make it work - Done

## Overarching structure

- work out headline model and create different files which will hold the
  engines. Rest of tasks are based on what was used last time.
  - Format of different files - each one is a QMD file that renders both
    as R markdown and HTML
  - For each engine we have a QMD file., each independent but need to
    render them in order

## Get data

- Connect to FPL API to get its data

- Find data of results by team from open football and fixture download

- In time, I might find other team or player data

- Or other projections to compare with mine

## Predict the scores of matches based on the teams involved

- Import historic team score data

- Create a model for projecting the scores of future fixtures - poisson
  attack vs defence scores + home advantage + exponential time weighting
  backoff

- This also displays a table of a predicted league table, with as-of-now
  scores for attack and defence ability

## Next - predict player scores

- Import player data

- Model different kinds of player scores:

  - Goalkeeper

  - Defender

  - Midfielder

  - Striker

- Prediction for each player score for future games

## Pick a team

- Create and model algorithm for picking players to make up a 15

## Add other features of the game - note here we will need to go back and edit the other engines.

- Injuries

- Transfers

- Chips

- Assistant manager

- Newly promoted teams

- New players

- Bonus points

- Yellow cards

## Refining and modelling - not in place yet.

- Create season emulator for projecting the performance of different
  models across the season / for predicting the next gameweek

- Think of different options for model - both in individual elements and
  in overarching structure

- Try out different algorithms for each part of the model and evaluate
  them
