# covid-19
> COVID-19 analysis

This project takes the data from https://github.com/CSSEGISandData,
does some data wrangling, fits exponential smoothing state space model,
and deploys the 14-day forecast along the raw data as a
static JSON API using GitHub pages.

Countries with multiple provinces/states are also combined.

The API is based on the slugified region names that are listed here:
https://psolymos.github.io/covid-19/api/v1/regions/

For example the slug `canada-combined` takes us to
https://psolymos.github.io/covid-19/api/v1/regions/canada-combined/

The date of last update and R session info can be found at
https://psolymos.github.io/covid-19/api/v1/

Travis CI is updating the API using a daily cron job:
https://travis-ci.org/github/psolymos/covid-19
