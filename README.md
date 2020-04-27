# covid-19
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fanalythium%2Fcovid-19.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fanalythium%2Fcovid-19?ref=badge_shield)

> COVID-19 analysis

This project takes the data from https://github.com/CSSEGISandData/COVID-19,
does some data wrangling, fits exponential smoothing state space model,
and deploys the forecast along the raw data as a
static JSON API using GitHub pages.
Countries with multiple provinces/states are also combined.

The API is based on the slugified region names that are listed here:
https://analythium.github.io/covid-19/api/v1/regions/

For example the slug `canada-combined` takes us to
https://analythium.github.io/covid-19/api/v1/regions/canada-combined/

The date of last update and R session info can be found at
https://analythium.github.io/covid-19/api/v1/

Travis CI is updating the API using a daily cron job:
https://travis-ci.org/github/analythium/covid-19

We also scrape the Alberta COVID-19 report: https://covid19stats.alberta.ca/


## License
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fanalythium%2Fcovid-19.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fanalythium%2Fcovid-19?ref=badge_large)