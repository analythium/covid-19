# covid-19

> This reporitory provides the data processing for the  [**COVID-19 App**](https://hub.analythium.io/covidapp/) by Analythium.

[![deploy](https://github.com/analythium/covid-19/workflows/deploy/badge.svg)](https://github.com/analythium/covid-19/actions)

## Global

### Data sources

This project takes the data from https://github.com/CSSEGISandData/COVID-19,
does some data wrangling, fits exponential smoothing state space model,
and deploys the forecast along the raw data as a
static JSON API.
Countries with multiple provinces/states are also combined.

### API

The API is based on the slugified region names that are listed here:
https://hub.analythium.io/covid-19/api/v1/regions/

For example the slug `canada-combined` takes us to
https://hub.analythium.io/covid-19/api/v1/regions/canada-combined/

The date of last update and session info can be found at
https://hub.analythium.io/covid-19/api/v1/

Latest workdwide cases with latitude and longitude: https://hub.analythium.io/covid-19/api/v1/data/world/latest

## Canada

### Data sources

https://health-infobase.canada.ca/src/data/covidLive/covid19.csv

### API

https://hub.analythium.io/covid-19/api/v1/data/canada/regions

## Alberta

### Data sources

We scrape the Alberta COVID-19 report: https://covid19stats.alberta.ca/

### API

Go to https://hub.analythium.io/covid-19/api/v1/data/alberta/latest.

Daily data are avaibale as https://hub.analythium.io/covid-19/api/v1/data/alberta/2020-10-22.json. 
Note: this data set is a raw data dump for archival purposes, format/structure has changed over the months.

## License

MIT (c) 2019-2022 Analythium Solutions Inc.
