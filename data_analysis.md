data\_analysis
================
Qetsiyah Wang
11/25/2020

# Tidy Dataset

``` r
raw_eathealth = read.csv("ehresp_2014.csv") %>%
  mutate(
    tucaseid = as.factor(tucaseid)
  ) %>%
  select(tucaseid, starts_with(c("er", "eu")), -erhhch)
```

## A. Descriptive Statistics

``` r
desc_eathealth = raw_eathealth %>%
  select(erbmi, euwgt, euhgt, erincome) %>%
  filter_all(all_vars(. > 0)) %>%
  mutate(
    erincome = factor(erincome, labels = c("Income > 185% of poverty threhold", "Income <= 185% of poverty threshold",
                                           "130% < Income < 185% of poverty threshold",
                                           "Income > 130% of poverty threshold", "Income <= 130% of poverty threshold"))
  )
```

## B. External and Internal Indicators that affects the eating health

``` r
external_indicators = raw_eathealth %>%
  select(erbmi, eugenhth, erincome, eustores) %>%
  filter_all(all_vars(. > 0)) %>%
  rename("physical_health" = "eugenhth") %>%
  mutate(
    physical_health = factor(physical_health, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
     erincome = factor(erincome, labels = c("Income > 185% of poverty threhold", "Income <= 185% of poverty threshold",
                                           "130% < Income < 185% of poverty threshold",
                                           "Income > 130% of poverty threshold", "Income <= 130% of poverty threshold")),
    eustores = factor(eustores, labels = c("Grocery Store", "Supercenter", "Warehouse Club", 
                                           "Drugstore or Convenience Store", "Some Other Place"))
  )

internal_indicators = raw_eathealth %>%
  select(erbmi, eugenhth, eufastfdfrq, euexfreq) %>%
  filter_all(all_vars(. > 0)) %>%
  rename("physical_health" = "eugenhth") %>%
  mutate(
    physical_health = factor(physical_health, labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")))
```

## C. Hypothesis Testing between eating

``` r
prim_sec_eat = raw_eathealth %>%
  select(erbmi, ertpreat, ertseat) %>%
  filter_all(all_vars(. > 0))
```
