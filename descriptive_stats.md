Eating habits
================
Yue Chen
11/29/20

``` r
ehresp = read.csv("ehresp_2014.csv")
```

The dataset contains 11212 responses and 37 variables. We will examine
bmi as an outcome variable, and how factors such as income level,
exercise frequency, access to grocery stores, primary eating time, and
secondary eating time, contribute to bmi.

## Descriptive statistics

Income.

``` r
ehresp %>%
   filter(erincome > 0) %>%
   group_by(erincome) %>%
   summarise(n_distinct(tucaseid)) %>%
   mutate(
      income_level = case_when(
         erincome == 1 ~ "Income > 185% of poverty threshold", 
         erincome == 2 ~ "Income <= 185% of poverty threshold", 
         erincome == 3 ~ "130% of poverty threshold < Income < 185% of poverty threshold", 
         erincome == 4 ~ "Income > 130% of poverty threshold",
         erincome == 5 ~ "Income <= 130% of poverty threshold"))
```

    ## # A tibble: 5 x 3
    ##   erincome `n_distinct(tucaseid… income_level                                   
    ##      <int>                 <int> <chr>                                          
    ## 1        1                  6990 Income > 185% of poverty threshold             
    ## 2        2                   533 Income <= 185% of poverty threshold            
    ## 3        3                   976 130% of poverty threshold < Income < 185% of p…
    ## 4        4                    36 Income > 130% of poverty threshold             
    ## 5        5                  2397 Income <= 130% of poverty threshold

BMI, height in inches, and weight in pounds.

``` r
ehresp %>%
   select(erbmi, euhgt, euwgt) %>%
   filter(erbmi > 0, euhgt > 0, euwgt > 0) %>%
   summary() %>%
   format(scientific = F, digits = 2)
```

    ##      erbmi             euhgt             euwgt        
    ##  "Min.   :13.00  " "Min.   :56.00  " "Min.   : 98.0  "
    ##  "1st Qu.:23.60  " "1st Qu.:64.00  " "1st Qu.:145.0  "
    ##  "Median :26.60  " "Median :66.00  " "Median :170.0  "
    ##  "Mean   :27.77  " "Mean   :66.69  " "Mean   :176.3  "
    ##  "3rd Qu.:30.70  " "3rd Qu.:70.00  " "3rd Qu.:200.0  "
    ##  "Max.   :73.60  " "Max.   :77.00  " "Max.   :340.0  "
