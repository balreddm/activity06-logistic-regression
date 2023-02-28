Activity 6 - Logistic Regression
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(tidymodels)
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.0.0 ──
    ## ✔ broom        1.0.0     ✔ rsample      1.1.0
    ## ✔ dials        1.0.0     ✔ tune         1.0.0
    ## ✔ infer        1.0.3     ✔ workflows    1.0.0
    ## ✔ modeldata    1.0.0     ✔ workflowsets 1.0.0
    ## ✔ parsnip      1.0.1     ✔ yardstick    1.0.0
    ## ✔ recipes      1.0.1     
    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ scales::discard() masks purrr::discard()
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ recipes::fixed()  masks stringr::fixed()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ yardstick::spec() masks readr::spec()
    ## ✖ recipes::step()   masks stats::step()
    ## • Search for functions across packages at https://www.tidymodels.org/find/

## Lading dataset

``` r
resume = readr::read_csv("https://www.openintro.org/data/csv/resume.csv")
```

    ## Rows: 4870 Columns: 30
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): job_city, job_industry, job_type, job_ownership, job_req_min_exper...
    ## dbl (20): job_ad_id, job_fed_contractor, job_equal_opp_employer, job_req_any...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
resume
```

    ## # A tibble: 4,870 × 30
    ##    job_ad_id job_city job_indu…¹ job_t…² job_f…³ job_e…⁴ job_o…⁵ job_r…⁶ job_r…⁷
    ##        <dbl> <chr>    <chr>      <chr>     <dbl>   <dbl> <chr>     <dbl>   <dbl>
    ##  1       384 Chicago  manufactu… superv…      NA       1 unknown       1       0
    ##  2       384 Chicago  manufactu… superv…      NA       1 unknown       1       0
    ##  3       384 Chicago  manufactu… superv…      NA       1 unknown       1       0
    ##  4       384 Chicago  manufactu… superv…      NA       1 unknown       1       0
    ##  5       385 Chicago  other_ser… secret…       0       1 nonpro…       1       0
    ##  6       386 Chicago  wholesale… sales_…       0       1 private       0       0
    ##  7       386 Chicago  wholesale… sales_…       0       1 private       0       0
    ##  8       385 Chicago  other_ser… secret…       0       1 nonpro…       1       0
    ##  9       386 Chicago  wholesale… sales_…       0       1 private       0       0
    ## 10       386 Chicago  wholesale… sales_…       0       1 private       0       0
    ## # … with 4,860 more rows, 21 more variables: job_req_education <dbl>,
    ## #   job_req_min_experience <chr>, job_req_computer <dbl>,
    ## #   job_req_organization <dbl>, job_req_school <chr>, received_callback <dbl>,
    ## #   firstname <chr>, race <chr>, gender <chr>, years_college <dbl>,
    ## #   college_degree <dbl>, honors <dbl>, worked_during_school <dbl>,
    ## #   years_experience <dbl>, computer_skills <dbl>, special_skills <dbl>,
    ## #   volunteer <dbl>, military <dbl>, employment_holes <dbl>, …

``` r
# The {tidymodels} method for logistic regression requires that the response be a factor variable
resume <- resume %>% 
  mutate(received_callback = as.factor(received_callback))

resume_mod <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(received_callback ~ race, data = resume, family = "binomial")

tidy(resume_mod) %>% 
  knitr::kable(digits = 3)
```

| term        | estimate | std.error | statistic | p.value |
|:------------|---------:|----------:|----------:|--------:|
| (Intercept) |   -2.675 |     0.083 |   -32.417 |       0 |
| racewhite   |    0.438 |     0.107 |     4.083 |       0 |

``` r
resume_select <- resume %>% 
  rename(sex = gender) %>% 
  filter(job_city == "Chicago") %>% 
  mutate(race = case_when(
         race == "white" ~ "White",
         TRUE ~ "Black"
       ),
       sex = case_when(
         sex == "f" ~ "female",
         TRUE ~ "male"
       )) %>% 
  select(received_callback, years_experience, race, sex)
```
