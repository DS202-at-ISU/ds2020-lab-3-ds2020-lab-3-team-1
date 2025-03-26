
<!-- README.md is generated from README.Rmd. Please edit the README.Rmd file -->

# Lab report \#3 - instructions

Follow the instructions posted at
<https://ds202-at-isu.github.io/labs.html> for the lab assignment. The
work is meant to be finished during the lab time, but you have time
until Monday evening to polish things.

Include your answers in this document (Rmd file). Make sure that it
knits properly (into the md file). Upload both the Rmd and the md file
to your repository.

All submissions to the github repo will be automatically uploaded for
grading once the due date is passed. Submit a link to your repository on
Canvas (only one submission per team) to signal to the instructors that
you are done with your submission.

# Lab 3: Avenger’s Peril

## As a team

Extract from the data below two data sets in long form `deaths` and
`returns`

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# Read the data
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)


deaths <- av %>%
  select(Name.Alias, starts_with("Death")) %>%
  pivot_longer(cols = starts_with("Death"),
               names_to = "Time",
               names_prefix = "Death",
               values_to = "Death") %>%
  mutate(Time = parse_number(Time),
         Death = ifelse(Death == "", NA, Death)) 

head(deaths)
```

    ## # A tibble: 6 × 3
    ##   Name.Alias                     Time Death
    ##   <chr>                         <dbl> <chr>
    ## 1 "Henry Jonathan \"Hank\" Pym"     1 YES  
    ## 2 "Henry Jonathan \"Hank\" Pym"     2 <NA> 
    ## 3 "Henry Jonathan \"Hank\" Pym"     3 <NA> 
    ## 4 "Henry Jonathan \"Hank\" Pym"     4 <NA> 
    ## 5 "Henry Jonathan \"Hank\" Pym"     5 <NA> 
    ## 6 "Janet van Dyne"                  1 YES

``` r
returns <- av %>%
  select(Name.Alias, starts_with("Return")) %>%
  pivot_longer(cols = starts_with("Return"),
               names_to = "Time",
               names_prefix = "Return",
               values_to = "Return") %>%
  mutate(Time = parse_number(Time),
         Return = ifelse(Return == "", NA, Return)) 

head(returns)
```

    ## # A tibble: 6 × 3
    ##   Name.Alias                     Time Return
    ##   <chr>                         <dbl> <chr> 
    ## 1 "Henry Jonathan \"Hank\" Pym"     1 NO    
    ## 2 "Henry Jonathan \"Hank\" Pym"     2 <NA>  
    ## 3 "Henry Jonathan \"Hank\" Pym"     3 <NA>  
    ## 4 "Henry Jonathan \"Hank\" Pym"     4 <NA>  
    ## 5 "Henry Jonathan \"Hank\" Pym"     5 <NA>  
    ## 6 "Janet van Dyne"                  1 YES

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

Similarly, deal with the returns of characters.

Based on these datasets calculate the average number of deaths an
Avenger suffers.

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check. Questions we can
> pick from 1. “Out of 173 listed Avengers, my analysis found that 69
> had died at least one time after they joined the team.” 2. “Jocasta —
> an android based on Janet van Dyne and built by Ultron — has been
> destroyed five times and then recovered five times.” 3. “On 57
> occasions the individual made a comeback.” 4.

Tirmidi Mohamed: “”

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

Tirmidi’s code -

### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor.

Upload your changes to the repository. Discuss and refine answers as a
team.
