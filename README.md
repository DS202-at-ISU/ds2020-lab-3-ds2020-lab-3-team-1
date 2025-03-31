
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

    ## Warning: package 'tidyverse' was built under R version 4.4.2

    ## Warning: package 'readr' was built under R version 4.4.2

    ## Warning: package 'forcats' was built under R version 4.4.2

    ## Warning: package 'lubridate' was built under R version 4.4.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# Read the data
av <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/avengers/avengers.csv", stringsAsFactors = FALSE)

av %>% 
  select(
    Name.Alias,
    starts_with("Death")
  ) %>% 
  head()
```

    ##                    Name.Alias Death1 Death2 Death3 Death4 Death5
    ## 1   Henry Jonathan "Hank" Pym    YES                            
    ## 2              Janet van Dyne    YES                            
    ## 3 Anthony Edward "Tony" Stark    YES                            
    ## 4         Robert Bruce Banner    YES                            
    ## 5                Thor Odinson    YES    YES                     
    ## 6      Richard Milhouse Jones     NO

``` r
deaths <- av %>% 
  pivot_longer(
    starts_with("Death"),
    names_to = "Time",
    values_to = "Died"
  ) %>%
  select(
    URL, Name.Alias, Time, Died
  )
head(deaths)
```

    ## # A tibble: 6 × 4
    ##   URL                                                Name.Alias      Time  Died 
    ##   <chr>                                              <chr>           <chr> <chr>
    ## 1 http://marvel.wikia.com/Henry_Pym_(Earth-616)      "Henry Jonatha… Deat… "YES"
    ## 2 http://marvel.wikia.com/Henry_Pym_(Earth-616)      "Henry Jonatha… Deat… ""   
    ## 3 http://marvel.wikia.com/Henry_Pym_(Earth-616)      "Henry Jonatha… Deat… ""   
    ## 4 http://marvel.wikia.com/Henry_Pym_(Earth-616)      "Henry Jonatha… Deat… ""   
    ## 5 http://marvel.wikia.com/Henry_Pym_(Earth-616)      "Henry Jonatha… Deat… ""   
    ## 6 http://marvel.wikia.com/Janet_van_Dyne_(Earth-616) "Janet van Dyn… Deat… "YES"

``` r
total_deaths <- deaths %>%
  filter(tolower(Died) == "yes") %>%
  nrow()
```

Get the data into a format where the five columns for Death\[1-5\] are
replaced by two columns: Time, and Death. Time should be a number
between 1 and 5 (look into the function `parse_number`); Death is a
categorical variables with values “yes”, “no” and ““. Call the resulting
data set `deaths`.

``` r
average_deaths <- deaths %>%
  mutate(
    Time = parse_number(Time)
  ) %>% 
  group_by(URL, Died) %>% 
  summarise(
    total_death = max(Time)
  ) %>% 
  filter(Died != "") %>%
  ungroup() %>% 
  mutate(
    total_death = if_else(Died == "NO" & total_death == 1, 0, total_death) #Sets the people who never died to 0 deaths
  ) %>%
  summarise(
    average_total_death = mean(total_death, na.rm = TRUE)
  ) %>%
  pull(average_total_death) # Extract the value
```

    ## `summarise()` has grouped output by 'URL'. You can override using the `.groups`
    ## argument.

``` r
print(average_deaths)
```

    ## [1] 0.5229885

``` r
maxdeaths <- deaths %>% 
  mutate(
    Time = parse_number(Time)
  ) %>% 
  group_by(URL, Died) %>% 
  summarise(
    total_death = max(Time)
  ) %>%
  filter(Died != "")
```

    ## `summarise()` has grouped output by 'URL'. You can override using the `.groups`
    ## argument.

``` r
maxdeaths %>% 
  ungroup() %>% 
  count(Died, total_death)
```

    ## # A tibble: 6 × 3
    ##   Died  total_death     n
    ##   <chr>       <dbl> <int>
    ## 1 NO              1   104
    ## 2 NO              2     1
    ## 3 YES             1    53
    ## 4 YES             2    14
    ## 5 YES             3     1
    ## 6 YES             5     1

``` r
grouped_deaths <- maxdeaths %>%
  group_by(Died, total_death) %>%
  tally()
```

Similarly, deal with the returns of characters.

``` r
returns <- av %>% 
  pivot_longer(
    starts_with("Return"),
    names_to = "TimeReturned",
    values_to = "Returned"
  ) %>%
  select(
    URL, Name.Alias, TimeReturned, Returned,
  )

maxreturns <- returns %>% 
  mutate(
    TimeReturned = parse_number(TimeReturned)
  ) %>% 
  group_by(URL, Returned) %>% 
  summarise(
    total_return = max(TimeReturned)
  ) %>%
  filter(Returned != "")
```

    ## `summarise()` has grouped output by 'URL'. You can override using the `.groups`
    ## argument.

``` r
maxreturns %>% 
  ungroup() %>% 
  count(Returned, total_return)
```

    ## # A tibble: 6 × 3
    ##   Returned total_return     n
    ##   <chr>           <dbl> <int>
    ## 1 NO                  1    23
    ## 2 NO                  2     8
    ## 3 NO                  3     1
    ## 4 YES                 1    38
    ## 5 YES                 2     7
    ## 6 YES                 5     1

``` r
maxreturns %>%
  group_by(Returned, total_return) %>%
  tally()
```

    ## # A tibble: 6 × 3
    ## # Groups:   Returned [2]
    ##   Returned total_return     n
    ##   <chr>           <dbl> <int>
    ## 1 NO                  1    23
    ## 2 NO                  2     8
    ## 3 NO                  3     1
    ## 4 YES                 1    38
    ## 5 YES                 2     7
    ## 6 YES                 5     1

Based on these datasets calculate the average number of deaths an
Avenger suffers.

The average death an avenger suffers is 0.5229, assuming we treat the
Died=NO and total_death=1 as 0 deaths. However, if you just average the
results from the maxdeaths (let the Died=NO and total_death=1 stay as 1
death) table you get 1.12069.

## Individually

For each team member, copy this part of the report.

Each team member picks one of the statements in the FiveThirtyEight
[analysis](https://fivethirtyeight.com/features/avengers-death-comics-age-of-ultron/)
and fact checks it based on the data. Use dplyr functionality whenever
possible.

### FiveThirtyEight Statement

> Quote the statement you are planning to fact-check. Questions we can
> pick from 2. “Jocasta — an android based on Janet van Dyne and built
> by Ultron — has been destroyed five times and then recovered five
> times.” 3. “On 57 occasions the individual made a comeback.”

Tirmidi Mohamed: Question: “I counted 89 total deaths.”

### Include the code

Make sure to include the code to derive the (numeric) fact for the
statement

Tirmidi’s code -

``` r
total_deaths <- deaths %>%
  filter(tolower(Died) == "yes") %>%
  nrow()

total_deaths
```

    ## [1] 89

Megan’s code - Out of 173 listed Avengers, my analysis found that 69 had
died at least one time after they joined the team.

``` r
at_least_once <- deaths %>%
  mutate(
    Time = parse_number(Time)
  ) %>% 
  group_by(URL, Died) %>% 
  summarise(
    total_death = max(Time)
  ) %>%
  filter(Died != "") %>%
  filter(Died == "YES") %>%
  nrow()
```

    ## `summarise()` has grouped output by 'URL'. You can override using the `.groups`
    ## argument.

``` r
at_least_once
```

    ## [1] 69

Ryan Jensen’s code - There’s a 2-in-3 chance that a member of the
Avengers returned from their first stint in the afterlife, but only a 50
percent chance they recovered from a second or third death.8

``` r
died_once <- deaths %>% 
  mutate(
    Time = parse_number(Time)
  ) %>% 
  filter(Time == 1,
         Died == "YES") %>% 
  nrow()
died_once
```

    ## [1] 69

``` r
died_Two_or_Three <- deaths %>% 
  mutate(
    Time = parse_number(Time)
  ) %>% 
  filter(Time == 2 | Time == 3,
         Died == "YES") %>% 
  nrow()
died_Two_or_Three
```

    ## [1] 18

``` r
one_return <- returns %>%
  mutate(
    return = parse_number(TimeReturned)
  ) %>%
  filter(Returned == "YES", return == 1) %>%
  nrow()
one_return
```

    ## [1] 46

``` r
two_or_three_return <- returns %>%
  mutate(
    return = parse_number(TimeReturned)
  ) %>%
  filter(Returned == "YES", return == 2 | return == 3) %>%
  nrow()
two_or_three_return
```

    ## [1] 9

Next students code -

### Include your answer

Include at least one sentence discussing the result of your
fact-checking endeavor. **Tirmidi’s answer** - The statement that there
were a total of 89 deaths is true. I was able to check by tallying up
all the yes for deaths and it adds up to 89 so this is true.

**Megan’s answer** - The print statement shows that the number of
avengers who have died at least once is equal to 69. I used the deaths
table, calculated the total deaths similar to the maxdeaths table, and
filtered if they had died or not. Each row represents a unique avenger
who has died, so the count of rows, which is 69, proves that 69 avengers
died at least once.

**Ryan’s answer** Out of the 69 Avengers that died once, 46 returned
from that death leaving a return rate of 46/69 = 2/3. Out of the 18
avengers that died two or three times, 9 of them returned leaving a
return rate of 9/18 = 1/2.

Upload your changes to the repository. Discuss and refine answers as a
team.
