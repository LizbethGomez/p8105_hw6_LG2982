p8105\_hw6\_LG2982
================
Lizbeth Gomez
11/22/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.3
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
theme_set(theme_bw() + theme(legend.position = "bottom"))
```

# Data import and cleaning:

  - Load and clean the data for regression analysis:

<!-- end list -->

``` r
birth = read_csv("./data/birthweight.csv") %>% 
  mutate(babysex = recode(babysex, 
                          "1" = "Male",
                          "2" = "Female"),
         fincome = fincome * 100,
         frace = recode(frace,
                        "1" = "White",
                        "2" = "Black",
                        "3" = "Asian",
                        "4" = "Puetro Rican",
                        "8" = "Other",
                        "9" = "Unknown"),
         malform = recode(malform, 
                          "0" = "Yes",
                          "1" = "No"
                          ),
         mrace = recode(mrace,
                        "1" = "White",
                        "2" = "Black",
                        "3" = "Asian",
                        "4" = "Puetro Rican",
                        "8" = "Other")
         ) 
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
sum(is.na(birth))
```

    ## [1] 0

*There are no missing values in this dataset*

  - Propose a regression model for birthweight.
