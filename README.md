fluoR
=====

Calcium imaging methods produce massive datasets that require immense
data manipulation, exploration, and summarizing. fluoR provides
highly-accessible functions to address these issues for both
inexperienced and seasoned R users to save researchers time. This
package is catered to live-recorded fluorescence data, but works with
any type of waveform data. A few functions include:

-   `format_data` converts a data frame or matrix to a fluoR-friendly
    format
-   `summarize_trials` calculates a summary statistic; collapsing across
    trials for each timestamp
-   `inflect_points` finds inflection points by using derivatives
-   `find_peaks` uses an algorithm to find peaks/valleys with n points
    increasing/decreasing on each side
-   `z_score` calculates a standard or modified z-score without adding R
    object attributes

The Examples vignette explains these functions and more in greater
detail. This can be viewed using `browseVignettes('fluoR')`.

There is also [a fluoR manual in Gitbook
format](https://bookdown.org/anta8363/fluoR_bookdown/) that I will be
regularly updating over the next few months.

Installation
------------

``` r
### Install from Github
devtools::install_github('atamalu/fluoR', build_vignettes = TRUE)
```

``` r
library(fluoR)
```

Getting started
---------------

Since there is currently no ubiquitous way to analyze or format calcium
imaging data, most of fluoR’s commands require the data frame to be in
“fluoR format.” This is essentially a transposed time series data frame;
where the times of recorded signals are in the first row or column, and
the observed values of each trial are in the following rows or columns.

``` r
### Format data
df <- format_data(GCaMP)
```

``` r
### What does the median value of trials 1, 2, 3, and 4 look like?
head( summarize_trials(dataframe = df, trials = 1:4, summary.type = 'median') )
#>      Time summ.trials
#> 1 -3.9902     82.7735
#> 2 -3.9803     82.7890
#> 3 -3.9705     82.8170
#> 4 -3.9607     82.8565
#> 5 -3.9508     82.8985
#> 6 -3.9410     82.9365
```

``` r
### Where are the inflection points (peaks/valleys) in trial 1?
head( inflect_points(x = df$Trial1) )
#> [1] 0 0 2 0 0 0
```

``` r
### Where are the peaks in trial 1 with at least 10 decreasing values on 
head( find_peaks(xvals = df$Trial1, n.points = 10) )
#> [1]   8  27  44  78 100 122
```

``` r
### Transform raw input values into modified z-scores (using median & mad) for trial 1
head( z_score(xvals = df$Trial1, z.type = 'modified') )
#> [1] 0.5840183 0.4935366 0.4770854 0.5346646 0.6470813 0.7704654
```
