Discrimination simulation
================
R.C.B.
1/15/2020

\*This

## Data Generating Process

Let’s consider a situation in which wages depend on ability \(A_i\) and
some measure of status \(C_i\). Ability affects this standing as well,
but so does discrimination \(D_i\). For individuals facing
discrimination (\(D_i = 1\)), standing is otherwise lower than it would
be if they were a “non-oppressed”-type. an important point is,
regardless of how one interprets the model, ability affects both
status/standing as well as wages on the job– no matter who you are, if
you have higher ability you can get a better job *and* get paid more.

In practice, this could perhaps be the labor market for science or
engineering; fields where women have faced sexism and unwelcomeness from
the academy. In this case \(C_i\) would be some measure of “education
status,” and women would be less inclined (\(\bar C_D < \bar C\) where
\(\bar C_D\) is the group average education for the discriminated group)
to achieve educational standing in these fields. Alternatively \(C_i\)
could represent some measure of “occupational standing” where
under-represented groups are more-inclined to be “shut-out” from
higher-status jobs, even if they have the same innate ability as the
rest of the population.

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](discrimination_sim_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
