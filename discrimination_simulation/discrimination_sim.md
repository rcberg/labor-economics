Introduction
============

The purpose of this simulation (inspired by a similar simulation done in
Stata by Dr. Scott Cunningham of Baylor Univ.) is to illustrate how some
controls can seem sensible to include in a regression, but actually be
produce completely “wrong” results (or, specifically, produce estimates
biased in a direction *opposite* of the causal relationship).

Data Generating Process
=======================

Let’s consider a situation in which wages depend on **unobserved**
ability *A*<sub>*i*</sub> and some measure of status *C*<sub>*i*</sub>.
Ability further affects this standing as well, but so does
discrimination *D*<sub>*i*</sub>. For individuals facing discrimination
(*D*<sub>*i*</sub> = 1), standing is otherwise lower than it would be if
they were a “non-oppressed”-type. an important point is, regardless of
how one interprets the model, ability affects both status/standing as
well as wages on the job– no matter who you are, if you have higher
ability you can get a better job *and* get paid more.

In practice, this could perhaps be the labor market for science or
engineering; fields where women have faced sexism and unwelcomeness from
the academy. In this case *C*<sub>*i*</sub> would be some measure of
“education status,” and women would be less inclined
(*C̄*<sub>*D*</sub> &lt; *C̄* where *C̄*<sub>*D*</sub> is the group average
education for the discriminated group) to achieve educational standing
in these fields. Alternatively *C*<sub>*i*</sub> could represent some
measure of “occupational standing” where under-represented groups are
more-inclined to be “shut-out” from higher-status jobs, even if they
have the same innate ability as the rest of the population.

The model is thus:

*C*<sub>*i*</sub> = *μ*<sub>*c*</sub> + *β*<sub>*D*</sub>*D*<sub>*i*</sub> + *β*<sub>*A*</sub>*A*<sub>*i*</sub> + *ϵ*<sub>*i*</sub>
*w*<sub>*i*</sub> = *μ*<sub>*w*</sub> + *α*<sub>*C*</sub>*C*<sub>*i*</sub> + *α*<sub>*A*</sub>*A*<sub>*i*</sub> + *ν*<sub>*i*</sub>
\[**Note, we are ruling-out a channel from wages back to
status/standing.** Including such a link would of course produce a
reduced-form dependency of (*w* , *C*) on (*D* , *A*).\]

Simulation model
================

### Parameter setup

    a_c = 0 # "status" intercept
    a_w = 10 # wage intercept
    sd_c = 1 # "randomness" alt. role of luck in status
    b_disc = 1 # parameter governing discrimination penalty (0-1)
    b_c_disc = -1 # discrimination hurts your status
    b_c_ability = 2 # natural ability determines status
    b_w_c = 2 # status leads ot higher wages
    b_w_ability = 2 # talent pays off 

    D = rbinom(10000 , 1 , 0.5) # half the population is subject to discrimination in this example
    ability = rnorm(10000)
    discrimination = b_disc*D # targets of discrimination

The following is the code for the equations shown in the DGP section
above:

    C = a_c + b_c_disc*discrimination + b_c_ability*ability + rnorm(10000, mean = 0 , sd = sd_c) # top eqn above
    wage = a_w + b_w_c*C + b_w_ability*ability + rnorm(10000)  # bottom eqn above

Analysis
========

First, just to see that there’s “no funny business,” let’s look at the
results from our “God regression” that includes unobserved ability–
which we will of course rule-out the econometrician being able to
estimate.

    true_wage = lm(wage ~ C + ability ) # God regression

    summary(true_wage)

    ## 
    ## Call:
    ## lm(formula = wage ~ C + ability)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4647 -0.6908  0.0130  0.6806  4.4433 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 9.983642   0.011015  906.37   <2e-16 ***
    ## C           2.001060   0.009014  222.01   <2e-16 ***
    ## ability     2.004910   0.020714   96.79   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.009 on 9997 degrees of freedom
    ## Multiple R-squared:  0.9754, Adjusted R-squared:  0.9754 
    ## F-statistic: 1.983e+05 on 2 and 9997 DF,  p-value: < 2.2e-16

As we can clearly see, it correctly estimates the DGP for wages.

What about an econometrician tasked with measuring discriminatory wage
gaps? Their first instinct might be to look at the raw wage gap between
the discriminated group and the rest of the population by regressing
wages on an indicator for being in the discriminated class:

    ecms_wage = lm(wage ~ discrimination ) # non-Godly regression

    summary(ecms_wage)

    ## 
    ## Call:
    ## lm(formula = wage ~ discrimination)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -26.3256  -4.2170   0.0164   4.2599  27.7160 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    10.07602    0.08947  112.62   <2e-16 ***
    ## discrimination -2.12587    0.12689  -16.75   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.344 on 9998 degrees of freedom
    ## Multiple R-squared:  0.02731,    Adjusted R-squared:  0.02721 
    ## F-statistic: 280.7 on 1 and 9998 DF,  p-value: < 2.2e-16

This correctly estimates what you’d if you plugged-in the status
variable into wage, and estimated the effect of discrimination. This is
what the media might quote as the “discrimination gap”– the mean
difference in earnings between the discriminated and non-discriminated
class.

What if someone told the econometrician “Actually, members of the
discriminated class simply *appear* to have a lower wage, but they just
*choose* to \[not get a high-status education; go into a high-status
occupation; etc.\] so you have to control for *C*<sub>*i*</sub>!”
Suppose, then, the econometrician estimates the wage equation with
*C*<sub>*i*</sub> included.

    ecms_wage_bad = lm(wage ~ discrimination + C) 

    summary(ecms_wage_bad)

    ## 
    ## Call:
    ## lm(formula = wage ~ discrimination + C)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7454 -0.9199 -0.0062  0.9225  5.3789 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    9.959011   0.018938  525.88   <2e-16 ***
    ## discrimination 0.843963   0.027615   30.56   <2e-16 ***
    ## C              2.803700   0.006072  461.73   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.343 on 9997 degrees of freedom
    ## Multiple R-squared:  0.9564, Adjusted R-squared:  0.9564 
    ## F-statistic: 1.097e+05 on 2 and 9997 DF,  p-value: < 2.2e-16

We’ve estimated a *positive* effect of being in a discriminated group!
But this clearly isn’t right because as the Gods of this simulation we
**know** that this is false.

This illustrates what happens when one *conditions on a “collider.”*
Here, *C*<sub>*i*</sub> (education; occupation; etc) is called a
“collider” (see Scott Cunningham’s *Mixtape* for a deeper treatment).
The intuitive reason behind this bias is that we could’ve conditioned on
status *or* a dummy for discriminated group membership, but when we
include both, we force our model to compare discriminated and
non-discriminated individuals with the same fixed level of status *C̃*.
Yet, we know that the only individuals who face discrimination but could
get the same *C̃* as the non-discriminated, are those who have a much
higher average ability than the non-discriminated group… but these are
the very people who also receive a higher wage! In this light, we can
recognize “it”collider conditioning" as a self-inflicted form of
selection bias.
