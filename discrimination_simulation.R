a_occ = 0 # "occupation status" intercept
a_w = 10 # wage intercept
sd_occ = 1 # "randomness" alt. role of luck in occupation status
b_disc = 0.25 # parameter governing discrimination penalty "women" face (0-1)
b_occ_disc = 1 # discrimination affects your occupational status
b_occ_ability = 2 # natural ability determines occupational status
b_w_occ = 2 # occupational status leads ot higher wages
b_w_ability = 2 # 

women = rbinom(10000 , 1 , 0.5)
ability = rnorm(10000)

discrimination = b_disc*women # women are targets of discrimination
occupation = a_occ - b_occ_disc*discrimination + b_occ_ability*ability + rnorm(10000, mean = 0 , sd = sd_occ) # higher ability sort into higher occupational status
wage = a_w + b_w_occ*occupation + b_w_ability*ability + rnorm(10000) # in this model, regardless of whether you're good at your job or not, higher "occupational status" pays off. similarly no matter your job, being good at your job pays off.

true_wage = lm(wage ~ occupation + ability) # this estimates the true wage DGP
ecms_wage = lm(wage ~ women ) # media quotes a wage gap
ecms_wage_bad = lm(wage ~ occupation + women) # idiot thinks controlling for occupation helps; estimates positive effects which we know for a fact aren't causal

summary(ecms_wage)
summary(ecms_wage_bad)

summary(true_wage)