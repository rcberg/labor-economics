a_O = 100
a_W = 10
sd_O = 0.5
b_d = 1 # parameter governing discrimination penalty "women" face (0-1)
b_O_disc = 5
b_O_ability = 5
b_W_occ = 2
b_W_disc = 1
b_W_ability = 2

women = rbinom(10000 , 1 , 0.5)
ability = rnorm(10000)

discrimination = b_d*women
occupation = a_O - b_O_disc*discrimination + b_O_ability * ability + rnorm(10000, mean = 0 , sd = sd_O)
wage = a_W + b_W_occ*occupation - b_W_disc*discrimination + b_W_ability*ability + rnorm(10000)

true_wage = lm(wage ~ occupation + discrimination + ability)
ecms_wage = lm(wage ~ occupation + women )

summary(true_wage)
summary(ecms_wage)
