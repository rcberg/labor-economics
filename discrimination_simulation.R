a_occ = 0
a_w = 10
sd_w = 1
b_disc = 1 # parameter governing discrimination penalty "women" face (0-1)
b_occ_disc = 1
b_occ_ability = 2
b_w_occ = 2
b_w_disc = 1
b_w_ability = 2

women = rbinom(10000 , 1 , 0.5)
ability = rnorm(10000)

discrimination = b_disc*women
occupation = a_occ - b_occ_disc*discrimination + b_occ_ability*ability + rnorm(10000, mean = 0 , sd = sd_O)
wage = a_W + b_w_occ*occupation + b_w_ability*ability + rnorm(10000)

true_wage = lm(wage ~ occupation + ability)
ecms_wage = lm(wage ~ women )
ecms_wage_bad = lm(wage ~ occupation + women)

summary(ecms_wage)
summary(ecms_wage_bad) # estimates positive effects!! wtf? simulations rool

summary(true_wage)