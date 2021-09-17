#---------------------------------------------------#
# EDP 380C.16: Hierarchical Linear Modeling
# Multiple Regression Review - Analysis
#
# Copyright Brian T. Keller 2021 all rights reserved
#---------------------------------------------------#

#---------------------------------------------------#
#### Prepare Data ####

# Check for package updates and set filedir
remotes::update_packages('hlm', upgrade = 'always')
hlm::set_filedir()

# Subset dropout data
dropout <- na.omit(hlm::dropout[,c('math7','math8','math10','male','mothed')])

#---------------------------------------------------#
#### Two Predictor Model ####

# Two Predictor Model via lm
mdl.2p.lm <- lm(math10 ~ math7 + math8, dropout)
summary(mdl.2p.lm)

# Two Predictor Model via blimp
mdl.2p.bl <- hlm::rblimp('math10 <- math7 math8', dropout,
                         burn = 1000, iter = 10000, seed = 398721)
hlm::describe(mdl.2p.bl)

# Two Predictor Model via rstanarm
mdl.2p.st <- rstanarm::stan_glm(math10 ~ math7 + math8,
                                data = dropout, seed = 139781)
summary(mdl.2p.st, digits = 3, probs = c(.025, 0.5, 0.975))

#---------------------------------------------------#
#### Bayesian Comparison of R2 ####

# Refit One Predictor Model with blimp
mdl.1p.bl <- hlm::rblimp('math10 <- math7', dropout,
                         burn = 1000, iter = 10000,
                         seed = 398721)

# Histogram of R2 from each model 
hist1 <- hlm::simplehist(mdl.1p.bl, 5, breaks=50, plot = F)
hist2 <- hlm::simplehist(mdl.2p.bl, 7, breaks=50, plot = F)
hlm::simplehist(list(hist1, hist2), col= c('#bf5700','#333f48'))

#---------------------------------------------------#
#### Multiple Predictor Model ####

# Three Predictor Model via lm
mdl.mp.lm <- lm(math10 ~ math7 + math8 + male, dropout)
summary(mdl.mp.lm)

# Three Predictor Model via blimp
mdl.mp.bl <- hlm::rblimp('math10 <- math7 math8 male', dropout,
                         burn = 1000, iter = 10000, seed = 398721)
hlm::describe(mdl.mp.bl)

# Three Predictor Model via rstanarm
mdl.mp.st <- rstanarm::stan_glm(math10 ~ math7 + math8 + male,
                                data = dropout, seed = 139781)
summary(mdl.mp.st, digits = 3, probs = c(.025, 0.5, 0.975))

#---------------------------------------------------#
#### Categorical Predictors ####

# Set up factor
dropout$mothedF <- factor(dropout$mothed)

# One-Factor ANOVA
mdl.1f.a <- aov(math10 ~ mothedF, dropout)
summary(mdl.1f.a)

# Categorical Predictors via lm
mdl.1f.lm <- lm(math10 ~ mothedF, dropout)
summary(mdl.1f.lm)

# Test between two models
mdl.nu.lm <- lm(math10 ~ 1, dropout) # Null Model
anova(mdl.nu.lm, mdl.1f.lm)

# Categorical Predictors via lm
mdl.1f.bl <- hlm::rblimp('math10 <- mothed', dropout,
                         waldtest = 'math10 <- mothed@0',
                         nominal = 'mothed',
                         seed = 198373, iter = 10000)
hlm::summary(mdl.1f.bl)
hlm::waldtest(mdl.1f.bl) # Prints out waldtest information

# Categorical Predictors via rstanarm
mdl.1f.st <- rstanarm::stan_glm(math10 ~ mothedF, 
                                data = dropout, seed = 1291)
summary(mdl.1f.st, digits = 3, probs = c(.025, 0.5, 0.975))

#---------------------------------------------------#
#### Estimating an ANCOVA with Adjusted means ####

# Center function
center <- function(x) x - mean(x)

# ANCOVA via lm
mdl.ancova.lm <- lm(math10 ~ mothedF + center(math7), dropout)
summary(mdl.ancova.lm)

# ANCOVA via blimp
mdl.ancova.bl <- hlm::rblimp('math10 <- mothed center(math7)', dropout,
                             nominal = 'mothed',
                             seed = 198373, iter = 10000)

# ANCOVA via rstanarm
mdl.ancova.st <- rstanarm::stan_glm(math10 ~ mothedF + center(math7), 
                                    data = dropout, seed = 1291)
summary(mdl.ancova.st, digits = 3, probs = c(.025, 0.5, 0.975))

# Computing adjusted means via lm
# Extract Regression Coefficients
gammas <- coef(mdl.ancova.lm)
adj_mns.lm <- c(
    # First Mean is the intercept (gamma_0)
    mothedF1 = gammas[[1]],
    # The rest are (gamma_0) + the dummy code slope
    gammas[1] + gammas[c(-1,-6)]
)
adj_mns.lm
