#---------------------------------------------------#
# EDP 380C.16: Hierarchical Linear Modeling
# Rethinking Regression - Analysis
#
# Copyright Brian T. Keller 2021 all rights reserved
#---------------------------------------------------#

#---------------------------------------------------#
#### Prepare Data ####

# Check for package updates and set filedir
remotes::update_packages('hlm', upgrade = 'always')
hlm::set_filedir()

# Subset dropout data
dropout <- na.omit(hlm::dropout[,c('math7','math8','math10','mothed')])
hlm::describe(dropout[,c('math7','math10')])

#---------------------------------------------------#
#### Fitting Single Predictor Model ####

# Linear Model with lm
mdl.lm <- lm(math10 ~ math7, dropout)
summary(mdl.lm)

# Linear Model with rstanarm
mdl.st <- rstanarm::stan_glm(math10 ~ math7, data = dropout,
                             seed = 139781)
# Print to 3 decimals, median, and with 95% intervals
summary(mdl.st, digits = 3, probs = c(.025, 0.5, 0.975))

# View Histogram of gamma1 with rstanarm
hlm::simplehist(as.matrix(mdl.st)[,'math7'], round = F, breaks = 70)

# Linear Model with blimp
mdl.bl <- hlm::rblimp('math10 <- math7', fixed = 'math7', dropout,
                      burn = 1000, iter = 10000, seed = 398721)
# Print Estimates
hlm::describe(mdl.bl)

# View histogram of gamma1
hlm::simplehist(mdl.bl, param = 3, round = F, breaks = 70)

#---------------------------------------------------#
#### Sampling Dist v. Posterior Dist ####

# Extract intercept and slope
b_0 <- coef(mdl.lm)[1]; b_1 <- coef(mdl.lm)[2];

# Extract residual variance
sigma_e <- sigma(mdl.lm)

# Function to simulate one replication
oneRep <- function() {
    # Get sample size
    N <- NROW(dropout)
    
    # Simulate 'math7'
    math7 <- with(dropout, rnorm(N, mean(math7), sd(math7)))
    
    # Simulate 'math10'
    math10 <- rnorm(N, mean = b_0 + b_1*math7, sd = sigma_e)
    
    # Return the regression parameters using simulated data
    results <- coef( lm(math10 ~ math7) )
    return(results)
}

# Set pseudo random number seed
set.seed(12723)

# Run simulation 4000 times
results <- replicate(4000, oneRep())

# Histogram of sampling distribution
hlm::simplehist(results[2,],round = F, breaks = 70)

# Obtain draws from rstanarm
draws <- as.matrix(mdl.st)[,'math7']

# Obtain objects of plots, but don't plot them
sampPlot <- hlm::simplehist(results[2,], round = F,
                             breaks = 70, plot = F)
postPlot <- hlm::simplehist(draws, round = F,
                             breaks = 70, plot = F)

# Plot them together with small offset 'off = 0.005'
hlm::simplehist(list(sampPlot,postPlot), off = 0.005,
                col= c('#bf5700','#333f48'),
                main = 'Sampling Dist v. Posterior Dist.')

#---------------------------------------------------#
#### Transformations and Centering ####

## Centering

# Create inline center function
center <- function(x) x - mean(x)

# Centering Predictors with lm
mdl.lm.cent <- lm(math10 ~ center(math7), dropout)
summary(mdl.lm.cent)

# Centering Predictors with rstanarm
mdl.st.cent <- rstanarm::stan_glm(math10 ~ center(math7), 
                                  data = dropout, seed = 139781)
summary(mdl.st.cent, digits = 3, probs = c(.025, 0.5, 0.975))

# Centering Predictors with blimp
mdl.bl.cent <- hlm::rblimp('math10 <- center(math7)',
                           fixed = 'math7', dropout, seed = 398721,
                           burn = 1000, iter = 10000)
hlm::describe(mdl.bl.cent)

# Standardize math10 and math7 and save back into dropout
dropout <- within(dropout, {
    math10.std <- scale(math10); math7.std <- scale(math7);
})

# Standardized with lm
mdl.lm.std <- lm(math10.std ~ math7.std, dropout)
summary(mdl.lm.std)

# Standardized with blimp
# Note: blimp already produces standardized solution
mdl.bl

# Standardized with rstanarm
mdl.st.std <- rstanarm::stan_glm(math10.std ~ math7.std, 
                                 data = dropout, seed = 139781)
summary(mdl.st.std, digits = 3, probs = c(.025, 0.5, 0.975))

