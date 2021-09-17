#---------------------------------------------------#
# EDP 380C 16: HLM
# Introduction, Syllabus, and Review
#
# Copyright Brian T. Keller 2021 all rights reserved
#---------------------------------------------------#

# Install required packages
install.packages('remotes')
remotes::install_gitlab('bkeller/edp380c-16-hlm')

# Look at dropout data
dropout <- hlm::dropout
hlm::describe(dropout[,paste0('math',7:12)])

# Histogram of math10
hlm::simplehist(dropout$math10, xlab='math10',
                 main='Histogram of math10')

# Fit a normal distribution model to the data
mdl <- lm(math10 ~ 1, dropout)
summary(mdl) # Summarize model

### Construct diagnostic plot

# Compute model implied values
math10 <- na.omit(dropout$math10)
Theoretical <- qnorm(ppoints(math10), mean = 63.5738, sd = 13.65)

# Set up plot
plot(NULL, xlim=c(10,115), ylim=c(25,95),
     ylab="math10", xlab="Theoretical Values")

# Plot middle line
abline(0,1)

# Plot points
points(Theoretical,sort(math10))

# Using HLM package
# qq 'like' plot for model
hlm::simpleqq(mdl)

# Fit a normal distribution model with rblimp
mdl.bl <- hlm::rblimp('math10 ~ 1', dropout,
                      seed = 19873, iter = 10000)
hlm::describe(mdl.bl)

# Fit a normal distribution model with rstanarm
mdl.st <- rstanarm::stan_glm(math10 ~ 1,
                             data = dropout,
                             seed = 19873)
print(mdl, digits = 3)