## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(BayesGrowth)

## ----eval = FALSE-------------------------------------------------------------
#  install.packages("rstan")

## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github("jonathansmart/BayesGrowth")

## ----model,message=FALSE, warning=FALSE, eval=FALSE---------------------------
#  library(BayesGrowth)
#  
#  # built in dataset for running examples
#  data("example_data")
#  
#  ## Biological info - lengths in mm
#  max_size <- 440
#  max_size_se <- 5
#  birth_size <- 0
#  birth_size_se <- 0.001 #  cannot be zero
#  
#  # Use the function to estimate the rstan model
#  MCMC_example_results <- Estimate_MCMC_Growth(example_data,
#                                               Model = "VB" ,
#                                               iter = 10000,
#                                               n.chains = 4,  # minimum of 3 chains recommended
#                                               BurnIn = 1000, # default is 10% of iterations
#                                               thin = 10,      # a thinning rate of 10 is applied to overcome autocorrelation
#                                               Linf = max_size,
#                                               Linf.se = max_size_se,
#                                               L0 = birth_size,
#                                               L0.se = birth_size_se,
#                                               sigma.max = 100,
#                                               verbose = TRUE,
#                                               k.max = 1)
#  
#  

## ----echo = FALSE-------------------------------------------------------------

data("MCMC_example_results")

## ----summary,message=FALSE----------------------------------------------------
library(rstan)
summary(MCMC_example_results,pars = c("Linf", "k", "L0", "sigma"))$summary
str(MCMC_example_results,max.level = 2)

## ----Diagnostics, warning=FALSE, message=FALSE,  fig.height = 8, fig.width = 8----
library(bayesplot)

mcmc_combo(MCMC_example_results,pars = c("Linf", "k", "L0", "sigma")) 


## ----GelmabRubin, warning=FALSE, message=FALSE,  fig.height = 6, fig.width = 8----
rhats <- rhat(MCMC_example_results,pars = c("Linf", "k", "L0", "sigma"))

mcmc_rhat(rhats)


## ----Autocorr, warning=FALSE, message=FALSE,  fig.height = 8, fig.width = 8----
mcmc_acf(MCMC_example_results,pars = c("Linf", "k", "L0", "sigma"))

## ----pars, warning=FALSE, message=FALSE---------------------------------------
Get_MCMC_parameters(MCMC_example_results)

## ----plot, warning=FALSE, message=FALSE, fig.height = 6, fig.width = 8, warning=FALSE----
# Return a growth curve with 50th and 95th percentiles
growth_curve <- Calculate_MCMC_growth_curve(obj = MCMC_example_results, Model = "VB",
                                            max.age = max(example_data$Age), probs = c(.5,.95))
library(tidybayes)
library(ggplot2)

ggplot(growth_curve, aes(Age, LAA))+
  geom_point(data = example_data, aes(Age, Length), alpha = .3)+
geom_lineribbon(aes( ymin = .lower, ymax = .upper,
                       fill = factor(.width)), size = .8) +  labs(y = "Total Length (mm)", x = "Age (yrs)")+
  scale_fill_brewer(palette="BuPu", direction=1,name =NULL)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), breaks = seq(0,13,1))+
  theme_bw()+
  theme(text = element_text(size = 14))


## ----compare_plot, warning=FALSE, message=FALSE, fig.height = 6, fig.width = 8, echo=FALSE----

library(AquaticLifeHistory)
fixed_mod <- Estimate_Growth(example_data,n.bootstraps = 1, Birth.Len = 0, models = "VB", plots = FALSE)
free_mod <- Estimate_Growth(example_data,n.bootstraps = 1, models = "VB", plots = FALSE)
growth_curve <- Calculate_MCMC_growth_curve(MCMC_example_results, Model = "VB",
                                            max.age = max(example_data$Age), probs = c(.95))
ggplot(growth_curve, aes(Age, LAA))+
  geom_line(data = free_mod$Estimates,inherit.aes = FALSE,
            aes(Age, AVG, col = "nls model - free L0"), size = 1.5)+
  
  geom_line(data = fixed_mod$Estimates,inherit.aes = FALSE,
            aes(Age, AVG, col = "nls model - fixed L0"), size = 1.5)+
  geom_point(data = example_data, aes(Age, Length), alpha = .3)+
  geom_lineribbon( aes(ymin = .lower, ymax =.upper,fill = "MCMC model", col = "MCMC model"),size = 1.5, alpha = .5) +
  geom_line(aes(Age, LAA, col = "MCMC model"), size = 1.5)+
  labs(y = "Total Length (mm)", x = "Age (yrs)")+
  scale_color_viridis_d(name = "Model", begin = .3, end = .8)+
  scale_fill_viridis_d(begin = .5, end = .6)+
  guides(fill = FALSE)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), breaks = seq(0,13,1))+
  theme_bw()+
  theme(text = element_text(size = 14))


## ----DIC, warning=FALSE, message=FALSE, eval=FALSE----------------------------
#  Looic_example_results <- Compare_Growth_Models(data = example_data,
#                                                 stats = "LooIC",
#                                                 iter = 10000,
#                                                 n_cores = 3,
#                                                 n.chains = 4,
#                                                 BurnIn = 1000,
#                                                 thin = 1,
#                                                 Linf = max_size,
#                                                 Linf.se = max_size_se,
#                                                 L0 = birth_size,
#                                                 L0.se = birth_size_se,
#                                                 verbose = TRUE,
#                                                 sigma.max = 100,
#                                                 k.max = 1)
#  

## ----warning=FALSE, message=FALSE,echo=FALSE----------------------------------
data("Looic_example_results")

Looic_example_results

