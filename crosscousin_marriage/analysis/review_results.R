library(bayestraitr)
library(stringr)
library(tidyr)
library(dplyr)
library(patchwork)
library(bayesplot)
library(ggplot2)
library(coda)

## Custom colour scheme
sam_scheme <- c("#ED5C4D", "#ED5C4D",
                   "#57B5ED", "#273253",
                   "#FBBE4B", "#F4E9DA")
color_scheme_set(sam_scheme)


#### -- functions -- ####
plotANC = function(x){
  cols_idx = str_detect(colnames(x), "Root")
  d = data.frame(x[,cols_idx])
  boxplot(d)
  sum(apply(d, 1, function(x) all(x == 0.25))) / nrow(d)
  #which(apply(d, 1, function(x) all(x == 0.25)))
}

get_runs = function(file_pattern){
  files = list.files('crosscousin_marriage/bt_output/', pattern = file_pattern, full.names = TRUE)
  runs_list = lapply(files, bt_read.log) 
  #runs = do.call(rbind, runs_list)
  #runs$run = rep(1:length(files), each = 10000)
  #lapply(runs_list, as.matrix)
  lapply(runs_list, function(x){
    y = matrix(x$Lh)
    colnames(y) = "Lh"
    y
  })
}

plot_runs = function(runs){
  plot(runs$Lh[runs$run == 1], type = 'n')
  r = unique(runs$run)
  cols = c("black", "red")
  for(i in r)
    lines(runs$Lh[runs$run == i], col = cols[i], alpha = 0.4)
}

get_convergence = function(x){
  mcmc_object = lapply(x, mcmc)
  mcmc_l = as.mcmc.list(mcmc_object)
  stats = gelman.diag(mcmc_l)$psrf
  paste0(round(stats[1], 2), " (", round(stats[2], 2), ")")
}

# H1: Male crossness & Permitted 1st CCM ----
hypothesis="mXpermitted"

indep = get_runs(paste0(hypothesis, '-indep-[0-9].Log.txt'))
dep   = get_runs(paste0(hypothesis,'-dep-[0-9].Log.txt'))

m_i = get_convergence(indep)
m_d = get_convergence(dep)

plot_indep = mcmc_trace(indep, pars = c("Lh")) + ggtitle("Independant model")
plot_dep = mcmc_trace(dep, pars = c("Lh")) + ggtitle("Dependant model") + theme(legend.position = "none")

png('figures/male_traceplot.png')
mp =  plot_indep /  plot_dep
mp + plot_annotation(
  title = 'Trace plots for male bifurcate merging model',
  subtitle = '10,000 samples and an exponential prior with mean 10',
)
dev.off()

indep_mll = bt_read.stones(paste0('crosscousin_marriage/bt_output/', hypothesis,'-indep-2.Stones.txt'))
dep_mll = bt_read.stones(paste0('crosscousin_marriage/bt_output/', hypothesis,'-dep-2.Stones.txt'))


# Log Bayes Factor= 2(log marginal likelihood complex model –log marginal likelihood simple model)
2 * (dep_mll$marginal_likelihood - indep_mll$marginal_likelihood)

# plotANC(indep)
# plotANC(dep)

# transitions
# scale_factor = as.numeric(attributes(dep)$settings$`Scale Tree:`)
# (colMeans(dep[,str_detect(colnames(dep), "q")]) * scale_factor) * 6900
# (colMeans(indep[,str_detect(colnames(indep), "alpha|beta")]) * scale_factor) * 10000

# H2: Female crossness & permitted 1st CCM ----
hypothesis="fXpermitted"

indep = get_runs(paste0(hypothesis, '-indep-[0-9].Log.txt'))
dep   = get_runs(paste0(hypothesis,'-dep-[0-9].Log.txt'))

m_i = get_convergence(indep)
m_d = get_convergence(dep)

png('figures/female_traceplot.png')
plot_indep = mcmc_trace(indep, pars = c("Lh")) + ggtitle("Independant model")
plot_dep = mcmc_trace(dep, pars = c("Lh")) + ggtitle("Dependant model") + theme(legend.position = "none")

mp =  plot_indep /  plot_dep
mp + plot_annotation(
  title = 'Trace plots for female bifurcate merging model',
  subtitle = '10,000 samples and an exponential prior with mean 10',
)
dev.off()

indep_mll = bt_read.stones(paste0('crosscousin_marriage/bt_output/', hypothesis,'-indep-2.Stones.txt'))
dep_mll = bt_read.stones(paste0('crosscousin_marriage/bt_output/', hypothesis,'-dep-2.Stones.txt'))

indep_mll = bt_read.stones(paste0('crosscousin_marriage/bt_output/', hypothesis,'-indep-2.Stones.txt'))
dep_mll = bt_read.stones(paste0('crosscousin_marriage/bt_output/', hypothesis,'-dep-2.Stones.txt'))

# Log Bayes Factor= 2(log marginal likelihood complex model –log marginal likelihood simple model)
2 * (dep_mll$marginal_likelihood - indep_mll$marginal_likelihood)

# plotANC(indep)
# plotANC(dep)


# H3: Both sides crossness & Permitted 1st CCM ----
hypothesis="bothXpermitted"

indep = get_runs(paste0(hypothesis, '-indep-[0-9].Log.txt'))
dep   = get_runs(paste0(hypothesis,'-dep-[0-9].Log.txt'))

c_i = get_convergence(indep)
c_d = get_convergence(dep)

png('figures/both_traceplot.png')
plot_indep = mcmc_trace(indep, pars = c("Lh")) + ggtitle("Independant model")
plot_dep = mcmc_trace(dep, pars = c("Lh")) + ggtitle("Dependant model") + theme(legend.position = "none")

mp =  plot_indep /  plot_dep
mp + plot_annotation(
  title = 'Trace plots for complete bifurcate merging model',
  subtitle = '10,000 samples and an exponential prior with mean 10',
)
dev.off()

indep = bt_read.log(paste0('crosscousin_marriage/bt_output/', hypothesis, '-indep-2.Log.txt'))
dep = bt_read.log(paste0('crosscousin_marriage/bt_output/', hypothesis,'-dep-2.Log.txt'))

plot(indep$Lh, type = 'l')
plot(dep$Lh, type = 'l')

indep_mll = bt_read.stones(paste0('crosscousin_marriage/bt_output/', hypothesis,'-indep-2.Stones.txt'))
dep_mll = bt_read.stones(paste0('crosscousin_marriage/bt_output/', hypothesis,'-dep-2.Stones.txt'))

# Log Bayes Factor= 2(log marginal likelihood complex model –log marginal likelihood simple model)
2 * (dep_mll$marginal_likelihood - indep_mll$marginal_likelihood)

# convergence table
convergence_table = data.frame(model = c("Male BM", "Female BM", 
                                         "Complete BM"))
convergence_table$Independent = c(m_i, f_i, c_i)
convergence_table$Dependent = c(m_d, f_d, c_d)

write.csv(convergence_table, "crosscousin_marriage/results/convergence_table.csv")
