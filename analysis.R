library(tidyverse)
library(readxl)
library(data.table)
library(visreg)
library(patchwork)
library(ggdist)
invlogit <- function(x) exp(x) / (1 + exp(x))

# SCCS/EA High gods (standard) --------------------------------------------

vars <- read_csv('sccs/variables.csv')
codes <- read_csv('sccs/codes.csv')
socdf <- read_csv('sccs/societies.csv')
scdf <- read.csv('sccs/data.csv',
                 colClasses=c(
                   'character','character','numeric','character','numeric', rep('NULL',4)),
                 header=TRUE)
cdvar <- vars %>% dplyr::select(var_id=id, category, title, type)
scdf <- scdf %>% left_join(cdvar, by='var_id')

# var_list2 <- cdvar$var_id[cdvar$type=='Ordinal' | cdvar$type=='Continuous']
var_list2 <- c('SCCS238',  # High God
               'SCCS237',  # Society "Size"; NOTE: Jurisdictional hierarchy beyond local community
               'SCCS1122'  # Log10 of total pop. size
)  

df <- data.frame(sccs_id = unique(scdf$soc_id))
for(i in 1:length(var_list2)) {
  tmp <- as_tibble(scdf[scdf$var_id==var_list2[i],]) %>% 
    dplyr::select(sccs_id=soc_id,
                  code)
  colnames(tmp) <- c('sccs_id', vars$title[vars$id==var_list2[i]])
  df <- df %>% left_join(tmp, by='sccs_id')
}
colnames(df) <- c('id', 'highgods', 'size', 'pop10')
df$highgods <- as.numeric(df$highgods==4)

sccs <- df %>% 
  add_column(moral_gods = NA) %>% 
  select(
    society = id,
    high_gods = highgods,
    moral_gods,
    juris_levels = size,
    pop10
  ) %>% 
  add_column(dataset = 'SCCS (High gods)') %>% 
  as_tibble()

# Year adjusted -----------------------------------------------------------

tmp <- sccs %>%
  as_tibble() %>%
  left_join(
    tibble(scdf[scdf$var_id=='SCCS238',]) %>%
      dplyr::select(society=soc_id, year),
    by='society'
  )
sccsPre1965_percent <- round(mean(tmp$year<1965, na.rm=TRUE)*100, 2)
sccsPre1965 <- sum(tmp$year<1965, na.rm=TRUE)
sccsyearCount <- length(tmp$year[!is.na(tmp$year)])
sccsyear95 <- as.numeric(quantile(tmp$year, 0.95, na.rm=TRUE))

YearEcdf_Plot <- 
  sccs %>% 
  as_tibble() %>% 
  left_join(
    tibble(scdf[scdf$var_id=='SCCS238',]) %>% 
      dplyr::select(society=soc_id, year),
    by='society'
  ) %>% 
  ggplot(aes(x=year)) + 
  stat_ecdf() +
  theme_bw() +
  labs(x='\nYear', y='') +
  xlim(c(1500,2000)) +
  geom_vline(xintercept=1965, linetype=2, alpha=0.5) +
  geom_vline(xintercept=1871, linetype=2, alpha=0.5)


# #YearEcdf_Plot <- 
#   sccs %>% 
#   as_tibble() %>% 
#   left_join(
#     tibble(scdf[scdf$var_id=='SCCS238',]) %>% 
#       dplyr::select(society=soc_id, year),
#     by='society'
#   ) %>% 
#   ggplot(aes(x=year)) + 
#   stat_ecdf() +
#   theme_bw() +
#   labs(x='\nYear', y='') +
#   xlim(c(0,2000)) +
#   geom_vline(xintercept=1965, linetype=2, alpha=0.5) +
#   geom_vline(xintercept=1871, linetype=2, alpha=0.5)


# Swanson -----------------------------------------------------------------

d <- 
  read.csv('data/Swanson_Data.csv', sep=';') %>% 
  dplyr::select(   # choosing variables
    society=X,
    size=X4,   # size of society
    sovorg = X12, # sovereign organizations
    highgods=X25,  # high gods: 4 = NA, 3 = present, moralistic
    ancestor_punish=X29,   #  2= present, punish living humans
    moral_health=X37,
    moral_afterlife=X38,
    moral_other=X39   # sanctions for morality on: health, afterlife, other (e.g., crop failure)
  ) %>% 
  as_tibble() %>% 
  mutate(
    across(size:moral_other, as.numeric))    # converting characters to numeric

d$highgods[d$highgods==4] <- NA   
d$highgods <- as.numeric(d$highgods==3)    # high gods concerned about morality?

d$ancestor_punish <- as.numeric(d$ancestor_punish==2)    # ancestors punish moral violations?

d$bin <- as.numeric(
  d$ancestor_punish | 
    d$highgods | 
    d$moral_health | 
    d$moral_afterlife | 
    d$moral_other
) 

swanson <- d %>% 
  dplyr::select(
    society,
    high_gods=highgods,
    moral_gods=bin,
    juris_levels = sovorg,   # V12 in the Swanson dataset
    size
  ) %>% 
  add_column(dataset='Swanson (1960)')

sccs_mhg <- glm(high_gods ~ juris_levels, data=sccs, family='binomial')
swan_mhg <- glm(high_gods ~ juris_levels, data=swanson, family='binomial')
swan_mg <- glm(moral_gods ~ juris_levels, data=swanson, family='binomial')

sccs_highPlot <- 
  visreg::visreg(sccs_mhg, scale='response', rug=FALSE, gg=TRUE) +
  ylim(c(0,1)) +
  theme_classic(base_size = 14) +
  labs(x='\nJurisdictional hierarchy', y='Moralizing High Gods\n') +
  ggtitle('Standard Cross-Cultural Sample')

swan_highPlot <- 
  visreg::visreg(swan_mhg, scale='response', rug=FALSE, gg=TRUE) +
  ylim(c(0,1)) +
  theme_classic(base_size = 14) +
  labs(x='\nJurisdictional hierarchy', y='')
# ggtitle('Swanson (1964) dataset')

swan_moralPlot <- 
  visreg::visreg(swan_mg, scale='response', rug=FALSE, gg=TRUE) +
  ylim(c(0,1)) +
  theme_classic(base_size = 14) +
  labs(x='\nJurisdictional hierarchy', y='')


# Standard barplot --------------------------------------------------------

standardMHGplot <- 
  sccs %>% 
  group_by(juris_levels) %>% 
  summarise(mhg=mean(high_gods, na.rm=TRUE)) %>% 
  filter(!is.na(juris_levels)) %>% 
  ggplot(aes(x=juris_levels, y=mhg)) +
  geom_bar(stat='identity', width=0.6, fill='lightgray', colour='black') +
  theme_classic() +
  labs(x='\nJurisdictional hierarchy', y='Proportion of cultures with MHG\n') +
  ylim(c(0,1))

standardMHGcount <- sccs %>% 
  group_by(juris_levels) %>% 
  summarise(
    present=sum(high_gods==1, na.rm=TRUE),
    absent=sum(high_gods==0, na.rm=TRUE)
  ) %>% 
  filter(!is.na(juris_levels)) %>% 
  pivot_longer(-juris_levels, names_to='pres_abs') %>% 
  ggplot(aes(x=juris_levels, y=value, fill=pres_abs)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(
    values=c(viridis::magma(11)[4], viridis::magma(11)[8]),
    guide=guide_legend(reverse=TRUE)
  ) +
  theme_classic() +
  labs(x='\nJurisdictional hierarchy', y='Number of MHGs present or absent\n', fill='')

# MOSAIC PLOT?
# sccs %>% 
#   group_by(juris_levels) %>% 
#   summarise(
#     present=sum(high_gods==1, na.rm=TRUE),
#     absent=sum(high_gods==0, na.rm=TRUE)
#   ) %>% 
#   filter(!is.na(juris_levels)) %>% 
#   pivot_longer(-juris_levels, names_to='pres_abs') %>% 
#   group_by(juris_levels) %>%
#   mutate(ind=row_number()-1,
#          ind=abs(ind-1)) %>% 
#   summarise(value2=sum(value),
#             mean2=sum(value*ind)/value2) %>% 
#   ggplot() +
#   geom_mosaic(aes(weight=mean2, x=product(juris_levels), conds=value2))

# Evaluating false negatives in the literature ----------------------------

source('comparedatasets.R')


# Simulating false negatives from Swanson ---------------------------------

source('fnr-random-simulation.R')


# Increasingly biased simulations -----------------------------------------

set.seed(1989)
nobs <- 1e4
sc <- sample(1:5, nobs, replace=TRUE)
mg_obs <- as.numeric(runif(nobs) <= pr_mg)
pop <- tibble(sc, mg=mg_obs)
sample_size <- 186
num_trials <- 1e3
paramSpace <- seq(0.1, 0.5, by=0.01)
est2 <- rep(NA, length(paramSpace))
sd2 <- rep(NA, length(paramSpace))

# modeling the biased sensitivity to incorporate intercept changes
# for(k in 1:length(paramSpace)) {
#   est1 <- rep(NA, num_trials)
#   for(i in 1:num_trials) {
#     smsmp <- sample(1:nobs, size=sample_size)
#     dat <- pop[smsmp,]
#     sens <- invlogit(mean(est_intercept)-paramSpace[k] + paramSpace[k]*dat$sc)   # iterate
#     sens[dat$mg==0] <- 0
#     dat$mg2 <- as.numeric(sens > runif(length(sens)))
#     m1 <- glm(mg2 ~ sc, data=dat, family='binomial')
#     est1[i] <- m1$coefficients[2]
#   }
#   est2[k] <- mean(est1)
#   sd2[k] <- sd(est1)
#   
# }
# md2 <- tibble(bias=paramSpace, estimate=est2, sd=sd2) 
# save(md2, file='data/SimulationData2.rda')
load('data/SimulationData2.rda')

fnrBiasModels2 <- 
  md2 %>% 
  ggplot(aes(x=bias, y=estimate)) +
  geom_line(lwd=1) +
  geom_line(aes(x=bias, y=estimate-2*sd), linetype=2) +
  geom_line(aes(x=bias, y=estimate+2*sd), linetype=2) +
  geom_hline(yintercept=0, linetype=1, alpha=0.5) +
  # geom_point() +
  # geom_errorbar(aes(ymin=estimate-2*sd, ymax=estimate+2*sd)) +
  theme_bw() +
  ylim(c(-0.5,0.7)) +
  geom_ribbon(aes(ymin=estimate-2*sd, ymax=estimate+2*sd),
              fill='gray', alpha=0.3) +
  labs(x='\nBias', y='Estimates\n')

# 
# md <- tibble(est0, int0, p0,
#              est1, int1, p1)



# Expanding parameter space -----------------------------------------------

paramSpace_est <- seq(round(quantile(est_slope, 0.05),2),
                      round(quantile(est_slope, 0.95),2),
                      length.out=100)
paramSpace_int <- seq(round(quantile(est_intercept, 0.05),2),
                      round(quantile(est_intercept, 0.95),2),
                      length.out=100)
# pstab <- expand_grid(paramSpace_est, paramSpace_int)
# pstab$coef <- NA; pstab$sd <- NA
# 
# for(k in 1:nrow(pstab)) {
#   
#   est1 <- rep(NA, num_trials)
#   for(i in 1:num_trials) {
#     smsmp <- sample(1:nobs, size=sample_size)
#     dat <- pop[smsmp,]
#     sens <- invlogit(pstab$paramSpace_int[k] + pstab$paramSpace_est[k]*dat$sc)
#     sens[dat$mg==0] <- 0
#     dat$mg2 <- as.numeric(sens > runif(length(sens)))
#     m1 <- glm(mg2 ~ sc, data=dat, family='binomial')
#     est1[i] <- m1$coefficients[2]
#   }
#   
#   pstab$coef[k] <- mean(est1)
#   pstab$sd[k] <- sd(est1)
# }

# save(pstab, file='data/SimulationSpace.rda')
load('data/SimulationSpace.rda')

paramSpace_plot <- 
  pstab %>% 
  ggplot(aes(x=paramSpace_est, y=paramSpace_int, fill=coef)) +
  geom_raster(interpolate=FALSE) +
  scale_fill_viridis_c() +
  geom_vline(xintercept=mean(est_slope), alpha=0.75, linetype=3) +
  geom_hline(yintercept=mean(est_intercept), alpha=0.75, linetype=3) +
  theme_classic() +
  labs(x='Sensitivity bias', y='Intercept', fill='Association\ngenerated')




