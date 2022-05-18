# Estimating false negative/sensitivity? ----------------------------------

# Current version is a little circuitous -- might consider re-organizing when done!
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

tabz <- rbind(
  prop.table(table(d$highgods, d$sovorg),2)[2,],
  prop.table(table(d$bin, d$sovorg), 2)[2,]
)
#tabz <- rbind(tab1[2,], tab2[2,])

# par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
# barplot(tabz, beside = T, ylab = "Proportion Present", xlab = "Society Size",
#         cex.names = 1, cex.axis = 1)
# legend(.55, .95, legend = c("Moralistic High Gods", "Moralistic Supernatural Sanctions"),
#        fill = c("black", "lightgray"), bty = "n", cex = 1)
# 
# d %>% 
#   group_by(size) %>% 
#   summarise(diff=mean(bin, na.rm=TRUE)-mean(highgods, na.rm=TRUE))

dat <- rbindlist(list(
  
  prop.table(table(d$highgods, d$sovorg),2) %>% 
    t() %>% 
    as.data.frame() %>%
    rename(size=Var1, highgods=Var2) %>% 
    filter(highgods==1) %>%     # proportions of high gods present
    add_column(type = 'MHG') %>% 
    dplyr::select(-highgods),
  
  prop.table(table(d$bin, d$sovorg),2) %>% 
    t() %>% 
    as.data.frame() %>%
    rename(size=Var1, bin=Var2) %>% 
    filter(bin==1) %>% 
    add_column(type = 'MG') %>% 
    dplyr::select(-bin)
  
)) 

dat <- rbind(dat, data.frame(size=1:6,
                             Freq=dat$Freq[dat$type=='MG']-dat$Freq[dat$type=='MHG'],
                             type='MG - MHG'))


swanson_plot_JurisCompare <- 
  ggplot(dat, aes(x=size, y=Freq*100)) +
  geom_bar(stat='identity', position='dodge', aes(fill=type), colour='black', width=0.8) +
  theme_classic() +
  labs(x='\nSociety size', y='% gods present\n', fill='') +
    scale_fill_viridis_d()


#swanson_plot_JurisCounts <- 
  d %>% 
  dplyr::select(juris_levels=sovorg,
                high_gods=highgods,
                moral_gods=bin) %>% 
  filter(complete.cases(.)) %>% 
  group_by(juris_levels) %>% 
  summarise(p_MG=sum(moral_gods),
            a_MG=length(moral_gods)-sum(moral_gods),
            p_MHG=sum(high_gods),
            a_MHG=length(high_gods)-sum(high_gods)) %>% 
  pivot_longer(cols=-juris_levels, names_to = 'gods', values_to = 'counts', names_prefix = regex('._')) %>% 
  mutate(present=rep(c(1,0), length(gods)/2)) %>% 
  mutate(
    gods=case_when(
      gods=='MG' ~ 'Moralizing gods (MGs)',
      gods=='MHG' ~ 'Moralizing high gods (MHGs)'
    ),
    present=case_when(
      present==0 ~ 'Absent',
      present==1 ~ 'Present'
    )
  ) %>% 
  ggplot(aes(x=juris_levels, y=counts)) +
  geom_bar(stat='identity', position='dodge', aes(fill=fct_rev(factor(present))), colour='black', width=0.8) +
  theme_classic(16) +
  facet_grid(~gods) +
  labs(x='Jurisdictional levels', y='', fill='') +
  scale_fill_manual(
    values = c("#440154FF", "#228B22")
    # values=c(viridis::magma(11)[4],viridis::magma(11)[8])
                    ) +
    theme(legend.position = c(0.15,0.9),
          legend.background = element_rect(size=0.25, linetype=1, 
                                           colour ="black"),
          legend.direction = "horizontal")  # 7 x 10 pdf


d <- d %>% 
  dplyr::select(
    society,
    high_gods=highgods,
    moral_gods=bin,
    juris_levels = sovorg,   # V12 in the Swanson dataset
    size
  )

d$misses <- d$moral_gods-d$high_gods
d$hits <- (d$misses-1)*-1    # this col is the true positives


d <- d %>% 
  dplyr::select(juris_levels, hits)
d <- d[complete.cases(d),]


sim_runs <- 1e4
est_intercept <- rep(NA, sim_runs)
est_slope <- rep(NA, sim_runs)
for(i in 1:sim_runs) {
  tmp <- d[sample(1:nrow(d), size=nrow(d), replace=TRUE),]
  m0 <- glm(hits ~ juris_levels, data=tmp, family='binomial')$coef
  est_intercept[i] <- m0[1]
  est_slope[i] <- m0[2]
}


#SwansonParameterEst_plot <- 
  tibble(`$\\alpha$`=est_intercept,
       `$\\beta$`=est_slope) %>% 
  pivot_longer(cols=everything(), names_to='param', values_to='estimate') %>% 
  ggplot(aes(x=estimate, y=param, fill=param)) +
  ggdist::stat_halfeye(alpha=0.7) +
  theme_classic(base_size = 16) +
  labs(x='Estimate', y='', fill='Parameter') +
    scale_fill_manual(values = c("#440154FF", "#228B22")) +
    scale_colour_manual(values = c("#440154FF", "#228B22")) +
  # scale_fill_manual(values=c(viridis::magma(11)[4],
  #                            viridis::magma(11)[8])) +
  scale_y_discrete(labels = function(x) latex2exp::TeX(x)) +
  theme(legend.position = 'none') +
    geom_hline(yintercept=1) +
    geom_hline(yintercept = 2) +
    coord_cartesian(ylim=c(1.5,2.35)) +
    geom_vline(xintercept=0, linetype=2, alpha=0.5) +
  xlim(c(-5,3))

# Alternative approach: Assume randomness, validate sample ----------------

# Uncomment and run the following code chunk to replicate the simulation that gets saved to SimulationModel.rda
# set.seed(1989)
# 
# nobs <- 1e4
# sc <- sample(1:5, nobs, replace=TRUE)
pr_mg <- mean(swanson$moral_gods, na.rm=TRUE)
# mg_obs <- as.numeric(runif(nobs) <= pr_mg)
# 
# pop <- tibble(sc, mg=mg_obs)
# #summary(glm(mg ~ sc, data=pop, family='binomial'))
# 
# sample_size <- 186
# num_trials <- 1e4
# est0 <- rep(NA, num_trials)
# int0 <- rep(NA, num_trials)
# p0 <- rep(NA, num_trials)
# est1 <- rep(NA, num_trials)
# int1 <- rep(NA, num_trials)
# p1 <- rep(NA, num_trials)
# 
# #ind_sens <- sample(1:length(est_slope), num_trials, replace=TRUE)
# 
# for(i in 1:num_trials) {
#   smsmp <- sample(1:nobs, size=sample_size)
#   dat <- pop[smsmp,]
#   m0 <- glm(mg ~ sc, data=dat, family='binomial')
#   
#   sens <- invlogit(mean(est_intercept) + mean(est_slope)*dat$sc)    # pos corr -> low sensitivity in smaller populations
#   #sens <- invlogit(est_intercept[ind_sens[i]] + est_slope[ind_sens[i]]*dat$sc)
#   
#   sens[dat$mg==0] <- 0
#   dat$mg2 <- as.numeric(sens > runif(length(sens)))
#   m1 <- glm(mg2 ~ sc, data=dat, family='binomial')
#   int0[i] <- m0$coefficients[1]
#   est0[i] <- m0$coefficients[2]
#   p0[i] <- summary(m0)$coef[2,4]
#   int1[i] <- m1$coefficients[1]
#   est1[i] <- m1$coefficients[2]
#   p1[i] <- summary(m1)$coef[2,4]
# }
# 
# md <- tibble(est0, int0, p0,
#              est1, int1, p1)
# save(md, file='data/SimulationModel.rda')
load('data/SimulationModel.rda')

biasedP_Plot1 <- 
  tibble(`$\\beta$`=c(md$est0, md$est1),
       int=c(md$int0, md$int1),
       p=c(md$p0, md$p1),
       errtreat=c(rep('Perfect sensitivity', length(md$est0)),
                  rep('Biased sensitivity', length(md$est1)))) %>% 
  pivot_longer(-errtreat) %>% 
  filter(name=='p') %>% 
  ggplot(aes(x=value, fill=fct_rev(errtreat))) +
    geom_density(alpha=0.6) +
    theme_classic(base_size = 14) +
  labs(x='\nEstimated p-values', y='', fill='') +
  scale_fill_manual(values=c(viridis::magma(11)[4],
                             viridis::magma(11)[8])) +
  scale_colour_manual(values=c(viridis::magma(11)[4],
                             viridis::magma(11)[8]))
  # theme(legend.position = 'none') +
  # annotate(geom='rect', xmin=-0.55, xmax=-0.25, ymin=1.69, ymax=1.93, fill='white', colour='black') +
  # annotate(geom='point', x=-0.52, y=1.77, size=3, colour='#F76F5CFF') +
  # annotate(geom='text', hjust=0, x=-0.5, y=1.87, label='Perfect sensitivity', size=4) +
  # annotate(geom='point', x=-0.52, y=1.87, size=3, colour='#641A80FF') +
  # annotate(geom='text', hjust=0, x=-0.5, y=1.77, label='Biased sensitivity', size=4)


#biasedSimulation_Plot1 <-
  tibble(`$\\beta$`=c(md$est0, md$est1),
       int=c(md$int0, md$int1),
       p=c(md$p0, md$p1),
       # errtreat = c(rep('Perfect\nsensitivity', length(md$est0)),
       #              rep('Biased\nsensitivity', length(md$est1)))
       errtreat=c(rep('accurate', length(md$est0)),
                  rep('error-treated', length(md$est1)))
       ) %>% 
  pivot_longer(-errtreat) %>% 
  filter(name!='p' & name!='int') %>% 
  ggplot(aes(x=value, y=name, fill=factor(errtreat))) +
  ggdist::stat_halfeye(alpha=0.6) +
  #theme_ggdist() +
    theme_classic(base_size = 16) +
    geom_hline(yintercept = 1) +
  # scale_colour_viridis_d() +
  # scale_fill_viridis_d() +
  labs(x="Estimate", y="", fill="") +
    scale_fill_manual(values = c("#440154FF", "#228B22")) +
    scale_colour_manual(values = c("#440154FF", "#228B22")) +
    # labs(x='\nEstimate', y='', fill='Parameter') +
  # scale_fill_manual(values=c(viridis::magma(11)[4],
  #                            viridis::magma(11)[8])) +
  # scale_colour_manual(values=c(viridis::magma(11)[4],
  #                              viridis::magma(11)[8])) +
  scale_y_discrete(labels = function(x) latex2exp::TeX(x)) +
  geom_vline(xintercept=0, linetype=2, alpha=0.5) +
    # theme(legend.position = c(0.1,0.9),
    #       legend.background = element_rect(size=0.25, linetype=1, 
    #                                        colour ="black"),
    #       legend.direction = "horizontal")
  theme(legend.position = 'none') +
    coord_cartesian(ylim=c(1.5,1.6)) +
  annotate(geom='rect', xmin=-0.55, xmax=-0.185, ymin=1.69, ymax=1.93, fill='white', colour='black') +
  annotate(geom='point', x=-0.52, y=1.77, size=3, colour='#228B22') +
  annotate(geom='text', hjust=0, x=-0.5, y=1.87, label='Perfect sensitivity', size=5) +
  annotate(geom='point', x=-0.52, y=1.87, size=3, colour='#440154FF') +
  annotate(geom='text', hjust=0, x=-0.5, y=1.77, label='Biased sensitivity', size=5)


# ggdist with p1 vs. p0 as colors?

# smsmp <- sample(1:nobs, size=sample_size)
# dat <- pop[smsmp,]
# summary(glm(mg ~ sc, data=dat, family='binomial'))
# sens <- invlogit(mean(est_intercept) + mean(est_slope)*dat$sc)    # pos corr -> low sensitivity in smaller populations
# plot(dat$sc, sens)
# 
# sens[dat$mg==0] <- 0
# dat$mg2 <- as.numeric(sens > runif(length(sens)))
# 
# dat$mg2
# summary(glm(mg2 ~ sc, data=dat, family='binomial'))

# Next steps: simulate this process, to show that it *generally* increases the correlation
# What's the right model, given the estimated sensitivity (diff from false negative rate)? -- might find from swanson a rough estimate

