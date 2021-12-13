library(tidyverse)
library(readxl)
library(data.table)

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
var_list2 <- c('SCCS238'  # High God
               # 'SCCS237',  # Society "Size"; NOTE: Jurisdictional hierarchy beyond local community
               # 'SCCS798',  # Date of publication
               # 'SCCS1122'  # Log10 of total pop. size
)  

df <- data.frame(sccs_id = unique(scdf$soc_id))

for(i in 1:length(var_list2)) {
  tmp <- as_tibble(scdf[scdf$var_id==var_list2[i],]) %>% 
    dplyr::select(sccs_id=soc_id,
                  code)
  colnames(tmp) <- c('sccs_id', vars$title[vars$id==var_list2[i]])
  df <- df %>% left_join(tmp, by='sccs_id')
}

colnames(df) <- c('id', 'highgods') #, 'size', 'date', 'pop10')
df$highgods <- as.numeric(df$highgods==4)

sccs <- df %>% 
  add_column(moral_gods = NA) %>% 
  select(
    society = id,
    high_gods = highgods,
    moral_gods
  ) %>% 
  add_column(dataset = 'SCCS (High gods)')

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

# d$size_labels <- NA
# d$size_labels[d$size==0] <- "1-49"
# d$size_labels[d$size==1] <- "50-399"
# d$size_labels[d$size==2] <- "400-9999"
# d$size_labels[d$size==3] <- "10000+"
# d$size_labels <- factor(d$size_labels, levels=c('1-49', '50-399', '400-9999', '10000+'))

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
    moral_gods=bin
  ) %>% 
  add_column(dataset='Swanson (1960)')


# Skoggard et al. ---------------------------------------------------------

## NOTE: these are gods that are specifically associated with weather

# read in the data here
ang<-read.csv("data/DT-GodsAndResourceStress_Long.csv")

d <- ang %>% dplyr::select(
  id=SCCS_ID, 
  fam=language_family, 
  highgod_punish=G4_Punitive,
  supgod_punish=SG4_Punitive,
  spirits_punish=MS4_Punitive,
  # stress=stress2   # NOTE: Part A's stress2 != stress mean used in Part B
) %>% as_tibble()

# sum(d$highgod_punish, na.rm = TRUE)
# sum(d$supgod_punish, na.rm = TRUE)
# sum(d$spirits_punish, na.rm = TRUE)

d$id[is.na(d$id)] <- 99   # This appears to be a mistake in the original, but should check on it

d$uid <- 1:nrow(d)

# NOTE: need to get surgical here, because need to consider how we're interpreting NA's
tmp <- d %>% 
  select(
    uid,
    #id,
    highgod_punish:spirits_punish)

tmp$allNA <- rowSums(is.na(tmp))==(ncol(tmp)-1)
tmp <- tmp[tmp$allNA==FALSE,]
tmp <- tmp %>% replace(is.na(.), 0)
tmp$moral_punitive <- as.numeric(tmp$highgod_punish | tmp$supgod_punish | tmp$spirits_punish)

# d$highgod_punish[is.na(d$highgod_punish)] <- 0     # yes, I know that AE!=EA and my anayses won't imply otherwise
# d$supgod_punish[is.na(d$supgod_punish)] <- 0
# d$spirits_punish[is.na(d$spirits_punish)] <- 0

## d$moral_punitive <- as.numeric(d$highgod_punish | d$supgod_punish | d$spirits_punish)
# d$moral_count <- as.numeric(d$highgod_punish + d$supgod_punish + d$spirits_punish)


#' S3_Resolved_IA_TR: "Is food shared outside the typical household frequently during certain
#' seasons in ways different than the daily sharing of food?" (0) No, including inferred no; (1) Yes
#' (codes from Ember et al. 2018)
# d2 <- read_csv('DT-GodsAndResourceStress.csv')
# d2$S3_Resolved_IA_TR

d <- d %>% left_join(tmp %>% select(uid, moral_punitive), by='uid')

skoggard <- 
  d %>% 
  group_by(society=id) %>% 
  summarise(high_gods = as.numeric(mean(highgod_punish, na.rm=TRUE)>0),
            moral_gods = as.numeric(sum(moral_punitive)>0)) %>% 
  add_column(dataset='Skoggard et al. (2020)')


# Watts et al BSP ---------------------------------------------------------

d <- read_excel('data/bsp.xlsx')
d <- d[rowSums(is.na(d))!=ncol(d),]
watts <- d %>% 
  dplyr::select(
    society=culture,
    high_gods=mhg,
    moral_gods=bsp
  ) %>% 
  add_column(dataset='Watts et al. (2015)')



# Peoples et al 2016 ------------------------------------------------------

d <- read_csv('data/peoples2016.csv')
d$moral_gods <- as.numeric(d$active_highgods|d$active_ancestors)
peoples <- d %>% 
  dplyr::select(
    society,
    high_gods = active_highgods,
    moral_gods
  ) %>% 
  add_column(dataset='Peoples et al. (2016)')


# Seshat 2021 -------------------------------------------------------------

d <- read_csv('data/MSP_SOM_17sep21/MSP_data_4mar2021.csv')

d$moral_gods <-
  d %>%
  select(
    primary:agency
  ) %>%
  rowSums(na.rm=TRUE)
d$moral_gods <- as.numeric(d$moral_gods>0)

# sort(table(d$PolID))   # remove duplicates?
# Might be better (more conservative) to use regions?

# d <- d %>% 
#   select(
#     region = NGA,
#     primary:agency
#   ) %>% 
#   replace(is.na(.), 0) %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) %>% 
#   group_by(region) %>% 
#   summarise(present = as.numeric(mean(sum) > 0))

seshat <- d %>%
  add_column(high_gods = NA) %>% 
  dplyr::select(
    society = PolID,
    high_gods,
    moral_gods
  ) %>% 
  add_column(dataset='Turchin et al. (2021)')


# Boehm -------------------------------------------------------------------

d <- read_excel('data/Boehm supernatural punishment.xlsx') %>% replace(is.na(.), 0)
# hagenutils::hagenheat(d %>% select(where(is.numeric)) %>% t(), seriation_method = 'PCA_angle')

d$moral_gods <- as.numeric(rowSums(d %>% select(deviance:jealousy)) > 0 )

boehm <- d %>% 
  add_column(high_gods = NA) %>% 
  dplyr::select(
    society,
    high_gods,
    moral_gods
  ) %>% 
  add_column(dataset = 'Boehm (2008)')

# Combining datasets ------------------------------------------------------

dat <- rbindlist(
  list(
    swanson,
    skoggard,
    watts,
    peoples,
    seshat,
    sccs,
    boehm
  )
)

# Plotting comparisons ----------------------------------------------------

dat <- dat %>% 
  pivot_longer(c(high_gods, moral_gods), names_to = 'gods', values_to = 'present') %>% 
  group_by(dataset, gods) %>% 
  summarise(
    average = mean(present, na.rm=TRUE),
    se = sd(replicate(1e4,
                      mean(sample(present, replace=TRUE), na.rm=TRUE)))
  ) %>% 
  mutate(
    gods = case_when(
      gods =='high_gods' ~ 'High gods',
      gods =='moral_gods' ~ 'Moralizing gods'
    )
  )
dat$dataset <- factor(dat$dataset, levels=c('Swanson (1960)',
                                            'Boehm (2008)',
                                            'Watts et al. (2015)', 
                                            'Peoples et al. (2016)', 'Skoggard et al. (2020)', 
                                            'Turchin et al. (2021)',
                                            'SCCS (High gods)'))
dat$dataset <- factor(dat$dataset, levels=rev(levels(dat$dataset)))

dat <- dat[complete.cases(dat),]

dat$upper <- dat$average + 2*dat$se
dat$lower <- dat$average - 2*dat$se

dat$upper[dat$dataset=='Boehm (2008)'] <- as.numeric(prop.test(sum(boehm$moral_gods), length(boehm$moral_gods))$conf)[2]
dat$lower[dat$dataset=='Boehm (2008)'] <- as.numeric(prop.test(sum(boehm$moral_gods), length(boehm$moral_gods))$conf)[1]

compare_dataplot <- ggplot(dat, aes(x=dataset, y=average, shape=gods)) +
  geom_point(position=position_dodge(width=0.15), size=3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(width=0.15), width=0.15, alpha=0.5) +
  ylim(c(0,1)) +
  scale_shape_manual(values=c(1,19)) +
  labs(x='', y='\nProportion', shape = '') +
  coord_flip() +
  theme_classic(base_size = 14)
