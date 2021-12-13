library(AnthroTools)  # for salience calculations
library(xtable)       # for printing LaTex tables
library(tidyverse)
library(pheatmap)
library(patchwork)

data <- read.csv('data/FreeList_CERC_V0.1_FIN.csv', sep=';') %>% as_tibble()
cerc <- read.csv('data/CERC Dataset (Wave 1) Version 6.0.csv', sep=';') %>% as_tibble()

d <- data %>% 
  dplyr::select(
    id=CERCID,
    culture=Culture,
    order=Order,
    BGL, BGD,
    LGL, LGD,
    POL, POD
  ) %>% 
  pivot_longer(BGL:POD) %>% 
  #group_by(id, name) %>% 
  #arrange(order, .by_group=TRUE) %>% 
  filter(!is.na(value))

# data %>% 
#   dplyr::select(
#     id=CERCID,
#     culture=Culture,
#     order=Order,
#     BGL, BGD,
#     LGL, LGD,
#     POL, POD
#   )

# LEFT OFF HERE; FINALLY GOT THE PRESENCE/ABSENCE CONVERSION FIGURED OUT
LG_panel <- 
  data %>% 
  dplyr::select(
    id=CERCID,
    culture=Culture,
    order=Order,
    BGL, BGD,
    LGL, LGD,
    POL, POD
  ) %>% 
  pivot_longer(BGL:POD) %>% 
  filter(!is.na(value)) %>% 
  filter(name=='LGL' | name=='LGD') %>% 
  distinct() %>% 
  group_by(id, value) %>% 
  summarise(present=sum(order)) %>% 
  pivot_wider(names_from=value, values_from=present, values_fill=0) %>% 
  ungroup() %>% 
  mutate(across(-id, ~replace(., .>1, 1))) %>%
  dplyr::select(-id) %>% 
  as.matrix() %>% 
  t() 
  # pheatmap(color = viridis::inferno(20),
  #          treeheight_col = 0, treeheight_row = 0,
  #          legend=FALSE,
  #          main='Local Gods')
  # heatmap(scale='none')
  # hagenutils::hagenheat(seriation_method = 'PCA_angle') +
  # theme(legend.position = 'none') +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank()) +
  #   ggtitle('Local gods')
  # theme(axis.title.y=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank())

BG_panel <- 
  data %>% 
    dplyr::select(
      id=CERCID,
      culture=Culture,
      order=Order,
      BGL, BGD,
      LGL, LGD,
      POL, POD
    ) %>% 
    pivot_longer(BGL:POD) %>% 
    filter(!is.na(value)) %>% 
    filter(name=='BGL' | name=='BGD') %>% 
    # filter(name=='LGL' | name=='LGD') %>% 
    distinct() %>% 
    group_by(id, value) %>% 
    summarise(present=sum(order)) %>% 
    pivot_wider(names_from=value, values_from=present, values_fill=0) %>% 
    ungroup() %>% 
    mutate(across(-id, ~replace(., .>1, 1))) %>%
    dplyr::select(-id) %>% 
    as.matrix() %>% 
    t() 
    # heatmap(scale='none', labCol = FALSE, main='Big Gods')
    # pheatmap(color = viridis::inferno(20), 
    #          treeheight_col = 0, treeheight_row = 0, 
    #          legend=FALSE,
    #          main='Big Gods')
  #   hagenutils::hagenheat(seriation_method = 'PCA_angle') +
  #   theme(legend.position = 'none') +
  #   theme(axis.title.x=element_blank(),
  #         axis.text.x=element_blank(),
  #         axis.ticks.x=element_blank()) +
  # ggtitle('Big Gods')
  
# BG_panel / LG_panel

bgpun <- round(mean(cerc$BGPUNISH, na.rm=TRUE)*100)
lgpun <- round(mean(cerc$LGPUNISH, na.rm=TRUE)*100)

## MIGHT combine into a longer dataset and add c(bg,lg) var to see how they do (or don't) cluster


# Boehm reanalysis --------------------------------------------------------

# d <- read_excel('data/Boehm supernatural punishment.xlsx') %>% replace(is.na(.), 0)
# 
# d %>% 
#   rename_with(~paste0('Moral_', .x), .cols=deviance:jealousy) %>% 
#   rename_with(~paste0('Non-moral_', .x), .cols=food:object) %>% 
#   pivot_longer(-society) %>% 
#   add_column(domain=NA) %>% 
#   mutate(domain = str_split(name, '_'),
#          domain = unlist(lapply(domain, function(x) x[1])),
#          name = str_split(name, '_'),
#          name = unlist(lapply(name, function(x) x[2]))) %>% 
#   group_by(domain, name) %>% 
#   summarise(n=sum(value)) %>%
#   #arrange(n) %>% 
#   ggplot(aes(x=reorder_within(name, by=n, within=domain,), y=n)) +
#   geom_bar(stat='identity') +
#   facet_wrap(~domain, scales='free_y') +
#   theme_classic() +
#   scale_x_reordered() +
#   labs(x='', y='') +
#   coord_flip()
# 
# 
# d[-1] %>% 
#   as.matrix() %>% 
#   t() %>% 
#   heatmap(scale = 'none')









