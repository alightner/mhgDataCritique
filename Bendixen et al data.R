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
  filter(!is.na(value))

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
    distinct() %>% 
    group_by(id, value) %>% 
    summarise(present=sum(order)) %>% 
    pivot_wider(names_from=value, values_from=present, values_fill=0) %>% 
    ungroup() %>% 
    mutate(across(-id, ~replace(., .>1, 1))) %>%
    dplyr::select(-id) %>% 
    as.matrix() %>% 
    t() 

bgpun <- round(mean(cerc$BGPUNISH, na.rm=TRUE)*100)
lgpun <- round(mean(cerc$LGPUNISH, na.rm=TRUE)*100)

