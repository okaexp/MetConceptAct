#code to replicate 3.3. Semantic distances of the topic words, vehicle words, and the features calculated by the language model (BERT) on the metaphor form selection

# load libraries ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(psych)
library(forcats)
source("http://aoki2.si.gunma-u.ac.jp/R/src/mycor.R", encoding="euc-jp") #mycorを持ってくる

# 1. prepare data----
dat_dist_aptnss_item_wide <- read.csv("../data/dat_dist_aptnss_item_wide.csv")#data obtained in metaphor aptness task and feature arrangement task
dat_model_ed <- read.csv("../result/words_to_bert_edited_with_euclid_distance.csv")

dat_agg <- dat_dist_aptnss_item_wide %>%
  dplyr::inner_join(dat_model_ed, by="NID") %>%
  dplyr::select(-starts_with("AveDist"), -starts_with("T")) %>%
  dplyr::select(starts_with("AveAptness"), NID,starts_with("EucDist"), -NID)

# 2. calculate correlations ----
mycor(1:13, dat_agg, latex = FALSE)

# 3. prepare summary statistics
psych::describe(dat_agg)
