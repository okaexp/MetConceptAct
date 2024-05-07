#last modified: 2024/5/6, 22:43
library(tidyr)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(psych)
library(forcats)
library(lme4)
library(lmerTest)
library(scales)
library(sjstats)
source("http://aoki2.si.gunma-u.ac.jp/R/src/mycor.R", encoding="euc-jp")

# 1. prepare data for analysis----
raw_demographic <- read.csv("../data/raw_data/20240322_1732/20240321_MetConceptAct_Exp2_StartToDemographic.csv")
raw_finish <- read.csv("../data/raw_data/20240322_1732/20240321_MetConceptAct_Exp2_End.csv")
raw_aptness <- read.csv("../data/raw_data/20240322_1732/MetConceptAct_Exp2_Aptness.csv")
raw_sorting <- read.csv("../data/raw_data/20240322_1732/MetConceptAct_Exp2_Sorting.csv")

# dat_finish: participants who participated in all the tasks(target participants)
dat_finish <- raw_finish %>%
  dplyr::slice(3:nrow(.)) %>%#データの読み込み
  dplyr::select(cwid, Condition)

cwid_finished_lst <- unique(dat_finish$cwid)#117名
dat_finish_condition <- dat_finish %>%
  dplyr::select(Condition)  %>% 
  group_by(Condition) %>% 
  summarise(count = n())

# dat_demographic:
# - Sex
# - Age
dat_demographic <- raw_demographic %>%
  dplyr::slice(3:nrow(.)) %>%#データの読み込み
  dplyr::select(CrowdworksID, Sex, Age, AcademicDegree) %>%
  dplyr::mutate(CrowdworksID = as.factor(CrowdworksID),
                Sex = as.factor(Sex),
                Age = as.integer(Age),
                AcademicDegree = as.factor(AcademicDegree)) %>% #変換
  dplyr::filter(CrowdworksID %in% cwid_finished_lst) %>% #finishにいるcwidを抽出
  dplyr::distinct(CrowdworksID, .keep_all = TRUE)#重複している人を除外

dat_age <- dat_demographic %>% 
  dplyr::select(Age)  %>% 
  summarise(m_age = mean(Age), sd_age = sd(Age), min_age = min(Age), max_age = max(Age))

dat_sex <- dat_demographic %>% 
  dplyr::select(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n())

# dat_aptness: results of metaphor aptness task
# - cwid
# - Topic
# - NumFeatures
# - Aptness
dat_aptness <- raw_aptness %>%
  dplyr::select(trial_index, cwid, rt, task,
                TrialType, SetType, NID, NumFeatures, Aptness) %>% #必要列の抽出
  dplyr::mutate(cwid = as.factor(cwid),
                rt = as.integer(rt),
                task = as.factor(task),
                TrialType = as.factor(TrialType),
                SetType = as.factor(SetType),
                NID = as.factor(NID),
                NumFeatures = as.factor(NumFeatures),
                Aptness = as.integer(Aptness)
                ) %>% #必要列の変換
  dplyr::filter(cwid %in% cwid_finished_lst & TrialType == "Main") %>%
  dplyr::distinct(cwid, NID, .keep_all = TRUE)#重複している人を除外

# dat_sorting: results of feature arrangement task
dat_sorting <- raw_sorting %>% 
  dplyr::select(cwid, rt,
                TrialType, Condition, NID, NumFeatures,
                finloc_T_x, finloc_T_y, finloc_V_x, finloc_V_y, finloc_F1_x, finloc_F1_y,
                finloc_F2_x, finloc_F2_y, finloc_F3_x, finloc_F3_y) %>% #必要列の抽出
  dplyr::mutate(cwid = as.factor(cwid),
                rt = as.integer(rt),
                TrialType = as.factor(TrialType),
                Condition = as.factor(Condition),
                NID = as.factor(NID),
                NumFeatures = as.factor(NumFeatures),
                finloc_T_x = as.integer(finloc_T_x),
                finloc_T_y = as.integer(finloc_T_y),
                finloc_V_x = as.integer(finloc_V_x),
                finloc_V_y= as.integer(finloc_V_y),
                finloc_F1_x = as.integer(finloc_F1_x),
                finloc_F1_y = as.integer(finloc_F1_y),
                finloc_F2_x = as.integer(finloc_F2_x),
                finloc_F2_y = as.integer(finloc_F2_y),
                finloc_F3_x = as.integer(finloc_F3_x),
                finloc_F3_y = as.integer(finloc_F3_y)
                ) %>% #必要列の変換
  dplyr::filter(cwid %in% cwid_finished_lst & TrialType == "Main") %>%
  dplyr::distinct(cwid, NID, .keep_all = TRUE)#重複している人を除外

# 2.1 results of 3.1. Effects of the number of topic-vehicle shared features on metaphor aptness ----
dat_aptness_glm <- dat_aptness %>%
  dplyr::select(-trial_index, -rt, -task, -SetType, -TrialType) %>%
  dplyr::mutate(
    CondDum_2m1 = case_when(
      NumFeatures == "2" ~ "1",
      TRUE ~ "0"),
    CondDum_3m1 = case_when(
      NumFeatures == "3" ~ "1",
      TRUE ~ "0"),
    CondDum_3m2 = case_when(
      NumFeatures == "3" ~ "1",
      TRUE ~ "0"),
    CondDum_1m2 = case_when(
      NumFeatures == "1" ~ "1",
      TRUE ~ "0")
    )
glm_aptness_based_1 <- lmer(formula = Aptness ~ CondDum_2m1 + CondDum_3m1 + (1|cwid) + (1 + CondDum_2m1 + CondDum_3m1|NID),
                    data = dat_aptness_glm)
summary(glm_aptness_based_1)

glm_aptness_based_2 <- lmer(formula = Aptness ~ CondDum_3m2 + CondDum_1m2 + (1|cwid) + (1 + CondDum_3m2 + CondDum_1m2|NID),
                            data = dat_aptness_glm)
summary(glm_aptness_based_2)

# 2.2 figure of 3.1. Effects of the number of topic-vehicle shared features on metaphor aptness ----
dat_part_aptness_bg <- dat_part_aptness_anova %>%
  dplyr::group_by(NumFeatures) %>%
  dplyr::summarise(M_Aptness = mean(AveAptness),
                   SD_Aptness = sd(AveAptness))

#quartz(type = "pdf", file = "../result/BarPlotAptness_v1.0.pdf")
ggplot()+theme_set(theme_classic(base_size = 12,base_family="Hiragino Mincho Pro W3"))
g <- ggplot(dat_part_aptness_bg, aes(y=M_Aptness, x=NumFeatures)) #irisOderedのデータフレームを使う。y軸とｘ軸の変数を指定。
g <- g + geom_bar(position = "dodge", stat = "identity")#平均値を表したbarの準備
g <- g + geom_errorbar(aes(ymin = M_Aptness - SD_Aptness, ymax = M_Aptness + SD_Aptness), width = .2, position = position_dodge(.9))#標準誤差
g <- g + scale_y_continuous(name = "Mean aptness rating", breaks = seq(1, 7, length=7), limits=c(1,7), oob=oob_keep)
g <- g + scale_x_discrete(name = "Number of topic-vehicle shared features",
                          labels = c("1" = "One", "2" = "Two", "3" = "Three"))
plot(g)
#dev.off()

# 3.0 prepare data for 3.2. The distances of the vehicle words and the features on the metaphor aptness ----

#ref: https://en.wikipedia.org/wiki/Euclidean_distance
calc_distance <- function(p1_x, p1_y, p2_x, p2_y) {
  # 距離を求める関数
  # p1_x, p1_y: 1つ目の座標のx,y
  # p2_x, p2_y: 2つ目の座標のx,y
  # return ユークリッド距離
  distance <- sqrt((p1_x - p2_x)^2 + (p1_y - p2_y)^2)
  return (distance)
}

# test: calc_distance
tmp_Tx <- c(80)
tmp_Ty <- c(260)
tmp_Fx <- c(0)
tmp_Fy <- c(0)
calc_distance(tmp_Tx, tmp_Ty, tmp_Fx, tmp_Fy)
sqrt(80^2+260^2)

dat_sorting_dist <- dat_sorting %>% 
  dplyr::select(-rt, -Condition, -TrialType) %>% 
  dplyr::mutate(finloc_T_F1 = calc_distance(finloc_T_x, finloc_T_y, finloc_F1_x, finloc_F1_y),
                finloc_T_F2 = calc_distance(finloc_T_x, finloc_T_y, finloc_F2_x, finloc_F2_y),
                finloc_T_F3 = calc_distance(finloc_T_x, finloc_T_y, finloc_F3_x, finloc_F3_y),
                finloc_V_F1 = calc_distance(finloc_V_x, finloc_V_y, finloc_F1_x, finloc_F1_y),
                finloc_V_F2 = calc_distance(finloc_V_x, finloc_V_y, finloc_F2_x, finloc_F2_y),
                finloc_V_F3 = calc_distance(finloc_V_x, finloc_V_y, finloc_F2_x, finloc_F3_y),
                finloc_F1_F2 = calc_distance(finloc_F1_x, finloc_F1_y, finloc_F2_x, finloc_F2_y),
                finloc_F1_F3 = calc_distance(finloc_F1_x, finloc_F1_y, finloc_F3_x, finloc_F3_y),
                finloc_F2_F3 = calc_distance(finloc_F2_x, finloc_F2_y, finloc_F3_x, finloc_F3_y)) %>%
  dplyr::select(-finloc_T_x, -finloc_T_y, -finloc_V_x, -finloc_V_y,
                -finloc_F1_x, -finloc_F1_y, -finloc_F2_x, -finloc_F2_y, -finloc_F3_x, -finloc_F3_y)

dat_dist_aptness <- dat_sorting_dist %>%
  dplyr::inner_join(dat_aptness, by=c("cwid", "NID", "NumFeatures")) %>%
  dplyr::select(-TrialType, -rt, -task, -TrialType, -trial_index, -SetType)

dat_dist_aptness_item <- dat_dist_aptness %>%
  dplyr::group_by(NID, NumFeatures) %>%
  dplyr::summarise(AveDist_T_F1 = mean(finloc_T_F1),
                   AveDist_T_F2 = mean(finloc_T_F2),
                   AveDist_T_F3 = mean(finloc_T_F3),
                   AveDist_V_F1 = mean(finloc_V_F1),
                   AveDist_V_F2 = mean(finloc_V_F2),
                   AveDist_V_F3 = mean(finloc_V_F3),
                   AveDist_F1_F2 = mean(finloc_F1_F2),
                   AveDist_F1_F3 = mean(finloc_F1_F3),
                   AveDist_F2_F3 = mean(finloc_F2_F3),
                   AveAptness = mean(Aptness))


# 3.1 results of 3.2. The distances of the vehicle words and the features on the metaphor aptness ----
dat_dist_aptness_item_cor <- dat_dist_aptness_item %>%
  dplyr::mutate(AveDist = case_when(
    NumFeatures == "1" ~ NA,
    NumFeatures == "2" ~ AveDist_F1_F2,
    NumFeatures == "3" ~ (AveDist_F1_F2 + AveDist_F1_F3 + AveDist_F2_F3)/3,
    TRUE ~ NA
  )) %>%
  dplyr::filter(NumFeatures != "1") %>%
  dplyr::select(NID, NumFeatures, AveDist, AveAptness)
cor.test(dat_dist_aptness_item_cor$AveDist, dat_dist_aptness_item_cor$AveAptness)

dat_dist_aptness_item_cor_two <- dat_dist_aptness_item_cor %>%
  dplyr::filter(NumFeatures == "2")
cor.test(dat_dist_aptness_item_cor_two$AveDist, dat_dist_aptness_item_cor_two$AveAptness)

dat_dist_aptness_item_cor_three <- dat_dist_aptness_item_cor %>%
  dplyr::filter(NumFeatures == "3")
cor.test(dat_dist_aptness_item_cor_three$AveDist, dat_dist_aptness_item_cor_three$AveAptness)

# figure of 3.2. The distances of the vehicle words and the features on the metaphor aptness
# ref: http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
#quartz(type = "pdf", file = "../result/scatter_plot_average_dist_of_features_and_aptness_rev1.pdf")
ggplot()+theme_set(theme_classic(base_size = 16,base_family="Hiragino Mincho Pro W3"))
g <- ggplot(dat_dist_aptness_item_cor, aes(x = AveDist, y = AveAptness, color = NumFeatures))
g <- g + scale_y_continuous(name = "Mean aptness rating", breaks = seq(1, 7, length=7), limits=c(1,7), oob=oob_keep)
g <- g + scale_x_continuous(name = "Mean distances between features", limits=c(1,NA))
g <- g + geom_point(aes(shape=NumFeatures)) + labs(color="Number of\ntopic-vehicle\nshared features", shape="Number of\ntopic-vehicle\nshared features") +
  scale_color_discrete(labels = c("2"="Two", "3"="Three")) +
  scale_shape_discrete(labels = c("2"="Two", "3"="Three"))
g
#dev.off()

# 3.3 results of appendix Table S1, S2, S3 ----
dat_dist_aptness_item_cor_one <- dat_dist_aptness_item %>%
  dplyr::filter(NumFeatures == "1") %>%
  dplyr::select(NID, NumFeatures, AveDist_T_F1, AveDist_V_F1, AveAptness)
mycor(3:5, dat_dist_aptness_item_cor_one, latex = FALSE)

dat_dist_aptness_item_cor_two <- dat_dist_aptness_item %>%
  dplyr::filter(NumFeatures == "2") %>%
  dplyr::select(NID, NumFeatures, AveDist_T_F1, AveDist_V_F1, AveDist_T_F2, AveDist_V_F2, AveDist_F1_F2, AveAptness)
mycor(3:8, dat_dist_aptness_item_cor_two, latex = FALSE)

dat_dist_aptness_item_cor_three <- dat_dist_aptness_item %>%
  dplyr::filter(NumFeatures == "3")
mycor(3:12, dat_dist_aptness_item_cor_three, latex = FALSE)

# results of glm
dat_dist_aptness_glm_one <- dat_dist_aptness %>%
  dplyr::filter(NumFeatures == "1") %>%
  dplyr::select(cwid, NID, finloc_V_F1, Aptness)
dat_dist_aptness_glm_one_z <- sjmisc::std(dat_dist_aptness_glm_one)

glm_dist_aptness_one_z <- lmer(formula = Aptness_z ~ finloc_V_F1_z + (1|cwid) + (1|NID),
                             data = dat_dist_aptness_glm_one_z)
summary(glm_dist_aptness_one_z)

dat_dist_aptness_glm_two <- dat_dist_aptness %>%
  dplyr::filter(NumFeatures == "2") %>%
  dplyr::select(cwid, NID, finloc_V_F1, finloc_V_F2, finloc_F1_F2, Aptness)
dat_dist_aptness_glm_two_z <- sjmisc::std(dat_dist_aptness_glm_two)

glm_dist_aptness_two_z <- lmer(formula = Aptness_z ~ finloc_V_F1_z + finloc_V_F2_z + finloc_F1_F2_z +(1|cwid) + (1|NID),
                             data = dat_dist_aptness_glm_two_z)
summary(glm_dist_aptness_two_z)

dat_dist_aptness_glm_three <- dat_dist_aptness %>%
  dplyr::filter(NumFeatures == "3") %>%
  dplyr::select(cwid, NID, finloc_V_F1, finloc_V_F2, finloc_V_F3, finloc_F1_F2, finloc_F1_F3, finloc_F2_F3, Aptness)
dat_dist_aptness_glm_three_z <- sjmisc::std(dat_dist_aptness_glm_three)

glm_dist_aptness_three_z <- lmer(formula = Aptness_z ~ finloc_V_F1_z + finloc_V_F2_z + finloc_V_F3_z +
                                 finloc_F1_F2_z + finloc_F1_F3_z + finloc_F2_F3_z + (1|cwid) + (1|NID),
                               data = dat_dist_aptness_glm_three_z)
summary(glm_dist_aptness_three_z)