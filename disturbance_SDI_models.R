library(dplyr)
library(glmmTMB)
library(lme4)
library(Matrix)
library(emmeans)
library(car)

#dat <- readRDS("C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/data/sPlot4_sel_var_plot_inv_myc_scaled_SC0625.rds")
dat1 <- readRDS("data/sPlot4_sel_var_plot_inv_myc_scaled_SC0625.rds")
#colSums(is.na(dat))
dat.sel <- dat[!is.na(dat$shan_dist), ]

#select only date range 1999-2019
library(lubridate)
dat.sel_SDI <- dat.sel %>% 
  mutate(year = year(as.Date(date))) %>% 
  filter( year>=1999 & year<=2019) #249947

###GLOBAL
#Calculate Q25 and Q75 outside of mutate
Q25 <- quantile(dat.sel_SDI$shan_dist, 0.25)
Q75 <- quantile(dat.sel_SDI$shan_dist, 0.75)

#Label data based on quantiles
dat.sel_SDI_global <- dat.sel_SDI %>%
  mutate(
    SDI.gl = case_when(
      shan_dist <= Q25 ~ "low",      # Below 25th percentile
      shan_dist >= Q75 ~ "high",     # Above 75th percentile
      TRUE ~ "medium"                  # Between 25th and 75th percentiles
    )
  )

#Filter only 'low' and 'high'
dat.sel_SDI_global <- dat.sel_SDI_global %>%
  filter(SDI.gl %in% c("high", "low"))

#model NM vs ALL
mod_status_NMvsALL_SDI <- glmmTMB(cbind(NMct, (AMct+EMct+AMNMct))  ~  status * SDI.gl +
                                    MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                    (1|plot_id), 
                                  data = dat.sel_SDI_global, family=binomial(link ="logit"),
                                  control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_NMvsALL_SDI, "mod_status_NMvsALL_SDI.rds")

mod_status_AMNMvsAM_NM_SDI <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  status * SDI.gl +
                                        MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                        (1|plot_id), 
                                      data = dat.sel_SDI_global, family=binomial(link ="logit"),
                                      control = glmmTMBControl(parallel = 16))
saveRDS(mod_status_AMNMvsAM_NM_SDI, "mod_status_AMNMvsAM_NM_SDI.rds")

########################################################################################################################
## BIOME
dat.sel_SDI <- dat.sel_SDI[!is.na(dat.sel_SDI$biome), ]
#BIOME MODELS
low.biomes<- c("Mangroves", "Tropical_Coniferous", "Flooded_Grasslands", "Tropical_Deciduous_Broadleaf")
dat.sel_SDI <- dat.sel_SDI %>% filter (!biome %in% low.biomes)  %>% droplevels()
count(dat.sel_SDI, biome)

dat.sel_SDI_biome <- dat.sel_SDI %>%
  group_by(biome) %>%
  mutate(
    Q25 = quantile(shan_dist, 0.25),
    Q75 = quantile(shan_dist, 0.75)) %>% 
  mutate(
    SDI.bspec = case_when(
      shan_dist <= Q25 ~ "low",      # Below 25th percentile
      shan_dist >= Q75 ~ "high",     # Above 75th percentile
      TRUE ~ "medium")) %>%           # Between 25th and 75th percentiles
  ungroup()

dat.sel_SDI_biome <- dat.sel_SDI_biome %>% filter(SDI.bspec %in% c("high", "low"))

count(dat.sel_SDI_biome, biome, Q25, Q75)
count(dat.sel_SDI_biome, biome, SDI.bspec)

mod_biome_NMvsALL_SDI_bspec <- glmmTMB(cbind(NMct, (AMct+EMct+AMNMct))  ~  biome * status * SDI.bspec +
                                         MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                         (1|plot_id), 
                                       data = dat.sel_SDI_biome, family=binomial(link ="logit"),
                                       #ziformula = ~1,
                                       control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_NMvsALL_SDI_bspec, "mod_biome_status_NMvsALL_SDI.rds")

mod_biome_AMNMvsAM_NM_SDI_bspec <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  biome * status * SDI.bspec +
                                             MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                             (1|plot_id), 
                                           data = dat.sel_SDI_biome, family=binomial(link ="logit"),
                                           #ziformula = ~1,
                                           control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsAM_NM_SDI_bspec, "mod_biome_status_AMNMvsAM_NM_SDI.rds")

##EMMEANS
means_global_NMvsALL_SDI <- emmeans (mod_status_NMvsALL_SDI, pairwise ~ status | SDI.gl , 
                                     component = "response", cov.reduce = mean, data = dat.sel_SDI_global)
saveRDS(means_global_NMvsALL_SDI, "means_global_NMvsALL_SDI.rds")

means_global_AMNMvsAM_NM_SDI <- emmeans (mod_status_AMNMvsAM_NM_SDI, pairwise ~ status | SDI.gl , 
                                         component = "response", cov.reduce = mean, data = dat.sel_SDI_global)
saveRDS(means_global_AMNMvsAM_NM_SDI, "means_global_AMNMvsAM_NM_SDI.rds")

means_biome_NMvsALL_SDI <- emmeans (mod_biome_NMvsALL_SDI_bspec, pairwise ~ status | biome | SDI.bspec , 
                                    component = "response", cov.reduce = mean, data = dat.sel_SDI_biome)
saveRDS(means_biome_NMvsALL_SDI, "means_biome_NMvsALL_SDI.rds")

means_biome_AMNMvsAM_NM_SDI <- emmeans (mod_biome_AMNMvsAM_NM_SDI_bspec, pairwise ~ status | biome | SDI.bspec , 
                                        component = "response", cov.reduce = mean, data = dat.sel_SDI_biome)
saveRDS(means_biome_AMNMvsAM_NM_SDI, "means_biome_AMNMvsAM_NM_SDI.rds")



#########################
#model for AMNM vs AM + NM
#Remove "Tundra", "Montane_Grasslands", "Boreal" because they have combinations with all 0

## BIOME
dat.sel_SDI <- dat.sel_SDI[!is.na(dat.sel_SDI$biome), ]
#BIOME MODELS
low.biomes<- c("Mangroves", "Tropical_Coniferous", "Flooded_Grasslands", "Tropical_Deciduous_Broadleaf", "Tundra", "Montane_Grasslands", "Boreal")
dat.sel_SDI <- dat.sel_SDI %>% filter (!biome %in% low.biomes)  %>% droplevels()
count(dat.sel_SDI, biome)

dat.sel_SDI_biome <- dat.sel_SDI %>%
  group_by(biome) %>%
  mutate(
    Q25 = quantile(shan_dist, 0.25),
    Q75 = quantile(shan_dist, 0.75)) %>% 
  mutate(
    SDI.bspec = case_when(
      shan_dist <= Q25 ~ "low",      # Below 25th percentile
      shan_dist >= Q75 ~ "high",     # Above 75th percentile
      TRUE ~ "medium")) %>%           # Between 25th and 75th percentiles
  ungroup()

dat.sel_SDI_biome <- dat.sel_SDI_biome %>% filter(SDI.bspec %in% c("high", "low"))

count(dat.sel_SDI_biome, biome, Q25, Q75)
count(dat.sel_SDI_biome, biome, SDI.bspec)

mod_biome_AMNMvsAM_NM_SDI_bspec_lessbiomes <- glmmTMB(cbind(AMNMct, (AMct+NMct))  ~  biome * status * SDI.bspec +
                                             MAT + MAP + coarsefrag + SOCstock + sand + PH +
                                             (1|plot_id), 
                                           data = dat.sel_SDI_biome, family=binomial(link ="logit"),
                                           #ziformula = ~1,
                                           control = glmmTMBControl(parallel = 16))
saveRDS(mod_biome_AMNMvsAM_NM_SDI_bspec_lessbiomes, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/mod_biome_status_AMNMvsAM_NM_SDI_lessbiomes.rds")

means_biome_AMNMvsAM_NM_SDI <- emmeans (mod_biome_AMNMvsAM_NM_SDI_bspec_lessbiomes, pairwise ~ status | biome | SDI.bspec , 
                                        component = "response", cov.reduce = mean, data = dat.sel_SDI_biome)
saveRDS(means_biome_AMNMvsAM_NM_SDI, "C:/Users/sarag/OneDrive - ETH Zurich/1_Invasion/Inv_Data/sPlot4_Sara/Models/means_biome_AMNMvsAM_NM_SDI_lessbiomes.rds")
