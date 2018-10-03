## TabsFigs ####
# make figures comparing survey results using core vs non-core. 

## Load ####
library(tidyverse)
library(reshape2)
library(extrafont)
font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

read.csv("output/byYear_xz_w17_new.csv")-> surv_n
read.csv("output/byYear_xz_w17_old.csv")-> surv_o
read.csv("output/long_lbs.csv") -> long #cpue and se, surveywide, new and old stations. From byYear_xz_w17_old.csv and byYear_xz_w17_new.csv.   
read.csv("output/long_lbs_byArea.csv") ->long_byArea #cpue and se, by area, new and old stations. From byArea_xz_w17_old.csv and byArea_xz_w17_new.csv. 

#SURVEY_WIDE ####
long %>% transmute( Year = as.factor(year), Stations = stations, Size = size, cpue, se) -> dat 
dat$Stations <-  factor(dat$Stations, levels = c("old", "new")) # reorder old on left

dat %>% filter (Size == "lrg") -> lrg
dat %>% filter (Size == "all") -> all 

lrg %>% 
  ggplot(aes(x = Year, y = cpue, color = Stations)) + 
  scale_y_continuous(breaks = seq(0,5,.5), lim = c(0,3.5)) +
  geom_point(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin= cpue - se, ymax = cpue + se), width =.2, position=position_dodge(.3) ) + 
  labs( title = "Larges, survey-wide", x = "Year", y = "CPUE (lbs/pot)")
  ggsave("./figs/surveyWideCPUE_lbs_newVsOld_Lrg.png", dpi=300, height=3, width=4, units="in")

all %>% 
  ggplot(aes(x = Year, y = cpue, color = Stations)) + 
  scale_y_continuous(breaks = seq(0,8,.5), lim = c(0,7.5)) +
  geom_point(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin= cpue - se, ymax = cpue + se), width =.2, position = position_dodge(.3) ) + 
  labs( title = "All Sizes, survey-wide", x = "Year", y = "CPUE (lbs/pot)")
  ggsave("./figs/surveyWideCPUE_lbs_newVsOld_All.png", dpi=300, height=3, width=4, units="in")
  
#BY_AREA ####
long_byArea %>% transmute(Area = as.factor(area), Year = as.factor(year), Stations = stations, Size = size, cpue, se) -> dat_byArea
dat_byArea$Stations <-  factor(dat_byArea$Stations, levels = c("old", "new")) # reorder old on left
  
dat_byArea %>% filter (Size == "lrg") -> lrg_byArea
dat_byArea %>% filter (Size == "all") -> all_byArea

lrg_byArea %>% 
  ggplot(aes(x = Year, y = cpue, color = Stations)) + 
  scale_y_continuous(breaks = seq(0,5,.5), lim = c(0,3.5)) +
  geom_point(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin= cpue - se, ymax = cpue + se), width =.2, position=position_dodge(.3) ) + 
  labs( title = "Larges, by area ", x = "Year", y = "CPUE (lbs/pot)")+
  facet_wrap(~Area)
  ggsave("./figs/surveyWideCPUE_lbs_newVsOld_Lrg_byArea.png", dpi=300, height=3, width=4, units="in")

all_byArea %>% 
  ggplot(aes(x = Year, y = cpue, color = Stations)) + 
  scale_y_continuous(breaks = seq(0,8,.5), lim = c(0,7.5)) +
  geom_point(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin= cpue - se, ymax = cpue + se), width =.2, position=position_dodge(.3) ) + 
  labs( title = "All Sizes, by area", x = "Year", y = "CPUE (lbs/pot)")+
  facet_wrap(~Area)  
  ggsave("./figs/surveyWideCPUE_lbs_newVsOld_All_byArea.png", dpi=300, height=3, width=4, units="in")
  