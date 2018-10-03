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
read.csv("output/long_lbs.csv") -> long 

long %>% transmute( Year = as.factor(year), Stations = stations, Size = size, cpue, se) -> dat
str(dat)

dat %>% filter (Size == "lrg") -> lrg
dat %>% filter (Size == "all") -> all 

#SURVEY_WIDE
lrg %>% 
  ggplot(aes(x = Year, y = cpue, color = Stations)) + 
  scale_y_continuous(breaks = seq(0,5,.5), lim = c(0,2.5)) +
  geom_point(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin= cpue - se, ymax = cpue + se), width =.2, position=position_dodge(.3) ) + 
  labs( title = "Larges (CL > 32mm) ", x = "Year", y = "CPUE (lbs/pot)")
  ggsave("./figs/surveyWideCPUE_lbs_newVsOld_Lrg.png", dpi=300, height=3, width=4, units="in")

all %>% 
  ggplot(aes(x = Year, y = cpue, color = Stations)) + 
  scale_y_continuous(breaks = seq(0,5,.5), lim = c(0,5)) +
  geom_point(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin= cpue - se, ymax = cpue + se), width =.2, position = position_dodge(.3) ) + 
  labs( title = "All Sizes", x = "Year", y = "CPUE (lbs/pot)")
  ggsave("./figs/surveyWideCPUE_lbs_newVsOld_All.png", dpi=300, height=3, width=4, units="in")
  
#BY_AREA
  
  
  
  