## CPUE ####
# Calculates CATCH and CPUE for both Large and Alls from PWS spot shrimp pot survey. Used for to calc variances for 2017 BOF report. 
#Point ests used in report are from static SS output from old queries.
## VARs in report were output from this scripts 171103
# Josh Mumm 
## modified to follow Xinxians recomendatoins 171129.  From CPUE_revisedRat.R
## modified here for new vs old stations comparison 181002

## PREP ----
library(tidyverse)
cpp <- read.csv('data/CPP_lessSamps_d_180119.csv') # changed to cpp w 2017 in 2017 branch on 180119
  # _lessSamps is from temporaily correcting sample pot indicator on potperformance for those pots labeled sample but w/out any awls
  # lessSamps_d is is from modifying cpp sqls to remove rounding in step 6 and remove pop blank weight from count in 2. 
  # Also after editing 2011 DB (removed cc recs wtih 0 count and weight - this shouldn't have made a dif).  
  # Will use _d.  ALLs - both tau and mu match old query for all years rounded to nearest .1 %.   Large Tau and Mu are < .7% dif for all years 
read.csv('data/SiteStatArea_LUT.csv')  %>% transmute (Site = as.factor(SiteNum), Area = ShrimpArea) -> area

#rename vars and calc r 
  cpp %>% transmute(year = YEAR, 
                 Event = EVENT_ID, 
                 Site = as.factor(SITE_ID), 
                 Station = STATION, 
                 Pot = as.factor(POT_ID),
                 Sample = SAMPLE_POT, 
                 all_cnt = all_Cnt_cc, 
                 all_kg = all_Kg_cc, 
                 lrg_cnt = lrg_Cnt,
                 lrg_kg = lrg_Kg,
                 r_kg = lrg_kg/all_kg, 
                 r_cnt = lrg_cnt/all_cnt) -> cpp

left_join(cpp, area) -> cpp
## ALLS ----
  #survey-wide 
  # cpp %>% filter (Site != '11' & Station %in% c("A","B","C","D") & year > 2015) %>% group_by(year) %>%   # old filter
   cpp %>% filter (Site != '11' & Station %in% c("W","X","Y","Z") & year > 2015) %>% group_by(year) %>%   # new filter
    summarise ( 
      N = n(),
      tau_all_cnt = sum(all_cnt), 
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N,  
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/(N-1), # n-1 since treating as sample not pop now(10/21).  Leaving N as N, rather than  n for now. 
      var_all_kg = sum((all_kg - mu_all_kg)^2)/(N-1) , # xz has N insteasd of N-1
      se_all_cnt = (var_all_cnt^.5)/(N^.5),
      se_all_kg = (var_all_kg^.5)/(N^.5),
      cv_all_cnt = 100* (var_all_cnt^.5)/mu_all_cnt,
      cv_all_kg = 100* (var_all_kg^.5)/mu_all_kg) -> all_byYear
  #bySite
    # cpp %>% filter (Site != '11' & Station %in% c("A","B","C","D") & year > 2015) %>% group_by(year, Site) %>% # old filter
    cpp %>% filter (Site != '11' & Station %in% c("W","X","Y","Z") & year > 2015) %>% group_by(year, Site) %>% # new filter
    summarise (
      Area = first(Area),
      N = n(),
      tau_all_cnt = sum(all_cnt),
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N,
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/(N-1),
      var_all_kg = sum((all_kg - mu_all_kg)^2)/(N-1),
      se_all_cnt = (var_all_cnt^.5)/(N^.5),
      se_all_kg = (var_all_kg^.5)/(N^.5),
      cv_all_cnt = 100* (var_all_cnt^.5)/mu_all_cnt,
      cv_all_kg = 100* (var_all_kg^.5)/mu_all_kg) -> all_bySite
  #byArea
    # cpp %>% filter (Site != '11' & Station %in% c("A","B","C","D") & year > 2015) %>% group_by(year, Area) %>%  # old filter
    cpp %>% filter (Site != '11' & Station %in% c("W","X","Y","Z") & year > 2015) %>% group_by(year, Area) %>%  # new filter
    summarise (
      N = n(),
      tau_all_cnt = sum(all_cnt),
      tau_all_kg = sum(all_kg),
      mu_all_cnt = tau_all_cnt/N,
      mu_all_kg = tau_all_kg/N,
      var_all_cnt = sum((all_cnt - mu_all_cnt)^2)/(N-1),
      var_all_kg = sum((all_kg - mu_all_kg)^2)/(N-1),
      se_all_cnt = (var_all_cnt^.5)/(N^.5),
      se_all_kg = (var_all_kg^.5)/(N^.5),
      cv_all_cnt = 100*  (var_all_cnt^.5)/mu_all_cnt,
      cv_all_kg = 100* (var_all_kg^.5)/mu_all_kg) -> all_byArea

## LARGES ----
  # bySite 
    all_bySite %>% select(year, Site, Area, N, mu_all_cnt, mu_all_kg, tau_all_kg, var_all_kg) %>% right_join(cpp) %>% # join site-level stats to CPP
      filter (Sample == "Sample") %>% group_by(year, Site) %>% 
      summarise ( 
        Area = first(Area),
        n = n(),
        N = first(N),
        mu_hat_all_kg = sum(all_kg, na.rm = T)/n,
        var_all_kg = first(var_all_kg), # xz uses N instead od N-1 for num of var_all_kg
        tau_all_kg = first(tau_all_kg), 
        r_bar = sum(lrg_kg, na.rm = T)/sum(all_kg, na.rm = T),
        s2_h = sum((lrg_kg - r_bar*all_kg)^2, na.rm = T)/ (n-1),
        r_var = (s2_h/n) * (1/mu_hat_all_kg)^2 * (N-n)/N, 
        mu_lrg_kg  = r_bar * first(mu_all_kg),
        tau_lrg_kg =  mu_lrg_kg * N, 
        var_tau_lrg_kg = (tau_all_kg^2)*r_var + (r_bar^2)*var_all_kg - r_var*var_all_kg, # xz uses N instead od N-1 for num of var_all_kg
        var_mu_lrg_kg = var_tau_lrg_kg/(N^2) ) -> large_bySite      

    #byYear
    large_bySite %>% filter (Site != '11') %>% group_by (year) %>%
      summarise (
        n = sum(n),
        N = sum(N),
        tau_lrg_kg = sum(tau_lrg_kg),
        mu_lrg_kg  = tau_lrg_kg / N,
        var_tau_lrg_kg  = sum(var_tau_lrg_kg, na.rm = T), # Na.rm added as bandaid for 2011 site 5 no shrimp in cpp.  Edit data eventually.
        var_mu_lrg_kg = var_tau_lrg_kg/(N^2) ,
        se_lrg_kg = (var_mu_lrg_kg^.5)) -> large_byYear

      #byArea
      large_bySite %>% filter (Site != '11') %>% group_by (year,Area) %>%
        summarise (
          n = sum(n),
          N = sum(N),
          tau_lrg_kg = sum(tau_lrg_kg),
          mu_lrg_kg  = tau_lrg_kg / N,
          var_tau_lrg_kg  = sum(var_tau_lrg_kg, na.rm = T), # Na.rm added as bandaid for 2011 site 5 no shrimp in cpp.  Edit data eventually.
          var_mu_lrg_kg = var_tau_lrg_kg/(N^2) ,
          se_lrg_kg = (var_mu_lrg_kg^.5)) -> large_byArea

#select, join and Write ----
  all_byYear %>% left_join (large_byYear) %>% select(year, N, n, mu_all_kg, var_all_kg, se_all_kg, mu_lrg_kg ,var_tau_lrg_kg, se_lrg_kg)-> var_byYear
  all_byArea %>% left_join (large_byArea) %>% select(year, Area, N, n, mu_all_kg, var_all_kg, se_all_kg, mu_lrg_kg, var_tau_lrg_kg, se_lrg_kg)-> var_byArea
  
  # write.csv(var_byYear, "./output/byYear_xz_w17_old.csv", row.names = F)  
  # write.csv(var_byArea, "./output/byArea_xz_w17_old.csv", row.names = F)    
    
  write.csv(var_byYear, "./output/byYear_xz_w17_new.csv", row.names = F)
  write.csv(var_byArea, "./output/byArea_xz_w17_new.csv", row.names = F)
    
    
    
    
    
        

