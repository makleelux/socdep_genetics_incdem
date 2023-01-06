################################################################################
#                                                                              #
#                 Function definitions and package installation                #                    
#                                                                              #
#                                                                              #
#                                                                              #     
#                                                                              #
# 20211124 Matthias Klee                                                       #  
################################################################################

#### setup ####
# Specify your packages
my_packages <- c("tidyverse", "knitr", "survival", 
                 "broom", "mice", "cobalt",
                 "datawizard", "ggplot2", "scales",
                 "haven", "epiR", "ipw", "survey")
# Extract not installed packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])] 
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                               


#### packages ####
library(tidyverse)
library(knitr)
library(survival)
library(broom)
library(mice)
library(scales)
library(datawizard)
library(ggplot2)
library(haven)
library(epiR)
library(ipw)
library(survey)

#### preprocessing ####
preprocess = function(df){
  ### preprocesses data after computation of the lifestyle score
  ### requires:
  ### dat: data frame including either imputed or complete case data
  
  
  # --------- labelling --------- #
  
  # label factors according to UKB data coding
  lvl.100293 <- c(-3,-1,1,2,3,4,5)
  lbl.100293 <- c("Prefer not to answer","Do not know","None","One","Two","Three","Four or more")
  df$f.728.0.0 <- ordered(df$f.728.0.0, levels=lvl.100293, labels=lbl.100293)
  lvl.100286 <- c(-7,-3,1,2,3,4,5)
  lbl.100286 <- c("None of the above","Prefer not to answer","A house or bungalow","A flat, maisonette or apartment","Mobile or temporary structure (i.e. caravan)","Sheltered accommodation","Care home")
  df$f.670.0.0 <- ordered(df$f.670.0.0, levels=lvl.100286, labels=lbl.100286)
  lvl.100287 <- c(-7,-3,1,2,3,4,5,6)
  lbl.100287 <- c("None of the above","Prefer not to answer","Own outright (by you or someone in your household)","Own with a mortgage","Rent - from local authority, local council, housing association","Rent - from private landlord or letting agency","Pay part rent and part mortgage (shared ownership)","Live in accommodation rent free")
  df$f.680.0.0 <- ordered(df$f.680.0.0, levels=lvl.100287, labels=lbl.100287)
  lvl.100294 <- c(-3,-1,1,2,3,4,5)
  lbl.100294 <- c("Prefer not to answer","Do not know","Less than 18,000","18,000 to 30,999","31,000 to 51,999","52,000 to 100,000","Greater than 100,000")
  df$f.738.0.0 <- ordered(df$f.738.0.0, levels=lvl.100294, labels=lbl.100294)
  lvl.100292 <- c(-3,1,2,3,4,5,6,7,8)
  lbl.100292 <- c("Prefer not to answer","Husband, wife or partner","Son and/or daughter (include step-children)","Brother and/or sister","Mother and/or father","Grandparent","Grandchild","Other related","Other unrelated")
  df$f.6141.0.0 <- ordered(df$f.6141.0.0, levels=lvl.100292, labels=lbl.100292)
  lvl.100295 <- c(-7,-3,1,2,3,4,5,6,7)
  lbl.100295 <- c("None of the above","Prefer not to answer","In paid employment or self-employed","Retired","Looking after home and/or family","Unable to work because of sickness or disability","Unemployed","Doing unpaid or voluntary work","Full or part-time student")
  df$f.6142.0.0 <- ordered(df$f.6142.0.0, levels=lvl.100295, labels=lbl.100295)
  
  
  # --------- var types --------- #
  
  
  # output df
  res_df = df %>% 
    
    # adapt variable types and aggregate factor levels
    mutate(
      
      # dem diag
      inc.dem2 = as.numeric(as.character(inc.dem2)),
      
      # marital status
      marital_status = factor(case_when(
        f.6141.0.0 == "Husband, wife or partner" ~ "married / relationship",
        f.6141.0.0 == "Son and/or daughter (include step-children)" | 
          f.6141.0.0 == "Brother and/or sister" | 
          f.6141.0.0 == "Grandchild" | 
          f.6141.0.0 == "Mother and/or father" | 
          f.6141.0.0 == "Grandparent" | 
          f.6141.0.0 == "Other related" | 
          f.6141.0.0 == "Other unrelated" | 
          f.6141.0.0 == "Prefer not to answer"  ~ "not married / other living situation")),
      
      # employment status
      employment_status = factor(case_when(
        f.6142.0.0 == "In paid employment or self-employed" ~ "employed",
        f.6142.0.0 == "Retired" ~ "retired",
        f.6142.0.0 %in% c("Unable to work because of sickness or disability", "Doing unpaid or voluntary work", "Full or part-time student", "Looking after home and/or family", "Unemployed") ~ "no paid employment",
        f.6142.0.0 %in% c("None of the above", "Prefer not to answer") ~ "not disclosed"),
        levels = c("employed", "retired", "no paid employment", "not disclosed")),
      
      # income
      avg_hhl_income_b_tax = factor(case_when(
        f.738.0.0 %in% c("Prefer not to answer", "Do not know") ~ "income not disclosed", 
        f.738.0.0 == "Less than 18,000" ~ "income up to 18,000",
        f.738.0.0 == "18,000 to 30,999" ~ "income up to 30,999",
        f.738.0.0 == "31,000 to 51,999" | f.738.0.0 == "52,000 to 100,000" | f.738.0.0 == "Greater than 100,000" ~ "income greater 31,000"),
        levels = c("income greater 31,000", "income up to 30,999", "income up to 18,000", "income not disclosed")),
      
      # no ppl in household
      n_ppl_in_hh = ifelse(f.709.0.0 %in% c("-1", "-3") == FALSE, as.numeric(as.character(f.709.0.0)), NA),
      
      # Tdi
      tdi = f.189.0.0,
      tdi_quint = as.factor(ntile(tdi, 5)),
      tdi_bin = factor(case_when(
        tdi_quint == 1 | tdi_quint == 2 | tdi_quint == 3 | tdi_quint == 4 ~ "low-to-moderate deprivation",
        tdi_quint == 5 ~ "high deprivation"),
        levels = c("low-to-moderate deprivation", "high deprivation")),
      "Area Deprivation" = factor(case_when(
        tdi_quint == 1 ~ " Quintile 1 (Lowest Deprivation)",
        tdi_quint == 2 ~ " Quintile 2",
        tdi_quint == 3 ~ " Quintile 3",
        tdi_quint == 4 ~ " Quintile 4",
        tdi_quint == 5 ~ " Quintile 5 (Highest Deprivation)"), 
        levels = c(" Quintile 1 (Lowest Deprivation)", " Quintile 2", " Quintile 3", " Quintile 4", " Quintile 5 (Highest Deprivation)")),
      
      # Sprs (already z-standardized in final vars)
      Sprs_quint = as.factor(ntile(Sprs, 5)),
      Sprs_tert = factor(case_when(
        Sprs_quint == 1 ~ "Q1",
        Sprs_quint == 2 | Sprs_quint == 3 | Sprs_quint == 4 ~ "Q2-4",
        Sprs_quint == 5 ~ "Q5"),
        levels = c("Q1", "Q2-4", "Q5")),
      
      # combined sprs and tdi
      tdi_Sprs = factor(case_when(
        tdi_bin == "low-to-moderate deprivation" & Sprs_tert == "Q1" ~ "Q1 (low risk) Q1-4 (low-to-moderate deprivation)",
        tdi_bin == "high deprivation" & Sprs_tert == "Q1" ~ "Q1 (low risk) Q5 (high deprivation)",
        tdi_bin == "low-to-moderate deprivation" & Sprs_tert == "Q2-4" ~ "Q2-4 (intermediate risk) Q1-4 (low-to-moderate deprivation)",
        tdi_bin == "high deprivation" & Sprs_tert == "Q2-4" ~ "Q2-4 (intermediate risk) Q5 (high deprivation)",
        tdi_bin == "low-to-moderate deprivation" & Sprs_tert == "Q5" ~ "Q5 (high risk) Q1-4 (low-to-moderate deprivation)",
        tdi_bin == "high deprivation" & Sprs_tert == "Q5" ~ "Q5 (high risk) Q5 (high deprivation)"),
        levels = c("Q1 (low risk) Q1-4 (low-to-moderate deprivation)",
                   "Q1 (low risk) Q5 (high deprivation)",
                   "Q2-4 (intermediate risk) Q1-4 (low-to-moderate deprivation)",
                   "Q2-4 (intermediate risk) Q5 (high deprivation)",
                   "Q5 (high risk) Q1-4 (low-to-moderate deprivation)",
                   "Q5 (high risk) Q5 (high deprivation)")),
      
      # depressive symptoms
      depress2 = factor(case_when(f.2050.0.0 == "1" ~ "never", 
                                  f.2050.0.0 %in% c("2", "3", "4") ~ "several days, more than half or nearly every day",
                                  f.2050.0.0 %in% c("-1", "-3") ~ "Do not know / prefer not to answer"),
                        levels = c("never", "several days, more than half or nearly every day", "Do not know / prefer not to answer")),
      
      # third degree relatedness
      degree3 = as.factor(as.character(degree3)),
      
      # accomomodation type
      score_accommodation_type = factor(
        ifelse(f.670.0.0 %in% c("A house or bungalow", "A flat, maisonette or apartment"), "House / Flat", 
               ifelse(f.670.0.0 %in% c("Prefer not to answer", "None of the above"), "not disclosed", "other")),
        levels = c("House / Flat", "other", "not disclosed")),
      
      score_accommodation_type_bin = factor(
        ifelse(f.670.0.0 %in% c("A house or bungalow", "A flat, maisonette or apartment"), "House / Flat", "other"),
        levels = c("House / Flat", "other")),
      
      # accommodation ownership
      score_accommodation_asset = 
        factor(ifelse(f.680.0.0 %in% c("Own outright (by you or someone in your household)"), "Own outright", 
                      ifelse(f.680.0.0 %in% c("Prefer not to answer", "None of the above"), "not disclosed", "other")),
               levels = c("Own outright", "other", "not disclosed")),
      
      score_accommodation_asset_bin = 
        factor(ifelse(f.680.0.0 == "Own outright (by you or someone in your household)", "Own outright", "other"),
               levels = c("Own outright", "other")),
      
      
      # no vehicles
      score_vehicles = factor(ifelse(f.728.0.0 %in% c("One", "Two", "Three", "Four or more"), "Own car", 
                                     ifelse(f.728.0.0 == "None", "None", "not disclosed")),
                              levels = c("Own car", "None", "not disclosed")),
      
      score_vehicles_bin = factor(ifelse(f.728.0.0 == "One" |
                                           f.728.0.0 == "Two" |
                                           f.728.0.0 == "Three" |
                                           f.728.0.0 == "Four or more", 
                                         "Own car", "other"),
                                  levels = c("Own car", "other")),
      
      # edu ISCED
      edu_new = factor(case_when(
        edu.rowmax == "0" ~ "prefer not to answer edu / none of above",
        edu.rowmax == "1" ~ "low edu",
        edu.rowmax == "2" ~ "medium edu",
        edu.rowmax == "3" ~ "high edu"), 
        levels = c("high edu", "medium edu", "low edu", "prefer not to answer edu / none of above")),
      
      # smoking
      smok = factor(as.character(smok)),
      
      # diet
      diet.c = factor(as.character(diet.c)),
      
      # physical activity
      regpa2 = factor(as.character(regpa2)),
      
      # alc consumption
      modalc = factor(as.character(modalc))) %>% 
    
    # exclude vars
    select(-c("f.6141.0.0", "f.6142.0.0", "f.738.0.0", "f.670.0.0", "f.680.0.0", "f.728.0.0", "f.2050.0.0", "f.189.0.0", "f.709.0.0", "Sprs_quint", "edu.rowmax"))
  
  return(res_df)
}
set_wealth_Sprs_combi = function(data){
  ### append combined individual-level deprivation and sprs groups based on weighted wealth score
  ### requires: 
  ### data: dataframe including weighted wealth score and polygenic risk groups
  
  
  # outfile
  data_new = data %>% 
    
    # compute combined groups
    mutate(
      wealth_Sprs = factor(case_when(
        cwwealth_inc == "low deprivation" & Sprs_tert == "Q1" ~ "Q1 (low risk) Q1 (low deprivation)",
        cwwealth_inc == "intermediate deprivation" & Sprs_tert == "Q1" ~ "Q1 (low risk) Q2-4 (intermediate deprivation)",
        cwwealth_inc == "high deprivation" & Sprs_tert == "Q1" ~ "Q1 (low risk) Q5 (high deprivation)",
        cwwealth_inc == "low deprivation" & Sprs_tert == "Q2-4" ~ "Q2-4 (intermediate risk) Q1 (low deprivation)",
        cwwealth_inc == "intermediate deprivation" & Sprs_tert == "Q2-4" ~ "Q2-4 (intermediate risk) Q2-4 (intermediate deprivation)",
        cwwealth_inc == "high deprivation" & Sprs_tert == "Q2-4" ~ "Q2-4 (intermediate risk) Q5 (high deprivation)",
        cwwealth_inc == "low deprivation" & Sprs_tert == "Q5" ~ "Q5 (high risk) Q1 (low deprivation)",
        cwwealth_inc == "intermediate deprivation" & Sprs_tert == "Q5" ~ "Q5 (high risk) Q2-4 (intermediate deprivation)",
        cwwealth_inc == "high deprivation" & Sprs_tert == "Q5" ~ "Q5 (high risk) Q5 (high deprivation)"),
        levels = c("Q1 (low risk) Q1 (low deprivation)", "Q1 (low risk) Q2-4 (intermediate deprivation)", "Q1 (low risk) Q5 (high deprivation)", "Q2-4 (intermediate risk) Q1 (low deprivation)", "Q2-4 (intermediate risk) Q2-4 (intermediate deprivation)", "Q2-4 (intermediate risk) Q5 (high deprivation)", "Q5 (high risk) Q1 (low deprivation)", "Q5 (high risk) Q2-4 (intermediate deprivation)", "Q5 (high risk) Q5 (high deprivation)")))
  
  return(data_new)
}

#### cox modeling with pool ####
coxmodel_then_pool = function(right_hand_formula, data_ls, filter_cond = TRUE){
  ### models cox proportional-hazards regression for individual elements of data_ls
  ### outputs pooled results from individual cox models
  ### requires:
  ### right_hand_formula: string indicating right hand side of the formula required for coxph
  ### data_ls: a list of data sets (i.e. consisting of 5 imputed data sets or 1 complete case data set)
  ### filter_cond: string indicating optional filter condition (default does not apply filtering)
  
  
  ## init
  model_count = 0
  res = list()
  
  ## loop over 5 imputed data sets
  for(i in 1:length(data_ls)){
    model_count = model_count + 1
    
    ## cox models
    res_temp = coxph(as.formula(paste("Surv(duryears, inc.dem2)", right_hand_formula, sep = "~")), 
                     data = data_ls[i] %>% as.data.frame() %>% filter_(filter_cond))
    
    ## append results
    res[[model_count]] = res_temp
  }
  
  ## pool results
  res_pooled = pool(as.mira(res))
  return(res_pooled)
}
get_pvalue = function(mod){
  ### outputs p value for cox models with continuous individual-level socioeconomic deprivation score
  ### requires:
  ### mod: cox model including wwealth
  
  
  p.value = mod %>% tidy() %>% filter(term == "wwealth") %>% select(p.value)
  p.value = p.value$p.value
  `P value for trend1` = ifelse(p.value < .001, "<.001", sprintf("%.3f", round(p.value, 4)))
  
  return(`P value for trend1`)
}

#### incidence risk ####
inc_est = function(groups){
  ### outputs results table with incidence rates per 1000 person-years based on the first imputed data set
  ### requires:
  ### groups: vector containing strings of column names used for grouping 
  
  
  epi_dat = final_vars_aug.imp1 %>% 
    select(inc.dem2, duryears, groups) %>% 
    group_by_at(groups) %>% 
    summarise(`No. of Dementia Cases` = sum(inc.dem2 == 1), n_years = sum(duryears)/1000)
  
  inc_temp = epi.conf(
    cbind(epi_dat$`No. of Dementia Cases`, epi_dat$n_years) %>% as.matrix(), 
    ctype = "inc.rate", 
    method = "exact") %>% 
    transmute(`Incidence Rates per 1,000 Person-Years` = est, `CI lower` = lower, `CI upper` = upper)
  
  return(inc_temp)
}
risk_est = function(groups){
  ### outputs results table with absolute risk based on the first imputed data set
  ### requires:
  ### groups: vector containing strings of column names used for grouping 
  
  
  epi_dat = final_vars_aug.imp1 %>% 
    select(inc.dem2, duryears, groups) %>% 
    group_by_at(groups) %>% 
    summarise(`No. of Dementia Cases` = sum(inc.dem2 == 1), `Total No.` = n())
  
  risk_temp = epi.conf(
    cbind(epi_dat$`No. of Dementia Cases`, epi_dat$`Total No.`) %>% as.matrix(), 
    ctype = "inc.risk", 
    method = "exact") %>% 
    transmute(`Absolute Risk` = est*100, `CI Lower` = lower*100, `CI Upper` = upper*100)
  
  return(risk_temp)
  
}

#### imaging ####
set_ds_simple = function(i, data_ls){
  ### sets up data set for further analysis
  ### requires:
  ### data_ls: a list of data sets (i.e. consisting of 5 imputed data sets or 1 complete case data set)
  ### i: an integer indexing which data set of the list will be preprocessed
  
  
  # --------- merge bd (imaging) and prepared (complete/imputed) data --------- #
  
  ds = data_ls[i] %>% 
    
    # transform to df
    as.data.frame() %>% 
    
    # select columns of interest
    dplyr::select(f.eid, inc.dem2, duryears, degree3, 
                  pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, 
                  pc11, pc12, pc13, pc14, pc15, pc16, pc17, pc18, pc19, pc20, 
                  cnt1, sex, age, edu_new, marital_status, 
                  cwlife2, depress2, 
                  cwwealth_inc, tdi_Sprs, 
                  f.54.2.0, f.53.0.0, f.53.2.0, f.25020.2.0, f.25019.2.0, f.25006.2.0, f.25008.2.0, f.25010.2.0, f.25781.2.0, f.25000.2.0, 
                  wwealth, tdi, Sprs, tdi_bin, Sprs_tert,
                  smok, diet.c, regpa2, modalc, employment_status,
                  avg_hhl_income_b_tax, score_accommodation_type_bin, score_accommodation_asset_bin, score_vehicles_bin)
  
  
  # --------- preprocessing 1 --------- #
  
  ds_prep1 = ds %>% 
    # filter Bristol (N = 25) and NA cases 
    filter(f.54.2.0 != "11028") %>%
    
    # prep
    mutate(
      # update age at scan
      time_since_recruitement = round(as.numeric(difftime(f.53.2.0, f.53.0.0, units = "days") / 365.25), 0),
      age_at_scan = age + time_since_recruitement,
      
      # rename main IVs
      Area_Deprivation = factor(case_when(tdi_bin == "high deprivation" ~ "High", tdi_bin == "low-to-moderate deprivation" ~ "Low to Moderate"), levels = c("Low to Moderate", "High")),
      Individual_Deprivation = factor(case_when(cwwealth_inc == "high deprivation" ~ "High", cwwealth_inc == "intermediate deprivation" ~ "Intermediate", cwwealth_inc == "low deprivation" ~ "Low"), levels = c("Low", "Intermediate", "High")),
      Genetic_Risk = factor(case_when(Sprs_tert == "Q5" ~ "High", Sprs_tert == "Q2-4" ~ "Intermediate", Sprs_tert == "Q1" ~ "Low"), levels = c("Low", "Intermediate", "High")),
      
      # dummy codes for site
      site1 = factor(ifelse(f.54.2.0 == "11025", 1, 0)),
      site2 = factor(ifelse(f.54.2.0 == "11026", 1, 0)),
      
      # filter variable for site selection
      filter = factor(ifelse(site1 == 1, 1, ifelse(site2 == 1, 2, 3)))) %>% 
    
    # remove missing values
    # in case of unimputed data this allows further deduction of IPW for selection into the subset
    drop_na(f.25781.2.0, f.25020.2.0, f.25019.2.0, f.25010.2.0, f.25008.2.0, f.25006.2.0, 
            degree3, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, 
            pc11, pc12, pc13, pc14, pc15, pc16, pc17, pc18, pc19, pc20, 
            sex, age, edu_new, marital_status, tdi_bin, Sprs_tert, cnt1, cwwealth_inc,
            cwlife2, depress2,
            smok, diet.c, regpa2, modalc, employment_status,
            avg_hhl_income_b_tax, score_accommodation_type_bin, score_accommodation_asset_bin, score_vehicles_bin)
  
  
  # --------- create site-specific subsets --------- #
  
  ds_prep1_site1 = ds_prep1 %>% filter(filter == 1)
  ds_prep1_site2 = ds_prep1 %>% filter(filter == 2)
  ds_prep1_site3 = ds_prep1 %>% filter(filter == 3)
  
  
  # --------- preprocessing 2 --------- #
  
  ds_prep2 = ds_prep1 %>% 
    
    ## set SIMPLE and basic deconfounding variables
    mutate(
      
      ## WMH burden normalization
      f.25781.2.0 = log(f.25781.2.0),
      
      ## IDP outlier removal
      f.25010.2.0 = ifelse(f.25010.2.0 > quantile(f.25010.2.0, .75, na.rm = T) + 1.5*IQR(f.25010.2.0, na.rm = T) | f.25010.2.0 < quantile(f.25010.2.0, .25, na.rm = T) - 1.5*IQR(f.25010.2.0, na.rm = T), NA, f.25010.2.0),
      f.25008.2.0 = ifelse(f.25008.2.0 > quantile(f.25008.2.0, .75, na.rm = T) + 1.5*IQR(f.25008.2.0, na.rm = T) | f.25008.2.0 < quantile(f.25008.2.0, .25, na.rm = T) - 1.5*IQR(f.25008.2.0, na.rm = T), NA, f.25008.2.0),
      f.25006.2.0 = ifelse(f.25006.2.0 > quantile(f.25006.2.0, .75, na.rm = T) + 1.5*IQR(f.25006.2.0, na.rm = T) | f.25006.2.0 < quantile(f.25006.2.0, .25, na.rm = T) - 1.5*IQR(f.25006.2.0, na.rm = T), NA, f.25006.2.0),
      f.25019.2.0 = ifelse(f.25019.2.0 > quantile(f.25019.2.0, .75, na.rm = T) + 1.5*IQR(f.25019.2.0, na.rm = T) | f.25019.2.0 < quantile(f.25019.2.0, .25, na.rm = T) - 1.5*IQR(f.25019.2.0, na.rm = T), NA, f.25019.2.0),
      f.25020.2.0 = ifelse(f.25020.2.0 > quantile(f.25020.2.0, .75, na.rm = T) + 1.5*IQR(f.25020.2.0, na.rm = T) | f.25020.2.0 < quantile(f.25020.2.0, .25, na.rm = T) - 1.5*IQR(f.25020.2.0, na.rm = T), NA, f.25020.2.0),
      f.25781.2.0 = ifelse(f.25781.2.0 > quantile(f.25781.2.0, .75, na.rm = T) + 1.5*IQR(f.25781.2.0, na.rm = T) | f.25781.2.0 < quantile(f.25781.2.0, .25, na.rm = T) - 1.5*IQR(f.25781.2.0, na.rm = T), NA, f.25781.2.0),
      # to inspect outlier candidates
      #outliers = ds_prep2 %>% select(contains("f.25"))
      #outliers[!complete.cases(test),]
      
      ## date
      # split per site, 0-padding (global standardizing; otherwise median = 0 after padding)
      date_site1 = ifelse(filter == 1, difftime(f.53.2.0, min(ds_prep1_site1$f.53.2.0, na.rm = T), units = "days") %>% as.numeric(), 0),
      date_site2 = ifelse(filter == 2, difftime(f.53.2.0, min(ds_prep1_site2$f.53.2.0, na.rm = T), units = "days") %>% as.numeric(), 0),
      date_site3 = ifelse(filter == 3, difftime(f.53.2.0, min(ds_prep1_site3$f.53.2.0, na.rm = T), units = "days") %>% as.numeric(), 0),
      # merge for global standardization 1: by MAD (note: mad function already applies constant 1.48 for asymptotically normal consistency)
      date = date_site1 + date_site2 + date_site3,
      md_date = median(date),
      mad_date = mad(date),
      date_std = (date - md_date)/mad_date,
      # split per site, 0-padding
      date_std_site1 = ifelse(filter == 1, date_std, 0),
      date_std_site2 = ifelse(filter == 2, date_std, 0),
      date_std_site3 = ifelse(filter == 3, date_std, 0),
      # remove outliers
      date_std_clean_site1 = ifelse(date_std_site1 > 8 | date_std_site1 < -8, median(date_std_site1), date_std_site1),
      date_std_clean_site2 = ifelse(date_std_site2 > 8 | date_std_site2 < -8, median(date_std_site2), date_std_site2),
      date_std_clean_site3 = ifelse(date_std_site3 > 8 | date_std_site3 < -8, median(date_std_site3), date_std_site3),
      # standardize 2: by STD
      date_std2_clean_site1 = date_std_clean_site1 %>% standardize(robust = FALSE),
      date_std2_clean_site2 = date_std_clean_site2 %>% standardize(robust = FALSE),
      date_std2_clean_site3 = date_std_clean_site3 %>% standardize(robust = FALSE),
      
      ## date squared
      date_site1_squared = date_std2_clean_site1^2,
      date_site2_squared = date_std2_clean_site2^2,
      date_site3_squared = date_std2_clean_site3^2,
      
      # variables below this point also need a version that is not site-specific for basic deconfounding
      
      ## age
      # standardize 1: by MAD (note: mad function already applies constant 1.48 for asymptotically normal consistancy)
      md_age = median(age_at_scan),
      mad_age = mad(age_at_scan),
      age_std = (age_at_scan - md_age)/mad_age,
      # split per site, 0-padding
      age_std_site1 = ifelse(filter == 1, age_std, 0),
      age_std_site2 = ifelse(filter == 2, age_std, 0),
      age_std_site3 = ifelse(filter == 3, age_std, 0),
      # remove outliers
      age_std_clean = ifelse(age_std > 8 | age_std < -8, median(age_std), age_std),
      age_std_clean_site1 = ifelse(age_std_site1 > 8 | age_std_site1 < -8, median(age_std_site1), age_std_site1),
      age_std_clean_site2 = ifelse(age_std_site2 > 8 | age_std_site2 < -8, median(age_std_site2), age_std_site2),
      age_std_clean_site3 = ifelse(age_std_site3 > 8 | age_std_site3 < -8, median(age_std_site3), age_std_site3),
      # standardize 2: by STD
      age_std2_clean = age_std_clean %>% standardize(robust = FALSE),
      age_std2_clean_site1 = age_std_clean_site1 %>% standardize(robust = FALSE),
      age_std2_clean_site2 = age_std_clean_site2 %>% standardize(robust = FALSE),
      age_std2_clean_site3 = age_std_clean_site3 %>% standardize(robust = FALSE),
      
      ## age squared
      age_site1_squared = age_std2_clean_site1^2, 
      age_site2_squared = age_std2_clean_site2^2,
      age_site3_squared = age_std2_clean_site3^2,
      
      ## sex
      sex = ifelse(sex == 1, 1, -1),
      sex1 = (ifelse(filter == 1, sex, 0)),
      sex2 = (ifelse(filter == 2, sex, 0)),
      sex3 = (ifelse(filter == 3, sex, 0)),
      
      ## head size
      # standardize 1: by MAD (note: mad function already applies constant 1.48 for asymptotically normal consistancy)
      md_head = median(f.25000.2.0),
      mad_head = mad(f.25000.2.0),
      head_std = (f.25000.2.0 - md_head)/mad_head,
      # split per site, 0-padding
      head_std_site1 = ifelse(filter == 1, head_std, 0),
      head_std_site2 = ifelse(filter == 2, head_std, 0),
      head_std_site3 = ifelse(filter == 3, head_std, 0),
      # remove outliers
      head_std_clean = ifelse(head_std > 8 | head_std < -8, median(head_std), head_std),
      head_std_clean_site1 = ifelse(head_std_site1 > 8 | head_std_site1 < -8, median(head_std_site1), head_std_site1),
      head_std_clean_site2 = ifelse(head_std_site2 > 8 | head_std_site2 < -8, median(head_std_site2), head_std_site2),
      head_std_clean_site3 = ifelse(head_std_site3 > 8 | head_std_site3 < -8, median(head_std_site3), head_std_site3),
      # standardize 2: by STD
      head_std2_clean = head_std_clean %>% standardize(robust = FALSE),
      head_std2_clean_site1 = head_std_clean_site1 %>% standardize(robust = FALSE),
      head_std2_clean_site2 = head_std_clean_site2 %>% standardize(robust = FALSE),
      head_std2_clean_site3 = head_std_clean_site3 %>% standardize(robust = FALSE))
  
  return(ds_prep2)
} 
set_iptw_img_selection = function(ds_SIMPLE, i){
  ### provides weighting for selection into the imaging subset
  ### calculates iptw based on full set of covariates, main exposures, duryears and dementia incidence in complete data
  ### requires:
  ### ds_SIMPLE: data frame resulting from set_ds_simple()
  ### i: numeric indicating which element from data_ls is used, i.e. which multiply imputed data set
  
  # set current complete data and exclude missings in case of unimputed data
  # this allows complete data to match simple ds lines 
  complete_temp = as.data.frame(data_ls[i]) %>% 
    drop_na(degree3, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, 
          pc11, pc12, pc13, pc14, pc15, pc16, pc17, pc18, pc19, pc20, 
          sex, age, edu_new, marital_status, tdi_bin, Sprs_tert, cnt1, cwwealth_inc,
          cwlife2, depress2, smok, diet.c, regpa2, modalc, employment_status,
          avg_hhl_income_b_tax, score_accommodation_type_bin, score_accommodation_asset_bin, score_vehicles_bin) 
  
  # create dummy indicating selection into imaging subset
  dummy_img = as.numeric(complete_temp$f.eid %in% ds_SIMPLE$f.eid)
  
  # add dummy to current complete data
  complete_temp$dummy_img = dummy_img 
  
  # run log model in current complete data 
  # predict probability to be present in imaging subset
  # infer iptw according to (presence/predicted probability) + ((1-presence)/(1-predicted probability))
  # adapted from: https://www.r-bloggers.com/2020/12/inverse-probability-treatment-weighting/
  # adapted from: https://www.andrewheiss.com/blog/2020/12/01/ipw-binary-continuous/
  # reference for weight truncation and stabilization
  #   Cole SR, Hernan MA. Constructing inverse probability weights for marginal structural models.American Journal ofEpidemiology2008;168(6):656–664.17. 
  #   Lee BK, Lessler J, Stuart EA. Weight trimming and propensity score weighting.PLoS One2011;6(3):e18174. DOI:10.1371/journal.pone.0018174.
  #   https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4351790/
  weight <- ipwpoint(exposure = dummy_img, family = "binomial", link = "logit",
                     denominator =~ degree3 + 
                       pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + 
                       pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + 
                       sex + age + edu_new + marital_status + employment_status + 
                       smok + diet.c + regpa2 + modalc + depress2 +
                       tdi_bin + Sprs_tert + cnt1 + avg_hhl_income_b_tax + score_accommodation_type_bin + score_accommodation_asset_bin + score_vehicles_bin, 
                     data = complete_temp,
                     numerator =~1) 
  
  # add weights to current complete data
  complete_temp$weight = weight$ipw.weights
  
  # inspect weighting success
  #ipwplot(complete_temp$weight)
  #mean(complete_temp$weight)
  #psmod = glm(dummy_img ~ degree3 + 
  #              pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + 
  #              pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + 
  #              sex + age + edu_new + marital_status + employment_status + 
  #              smok + diet.c + regpa2 + modalc + depress2 +
  #              tdi + Sprs + cnt1 + avg_hhl_income_b_tax + score_accommodation_type_bin + score_accommodation_asset_bin + score_vehicles_bin, 
  #            data = complete_temp, family="binomial")
  #complete_temp$ps = predict(psmod, type = "response")
  #cobalt::bal.plot(dummy_img ~ degree3 + 
  #                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + 
  #                   pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + 
  #                   sex + age + edu_new + marital_status + employment_status + 
  #                   smok + diet.c + regpa2 + modalc + depress2 +
  #                   tdi + Sprs + cnt1 + avg_hhl_income_b_tax + score_accommodation_type_bin + score_accommodation_asset_bin + score_vehicles_bin + ps, 
  #                 data=complete_temp, 
  #                 var.name = "ps", 
  #                 weights = complete_temp$weight,
  #                 which = "both",
  #                 type = "histogram", 
  #                 mirror = TRUE,
  #                 colors = c("red","blue"))
  
  # add weights to img data
  ds_SIMPLE = ds_SIMPLE %>% left_join(complete_temp %>% select(f.eid, weight))
  
  return(ds_SIMPLE)
}
print_2step_reg_results = function(variable, data_ls){
  ### performs two step regression and prints (pooled) results for main IVs with basic and SIMPLE deconfounding applied
  ### requires:
  ### variable: a string indicating which IDP is to be used as DV
  ### data_ls: a list of data sets (i.e. consisting of 5 imputed data sets or 1 complete case data set)
  
  
  # --------- initialization --------- #
  
  model_count = 0
  res_basic = list()
  res_SIMPLE = list()
  
  
  # --------- perform two-step regression across provided data list --------- #
  
  for(i in 1:length(data_ls)){
    
    ## update model count
    model_count = model_count + 1
    
    ## set ds containing prepped deconfounding variables
    ds_SIMPLE = set_ds_simple(i = i, data_ls = data_ls) 
    
    ## drop missings due to outlier removal in dv
    ds_SIMPLE = ds_SIMPLE %>% drop_na(!!sym(variable))
    
    ## infer IPTW weights for selection into imaging subset
    # append weights
    ds_SIMPLE = set_iptw_img_selection(ds_SIMPLE = ds_SIMPLE, i = i)
    
    ## basic deconfounding: age, sex, age*sex, head size, site
    # set survey design weights for regression 1
    # if first regression should be weighted 
    # clus1 <- svydesign(id =~ 1, weights =~ weight, data = ds_SIMPLE)
    # res_dconf1 <- svyglm(as.formula(paste(variable, "sex * age_std2_clean + head_std2_clean + site1 + site2", sep = " ~ ")), design = clus1, family = gaussian)
    # regression 1
    res_dconf1 = lm(as.formula(paste(variable, "-1 + sex * age_std2_clean + head_std2_clean + site1 + site2", sep = " ~ ")), data = ds_SIMPLE)
    # extract residuals
    ds_SIMPLE$residuals1 = res_dconf1$residuals %>% scale()
    # set survey design weights for regression 2
    clus1 <- svydesign(id =~ 1, weights =~ weight, data = ds_SIMPLE)
    # regression 2
    # if second regression should not be weighted res_temp1 = lm(residuals1 ~ degree3 + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + cnt1 + edu_new + marital_status + cwlife2 + depress2 + Individual_Deprivation + Area_Deprivation + Genetic_Risk, data = ds_SIMPLE)
    res_temp1 <- svyglm(residuals1 ~ degree3 + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + cnt1 + edu_new + marital_status + cwlife2 + depress2 + Individual_Deprivation + Area_Deprivation + Genetic_Risk, design = clus1, family = gaussian)
    # append results
    res_basic[[model_count]] = res_temp1 
    
    ## SIMPLE deconfounding: Age (1 per site), Age squared (1 per site), Sex (1 per site), Age * Sex (1 per site), Head size (1 per site), Site (2 variables), Date (1 per site), Date squared (1 per site)
    # regression 1
    res_dconf2 = lm(as.formula(paste(variable, "-1 + age_std2_clean_site1 * sex1 + age_std2_clean_site2 * sex2 + age_std2_clean_site3 * sex3 + age_site1_squared + age_site2_squared + age_site3_squared + site1 + site2 + head_std2_clean_site1 + head_std2_clean_site2 + head_std2_clean_site3 + date_std2_clean_site1 + date_std2_clean_site2 + date_std2_clean_site3 + date_site1_squared + date_site2_squared + date_site3_squared", sep = " ~ ")), data = ds_SIMPLE)
    # extract residuals
    ds_SIMPLE$residuals2 = res_dconf2$residuals %>% scale()
    # set survey design weights for regression 2
    clus2 <- svydesign(id =~ 1, weights =~ weight, data = ds_SIMPLE)
    # regression 2
    # if second regression should not be weighted res_temp2 = lm(residuals2 ~ degree3 + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + cnt1 + edu_new + marital_status + cwlife2 + depress2 + Individual_Deprivation + Area_Deprivation + Genetic_Risk, data = ds_SIMPLE)
    res_temp2 = svyglm(residuals2 ~ degree3 + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + cnt1 + edu_new + marital_status + cwlife2 + depress2 + Individual_Deprivation + Area_Deprivation + Genetic_Risk, design = clus2, family = gaussian)
    # append results
    res_SIMPLE[[model_count]] = res_temp2
  }
  
  
  # --------- pool results in case data list contains 5 imputed data sets --------- #
  
  if(length(data_ls) > 1){
    res_basic_pooled = pool(as.mira(res_basic)) 
    res_SIMPLE_pooled = pool(as.mira(res_SIMPLE))
  }
  if(length(data_ls) == 1){
    res_basic_pooled = res_basic[[1]] 
    res_SIMPLE_pooled = res_SIMPLE[[1]]
  }
  
  
  # --------- neat printing --------- #
  
  kables(
    list(kable(tidy(res_basic_pooled, conf.int = T) %>% 
                 filter(grepl("Area", term) | grepl("Individual", term)) %>% 
                 mutate(term = str_replace(term, "Individual_Deprivation", "Individual Deprivation "), 
                        term = str_replace(term, "Area_Deprivation", "Area Deprivation "), 
                        `Coefficient (95% CI)`= paste0(sprintf("%.2f", round(estimate, 3)), " (", sprintf("%.2f", round(conf.low, 3)), "-", sprintf("%.2f", round(conf.high, 3)), ")"), 
                        SE = sprintf("%.2f", round(std.error, 3)),
                        `P value` = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 5))))) %>% 
                 dplyr::select(term, `Coefficient (95% CI)`, SE, `P value`), digits = 4),
         kable(tidy(res_SIMPLE_pooled, conf.int = T) %>% 
                 filter(grepl("Area", term) | grepl("Individual", term)) %>% 
                 mutate(term = str_replace(term, "Individual_Deprivation", "Individual Deprivation "), 
                        term = str_replace(term, "Area_Deprivation", "Area Deprivation "), 
                        `Coefficient (95% CI)`= paste0(sprintf("%.2f", round(estimate, 3)), " (", sprintf("%.2f", round(conf.low, 3)), "-", sprintf("%.2f", round(conf.high, 3)), ")"), 
                        SE = sprintf("%.2f", round(std.error, 3)), 
                        `P value` = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 5))))) %>% 
                 dplyr::select(term, `Coefficient (95% CI)`, SE, `P value`), digits = 4)), 
    caption = paste0("Variable = ", variable, " (Total No. = ", ds_SIMPLE %>% nrow(), ")"))
  
  #return(res_SIMPLE)
} 

#### demographics ####
plot_n_and_pct = function(variable, value){
  ### calculates and prints total N and percentage of categorical demo characteristics
  ### requires:
  ### variable: string indicating characteristic of interest
  ### value: string indicating which value should be counted
  
  
  N = n()
  n = sum(variable == value, na.rm = T)
  pct = sprintf("%.1f", round(n/N*100, 2))
  
  # divide total n by number of imputed data files, round it and let go of decimals
  res = paste0(as.character(sprintf("%d", round(n/length(data_ls), 0))), " (", as.character(pct), ")")
  
  return(res)
}

#### tables and plots ####
print_table1 = function(rbind_data){
  ### prints table 1 including counts and percentages or means and standard deviations
  ### requires:
  ### rbind_data: data frame in long format
  
  rbind_data %>% 
    
    ## incident dementia cases
    mutate(inc.dem = 
             ifelse(inc.dem2 == 0, 
                    paste0("No Incident Dementia (N = ", as.character(sum(inc.dem2 == 0)/length(data_ls)), ")"), 
                    ifelse(inc.dem2 == 1, 
                           paste0("Incident Dementia (N = ", as.character(sum(inc.dem2 == 1)/length(data_ls)), ")"), 
                           0))) %>%  
      
      # --------- calculate descriptives grouped by incident dementia --------- #
      
      ## grouping
      group_by(inc.dem) %>% 
      
      ## summary statistics
      summarise(`Age (SD)` = paste0(as.character(round(mean(age), 1)), " (", as.character(round(sd(age), 1)), ")"),
                
                Sex = "",
                Female = plot_n_and_pct(sex, 0),
                Male = plot_n_and_pct(sex, 1),
                
                Education = "",
                High = plot_n_and_pct(edu_new, "high edu"),
                Medium = plot_n_and_pct(edu_new, "medium edu"),
                Low = plot_n_and_pct(edu_new, "low edu"),
                Other = plot_n_and_pct(edu_new, "prefer not to answer edu / none of above"),
                
                `Married or in a relationship` = plot_n_and_pct(marital_status, "married / relationship"),
                
                `Healthy Lifestyle categories` = "", 
                `Favorable` = plot_n_and_pct(cwlife2, "favorable ls"),
                `Intermediate` = plot_n_and_pct(cwlife2, "intermediate ls"),
                `Unfavorable` = plot_n_and_pct(cwlife2, "unfavorable ls"),
                
                `Depressive symptoms in last 2 weeks` = plot_n_and_pct(depress2, "several days, more than half or nearly every day"),
                
                `Individual deprivation quintile` = "",
                `1 (low)` = plot_n_and_pct(cwwealth_inc, "low deprivation"),
                `2-4` = plot_n_and_pct(cwwealth_inc, "intermediate deprivation"),
                `5 (high)` = plot_n_and_pct(cwwealth_inc, "high deprivation"),
                
                `Area deprivation quintile` = "", 
                `1-4 (low-to-moderate)` = plot_n_and_pct(tdi_bin, "low-to-moderate deprivation"),
                `5 (high) ` = plot_n_and_pct(tdi_bin, "high deprivation"),
                
                `Genetic risk category` = "",
                `1 (low) ` = plot_n_and_pct(Sprs_tert, "Q1"),
                `2-4 ` = plot_n_and_pct(Sprs_tert, "Q2-4"),
                `5 (high)  ` = plot_n_and_pct(Sprs_tert, "Q5")) %>% 
      
      ## wide to long (1 row per summary of incident vs no incident dementia)
      pivot_longer(cols = -contains("inc.dem"),
                   names_to = "Characteristic",
                   values_to = "value") %>%
      
      ## long to wide (1 row per summary 1 col for incident vs no incident dementia)
      pivot_wider(names_from = "inc.dem") %>% 
      
      ## neat printing
      kable(align = "l")
}
print_figure1_res_table = function(group, mod){
  ### outputs result tables accompanying figure 1
  ### requires:
  ### group: variable name for which combination results should be printed (either tdi_Sprs or wealth_Sprs)
  ### mod: model including coefficients for chosen combination variable
  
  
  ## choose e.g. first imputed data set for descriptives
  data_ls[1] %>% as.data.frame() %>% 
    
    ## number of dementia cases per person-years
    group_by_at(group) %>% 
    summarise(`Total No. of participants` = n(),
              `No. of dementia cases/person-years` = paste0(as.character(sum(inc.dem2 == 1)), " / ", as.character(as.integer(sum(duryears))))) %>% 
    rename_with(function(x) ifelse(x == group, "Deprivation and Genetic Risk", x)) %>% 
    
    ## join HRs, confidence intervals and p values
    left_join(data.frame(term = "Q1 (low risk) Q1-4 (low-to-moderate deprivation)", 
                         hr = "1 [Reference]", 
                         conf.low = "",
                         conf.high = "",
                         p.value = "") %>% 
                rbind(mod %>%
                        tidy(exponentiate = TRUE, conf.int = T) %>% 
                        transmute(term = str_replace(term, "tdi_Sprs", ""),
                                  term = str_replace(term, "wealth_Sprs", ""),
                                  hr = round(estimate, 3),
                                  conf.low = round(conf.low, 3),
                                  conf.high = round(conf.high, 3),
                                  p.value = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 4))))) %>% 
                        filter(grepl("Q", term)) %>% 
                        arrange(term)) %>% 
                rename(`Deprivation and Genetic Risk` = term,
                       `HR (95% CI)` = hr,
                       `P Value` = p.value,
                       `Lower CI` = conf.low,
                       `Upper CI` = conf.high))
}
print_hr_results_area = function(model, data){
  ### outputs results table including HRs and p values for area deprivation
  ### requires:
  ### model: pooled results resulting from pool()
  ### data: single data set (e.g. the first imputed data file)
  
  
  table = data %>% 
    
    ## filter missings in case of complete data
    filter(is.na(tdi) == F) %>% 
    
    ## rename
    mutate(`Area-Level Socioeconomic Deprivation` = tdi_bin) %>% 
    
    ## grouping
    group_by(`Area-Level Socioeconomic Deprivation`) %>% 
    
    ## compute summary statistics (N, total years of FU and N dementia cases)
    summarise(`Total No.` = n(), 
              n_dem = as.character(sum(inc.dem2 == 1)),
              n_years = as.character(as.integer(sum(duryears))),
              `No. of dementia cases/person-years` = paste0(n_dem, " / ", n_years)) %>% 
    
    ## exclude helper variables
    select(-contains("_")) %>% 
    
    ## tidy results (HRs, p values, CIs) and join according to area deprivation categories
    left_join(model %>%
                tidy(exponentiate = TRUE, conf.int = T) %>% 
                filter(grepl("tdi_bin", term)) %>% 
                mutate(term = str_replace(term, "tdi_bin", ""),
                       term = str_replace(term, "Sprs_tert", "PGR "),
                       estimate = sprintf("%.2f", round(estimate, 3)),
                       conf.low = sprintf("%.2f", round(conf.low, 3)),
                       conf.high = sprintf("%.2f", round(conf.high, 3)),
                       p.value = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 4))))) %>% 
                select(term, estimate, contains("conf"), p.value) %>%
                mutate_if(is.double, as.numeric) %>% 
                mutate_if(is.numeric, as.character) %>% 
                rbind(c("low-to-moderate deprivation", "1 [Reference]", "", "", "")) %>% 
                arrange(term),
              by = c("Area-Level Socioeconomic Deprivation" = "term")) %>% 
    
    ## printing
    mutate(`HR (95% CI)`= paste0(estimate, " (", conf.low, "-", conf.high, ")"),
           `HR (95% CI)`= ifelse(grepl("Reference", `HR (95% CI)`), "1 [Reference]", `HR (95% CI)`), 
           `P value` = p.value) %>% 
    select(-contains(c("conf", "p.", "estimate")))
  
  return(table)
}
print_hr_results_individual = function(model, data, pvalue_for_trend){
  ### outputs results table including HRs and p values for individual deprivation
  ### requires:
  ### model: pooled results resulting from pool()
  ### data: single data set (e.g. the first imputed data file)
  ### pvalue_for_trend: string indicating p value for linear trend of continuous deprivation score
  
  
  table = data %>% 
    
    ## filter missings in case of complete data
    filter(is.na(wwealth) == F) %>% 
    
    ## rename
    mutate(`Individual-Level Socioeconomic Deprivation` = cwwealth_inc) %>%
    
    ## grouping
    group_by(`Individual-Level Socioeconomic Deprivation`) %>% 
    
    ## compute summary statistics (N, total years of FU and N dementia cases)
    summarise(`Total No.` = n(), 
              n_dem = as.character(sum(inc.dem2 == 1)),
              n_years = as.character(as.integer(sum(duryears))),
              `No. of dementia cases/person-years` = paste0(n_dem, " / ", n_years)) %>% 
    
    ## exclude helper variables
    select(-contains("_")) %>% 
    
    ## tidy results (HRs, p values, CIs) and join according to area deprivation categories
    left_join(model %>%
                tidy(exponentiate = TRUE, conf.int = T) %>% 
                filter(grepl("cwwealth_inc", term)) %>% 
                mutate(term = str_replace(term, "Sprs_tert", "PGR "),
                       term = str_replace(term, "cwwealth_inc", ""),
                       estimate = sprintf("%.2f", round(estimate, 3)),
                       conf.low = sprintf("%.2f", round(conf.low, 3)),
                       conf.high = sprintf("%.2f", round(conf.high, 3)),
                       p.value = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 4))))) %>% 
                select(term, estimate, contains("conf"), p.value) %>%
                mutate_if(is.double, as.numeric) %>% 
                mutate_if(is.numeric, as.character) %>% 
                rbind(c("low deprivation", "1 [Reference]", "", "", "")), 
              by = c("Individual-Level Socioeconomic Deprivation" = "term")) %>% 
    
    ## printing
    mutate(`HR (95% CI)`= paste0(estimate, " (", conf.low, "-", conf.high, ")"),
           `HR (95% CI)`=ifelse(grepl("Reference", `HR (95% CI)`), "1 [Reference]", `HR (95% CI)`), 
           # delete annoying (-) from reference group
           `P value` = p.value) %>% 
    select(-contains(c("conf", "p.", "estimate"))) %>% 
    cbind(`P value for trend` = c("", "", pvalue_for_trend))
  
  return(table)
}
model_row_area = function(){
  ### provides area-level socioeconomic deprivation table row indicating model number for easier copying 
  
  
  # count models
  model_count <<- model_count + 1 
  
  # set model row
  model_row = data.frame(starter = "") %>% 
    mutate(`Area-Level Socioeconomic Deprivation` = paste0("Model ", model_count), 
           `Total No.` = "",
           `No. of dementia cases/person-years` = "", 
           `HR (95% CI)` = "", 
           `P value` = "") %>%
    select(-starter)
  
  return(model_row)
}
model_row_individual = function(){
  ### provides individual-level socioeconomic deprivation table row indicating model number for easier copying
  
  
  # count models
  model_count <<- model_count + 1 
  
  # set model row
  model_row = data.frame(starter = "") %>% 
    mutate(`Individual-Level Socioeconomic Deprivation` = paste0("Model ", model_count), 
           `Total No.` = "",
           `No. of dementia cases/person-years` = "", 
           `HR (95% CI)` = "", 
           `P value` = "", 
           `P value for trend` =  "") %>%
    select(-starter)
  
  return(model_row)
}
plot_hazards = function(fit, exclude){
  ### plots hazard ratios on a log10 scale including 95% CIs
  ### requires:
  ### fit: a fit object resulting from coxph (can be also pooled)
  ### exclude: a vector of strings indicating which estimates not to depict in the plot
  
  
  # tidy fit object 
  res = broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>% 
    select(term, estimate, p.value, conf.high, conf.low) %>% 
    add_row(data.frame(term = "", estimate = 10, p.value = 999, conf.high = NA, conf.low = NA), .after = 0) %>%
    add_row(data.frame(term = "Area-Level Deprivation", estimate = 10, p.value = 0, conf.high = NA, conf.low = NA), .after = 1) %>% 
    add_row(data.frame(term = "  Quintile 1 (Lowest) [Reference]", estimate = 1, p.value = 0, conf.high = NA, conf.low = NA), .after = 2)
  
  # exclude estimates as requested
  if(sum(is.na(exclude)) == 0) {for(i in 1:length(exclude)) {res = res %>% filter(grepl(exclude[i], term) == F)}}
  
  ## plot results
  
  # rename for improved readability
  res_table = res %>% 
    mutate(p.value = ifelse(p.value == 0, NA, ifelse(p.value == 999, "P value", ifelse(p.value < .001, "<.001", as.character(sprintf("%.2f", round(p.value, 3))) %>% str_replace("0", "")))),
           term = str_replace_all(term, "`", ""),
           term = str_replace(term, "Area Deprivation", " "),
           term = str_replace(term, "Area.Deprivation", " "),
           term = str_replace(term, "Quintile 5 [(]Highest Deprivation[)]", "Quintile 5 (Highest)"),
           term = factor(term))
  
  level_order = res_table$term %>% str_replace_all("`", "")
  
  # plot
  res_table %>% 
    ggplot(aes(factor(term, levels = rev(level_order)), 
               estimate, 
               label = p.value, 
               ymin = conf.low, 
               ymax = conf.high)) +
    geom_point(pch = 15) +
    geom_errorbar(size = .3, width = .2) +
    scale_y_continuous(trans = log10_trans(), breaks = c(.5, 1, 2, 4), limits = c(.35,8), label = function(x){ifelse(x < 1, x, round(x, 0))}) +
    coord_flip() +
    geom_text(aes(y = 6)) +
    geom_text(aes(label = ifelse(estimate != 1, round(estimate, 2), ""), y = estimate), vjust = -.8) +
    geom_hline(yintercept = 1, lty = "dashed", colour = "black", alpha = .5) + 
    xlab("") +
    ylab("Hazard Ratio") +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(family = "Arial", colour="black", size=10), axis.title.y = element_blank(), axis.text.y = element_text(hjust = 0))
}

#### supplementary ####
print_etable1 = function(res_individual_weights, dat){
  ### prints supplementary table 1 based on cox proportional-hazards regression
  ### based on individual level deprivation score educts
  ### yields total numbers based on first imputed data set
  ### requires:
  ### res_individual_weights: tidied cox model including confidence interval
  ### dat: data frame with raw data
  
  
  ## Total N per group
  n_bin1_income = dat %>% group_by(avg_hhl_income_b_tax) %>% summarise(n = n())
  # fix income not disclosed absence when excluding not disclosed in sensitivity analysis
  if("income not disclosed" %in% n_bin1_income$avg_hhl_income_b_tax == F) n_bin1_income = add_row(n_bin1_income, avg_hhl_income_b_tax = "income not disclosed", n = 0)
  n_bin1_accomtype = dat %>% group_by(score_accommodation_type_bin) %>% summarise(n = n())
  n_bin1_accomasset = dat %>% group_by(score_accommodation_asset_bin) %>% summarise(n = n())
  n_bin1_vehicle = dat %>% group_by(score_vehicles_bin) %>% summarise(n = n())
  
  ## print results table
  res_individual_weights %>% 
    filter(grepl("score", term) | grepl("avg", term)) %>% 
    mutate(`Coefficient (95% CI)`= paste0(sprintf("%.2f", round(estimate, 3)), " (", sprintf("%.2f", round(conf.low, 3)), "-", sprintf("%.2f", round(conf.high, 3)), ")"), 
           SE = sprintf("%.2f", round(std.error, 3)),
           `P value` = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 4)))),
           term = str_replace(term, "avg_hhl_income_b_tax", ""),
           term = str_replace(term, "score_accommodation_type_bin", ""),
           term = str_replace(term, "score_accommodation_asset_bin", ""),
           term = str_replace(term, "score_vehicles_bin", "")) %>%
    dplyr::select(term, `Coefficient (95% CI)`, `P value`) %>% 
    ## init placeholder rows
    add_row(term = "Income", `Coefficient (95% CI)` = "", `P value` = "", .after = 0) %>% 
    add_row(term = "income greater 31,000", `Coefficient (95% CI)` = "0 [Reference]", `P value` = "", .after = 1) %>%
    add_row(term = "Housing Type", `Coefficient (95% CI)` = "", `P value` = "", .after = 5) %>% 
    add_row(term = "house or flat", `Coefficient (95% CI)` = "0 [Reference]", `P value` = "", .after = 6) %>%
    add_row(term = "Home Ownership", `Coefficient (95% CI)` = "", `P value` = "", .after = 8) %>% 
    add_row(term = "own outright", `Coefficient (95% CI)` = "0 [Reference]", `P value` = "", .after = 9) %>%
    add_row(term = "Car Ownership", `Coefficient (95% CI)` = "", `P value` = "", .after = 11) %>% 
    add_row(term = "one or more", `Coefficient (95% CI)` = "0 [Reference]", `P value` = "", .after = 12) %>%
    mutate(Total = c("", n_bin1_income$n, "", n_bin1_accomtype$n, "", n_bin1_accomasset$n, "", n_bin1_vehicle$n)) %>% 
    filter(Total != 0) %>% 
    kable(align = "l", digits = 3)
}
print_etable1213 = function(res_area_complete, combined_groups){
  ### prints supplementary table 12 based on cox proportional-hazards regression
  ### based on combined groups of genetic risk and socioeconomic deprivation
  ### based on complete case data
  ### requires:
  ### res_area_complete: cox model
  ### combined_groups: string indicating area or individual level combinations
  
  
  ## complete case data set for descriptives
  res_table = final_vars_aug %>% 
    
    ## filter missings in case of complete-case data
    drop_na(duryears, inc.dem2, degree3, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, pc11, pc12, pc13, pc14, pc15, pc16, pc17, pc18, pc19, pc20, cnt1, sex, age, edu_new, cwwealth_inc, cwlife2, depress2, tdi_Sprs, marital_status) %>% 
    
    ## number of dementia cases per person-years
    mutate(`Genetic risk and deprivation` = !!sym(combined_groups)) %>% 
    group_by(`Genetic risk and deprivation`) %>% 
    summarise(`Total No. of participants` = n(),
              n_dem = as.character(sum(inc.dem2 == 1)),
              n_years = as.character(as.integer(sum(duryears))),
              `No. of dementia cases/person-years` = paste0(n_dem, " / ", n_years)) %>% 
    
    ## exclude helper variables
    select(-contains("_")) %>% 
    
    ## join HRs, confidence intervals and p values
    left_join(res_area_complete %>%
                tidy(exponentiate = TRUE, conf.int = T) %>% 
                mutate(term = str_replace(term, "tdi_Sprs", ""),
                       term = str_replace(term, "wealth_Sprs", ""),
                       estimate = sprintf("%.2f", round(estimate, 3)),
                       conf.low = sprintf("%.2f", round(conf.low, 3)),
                       conf.high = sprintf("%.2f", round(conf.high, 3)),
                       p.value = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 4))))) %>% 
                filter(grepl("Q", term)) %>% 
                select(term, estimate, contains("conf"), p.value) %>%
                mutate_if(is.double, as.numeric) %>% 
                mutate_if(is.numeric, as.character) %>% 
                rbind(c("Q1 (low risk) Q1-4 (low-to-moderate deprivation)", "1 [Reference]", "", "", "")) %>%
                rbind(c("Q1 (low risk) Q1 (low deprivation)", "1 [Reference]", "", "", "")) %>%
                arrange(term),
              by = c("Genetic risk and deprivation" = "term"))
  
  if(combined_groups == "tdi_Sprs"){
    res_table$`Genetic risk and deprivation` = 
      factor(res_table$`Genetic risk and deprivation`,
             levels = c("Q5 (high risk) Q5 (high deprivation)", 
                        "Q5 (high risk) Q1-4 (low-to-moderate deprivation)", 
                        "Q2-4 (intermediate risk) Q5 (high deprivation)", 
                        "Q2-4 (intermediate risk) Q1-4 (low-to-moderate deprivation)", 
                        "Q1 (low risk) Q5 (high deprivation)", 
                        "Q1 (low risk) Q1-4 (low-to-moderate deprivation)"))
  }
  else{
    res_table$`Genetic risk and deprivation` = 
      factor(res_table$`Genetic risk and deprivation`,
             levels = c("Q5 (high risk) Q5 (high deprivation)", 
                        "Q5 (high risk) Q2-4 (intermediate deprivation)", 
                        "Q5 (high risk) Q1 (low deprivation)", 
                        "Q2-4 (intermediate risk) Q5 (high deprivation)",
                        "Q2-4 (intermediate risk) Q2-4 (intermediate deprivation)", 
                        "Q2-4 (intermediate risk) Q1 (low deprivation)", 
                        "Q1 (low risk) Q5 (high deprivation)", 
                        "Q1 (low risk) Q2-4 (intermediate deprivation)", 
                        "Q1 (low risk) Q1 (low deprivation)"))
  }
  
  
  ## print data table
  res_table %>% 
    transmute(`Genetic risk and deprivation` = `Genetic risk and deprivation`,
              `Total No. of participants` = `Total No. of participants`,
              `No. of dementia cases/person-years` = `No. of dementia cases/person-years`,
              `HR (95% CI) `= paste0(estimate, " (", conf.low, "-", conf.high, ")"),
              `HR (95% CI) `= ifelse(grepl("Reference", `HR (95% CI) `), "1 [Reference]", `HR (95% CI) `),
              `P value` = p.value) %>% 
    kable(align = "l")
}
proportion_individual_deprivation_lifestyle = function(data){
  ### outputs table with proportion of cases according to lifestyle and individual-level socioeconomic deprivation
  ### requires:
  ### data: data frame
  
  
  data %>% 
    mutate(Lifestyle = str_replace(cwlife2, " ls", "")) %>% 
    group_by(Lifestyle, cwwealth_inc) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = cwwealth_inc, values_from = n) %>% 
    mutate(total = `low deprivation` + `intermediate deprivation` + `high deprivation`, 
           `high deprivation` = round(`high deprivation`/total*100, 2),
           `low deprivation` = round(`low deprivation`/total*100, 2),
           `intermediate deprivation` = round(`intermediate deprivation`/total*100, 2))
}
plot_efigure2 = function(path_to_ls_part1.dta){
  ### plots eFigure 2
  ### requires:
  ### path_to_ls_part1.dta: path to outfile of 20211012_lifestyle_part1.do
  
  # read dat containing ls part 1
  dat_ls_part1 = read_dta(path_to_ls_part1.dta) %>% 
    set_names(~ str_to_lower(.) %>% str_replace_all("n_", "f.")) %>% 
    set_names(~ str_to_lower(.) %>% str_replace_all("[_]", ".")) %>% 
    mutate_if(is.labelled, as_factor) %>% 
    # unselect helper vars
    select(-c("breadsli", "cerealbw", "drrwine", "gdrrwine", "drwwine", "gdrwwine", 
              "drbeer", "gdrbeer", "drspirit", "gdrspirit", "drfortwine", 
              "gdrfortwine", "gdrrwine2", "drwwine2", "drrwine2", "drfortwine2", 
              "gdrwwine2", "drbeer2", "gdrbeer2", "drspirit2", "gdrspirit2", 
              "gdrfortwine2", "f.884.0.0", "f.894.0.0", "f.904.0.0", "f.914.0.0", 
              "f.20116.0.0", "f.1309.0.0", "f.1319.0.0", "f.1289.0.0", "f.1299.0.0", 
              "f.1329.0.0", "f.1339.0.0", "f.1349.0.0", "f.1379.0.0", "f.1369.0.0", 
              "f.1389.0.0", "f.1438.0.0", "f.1458.0.0", "f.1448.0.0", "f.1468.0.0", 
              "f.1568.0.0", "f.4407.0.0", "f.1558.0.0", "f.1578.0.0", "f.4418.0.0", 
              "f.1588.0.0", "f.4429.0.0", "f.1598.0.0", "f.4440.0.0", "f.1608.0.0", 
              "f.4451.0.0", "f.54.2.0", "f.53.0.0", "f.53.2.0", "f.25020.2.0", 
              "f.25019.2.0", "f.25006.2.0", "f.25008.2.0", "f.25010.2.0", 
              "f.25781.2.0", "f.25000.2.0"), -f.eid)
  
  # initial mice run
  ini <- mice(dat_ls_part1, maxit = 0, method = "rf")
  
  # repair names
  tidynames_ini = row.names(data.frame(ini$nmis)) %>% 
    str_replace_all("pc", "PC") %>% 
    str_replace_all("age", "Age") %>% 
    str_replace_all("sex", "Sex") %>% 
    str_replace_all("cnt1", "Total No. SNPs") %>% 
    str_replace_all("f.2050.0.0", "Depressive Symptoms in Last Two Weeks") %>% 
    str_replace_all("degree3", "3rd Degree Relatedness") %>% 
    str_replace_all("inc.dem2", "Dementia Incidence") %>% 
    str_replace_all("duryears", "Survival Time") %>% 
    str_replace_all("sprs", "Polygenic Risk") %>% 
    str_replace_all("f.", "") %>% 
    str_replace_all("670.0.0", "Housing Type") %>% 
    str_replace_all("680.0.0", "House Ownership") %>% 
    str_replace_all(".0.0", "") %>% 
    str_replace_all("6141", "Marital Status") %>% 
    str_replace_all("6142", "Employment Status") %>% 
    str_replace_all("738", "Household Income") %>% 
    str_replace_all("728", "Total No. Vehicles") %>% 
    str_replace_all("189", "Townsend Deprivation Index") %>% 
    str_replace_all("709", "Total No. People in Household") %>% 
    str_replace_all("edu.rowmax", "Education") %>% 
    str_replace_all("mpa.days", "Total No. Days of Moderate PA") %>% 
    str_replace_all("mpa.mns", "Duration of Moderate PA on a Typical Day") %>% 
    str_replace_all("vpa.days", "Total No. Days of Vigorous PA") %>% 
    str_replace_all("vpa.mns", "Duration of Vigorous PA on a Typical Day") %>% 
    str_replace_all("smok", "Smoking Status") %>%
    str_replace_all("ruit", "Fresh Fruit Intake") %>%
    str_replace_all("duit", "Dried Fruit Intake") %>% 
    str_replace_all("cveg", "Cooked Vegetables Intake") %>%
    str_replace_all("rveg", "Raw Vegetables Intake") %>%
    str_replace_all("oilsh", "Oily Fish Intake") %>%
    str_replace_all("othsh", "Other Fish Intake") %>%
    str_replace_all("promeat.c", "Processed Meat Intake") %>%
    str_replace_all("beef", "Beef Intake") %>% 
    str_replace_all("lamb", "Lamb Intake") %>% 
    str_replace_all("pork", "Pork Intake") %>% 
    str_replace_all("whbser", "Whole Grain Bread Servings Per Week") %>% 
    str_replace_all("whcerser", "Whole Grain Cereal Servings Per Week") %>%  
    str_replace_all("reser", "Refined Grain Bread Servings Per Week") %>% 
    str_replace_all("recerser", "Refined Grain Cereal Servings Per Week") %>% 
    str_replace_all("whbser", "Whole Grain Bread Servings Per Week") %>% 
    str_replace_all("rwine", "Daily Red Wine Consumption") %>% 
    str_replace_all("wwine", "Daily White Wine Consumption") %>% 
    str_replace_all("beer", "Beer Consumption") %>% 
    str_replace_all("spirit", "Spirit Consumption") %>% 
    str_replace_all("rtwine", "Fortifiedd Wine Consumption")
  
  # create plot
  eFigure2 = data.frame(variable = tidynames_ini, 
                        proportion = ini$nmis / nrow(dat_ls_part1) * 100) %>% 
    ggplot(aes(reorder(variable, proportion), proportion)) +
    geom_col() +
    coord_flip() +
    theme(panel.grid = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_blank(), 
          axis.line.x = element_line(), 
          axis.line.y = element_line(), 
          axis.text = element_text(colour="black", size=10)) + # optional family = "Arial"
    xlab("Variables Used in Multiple Imputation") +
    ylab("Proportion of Missing Values (%)") +
    scale_y_continuous(limits = c(0, 100))
  
  return(eFigure2)
}
plot_efigure3 = function(res_area_x_Sprs_pooled){
  ### plots eFigure 3
  ### requires:
  ### res_area_x_Sprs_pooled: cox model resulting from coxph
  
  # define variables not to plot
  exclude = c("pc", "ls", "depress2", "degree3", "cnt1", "edu_new", "marital_status", "sex", "age", "cwwealth_inc")
  
  # tidy fit object 
  res = tidy(res_area_x_Sprs_pooled, exponentiate = TRUE, conf.int = TRUE) %>% 
    select(term, estimate, p.value, conf.high, conf.low) %>% 
    add_row(data.frame(term = "", estimate = 10, p.value = 999, conf.high = NA, conf.low = NA), .after = 0) %>%
    add_row(data.frame(term = "Genetic Risk", estimate = 10, p.value = 0, conf.high = NA, conf.low = NA), .after = 32) %>% 
    add_row(data.frame(term = " Low [Reference]", estimate = 1, p.value = 0, conf.high = NA, conf.low = NA), .after = 33) %>% 
    add_row(data.frame(term = "Area-Level Deprivation", estimate = 10, p.value = 0, conf.high = NA, conf.low = NA), .after = 39) %>% 
    add_row(data.frame(term = " Low-to-Moderate [Reference]", estimate = 1, p.value = 0, conf.high = NA, conf.low = NA), .after = 40) %>% 
    add_row(data.frame(term = "Interaction Terms (Genetic Risk : Area-Level Deprivation)", estimate = 10, p.value = 0, conf.high = NA, conf.low = NA), .after = 42) %>%
    add_row(data.frame(term = " Low : Low-to-Moderate [Reference]", estimate = 1, p.value = 0, conf.high = NA, conf.low = NA), .after = 43)
  
  # exclude estimates as requested
  if(sum(is.na(exclude)) == 0) {for(i in 1:length(exclude)) {res = res %>% filter(grepl(exclude[i], term) == F)}}
  
  ## plot results
  # rename for improved readability
  res_table = res %>% 
    mutate(p.value = ifelse(p.value == 0, NA, 
                            ifelse(p.value == 999, "P value", 
                                   ifelse(p.value < .001, "<.001", as.character(sprintf("%.2f", round(p.value, 3))) %>% str_replace("0", "")))),
           term = str_replace(term, "Sprs_tert", "."),
           term = str_replace(term, "tdi_bin", ""),
           term = str_replace_all(term, "[.]Q5", " High"),
           term = str_replace_all(term, "Q5", " High "),
           term = str_replace(term, "[.]Q2-4", " Intermediate"),
           term = str_replace(term, "[.]Q1", " Low"),
           term = str_replace(term, "Q1-4", " Low-to-Moderate"))
  
  level_order = res_table$term %>% str_replace_all("`", "")
  
  # plot
  efigure3 = res_table %>% 
    ggplot(aes(factor(term, levels = rev(level_order)), ifelse(estimate != 0, estimate, NA), label = p.value, ymin = conf.low, ymax = conf.high)) +
    geom_point(pch = 15) +
    geom_errorbar(size = .3, width = .2) +
    scale_y_continuous(trans = log10_trans(), breaks = c(.5, 1, 2, 4), limits = c(.35,8), label = function(x){ifelse(x < 1, x, round(x, 0))}) +
    coord_flip() +
    geom_text(aes(y = 6)) +
    geom_text(aes(label = ifelse(estimate != 1, round(estimate, 2), ""), y = estimate), vjust = -.8) +
    geom_hline(yintercept = 1, lty = "dashed", colour = "black", alpha = .5) + 
    xlab("") +
    ylab("Hazard Ratio") +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(colour="black", size=10), axis.title.y = element_blank(), axis.text.y = element_text(hjust = 0)) # optional family = "Arial"
  
  return(efigure3)
}
plot_efigure4 = function(res_individual_x_Sprs_pooled){
  ### plots eFigure 4
  ### requires:
  ### res_individual_x_Sprs_pooled: cox model resulting from coxph
  
  # define variables not to plot
  exclude = c("pc", "ls", "depress2", "degree3", "cnt1", "edu_new", "marital_status", "sex", "age", "tdi_bin")
  
  # tidy fit object 
  res = tidy(res_individual_x_Sprs_pooled, exponentiate = TRUE, conf.int = TRUE) %>% 
    select(term, estimate, p.value, conf.high, conf.low) %>% 
    add_row(data.frame(term = "", estimate = 10, p.value = 999, conf.high = NA, conf.low = NA), .after = 0) %>%
    add_row(data.frame(term = "Genetic Risk", estimate = 10, p.value = 0, conf.high = NA, conf.low = NA), .after = 32) %>% 
    add_row(data.frame(term = " Low [Reference]", estimate = 1, p.value = 0, conf.high = NA, conf.low = NA), .after = 33) %>% 
    add_row(data.frame(term = "Individual-Level Deprivation", estimate = 10, p.value = 0, conf.high = NA, conf.low = NA), .after = 38) %>% 
    add_row(data.frame(term = " Low [Reference] ", estimate = 1, p.value = 0, conf.high = NA, conf.low = NA), .after = 39) %>% 
    add_row(data.frame(term = "Interaction Terms (Genetic Risk: Individual-Level Deprivation)", estimate = 10, p.value = 0, conf.high = NA, conf.low = NA), .after = 42) %>% 
    add_row(data.frame(term = " Low : Low [Reference]", estimate = 1, p.value = 0, conf.high = NA, conf.low = NA), .after = 43)
  
  # exclude estimates as requested
  if(sum(is.na(exclude)) == 0) {for(i in 1:length(exclude)) {res = res %>% filter(grepl(exclude[i], term) == F)}}
  
  
  ## plot results
  # rename for improved readability
  res_table = res %>% 
    mutate(p.value = ifelse(p.value == 0, NA, ifelse(p.value == 999, "P value", ifelse(p.value < .001, "<.001", as.character(sprintf("%.2f", round(p.value, 3))) %>% str_replace("0", "")))),
           term = str_replace(term, "Sprs_tert", "."),
           term = str_replace(term, "cwwealth_inc", ""),
           term = str_replace_all(term, ".Q5", " High "),
           term = str_replace_all(term, "high deprivation", " High  "),
           term = str_replace(term, ".Q2-4", " Intermediate "),
           term = str_replace(term, "intermediate deprivation", " Intermediate  "),
           term = str_replace(term, ".Q1", " Low "))
  
  level_order = res_table$term %>% str_replace_all("`", "")
  
  # plot
  efigure4 = res_table %>% 
    ggplot(aes(factor(term, levels = rev(level_order)), 
               ifelse(estimate != 0, estimate, NA), 
               label = p.value, 
               ymin = conf.low, 
               ymax = conf.high)) +
    geom_point(pch = 15) +
    geom_errorbar(size = .3, width = .2) +
    scale_y_continuous(trans = log10_trans(), breaks = c(.5, 1, 2, 4), limits = c(.35,8), label = function(x){ifelse(x < 1, x, round(x, 0))}) +
    coord_flip() +
    geom_text(aes(y = 6)) +
    geom_text(aes(label = ifelse(estimate != 1, round(estimate, 2), ""), y = estimate), vjust = -.8) +
    geom_hline(yintercept = 1, lty = "dashed", colour = "black", alpha = .5) + 
    xlab("") +
    ylab("Hazard Ratio") +
    theme_bw() +
    theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(colour="black", size=10), axis.title.y = element_blank(), axis.text.y = element_text(hjust = 0)) # optional family = "Arial"
  
  return(efigure4)
}

#### file handling ####
prep_for_stata = function(data){
  ### preps dataframe for STATA and renames variable for interoperatiblity with Lifestyle computation files
  ### requires:
  ### data: dataframe to be written
  
  
  # prep out file
  data_out = data %>%
    
    # recode sex
    mutate(sex = factor(ifelse(sex == "Male", 1, 0))) %>% 
    
    # exchange . with _
    set_names(~ str_to_lower(.) %>% str_replace_all("[.]", "_")) %>% 
    
    # exchange f with n
    set_names(~ str_to_lower(.) %>% str_replace_all("f", "n"))
  
  return(data_out)
}
