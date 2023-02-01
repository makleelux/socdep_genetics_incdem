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
my_packages <- c("tidyverse", 
                 "knitr", 
                 "kableExtra",
                 "survival", 
                 "broom", 
                 "mice", 
                 "scales",
                 "datawizard", 
                 "ggplot2", 
                 "haven", 
                 "epiR", 
                 "ipw", 
                 "survey", 
                 "data.table",
                 "cobalt")
# Extract not installed packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])] 
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)                               


#### packages ####
library(tidyverse)
library(knitr)
library(kableExtra)
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
library(data.table)
library(cobalt)


#### cox modeling with pool ####
coxmodel_then_pool = function(right_hand_formula, ds_list, filter_cond = TRUE){
  ### models cox proportional-hazards regression for individual elements of ds_list
  ### outputs pooled results from individual cox models
  ### requires:
  ### right_hand_formula: string indicating right hand side of the formula required for coxph
  ### ds_list: a list of data sets (i.e. consisting of 5 imputed data sets or 1 complete case data set)
  ### filter_cond: string indicating optional filter condition (default does not apply filtering)
  
  
  ## init
  model_count = 0
  res = list()
  
  ## loop over 5 imputed data sets
  for(i in 1:length(ds_list)){
    model_count = model_count + 1
    
    ## cox models
    res_temp = coxph(as.formula(paste("Surv(duryears, inc.dem2)", right_hand_formula, sep = "~")), 
                     data = ds_list[i] %>% as.data.frame() %>% filter_(filter_cond))
    
    ## append results
    res[[model_count]] = res_temp
  }
  
  ## pool results
  res_pooled = pool(as.mira(res))
  return(res_pooled)
}
get_wwealth_pvalue = function(mod){
  ### outputs p value for cox models with continuous individual-level socioeconomic deprivation score
  ### requires:
  ### mod: cox model including wwealth
  
  
  p.value = mod %>% tidy() %>% filter(term == "wwealth") %>% select(p.value)
  p.value = p.value$p.value
  `P value for trend1` = ifelse(p.value < .001, "<.001", sprintf("%.3f", round(p.value, 4)))
  
  return(`P value for trend1`)
}

#### incidence risk ####
inc_est = function(variable){
  ### outputs results table with incidence rates per 1000 person-years based on the first imputed data set
  ### requires:
  ### variable: vector containing strings of column names used for grouping 
  
  
  epi_dat = final_vars_aug.imp1 %>% 
    select(inc.dem2, duryears, variable) %>% 
    group_by_at(variable) %>% 
    summarise(`No. of Dementia Cases` = sum(inc.dem2 == 1), n_years = sum(duryears)/1000)
  
  inc_temp = epi.conf(
    cbind(epi_dat$`No. of Dementia Cases`, epi_dat$n_years) %>% as.matrix(), 
    ctype = "inc.rate", 
    method = "exact") %>% 
    transmute(`Incidence Rates per 1,000 Person-Years` = est, `CI lower` = lower, `CI upper` = upper)
  
  return(inc_temp)
}
risk_est = function(variable){
  ### outputs results table with absolute risk based on the first imputed data set
  ### requires:
  ### variable: vector containing strings of column names used for grouping 
  
  
  epi_dat = final_vars_aug.imp1 %>% 
    select(inc.dem2, duryears, variable) %>% 
    group_by_at(variable) %>% 
    summarise(`No. of Dementia Cases` = sum(inc.dem2 == 1), `Total No.` = n())
  
  risk_temp = epi.conf(
    cbind(epi_dat$`No. of Dementia Cases`, epi_dat$`Total No.`) %>% as.matrix(), 
    ctype = "inc.risk", 
    method = "exact") %>% 
    transmute(`Absolute Risk` = est*100, `CI Lower` = lower*100, `CI Upper` = upper*100)
  
  return(risk_temp)
  
}

#### imaging ####
set_img_ds = function(i, ds_list){
  ### sets up data set for further analysis
  ### requires:
  ### ds_list: a list of data sets (i.e. consisting of 5 imputed data sets or 1 complete case data set)
  ### i: an integer indexing which data set of the list will be preprocessed
  
  
  # --------- merge bd (imaging) and prepared (complete/imputed) data --------- #
  
  ds = ds_list[i] %>% 
    
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
      Area_Deprivation = factor(case_when(tdi_bin == "high deprivation" ~ "high", tdi_bin == "low-to-moderate deprivation" ~ "low-to-moderate"), levels = c("low-to-moderate", "high")),
      Individual_Deprivation = factor(case_when(cwwealth_inc == "high deprivation" ~ "high", cwwealth_inc == "intermediate deprivation" ~ "intermediate", cwwealth_inc == "low deprivation" ~ "low"), levels = c("low", "intermediate", "high")),
      Genetic_Risk = factor(case_when(Sprs_tert == "Q5" ~ "high", Sprs_tert == "Q2-4" ~ "intermediate", Sprs_tert == "Q1" ~ "low"), levels = c("low", "intermediate", "high")),
      
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
      # standardize 1: by MAD (note: mad function already applies constant 1.48 for asymptotically normal consistency)
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
set_img_ipw = function(img_ds, i){
  ### provides weighting for selection into the imaging subset
  ### calculates iptw based on full set of covariates, main exposures, duryears and dementia incidence in complete data
  ### requires:
  ### img_ds: data frame resulting from set_img_ds()
  ### i: numeric indicating which element from ds_list is used, i.e. which multiply imputed data set
  
  # set current complete data and exclude missings in case of unimputed data
  # this allows complete data to match simple ds lines 
  complete_temp = as.data.frame(ds_list[i]) %>% 
    drop_na(degree3, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, 
          pc11, pc12, pc13, pc14, pc15, pc16, pc17, pc18, pc19, pc20, 
          sex, age, edu_new, marital_status, tdi_bin, Sprs_tert, cnt1, cwwealth_inc,
          cwlife2, depress2, smok, diet.c, regpa2, modalc, employment_status,
          avg_hhl_income_b_tax, score_accommodation_type_bin, score_accommodation_asset_bin, score_vehicles_bin) 
  
  # create dummy indicating selection into imaging subset
  dummy_img = as.numeric(complete_temp$f.eid %in% img_ds$f.eid)
  
  # add dummy to current complete data
  complete_temp$dummy_img = dummy_img 
  
  # ipw
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
  img_ds = img_ds %>% left_join(complete_temp %>% select(f.eid, weight))
  
  return(img_ds)
}
print_2step_reg_results = function(variable, ds_list){
  ### performs two step regression and prints (pooled) results for main IVs with basic and SIMPLE deconfounding applied
  ### requires:
  ### variable: a string indicating which IDP is to be used as DV
  ### ds_list: a list of data sets (i.e. consisting of 5 imputed data sets or 1 complete case data set)
  
  
  # --------- initialization --------- #
  
  model_count = 0
  res_basic = list()
  res_SIMPLE = list()
  
  
  # --------- perform two-step regression across provided data list --------- #
  
  for(i in 1:length(ds_list)){
    
    ## update model count
    model_count = model_count + 1
    
    ## set ds containing prepped deconfounding variables
    img_ds = set_img_ds(i = i, ds_list = ds_list) 
    
    ## drop missings due to outlier removal in dv
    img_ds = img_ds %>% drop_na(!!sym(variable))
    
    ## infer IPTW weights for selection into imaging subset
    # append weights
    img_ds = set_img_ipw(img_ds = img_ds, i = i)
    
    ## basic deconfounding: age, sex, age*sex, head size, site
    # regression 1
    res_dconf1 = lm(as.formula(paste(variable, "-1 + sex * age_std2_clean + head_std2_clean + site1 + site2", sep = " ~ ")), data = img_ds)
    # extract residuals
    img_ds$residuals1 = res_dconf1$residuals %>% scale()
    # set survey design weights for regression 2
    clus1 <- svydesign(id =~ 1, weights =~ weight, data = img_ds)
    # regression 2
    res_temp1 <- svyglm(residuals1 ~ degree3 + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + cnt1 + edu_new + marital_status + cwlife2 + depress2 + Individual_Deprivation + Area_Deprivation + Genetic_Risk, design = clus1, family = gaussian)
    # append results
    res_basic[[model_count]] = res_temp1 
    
    ## SIMPLE deconfounding: Age (1 per site), Age squared (1 per site), Sex (1 per site), Age * Sex (1 per site), Head size (1 per site), Site (2 variables), Date (1 per site), Date squared (1 per site)
    # regression 1
    res_dconf2 = lm(as.formula(paste(variable, "-1 + age_std2_clean_site1 * sex1 + age_std2_clean_site2 * sex2 + age_std2_clean_site3 * sex3 + age_site1_squared + age_site2_squared + age_site3_squared + site1 + site2 + head_std2_clean_site1 + head_std2_clean_site2 + head_std2_clean_site3 + date_std2_clean_site1 + date_std2_clean_site2 + date_std2_clean_site3 + date_site1_squared + date_site2_squared + date_site3_squared", sep = " ~ ")), data = img_ds)
    # extract residuals
    img_ds$residuals2 = res_dconf2$residuals %>% scale()
    # set survey design weights for regression 2
    clus2 <- svydesign(id =~ 1, weights =~ weight, data = img_ds)
    # regression 2
    # if second regression should not be weighted res_temp2 = lm(residuals2 ~ degree3 + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + cnt1 + edu_new + marital_status + cwlife2 + depress2 + Individual_Deprivation + Area_Deprivation + Genetic_Risk, data = img_ds)
    res_temp2 = svyglm(residuals2 ~ degree3 + pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + pc11 + pc12 + pc13 + pc14 + pc15 + pc16 + pc17 + pc18 + pc19 + pc20 + cnt1 + edu_new + marital_status + cwlife2 + depress2 + Individual_Deprivation + Area_Deprivation + Genetic_Risk, design = clus2, family = gaussian)
    # append results
    res_SIMPLE[[model_count]] = res_temp2
  }
  
  
  # --------- pool results in case data list contains 5 imputed data sets --------- #
  
  if(length(ds_list) > 1){
    res_basic_pooled = pool(as.mira(res_basic)) 
    res_SIMPLE_pooled = pool(as.mira(res_SIMPLE))
  }
  if(length(ds_list) == 1){
    res_basic_pooled = res_basic[[1]] 
    res_SIMPLE_pooled = res_SIMPLE[[1]]
  }
  
  
  # --------- neat printing --------- #
  
  res_table = tidy(res_basic_pooled, conf.int = T) %>% 
    filter(grepl("Area", term) | grepl("Individual", term)) %>% 
    mutate(Set = c("Basic Confounders", " ", " "),
           term = str_replace(term, "Individual_Deprivation", "Individual deprivation "), 
           Term = str_replace(term, "Area_Deprivation", "Area deprivation "), 
           `Coefficient (95% CI)`= paste0(sprintf("%.2f", round(estimate, 3)), " (", sprintf("%.2f", round(conf.low, 3)), "-", sprintf("%.2f", round(conf.high, 3)), ")"), 
           SE = sprintf("%.2f", round(std.error, 3)),
           `P value` = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 5))))) %>% 
    dplyr::select(Set, Term, `Coefficient (95% CI)`, SE, `P value`) %>% 
    rbind(tidy(res_SIMPLE_pooled, conf.int = T) %>% 
            filter(grepl("Area", term) | grepl("Individual", term)) %>% 
            mutate(Set = c("SIMPLE Confounders", " ", " "),
                   term = str_replace(term, "Individual_Deprivation", "Individual deprivation "), 
                   Term = str_replace(term, "Area_Deprivation", "Area deprivation "), 
                   `Coefficient (95% CI)`= paste0(sprintf("%.2f", round(estimate, 3)), " (", sprintf("%.2f", round(conf.low, 3)), "-", sprintf("%.2f", round(conf.high, 3)), ")"), 
                   SE = sprintf("%.2f", round(std.error, 3)), 
                   `P value` = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 5))))) %>% 
            dplyr::select(Set, Term, `Coefficient (95% CI)`, SE, `P value`)) 
  
  cat(paste0("Variable = ", variable, " (Total No. = ", img_ds %>% nrow(), ")"))
  
  return(res_table)
} 

#### demographics ####
print_n_and_pct = function(variable, value){
  ### calculates and prints total N and percentage of categorical demo characteristics
  ### requires:
  ### variable: string indicating characteristic of interest
  ### value: string indicating which value should be counted
  
  
  N = n()
  n = sum(variable == value, na.rm = T)
  pct = sprintf("%.1f", round(n/N*100, 2))
  
  # divide total n by number of imputed data files, round it and let go of decimals
  res = paste0(as.character(sprintf("%d", round(n/length(ds_list), 0))), " (", as.character(pct), ")")
  
  return(res)
}

#### tables and plots ####
print_table1 = function(ds_long, ds_list){
  ### prints table 1 including counts and percentages or means and standard deviations
  ### requires:
  ### ds_long: data frame in long format
  
  res_table = ds_long %>% 
    
    ## incident dementia cases
    mutate(inc.dem = 
             ifelse(inc.dem2 == 0, 
                    paste0("No Incident Dementia (N = ", as.character(sum(inc.dem2 == 0)/length(ds_list)), ")"), 
                    ifelse(inc.dem2 == 1, 
                           paste0("Incident Dementia (N = ", as.character(sum(inc.dem2 == 1)/length(ds_list)), ")"), 
                           0))) %>%  
      
      # --------- calculate descriptives grouped by incident dementia --------- #
      
      ## grouping
      group_by(inc.dem) %>% 
      
      ## summary statistics
      summarise(`Age (SD)` = paste0(as.character(round(mean(age), 1)), " (", as.character(round(sd(age), 1)), ")"),
                
                Sex = "",
                Female = print_n_and_pct(sex, 0),
                Male = print_n_and_pct(sex, 1),
                
                Education = "",
                High = print_n_and_pct(edu_new, "high edu"),
                Medium = print_n_and_pct(edu_new, "medium edu"),
                Low = print_n_and_pct(edu_new, "low edu"),
                Other = print_n_and_pct(edu_new, "prefer not to answer edu / none of above"),
                
                `Married or in a relationship` = print_n_and_pct(marital_status, "married / relationship"),
                
                `Healthy Lifestyle categories` = "", 
                `Favorable` = print_n_and_pct(cwlife2, "favorable ls"),
                `Intermediate` = print_n_and_pct(cwlife2, "intermediate ls"),
                `Unfavorable` = print_n_and_pct(cwlife2, "unfavorable ls"),
                
                `Depressive symptoms in last 2 weeks` = print_n_and_pct(depress2, "several days, more than half or nearly every day"),
                
                `Individual deprivation quintile` = "",
                `1 (low)` = print_n_and_pct(cwwealth_inc, "low deprivation"),
                `2-4` = print_n_and_pct(cwwealth_inc, "intermediate deprivation"),
                `5 (high)` = print_n_and_pct(cwwealth_inc, "high deprivation"),
                
                `Area deprivation quintile` = "", 
                `1-4 (low-to-moderate)` = print_n_and_pct(tdi_bin, "low-to-moderate deprivation"),
                `5 (high) ` = print_n_and_pct(tdi_bin, "high deprivation"),
                
                `Genetic risk category` = "",
                `1 (low) ` = print_n_and_pct(Sprs_tert, "Q1"),
                `2-4 ` = print_n_and_pct(Sprs_tert, "Q2-4"),
                `5 (high)  ` = print_n_and_pct(Sprs_tert, "Q5")) %>% 
      
      ## wide to long (1 row per summary of incident vs no incident dementia)
      pivot_longer(cols = -contains("inc.dem"),
                   names_to = "Characteristic",
                   values_to = "value") %>%
      
      ## long to wide (1 row per summary 1 col for incident vs no incident dementia)
      pivot_wider(names_from = "inc.dem") 
  
  return(res_table)
}
print_figure1_restable = function(variable, mod){
  ### outputs result tables accompanying figure 1
  ### requires:
  ### variable: variable name for which combination results should be printed (either tdi_Sprs or wealth_Sprs)
  ### mod: model including coefficients for chosen combination variable
  
  
  ## choose e.g. first imputed data set for descriptives
  ds_list[1] %>% as.data.frame() %>% 
    
    ## number of dementia cases per person-years
    group_by_at(variable) %>% 
    summarise(`Total No. of participants` = n(),
              `No. of dementia cases/person-years` = paste0(as.character(sum(inc.dem2 == 1)), " / ", as.character(as.integer(sum(duryears))))) %>% 
    rename_with(function(x) ifelse(x == variable, "Deprivation and Genetic Risk", x)) %>% 
    
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
                       `Upper CI` = conf.high)) %>% 
    mutate(`Deprivation and Genetic Risk` = str_replace(`Deprivation and Genetic Risk`, "Q1 ", ""),
           `Deprivation and Genetic Risk` = str_replace(`Deprivation and Genetic Risk`, "Q1-4 ", ""),
           `Deprivation and Genetic Risk` = str_replace(`Deprivation and Genetic Risk`, "Q2-4 ", ""),
           `Deprivation and Genetic Risk` = str_replace(`Deprivation and Genetic Risk`, "Q5 ", ""),
           `Deprivation and Genetic Risk` = str_replace(`Deprivation and Genetic Risk`, " Q1", ""),
           `Deprivation and Genetic Risk` = str_replace(`Deprivation and Genetic Risk`, " Q2-4", ""),
           `Deprivation and Genetic Risk` = str_replace(`Deprivation and Genetic Risk`, " Q5", ""),)
}
print_hr_restable_area = function(mod, ds){
  ### outputs results table including HRs and p values for area deprivation
  ### requires:
  ### mod: pooled results resulting from pool()
  ### ds: single ds (e.g. the first imputed ds file)
  
  
  table = ds %>% 
    
    ## filter missings in case of complete ds
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
    left_join(mod %>%
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
print_hr_restable_individual = function(mod, ds, pvalue_for_trend){
  ### outputs results table including HRs and p values for individual deprivation
  ### requires:
  ### mod: pooled results resulting from pool()
  ### ds: single ds (e.g. the first imputed ds file)
  ### pvalue_for_trend: string indicating p value for linear trend of continuous deprivation score
  
  
  table = ds %>% 
    
    ## filter missings in case of complete ds
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
    left_join(mod %>%
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
add_restable_row_area = function(mod_name = NA){
  ### provides area-level socioeconomic deprivation table row indicating model number for easier copying 
  ### optional:
  ### mod_name: specify name, otherwise numbered
  
  # count models
  model_count <<- model_count + 1 
  
  # set model row
  model_row = data.frame(starter = "") %>% 
    mutate(`Area-Level Socioeconomic Deprivation` = paste0("Model ", ifelse(is.na(mod_name), model_count, mod_name)), 
           `Total No.` = "",
           `No. of dementia cases/person-years` = "", 
           `HR (95% CI)` = "", 
           `P value` = "") %>%
    select(-starter)
  
  return(model_row)
}
add_restable_row_individual = function(mod_name = NA){
  ### provides individual-level socioeconomic deprivation table row indicating model number for easier copying
  ### optional:
  ### mod_name: specify name, otherwise numbered
  
  # count models
  model_count <<- model_count + 1 
  
  # set model row
  model_row = data.frame(starter = "") %>% 
    mutate(`Individual-Level Socioeconomic Deprivation` = paste0("Model ", ifelse(is.na(mod_name), model_count, mod_name)), 
           `Total No.` = "",
           `No. of dementia cases/person-years` = "", 
           `HR (95% CI)` = "", 
           `P value` = "", 
           `P value for trend` =  "") %>%
    select(-starter)
  
  return(model_row)
}

#### supplementary ####
print_etable1 = function(mod, ds){
  ### prints supplementary table 1 based on cox proportional-hazards regression
  ### based on individual level deprivation score educts
  ### yields total numbers based on first imputed data set
  ### requires:
  ### mod: tidied cox model including confidence interval
  ### ds: data frame with raw data
  
  
  ## Total N per group
  n_bin1_income = ds %>% group_by(avg_hhl_income_b_tax) %>% summarise(n = n())
  # fix income not disclosed absence when excluding not disclosed in sensitivity analysis
  if("income not disclosed" %in% n_bin1_income$avg_hhl_income_b_tax == F) n_bin1_income = add_row(n_bin1_income, avg_hhl_income_b_tax = "income not disclosed", n = 0)
  n_bin1_accomtype = ds %>% group_by(score_accommodation_type_bin) %>% summarise(n = n())
  n_bin1_accomasset = ds %>% group_by(score_accommodation_asset_bin) %>% summarise(n = n())
  n_bin1_vehicle = ds %>% group_by(score_vehicles_bin) %>% summarise(n = n())
  
  ## print results table
  res_table = mod %>% 
    filter(grepl("score", term) | grepl("avg", term)) %>% 
    mutate(`Coefficient (95% CI)`= paste0(sprintf("%.2f", round(estimate, 3)), " (", sprintf("%.2f", round(conf.low, 3)), "-", sprintf("%.2f", round(conf.high, 3)), ")"), 
           SE = sprintf("%.2f", round(std.error, 3)),
           `P value` = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 4)))),
           term = str_replace(term, "avg_hhl_income_b_tax", ""),
           term = str_replace(term, "score_accommodation_type_bin", ""),
           term = str_replace(term, "score_accommodation_asset_bin", ""),
           Term = str_replace(term, "score_vehicles_bin", "")) %>%
    dplyr::select(Term, `Coefficient (95% CI)`, `P value`) %>% 
    ## init placeholder rows
    add_row(Term = "Income", `Coefficient (95% CI)` = "", `P value` = "", .after = 0) %>% 
    add_row(Term = "income greater 31,000", `Coefficient (95% CI)` = "0 [Reference]", `P value` = "", .after = 1) %>%
    add_row(Term = "Housing Type", `Coefficient (95% CI)` = "", `P value` = "", .after = 5) %>% 
    add_row(Term = "house or flat", `Coefficient (95% CI)` = "0 [Reference]", `P value` = "", .after = 6) %>%
    add_row(Term = "Home Ownership", `Coefficient (95% CI)` = "", `P value` = "", .after = 8) %>% 
    add_row(Term = "own outright", `Coefficient (95% CI)` = "0 [Reference]", `P value` = "", .after = 9) %>%
    add_row(Term = "Car Ownership", `Coefficient (95% CI)` = "", `P value` = "", .after = 11) %>% 
    add_row(Term = "one or more", `Coefficient (95% CI)` = "0 [Reference]", `P value` = "", .after = 12) %>%
    mutate(Total = c("", n_bin1_income$n, "", n_bin1_accomtype$n, "", n_bin1_accomasset$n, "", n_bin1_vehicle$n)) %>% 
    filter(Total != 0)
  
  return(res_table)
}
print_etable1213 = function(mod, variable){
  ### prints supplementary table 12 based on cox proportional-hazards regression
  ### based on combined groups of genetic risk and socioeconomic deprivation
  ### based on complete case data
  ### requires:
  ### mod: cox mod
  ### variable: string indicating area or individual level combinations
  
  
  ## complete case data set for descriptives
  res_table = final_vars_aug %>% 
    
    ## filter missings in case of complete-case data
    drop_na(duryears, inc.dem2, degree3, pc1, pc2, pc3, pc4, pc5, pc6, pc7, pc8, pc9, pc10, pc11, pc12, pc13, pc14, pc15, pc16, pc17, pc18, pc19, pc20, cnt1, sex, age, edu_new, cwwealth_inc, cwlife2, depress2, tdi_Sprs, marital_status) %>% 
    
    ## number of dementia cases per person-years
    mutate(`Genetic risk and deprivation` = !!sym(variable)) %>% 
    group_by(`Genetic risk and deprivation`) %>% 
    summarise(`Total No. of participants` = n(),
              n_dem = as.character(sum(inc.dem2 == 1)),
              n_years = as.character(as.integer(sum(duryears))),
              `No. of dementia cases/person-years` = paste0(n_dem, " / ", n_years)) %>% 
    
    ## exclude helper variables
    select(-contains("_")) %>% 
    
    ## join HRs, confidence intervals and p values
    left_join(mod %>%
                tidy(exponentiate = TRUE, conf.int = T) %>% 
                mutate(term = str_replace(term, "tdi_Sprs", ""),
                       term = str_replace(term, "wealth_Sprs", ""),
                       estimate = sprintf("%.2f", round(estimate, 3)),
                       conf.low = sprintf("%.2f", round(conf.low, 3)),
                       conf.high = sprintf("%.2f", round(conf.high, 3)),
                       p.value = ifelse(p.value < .001, "<.001", as.character(sprintf("%.3f", round(p.value, 4))))) %>% 
                filter(grepl("deprivation)", term)) %>% 
                select(term, estimate, contains("conf"), p.value) %>%
                mutate_if(is.double, as.numeric) %>% 
                mutate_if(is.numeric, as.character) %>% 
                rbind(c("Q1 (low risk) Q1-4 (low-to-moderate deprivation)", "1 [Reference]", "", "", "")) %>%
                rbind(c("Q1 (low risk) Q1 (low deprivation)", "1 [Reference]", "", "", "")) %>%
                arrange(term),
              by = c("Genetic risk and deprivation" = "term"))
  
  if(variable == "tdi_Sprs"){
    res_table = res_table %>% 
      mutate(`Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, "Q1 ", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, "Q1-4 ", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, "Q2-4 ", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, "Q5 ", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, " Q5", ""),
             `Genetic risk and deprivation` = factor(`Genetic risk and deprivation`,
                                                     levels = c("(high risk) (high deprivation)", 
                                                                "(high risk) (low-to-moderate deprivation)", 
                                                                "(intermediate risk) (high deprivation)", 
                                                                "(intermediate risk) (low-to-moderate deprivation)", 
                                                                "(low risk) (high deprivation)", 
                                                                "(low risk) (low-to-moderate deprivation)")))
  }
  else{
    res_table = res_table %>% 
      mutate(`Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, "Q1 ", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, " Q1", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, "Q1-4 ", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, "Q2-4 ", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, " Q2-4", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, "Q5 ", ""),
             `Genetic risk and deprivation` = str_replace(`Genetic risk and deprivation`, " Q5", ""),
             `Genetic risk and deprivation` = factor(`Genetic risk and deprivation`,
                                                     levels = c("(high risk) (high deprivation)", 
                                                                "(high risk) (intermediate deprivation)", 
                                                                "(high risk) (low deprivation)", 
                                                                "(intermediate risk) (high deprivation)",
                                                                "(intermediate risk) (intermediate deprivation)", 
                                                                "(intermediate risk) (low deprivation)", 
                                                                "(low risk) (high deprivation)", 
                                                                "(low risk) (intermediate deprivation)", 
                                                                "(low risk) (low deprivation)")))
  }
  
  
  ## print data table
  res_table = res_table %>% 
    transmute(`Genetic risk and deprivation` = `Genetic risk and deprivation`,
              `Total No. of participants` = `Total No. of participants`,
              `No. of dementia cases/person-years` = `No. of dementia cases/person-years`,
              `HR (95% CI) `= paste0(estimate, " (", conf.low, "-", conf.high, ")"),
              `HR (95% CI) `= ifelse(grepl("Reference", `HR (95% CI) `), "1 [Reference]", `HR (95% CI) `),
              `P value` = p.value)
  
  return(res_table)
}
print_etable18 = function(ds){
  ### outputs table with proportion of cases according to lifestyle and individual-level socioeconomic deprivation
  ### requires:
  ### ds: data frame
  
  
  res_table = ds %>% 
    mutate(Lifestyle = str_replace(cwlife2, "unfavorable ls", "Unfavorable"),
           Lifestyle = str_replace(Lifestyle, "intermediate ls", "Intermediate"),
           Lifestyle = str_replace(Lifestyle, "favorable ls", "Favorable")) %>% 
    group_by(Lifestyle, cwwealth_inc) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = cwwealth_inc, values_from = n) %>% 
    transmute(Total = `low deprivation` + `intermediate deprivation` + `high deprivation`, 
              `High deprivation` = round(`high deprivation`/Total*100, 2),
              `Low deprivation` = round(`low deprivation`/Total*100, 2),
              `Intermediate deprivation` = round(`intermediate deprivation`/Total*100, 2))
  
  return(res_table)
}
print_efigure1 = function(mod, exclude){
  ### plots hazard ratios on a log10 scale including 95% CIs
  ### requires:
  ### mod: a cox mod (can be also pooled)
  ### exclude: a vector of strings indicating which estimates not to depict in the plot
  
  
  # tidy mod object 
  res = broom::tidy(mod, exponentiate = TRUE, conf.int = TRUE) %>% 
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
    theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(colour="black", size=10), axis.title.y = element_blank(), axis.text.y = element_text(hjust = 0))
}
print_efigure3 = function(mod){
  ### plots eFigure 3
  ### requires:
  ### mod: cox mod resulting from coxph
  
  # define variables not to plot
  exclude = c("pc", "ls", "depress2", "degree3", "cnt1", "edu_new", "marital_status", "sex", "age", "cwwealth_inc")
  
  # tidy mod object 
  res = tidy(mod, exponentiate = TRUE, conf.int = TRUE) %>% 
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
    theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(colour="black", size=10), axis.title.y = element_blank(), axis.text.y = element_text(hjust = 0)) 
  
  return(efigure3)
}
print_efigure4 = function(mod){
  ### plots eFigure 4
  ### requires:
  ### mod: cox mod resulting from coxph
  
  # define variables not to plot
  exclude = c("pc", "ls", "depress2", "degree3", "cnt1", "edu_new", "marital_status", "sex", "age", "tdi_bin")
  
  # tidy mod object 
  res = tidy(mod, exponentiate = TRUE, conf.int = TRUE) %>% 
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
    theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(colour="black", size=10), axis.title.y = element_blank(), axis.text.y = element_text(hjust = 0)) 
  
  return(efigure4)
}
