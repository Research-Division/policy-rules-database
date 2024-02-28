# This file loads necessary data and projects wage growth by years of experience

# Load wage data 
#load(paste0(getwd(),dir.wagegrowth, "/coeff_all.rdata")) # Estimated wage growth (Current methodology)




# Function to construct career path (for CLIFF Dashboard) 
constructCareerPath.Dashboard<-function(data
                              , baselineOccupation
                              , baselineOccupation.empl_healthcare
                              , educationDuration
                              , hoursPT
                              , choiceOccupation
                              , choiceOccupation.empl_healthcare
                              , yrsofexp.past=0){
  
  # Use defaults for education duration if unspecified
  if(is.na(educationDuration)){
    educationDuration=table.eduattainment$educationDuration[table.eduattainment$occ_title==choiceOccupation]
    }else if(educationDuration == "999"){
    educationDuration=table.eduattainment$educationDuration[table.eduattainment$occ_title==choiceOccupation]*2
  }
  
  # Step 1 (Baseline occupation)
 # data$occ_title[data$year.index == 1]<-baselineOccupation
#  data$empl_healthcare[data$year.index == 1]<-baselineOccupation.empl_healthcare
##  data$hours[data$year.index == 1]<-40
#  data$experience[data$year.index == 1]<-data$year.index[data$year.index == 1]
  
  data$occ_title <- NA
  data$empl_healthcare <- NA
  data$hours <- NA
  data$experience <- NA
  
  # Step 2 (Education + Part-Time at the Baseline occupation)
  data$occ_title[data$year.index >= 1 & data$year.index<=educationDuration]<-baselineOccupation
  data$empl_healthcare[data$year.index >= 1 & data$year.index<=educationDuration]<-baselineOccupation.empl_healthcare
  data$hours[data$year.index >= 1 & data$year.index<=educationDuration]<-hoursPT
  data$experience[data$year.index >= 1 & data$year.index<=educationDuration]<-data$year.index[data$year.index >= 1 & data$year.index<=educationDuration]
  
  # Step 3 (Choice occupation) (decide whether to roll-out years of expereince or not)
  data$occ_title[data$year.index >= educationDuration+1]<-choiceOccupation
  data$empl_healthcare[data$year.index >= educationDuration+1]<-choiceOccupation.empl_healthcare
  data$hours[data$year.index >= educationDuration+1]<-40
  data$experience[data$year.index >= educationDuration+1]<-data$year.index[data$year.index >= educationDuration+1]-(educationDuration)
  
  return(data)
}





# Function to project wages
# Take data as given with pre-specified:
# - hours
# - years of experience
# - MSA
# - BLS occupation titles
function.projectWages<-function(data
                                ,retirement.age=65){
  
  data.wages$occ_title <- tolower(data.wages$occ_title)
  data$occ_title <- tolower(data$occ_title)
  
  # Replace with the state-level if estimates are missing
  data$MSA[paste(data$MSA, data$occ_title, sep="_") %in% table.missingEstimates$area_occ]<-data$stateName[paste(data$MSA, data$occ_title, sep="_") %in% table.missingEstimates$area_occ]
  
  data<-left_join(data, data.wages, by=c("MSA", "occ_title"))
  
  data<-data %>% # Merge on current and past minimum wages
    left_join(table.minWage, by=c("Year","stateName")) %>%
    
    mutate(wagegain=minwage-minwage.old) %>%       # difference between new and old minimum wages
    mutate(phaseout=minwage+wagegain*0.5)   # effect up to 50% of difference
    
  # Assign starting wage
  data<-data %>% 
    mutate(unadjustedWage=exp(intercept+yrsofexp*experience+yrsofexpsqrt*experience^0.5)) %>% 
    
    # Make sure that no one is making less than old minimum wage (otherwise spillovers formula won't work)
    mutate(unadjustedWage=case_when(unadjustedWage < minwage.old ~ minwage.old 
                                  ,unadjustedWage >= minwage.old ~ unadjustedWage)) %>%
    
    # Model spillovers effects
    mutate(income=case_when(unadjustedWage < phaseout & minwage > minwage.old ~ unadjustedWage + wagegain*(phaseout-unadjustedWage)/(phaseout-minwage.old) #what portion of the gain worker is getting (equal to 1 if wages right at the old min wage)
                                  ,unadjustedWage < phaseout & minwage == minwage.old ~ minwage # no spillovers, because no increase in min wage
                                  ,unadjustedWage >= phaseout ~ unadjustedWage))
    
  
  
  # 2. Project wages into the future
  data<-data %>% 
    mutate(income=round(hours*income*52,0)) %>%
    mutate(income=case_when(agePerson1>=retirement.age ~ 0 , TRUE ~ income)) # Adjustment for the retirement age
  

  return(data)
  
}


function.projectWages.old<-function(data
                                ,retirement.age=65){
  
  data.wages$occ_title <- tolower(data.wages$occ_title)
  data$occ_title <- tolower(data$occ_title)
  
  # Replace with the state-level if estimates are missing
  data$MSA[paste(data$MSA, data$occ_title, sep="_") %in% table.missingEstimates$area_occ]<-data$stateName[paste(data$MSA, data$occ_title, sep="_") %in% table.missingEstimates$area_occ]
  
  data<-left_join(data, data.wages, by=c("MSA", "occ_title"))
  
  # Assign starting wage
  data$startingWage<-exp(data$intercept) 
  
  
  # 1. Adjust STARTING WAGE
  
  data<-data %>% # Merge on current and past minimum wages
    left_join(table.minWage, by=c("Year","stateName")) %>%
    
    mutate(wagegain=minwage-minwage.old) %>%       # difference between new and old minimum wages
    mutate(phaseout=minwage+wagegain*0.5) %>%   # effect up to 50% of difference
    
    # Make sure that no one is making less than old minimum wage (otherwise spillovers formula won't work)
    mutate(startingWage=case_when(startingWage < minwage.old ~ minwage.old 
                                  ,startingWage >= minwage.old ~ startingWage)) %>%
    
    # Model spillovers effects
    mutate(startingWage=case_when(startingWage < phaseout & minwage > minwage.old ~ startingWage + wagegain*(phaseout-startingWage)/(phaseout-minwage.old) #what portion of the gain worker is getting (equal to 1 if wages right at the old min wage)
                                  ,startingWage < phaseout & minwage == minwage.old ~ minwage # no spillovers, because no increase in min wage
                                  ,startingWage >= phaseout ~ startingWage))
  
  
  # 2. Project wages into the future
  data<-data %>% 
    mutate(experiencegain=case_when(yrsofexp*experience+yrsofexpsqrt*experience^0.5>=0 ~ yrsofexp*experience+yrsofexpsqrt*experience^0.5
                                    ,yrsofexp*experience+yrsofexpsqrt*experience^0.5<0 ~ 0)) %>% # If this term is negative, wage growth is negative. Force it to 0
    mutate(income=exp(log(startingWage)+experiencegain)) %>% 
    mutate(income=round(hours*income*52,0)) %>%
    mutate(income=case_when(agePerson1>=retirement.age ~ 0 , TRUE ~ income)) # Adjustment for the retirement age
  
  
  return(data)
  
}


# Project wages depending on the parameters
# NOTE: there is no employer health insurance variable created at this step
# Either do separate function or add it later to this function
function.addStep<-function(MSA, occ_title, hours_per_week=40
                  , start_year=2019, end_year=2055, yrsofexp.past=0){
  
  # Intitialize to access years easier
  year<-seq(start_year, end_year, by=1)
  
  temp<-as.data.frame(matrix(year,ncol=1))
  temp<-rename(temp,year=c(1))
  temp$income<-NULL
  
  # Extract coefficients from the spreadsheet with wage growth estimates
  intercept.coef<-data.wages$intercept[data.wages$area_name==MSA & data.wages$occ_title==occ_title]
  yrsofexp.coef<-data.wages$yrsofexp[data.wages$area_name==MSA & data.wages$occ_title==occ_title]
  yrsofexpsqrt.coef<-data.wages$yrsofexpsqrt[data.wages$area_name==MSA & data.wages$occ_title==occ_title]
  
  for(i in 1:length(temp$year)){
    temp$income[i]<-exp(intercept.coef+yrsofexp.coef*(i+yrsofexp.past)+yrsofexpsqrt.coef*(i+yrsofexp.past)^(1/2))
  }
  
  
  temp$income<-round(temp$income*hours_per_week*52,0) # Translate into annual earnings
  
  return(temp$income)
  
}



#################################################################
# CLIFF Personal Planner
#################################################################

# Function to construct career path 
# Introduce roll-out of years of experience here
constructCareerPath.CLIFF<-function(data
                                    , choiceOccupation
                                    , choiceOccupation.empl_healthcare
                                    , choiceExperience = 0
                                    , choiceStartingWage = NA # Not specified - use intercept
                                    , choiceEducationDuration = NA # Not specified - use default
                                    , choiceHoursSchool = 40
                                    , choiceWagesSchool = 0
                                    , eduDurationYears
                                    , eduDurationMonths){
  
  # Use defaults for education duration if unspecified
  if(is.na(choiceEducationDuration)){choiceEducationDuration=table.eduattainment$educationDuration[table.eduattainment$occ_title==choiceOccupation]}
  
  data$wagesSchool<-NA_real_ # non-NA only while in school
  
  #------------------------------------------------------
  # Fill-in baseline occupation (ID=1) 
  # Step 1: Education
  subset<-data$year.index >= 1 & data$year.index <= choiceEducationDuration
  data$occ_title[subset]<-choiceOccupation
  #data$empl_healthcare[subset]<-choiceOccupation.empl_healthcare
  data$empl_healthcare[subset]<-0
  data$hours_inschool[subset]<-choiceHoursSchool*52 # Work hours while in school
  data$hours[subset]<-0                             # Work hours on the real job 
  data$experience[subset]<-data$year.index[subset]
  data$wagesSchool[subset]<-choiceWagesSchool
  
  # Adjust for training duration that is lower than 1 year
  subset<-data$year.index == choiceEducationDuration
  data$hours_inschool[subset]<-choiceHoursSchool*52*(case_when(eduDurationMonths==0~12, TRUE ~ eduDurationMonths)/12) # work part-time while in school
  data$hours[subset]<-40*52*(1-case_when(eduDurationMonths==0~12, TRUE ~ eduDurationMonths)/12) # work the rest of the year full-time 
  
  subset<-data$year.index >= 1 & data$year.index<choiceEducationDuration
  data$hours_inschool[subset]<-choiceHoursSchool*52 # Work hours while in school
  data$hours[subset]<-0                             # Work hours on the real job 
  
  # Step 2 (decide whether to roll-out years of expereince or not)
  subset<-data$year.index>choiceEducationDuration
  data$occ_title[subset]<-choiceOccupation
  data$empl_healthcare[subset]<-choiceOccupation.empl_healthcare
  data$hours[subset]<-40*52 # 40 hours per week, 52 weeks per year
  data$experience[subset]<-data$year.index[subset]-(choiceEducationDuration)
  data$wagesSchool[subset]<-NA_real_
  
  data$startingWage<-choiceStartingWage
  
  data$CareerPath<-choiceOccupation
  
  return(data)
}



# Function to project wages 
# Take data as given with pre-specified:
# - hours
# - years of experience
# - MSA
# - BLS occupation titles
# - NEW: ability to adjust starting wage
function.projectWages.CLIFF<-function(data
                                      , retirement.age=65){
  
  data.wages$occ_title <- tolower(data.wages$occ_title)
  data$occ_title <- tolower(data$occ_title)
  
  # Replace with the state-level if estimates are missing
  data$MSA[paste(data$MSA, data$occ_title, sep="_") %in% table.missingEstimates$area_occ]<-data$stateName[paste(data$MSA, data$occ_title, sep="_") %in% table.missingEstimates$area_occ]
  
  # Merge wage growth estimates
  data<-left_join(data, data.wages, by=c("MSA", "occ_title"))
  
  # Assign starting wage
  data$startingWage[is.na(data$startingWage)]<-exp(data$intercept[is.na(data$startingWage)]) # If starting wage is unspecified use default
  
  
  # 1. Adjust STARTING WAGE

  data<-data %>% # Merge on current and past minimum wages
    left_join(table.minWage, by=c("Year","stateName")) %>%

    mutate(wagegain=minwage-minwage.old) %>%       # difference between new and old minimum wages
    mutate(phaseout=minwage+wagegain*0.5) %>%   # effect up to 50% of difference

    # Make sure that no one is making less than old minimum wage (otherwise spillovers formula won't work)
    mutate(startingWage_adj=case_when(startingWage < minwage.old ~ minwage.old 
                            ,startingWage >= minwage.old ~ startingWage)) %>%
    
    # Model spillovers effects
    mutate(startingWage_adj=case_when(startingWage_adj < phaseout & minwage > minwage.old ~ startingWage_adj + wagegain*(phaseout-startingWage_adj)/(phaseout-minwage.old) #what portion of the gain worker is getting (equal to 1 if wages right at the old min wage)
                            ,startingWage_adj < phaseout & minwage == minwage.old ~ minwage # no spillovers, because no increase in min wage
                            ,startingWage_adj >= phaseout ~ startingWage_adj))
    

   # Project wages into the future
    data<-data %>% 
    mutate(experiencegain=case_when(yrsofexp*experience+yrsofexpsqrt*experience^0.5>=0 ~ yrsofexp*experience+yrsofexpsqrt*experience^0.5
                          ,yrsofexp*experience+yrsofexpsqrt*experience^0.5<0 ~ 0)) %>% # If this term is negative, wage growth is negative. Force it to 0
    mutate(wage_hrly=exp(log(startingWage_adj)+experiencegain)) %>% 
    mutate(income=round(hours*wage_hrly,0)) %>%
    mutate(income=case_when(agePerson1>=retirement.age ~ 0 , TRUE ~ income)) # Adjustment for the retirement age
  
  
  # Adjust wages while in school
  data<-data %>% 
    mutate(income=case_when(!is.na(wagesSchool) ~ hours*wage_hrly + hours_inschool*wagesSchool, TRUE ~ income))
  
  
  return(data)
  
}





