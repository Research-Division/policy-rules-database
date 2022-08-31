#-------------------------------------------------------------
# FUNCTIONS TO CALCULATE DEFAULT EXPENSES FOR A FAMILY
#-------------------------------------------------------------
# SOURCES:
# Childcare: UW SSS, Childaware
# Housing: HUD
# Healthcare: UW SSS, MEPS
# Miscalleneous: UW SSS, Imputed
# Transportation: UW SSS, Imputed
# Food: UW SSS, USDA
# School Meals: USDA
# Utility: CEX


# Uncomment the following lines only if using the file in isolation
#set wd
#setwd("C:/Users/f1extm00/Dropbox")
#setwd("C:/Users/f1ixi02/Dropbox (ATL-FRB)")
#setwd("C:/Users/ellie/Dropbox")

#load directories
#source(paste0(getwd(),"/WorkForceDevProj/directories.R"),local=TRUE)

#load libraries
#source(paste0(getwd(), dir.benefitsexpenses,"/programs/libraries.R"), local=TRUE) # Load required packages


#Load data (created in create_expense_data.R)
#load(paste0(getwd(), dir.benefitsexpenses,"/Database/tables.rdata"))
#load(paste0(getwd(),dir.benefitsexpenses,"/Database/expenses.rdata"))
#load(paste0(getwd(), dir.benefitsexpenses,"/Database/parameters.defaults.rdata"))




###################################################
###################################################
# Childcare Expenses
###################################################
###################################################

#----------------------------------------------------------------------
# University of Washington Self-Sufficiency Standard
#----------------------------------------------------------------------
#Currently the CCDF calculations in TFA assume school age kids have year round part-time copays. .
#If we add PT school costs to TFA then update this function accordingly,
#If we switch to using our own copay CCDF function we should change the assumptions 3 of days of care to be consistent with this.
#for school age kids, UW methodology is 182 school days (national avg) & 82 part-time days (summer)

function.childcareExp.UW<-function(data
                                   , ageofPersonvar
                                   , currentyr = 2021
                                   , schoolagesummercare="PT"){
#set summercare=FT for FL experiment (NOTE: in TFA copays for school age kids are computed as if child is in part-time care all year but expenses are part-time care while in school)
  
  # Rename variables if necessary
  colnames(data)[colnames(data)==ageofPersonvar]<-"ageofchild"
  
  data<-left_join(data, exp.childcareData.UW, by=c("stateFIPS", "countyortownName"))
  
  data<-data %>% 
    mutate(exp.childcareChild=0) %>% 
    mutate(careduringsummer=(case_when(`schoolagesummercare`=="PT" ~ 0.5,`schoolagesummercare`=="FT" ~ 1)))
  
  
  # Determine Expense based on Age of Children
  # part-time care is 3 days of full time durint the week. 
  # after school care is assumed to be 1/2 the cost of full tiem care (alice report assumes after school care is roughly 3/8 that of 4 year old FT care)
  data<-data %>% 
    mutate(exp.childcareChild=(case_when(ageofchild %in% c(0:2)~ftdailyrate.infant*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
                                         ageofchild %in% c(3:4)~ftdailyrate.toddler*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
                                         ageofchild %in% c(5:12)~ftdailyrate.schoolage*(0.5*(parameters.defaults$numberofSummerChildcareDays[1]*careduringsummer+parameters.defaults$numberofSchoolDays[1])),
                                         TRUE~0)))
  
  data$exp.childcareChild<-data$exp.childcareChild*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$exp.childcareChild<-round(data$exp.childcareChild,0)
  
  return(data$exp.childcareChild)
}


#---------------------
#ALICE DATA
#---------------------

function.childcareExp.ALICE<-function(data
                                   , ageofPersonvar
                                   , currentyr = 2021
                                   , schoolagesummercare="PT"){
  #set summercare=FT for FL experiment (NOTE: in TFA copays for school age kids are computed as if child is in part-time care all year but expenses are part-time care while in school)
  
  # Rename variables if necessary
  colnames(data)[colnames(data)==ageofPersonvar]<-"ageofchild"
  
  data<-left_join(data, exp.childcareData.ALICE, by=c("stateFIPS", "countyortownName"))
  
  data<-data %>% 
    mutate(exp.childcareChild=0) %>% 
    mutate(careduringsummer=(case_when(`schoolagesummercare`=="PT" ~ 0.5,`schoolagesummercare`=="FT" ~ 1)))
  
  
  # Determine Expense based on Age of Children
  data<-data %>% 
    mutate(exp.childcareChild=(case_when(ageofchild %in% c(0:2)~ftdailyrate.infant*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
                                         ageofchild %in% c(3:4)~ftdailyrate.toddler*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]), #note in the UW data they have only 'preschool' and say its ages 3-5
                                         ageofchild %in% c(5:12)~ftdailyrate.schoolage*(parameters.defaults$numberofSummerChildcareDays[1]*careduringsummer+parameters.defaults$numberofSchoolDays[1]*(1/2)),
                                         TRUE~0)))
  
  
  data$exp.childcareChild<-data$exp.childcareChild*(1+(.021))^(currentyr-data$yearofdata)
  data$exp.childcareChild<-data$exp.childcareChild*(1+(.021-parameters.defaults$inflationrate[1]))^(data$Year-currentyr)
  
  data$exp.childcareChild<-round(data$exp.childcareChild,0)
  
  return(data$exp.childcareChild)
}

#----------------------------------------------------------------------
# Childaware Data (Use if UW costs are missing)
#----------------------------------------------------------------------
function.childcareExp.childaware<-function(data
                                           , ageofPersonvar
                                           , currentyr = 2021
                                           , schoolagesummercare="PT"){
  
  # Rename variables if necessary
  colnames(data)[colnames(data)==ageofPersonvar]<-"ageofchild"

  data<-left_join(data, exp.childcareData.childaware, by=c("stateFIPS"))
  
  data<-data %>% 
    mutate(exp.childcareChild=0) %>% 
    mutate(careduringsummer=(case_when(`schoolagesummercare`=="PT" ~ 0.5,`schoolagesummercare`=="FT" ~ 1)))
  
  
  
  # Determine Expense based on Age of Children
  data<-data %>% 
    mutate(exp.childcareChild=(case_when(ageofchild==0~ftdailyrate.infant*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
                                         ageofchild %in% c(1:3)~ftdailyrate.toddler*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]), #note, i think that toddler had some funkiness in the data
                                         ageofchild %in% c(4:4)~ftdailyrate.preschool*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
                                         ageofchild %in% c(5:12)~ftdailyrate.schoolage*(parameters.defaults$numberofSummerChildcareDays[1]*careduringsummer+parameters.defaults$numberofSchoolDays[1]*(1/2)),
                                         TRUE~0)))

  data$exp.childcareChild<-data$exp.childcareChild*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
   
  data$exp.childcareChild<-round(data$exp.childcareChild,0)
  
  return(data$exp.childcareChild)
}





###################################################
###################################################
# Transportation Expenses
###################################################
###################################################

#----------------------------------------------------------------------
# University of Washington Self-Sufficiency Standard
#----------------------------------------------------------------------
function.transpExp.UW<-function(data
                                , currentyr = 2021){

  
  data<-left_join(data, exp.transportationData.UW, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.transportation<-data$expense.transportation*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.transportation<-round(data$expense.transportation,0)
  
  return(data$expense.transportation)
}


#----------------------------------------------------------------------
# Imputed (Use if UW costs are missing)
#----------------------------------------------------------------------
function.transpExp.imputed<-function(data
                                     , currentyr = 2021){
  
  
  data<-left_join(data, exp.transportationData.imputed, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))

  data$expense.transportation<-data$expense.transportation*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.transportation<-round(data$expense.transportation,0)
  
  return(data$expense.transportation)
}

#----------------------------------------------------------------------
# ALICE DATA
#----------------------------------------------------------------------
function.transpExp.ALICE<-function(data
                                , currentyr = 2021){
  
  data<-left_join(data, exp.transportationData.ALICE, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.transportation<-data$expense.transportation*(1+(.022))^(currentyr-data$yearofdata)
  data$expense.transportation<-data$expense.transportation*(1+(.022-parameters.defaults$inflationrate[1]))^(data$Year-currentyr)
  
  data$expense.transportation<-round(data$expense.transportation,0)
  
  return(data$expense.transportation)
}


#----------------------------------------------------------------------
# EEE Measure
#----------------------------------------------------------------------
function.transpExp.EEE<-function(data
                                   , currentyr = 2021){
  
  data<-left_join(data, exp.transportationData.EEE, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.transportation<-data$expense.transportation*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  #data$expense.transportation<-data$expense.transportation*(1+(.022-parameters.defaults$inflationrate[1]))^(currentyr-data$year)
  
  data$expense.transportation<-round(data$expense.transportation,0)
  
  return(data$expense.transportation)
}



###################################################
###################################################
# MISC Expenses
###################################################
###################################################

#----------------------------------------------------------------------
# University of Washington Self-Sufficiency Standard
#----------------------------------------------------------------------
function.miscExp.UW<-function(data
                              , currentyr = 2021){
  
  data<-left_join(data, exp.miscData.UW, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))

  data$expense.misc<-data$expense.misc*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.misc<-round(data$expense.misc,0)
  
  return(data$expense.misc)
}


#----------------------------------------------------------------------
# Imputed (Use if UW costs are missing)
#----------------------------------------------------------------------
function.miscExp.imputed<-function(data
                                   , currentyr = 2021){
  
  data<-left_join(data, exp.miscData.imputed, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))

  data$expense.misc<-data$expense.misc*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.misc<-round(data$expense.misc,0)
  
  return(data$expense.misc)
}


#----------------------------------------------------------------------
# ALICE DATA
#----------------------------------------------------------------------
function.miscExp.ALICE<-function(data
                              , currentyr = 2021){
  
  data<-left_join(data, exp.miscData.ALICE, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.misc<-data$expense.misc*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.misc<-round(data$expense.misc,0)
  
  return(data$expense.misc)
}

function.techExp.ALICE<-function(data
                                 , currentyr = 2021){
  
  data<-left_join(data, exp.techData.ALICE, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.tech<-data$expense.tech*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.tech<-round(data$expense.tech,0)
  
  return(data$expense.tech)
}


###################################################
###################################################
# Food Expenses
###################################################
###################################################

#----------------------------------------------------------------------
# University of Washington Self-Sufficiency Standard
#----------------------------------------------------------------------
function.foodExp.UW<-function(data
                              , currentyr=2020){
  
  data<-left_join(data, exp.foodData.UW, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.food<-data$expense.food*(1+parameters.defaults$inflationrate[1])^(data$Year-data$yearofdata)
  
  data$expense.food<-round(data$expense.food,0)
  
  return(data$expense.food)
}


#----------------------------------------------------------------------
# USDA (Use if UW costs are missing)
#----------------------------------------------------------------------
function.foodExp.USDA<-function(data
                                , currentyr = 2021){
                 
  data<-left_join(data, exp.foodData.USDA, by=c("AKorHI","famsize"))
  
  data<-data %>% 
    mutate(food1=case_when(agePerson1 %in% c(0:1)~ `Age.1.year`,
                           agePerson1 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson1 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson1 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson1 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson1 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson1 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson1 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson1 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson1 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food2=case_when(agePerson2 %in% c(0:1)~ `Age.1.year`,
                           agePerson2 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson2 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson2 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson2 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson2 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson2 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson2 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson2 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson2 <=71 ~ `Age.71+.years`)) %>%
    
    mutate(food3=case_when(agePerson3 %in% c(0:1)~ `Age.1.year`,
                           agePerson3 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson3 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson3 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson3 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson3 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson3 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson3 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson3 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson3 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food4=case_when(agePerson4 %in% c(0:1)~ `Age.1.year`,
                           agePerson4 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson4 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson4 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson4 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson4 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson4 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson4 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson4 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson4 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food5=case_when(agePerson5 %in% c(0:1)~ `Age.1.year`,
                           agePerson5 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson5 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson5 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson5 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson5 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson5 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson5 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson5 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson5 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food6=case_when(agePerson6 %in% c(0:1)~ `Age.1.year`,
                           agePerson6 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson6 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson6 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson6 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson6 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson6 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson6 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson6 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson6 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food7=case_when(agePerson7 %in% c(0:1)~ `Age.1.year`,
                           agePerson7 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson7 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson7 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson7 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson7 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson7 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson7 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson7 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson7 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food8=case_when(agePerson8 %in% c(0:1)~ `Age.1.year`,
                           agePerson8 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson8 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson8 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson8 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson8 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson8 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson8 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson8 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson8 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food9=case_when(agePerson9 %in% c(0:1)~ `Age.1.year`,
                           agePerson9 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson9 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson9 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson9 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson9 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson9 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson9 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson9 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson9 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food10=case_when(agePerson10 %in% c(0:1)~ `Age.1.year`,
                           agePerson10 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson10 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson10 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson10 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson10 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson10 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson10 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson10 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson10 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food11=case_when(agePerson11 %in% c(0:1)~ `Age.1.year`,
                           agePerson11 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson11 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson11 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson11 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson11 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson11 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson11 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson11 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson11 <=71 ~ `Age.71+.years`)) %>% 
    
    mutate(food12=case_when(agePerson12 %in% c(0:1)~ `Age.1.year`,
                           agePerson12 %in% c(2:3)~ `Age.2-3.years`,
                           agePerson12 %in% c(4:5)~ `Age.4-5.years`,
                           agePerson12 %in% c(6:8)~ `Age.6-8.years`,
                           agePerson12 %in% c(9:11)~ `Age.9-11.years`,
                           agePerson12 %in% c(12:13)~ `Age.12-13.years`,
                           agePerson12 %in% c(14:18)~ `Age.14-18.years`,
                           agePerson12 %in% c(19:50)~ `Age.19-50.years`,
                           agePerson12 %in% c(51:70)~ `Age.51-70.years`, 
                           agePerson12 <=71 ~ `Age.71+.years`))
  
  
     
  data$expense.food<-rowSums(cbind(data$food1,data$food2, data$food3, data$food4, data$food5, data$food6, data$food7, data$food8, data$food9, data$food10, data$food11, data$food12), na.rm = TRUE)
    
  data$expense.food<-data$expense.food*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.food<-round(data$expense.food,0)
  
  return(data$expense.food)
}


#----------------------------------------------------------------------
# ALICE
#----------------------------------------------------------------------
function.foodExp.ALICE<-function(data
                              , currentyr=2020){
  
  data<-left_join(data, exp.foodData.ALICE, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.food<-data$expense.food*(1+(.055))^(currentyr-data$yearofdata)
  data$expense.food<-data$expense.food*(1+(.055-parameters.defaults$inflationrate[1]))^(data$Year-currentyr)
  
  data$expense.food<-round(data$expense.food,0)
  
  return(data$expense.food)
}

###################################################
###################################################
# Healthcare Expenses
###################################################
###################################################

#----------------------------------------------------------------------
# University of Washington Self-Sufficiency Standard
#----------------------------------------------------------------------
function.healthcareExp.UW<-function(data
                                    , currentyr = 2021){
  
  data<-left_join(data, exp.healthcareData.UW, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
 
  data$expense.healthcare<-data$expense.healthcare*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.healthcare<-round(data$expense.healthcare,0)
  
  return(data$expense.healthcare)
}

#----------------------------------------------------------------------
# ALICE
#----------------------------------------------------------------------
function.healthcareExp.ALICE<-function(data
                                    , currentyr = 2021){
  
  data<-left_join(data, exp.healthcareData.ALICE, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.healthcare<-data$expense.healthcare*(1+(.033))^(currentyr-data$yearofdata)
  data$expense.healthcare<-data$expense.healthcare*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-currentyr)
  
  data$expense.healthcare<-round(data$expense.healthcare,0)
  
  return(data$expense.healthcare)
}

#----------------------------------------------------------------------
# MEPS (Use if UW costs are missing)
#----------------------------------------------------------------------
function.healthcareExp.MEPS<-function(data
                                      , famsizevar
                                      , currentyr = 2021){
  
  # Rename variables if necessary
  colnames(data)[colnames(data)==famsizevar]<-"famsize.enrolled"
  colnames(exp.healthcareData.MEPS)[colnames(exp.healthcareData.MEPS)=="famsize"]<-"famsize.enrolled"
  
  data<-left_join(data, exp.healthcareData.MEPS, by=c("stateFIPS", "famsize.enrolled"))
  
  data$expense.healthcare.employer<-data$expense.healthcare.employer*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  #Annualize & round
  
  data$expense.healthcare.employer<-round(data$expense.healthcare.employer,0)*12
  
    return(data$expense.healthcare.employer)
}


#----------------------------------------------------------------------
# Medicaid Costs - calculated as a state government spending per enrollee
#----------------------------------------------------------------------
function.healthcareExp.Medicaid<-function(data
                                          , ageofpersonvar
                                          , currentyr = 2021){

  colnames(data)[colnames(data)==ageofpersonvar]<-"ageofperson"
  
  data<-left_join(data, exp.healthcareData.Medicaid, by=c("stateFIPS"))
  
  data$expense.medicaidAdults<-data$expense.medicaidAdults*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  data$expense.medicaidChildren<-data$expense.medicaidChildren*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  # Assign value of medicaid based on whether person is a kid or an adult
  data<-data %>% 
  mutate(expense.medicaid=case_when(ageofperson >= 19 ~ expense.medicaidAdults
                                  ,ageofperson < 19 ~ expense.medicaidChildren
                                  , TRUE ~ 0))
  
  data$expense.medicaid<-round(data$expense.medicaid,0)
  
  return(data$expense.medicaid)
}



#----------------------------------------------------------------------
# ACA Costs - costs of the Second Lowest Costs Silverplan
#----------------------------------------------------------------------
function.healthcareExp.SLCS<-function(data
                                      , ageofpersonvar
                                          , currentyr = 2021){
  
  colnames(data)[colnames(data)==ageofpersonvar]<-"ageofperson"
  
  data<-left_join(data, exp.healthcareData.healthexchange, by=c("stateFIPS"))
  
  # Transform into annual values and apply age curve
  data<-data %>% 
    mutate(expense.SLCS = 12*case_when(ageofperson<=14 ~ `age.0-14`
                                               ,ageofperson==15 ~ age.15
                                               ,ageofperson==16 ~ age.16
                                               ,ageofperson==17 ~ age.17
                                               ,ageofperson==18 ~ age.18
                                               ,ageofperson==19 ~ age.19
                                               ,ageofperson==20 ~ age.20
                                               ,ageofperson==21 ~ age.21
                                               ,ageofperson==22 ~ age.22
                                               ,ageofperson==23 ~ age.23
                                               ,ageofperson==24 ~ age.24
                                               ,ageofperson==25 ~ age.25
                                               ,ageofperson==26 ~ age.26
                                               ,ageofperson==27 ~ age.27
                                               ,ageofperson==28 ~ age.28
                                               ,ageofperson==29 ~ age.29
                                               ,ageofperson==30 ~ age.30
                                               ,ageofperson==31 ~ age.31
                                               ,ageofperson==32 ~ age.32
                                               ,ageofperson==33 ~ age.33
                                               ,ageofperson==34 ~ age.34
                                               ,ageofperson==35 ~ age.35
                                               ,ageofperson==36 ~ age.36
                                               ,ageofperson==37 ~ age.37
                                               ,ageofperson==38 ~ age.38
                                               ,ageofperson==39 ~ age.39
                                               ,ageofperson==40 ~ age.40
                                               ,ageofperson==41 ~ age.41
                                               ,ageofperson==42 ~ age.42
                                               ,ageofperson==43 ~ age.43
                                               ,ageofperson==44 ~ age.44
                                               ,ageofperson==45 ~ age.45
                                               ,ageofperson==46 ~ age.46
                                               ,ageofperson==47 ~ age.47
                                               ,ageofperson==48 ~ age.48
                                               ,ageofperson==49 ~ age.49
                                               ,ageofperson==50 ~ age.50
                                               ,ageofperson==51 ~ age.51
                                               ,ageofperson==52 ~ age.52
                                               ,ageofperson==53 ~ age.53
                                               ,ageofperson==54 ~ age.54
                                               ,ageofperson==55 ~ age.55
                                               ,ageofperson==56 ~ age.56
                                               ,ageofperson==57 ~ age.57
                                               ,ageofperson==58 ~ age.58
                                               ,ageofperson==59 ~ age.59
                                               ,ageofperson==60 ~ age.60
                                               ,ageofperson==61 ~ age.61
                                               ,ageofperson==62 ~ age.62
                                               ,ageofperson==63 ~ age.63
                                               ,ageofperson>=64 ~ `age.64+`))
  
  data$expense.SLCS<-data$expense.SLCS*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.SLCS<-round(data$expense.SLCS,0)
  
  return(data$expense.SLCS)
}




###################################################
###################################################
# Required Taxes
###################################################
###################################################

#----------------------------------------------------------------------
# University of Washington Self-Sufficiency Standard
#----------------------------------------------------------------------
function.taxesExp.UW<-function(data
                               , currentyr = 2021){
  
  data<-left_join(data, exp.taxesData.UW, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))

  data$expense.taxes<-data$expense.taxes*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.taxes<-round(data$expense.taxes,0)
  
  return(data$expense.taxes)
}

#----------------------------------------------------------------------
# ALICE
#----------------------------------------------------------------------
function.taxesExp.ALICE<-function(data
                               , currentyr = 2021){
  
  data<-left_join(data, exp.taxesData.ALICE, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.taxes<-data$expense.taxes*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.taxes<-round(data$expense.taxes,0)
  
  return(data$expense.taxes)
}


###################################################
###################################################
# Utility Expenses
###################################################
###################################################

#----------------------------------------------------------------------
# CEX
#----------------------------------------------------------------------
function.utilityExp.CEX<-function(data
                                  , currentyr = 2021){
  
  data<-left_join(data, exp.utilityData, by=c("famsize"))

  data$expense.utilities<-data$expense.utilities*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.utilities<-round(data$expense.utilities,0)
  
  return(data$expense.utilities)
}





###################################################
###################################################
# School Meals
###################################################
###################################################

#----------------------------------------------------------------------
# USDA
#----------------------------------------------------------------------
function.schoolmealsExp<-function(data, currentyr = 2021){
  
    #no merging b/c the cost the same for all.
  cost<-exp.schoolmealData$expense.dailyschoolMeals
  yearofdata<-exp.schoolmealData$yearofdata
  
  data <- data %>% 
  mutate(expense.schoolmeals = cost,
         yearofdata = yearofdata)

  data$expense.schoolmeals<-data$expense.schoolmeals*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  data$expense.schoolmeals<-data$expense.schoolmeals*parameters.defaults$numberofSchoolDays[1] #annualize
  data$expense.schoolmeals<-round(data$expense.schoolmeals,2)
  
  return(data$expense.schoolmeals)
}



###################################################
###################################################
# Housing Expenses
###################################################
###################################################

#----------------------------------------------------------------------
# HUD
#----------------------------------------------------------------------
function.housingExp.HUD<-function(data
                                  , currentyr = 2021){
  
  data<-left_join(data, exp.housingData, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.rent<-data$expense.rent*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
  
  data$expense.rent<-round(data$expense.rent,0)
  
  return(data$expense.rent)
}


#-----------------
#ALICE
#-----------------

function.housingExp.ALICE<-function(data
                                   , currentyr = 2021){
  
  data<-left_join(data, exp.housingData.ALICE, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$expense.housing<-data$expense.housing*(1+(.032))^(currentyr-data$yearofdata)
  data$expense.housing<-data$expense.housing*(1+(.032-parameters.defaults$inflationrate[1]))^(data$Year-currentyr)
  
  data$expense.housing<-round(data$expense.housing,0)
  
  return(data$expense.housing)
}

###################################################
###################################################
# Special Medical Expenses for people with disabilities
###################################################
###################################################
function.specialExp<-function(data
                               , ageofPersonvar
                               , disabilityStatus
                               , currentyr = 2021){
  
  # Rename variables if necessary
  colnames(data)[colnames(data)==disabilityStatus]<-"disabled"
  colnames(data)[colnames(data)==ageofPersonvar]<-"ageofPerson"
  
  data$exp.special.disability[data$disabled==1]<-1000 # replace with actual values once integrated
  data$exp.special.disability[data$disabled==0]<-0
  
  
  return(data$exp.special.disability)
}


