
#---------------------------------------------------------------------------------------------------
# ALICE DATA is used for all expenses excluding Medicaid and ACA expenses which are provided by FRBA
#---------------------------------------------------------------------------------------------------

# Childcare Expenses ----

#note: each expense is inflated per the ALICE index (update every year) & subtract typical inflation to make consistent with wage data
#the 0.022 number comes from "ALICE_essentialindex final" please update as there are new data released!!

function.childcareExp.ALICE<-function(data
                                      , schoolagesummercare){
  
  # Add the most recent expenses to the current year if we do not have most up-to-date expenses
  years<-unique(data$Year) # years in data set
  yearsinexpdata<- unique(exp.childcareData.ALICE$yearofdata) # years in exp data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data to years in expense data set
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in expenses data set
  # Create data for the future
  maxyearofdata<-max(exp.childcareData.ALICE$yearofdata) # collect latest year of expense data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest expense year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stcountyfips2010=unique(exp.childcareData.ALICE$stcountyfips2010), Year=futureYrs)
    # Collect latest expense data there is and merge w/data frame
    expand2<-exp.childcareData.ALICE[exp.childcareData.ALICE$yearofdata==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stcountyfips2010"))%>%drop_na()
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing expense data
    expandPastMiss<-expand.grid(stcountyfips2010=unique(exp.childcareData.ALICE$stcountyfips2010), Year=nonFutureYrs)
    # Merge on expense data and for each past/missing year assign expense data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, exp.childcareData.ALICE, by=c("stcountyfips2010"))
    expandPastMiss2$yeardiff<-expandPastMiss2$yearofdata-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff))
  }  # Attach copied future, historical, and missing expense data
  exp.childcareData.ALICE$Year<-exp.childcareData.ALICE$yearofdata
  if(length(futureYrs)>0) {exp.childcareData.ALICE<-exp.childcareData.ALICE %>% rbind(expand)}
  if(length(nonFutureYrs)>0) {exp.childcareData.ALICE<-exp.childcareData.ALICE %>% rbind(expandPastMiss2)}
  
  data<-left_join(data, exp.childcareData.ALICE, by=c("stcountyfips2010", "Year", "stateFIPS")) 
  #not all counties merge!!! some are missing!! View missing data
  # data2<-data %>%
  #   filter(is.na(familyChildCare.infant.ftdailyrate))
  #   unique(data2$stcountyfips2010)
  # data <- data[data$yearofdata != 2018,]
  
  data<-data %>% 
    mutate(exp.childcareChild=0,
           careduringsummer=(case_when(`schoolagesummercare`=="PT" ~ 0.5,`schoolagesummercare`=="FT" ~ 1)))
  
  data$TotalInfants=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=2, na.rm=TRUE) 
  data$TotalPreSchoolers=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=3 &  cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=4,na.rm=TRUE) 
  data$TotalSchoolAge=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=5 &  cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=12,na.rm=TRUE) 
  
  # Determine Expense based on Age of Children
  
  #formula for survival
  if(budget.ALICE=="survival"|budget.ALICE== "survivalforcliff"){
    data<-data %>%  
      #replace the parameters.default values with numbers (need to make assumptions about number of school days and number of summer days- typically number of school days is 180)
      mutate(exp.childcareInfants=familyChildCare.infant.ftdailyrate*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
             exp.childcarePreSchoolers=familyChildCare.4yr.old.ftdailyrate*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
             ALICE.exp.childcareSchoolAge=familyChildCare.4yr.old.ftdailyrate*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])*(3/8),
             exp.childcareSchoolAge=familyChildCare.4yr.old.ftdailyrate*parameters.defaults$numberofSummerChildcareDays[1]*careduringsummer+familyChildCare.4yr.old.ftdailyrate*0.5*parameters.defaults$numberofSchoolDays[1],
             exp.childcare=exp.childcareInfants+exp.childcarePreSchoolers+exp.childcareSchoolAge,
             ALICE.exp.childcare=exp.childcareInfants+exp.childcarePreSchoolers+ALICE.exp.childcareSchoolAge)
  }
  
  #formula for stability
  if(budget.ALICE=="stability"){
    data<-data %>% 
      #replace the parameters.default values with numbers (need to make assumptions about number of school days and number of summer days- typically number of school days is 180)
      mutate(exp.childcareInfants=licensedChildcare.infant.ftdailyrate*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
             exp.childcarePreSchoolers=licensedChildcare.4yr.ftdailyrate*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
             ALICE.exp.childcareSchoolAge=licensedChildcare.4yr.ftdailyrate*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])*(3/8), #schoolage childcare for part time
             exp.childcareSchoolAge=licensedChildcare.4yr.ftdailyrate*parameters.defaults$numberofSummerChildcareDays[1]*careduringsummer+licensedChildcare.4yr.ftdailyrate*0.5*parameters.defaults$numberofSchoolDays[1],
             exp.childcare=exp.childcareInfants+exp.childcarePreSchoolers+exp.childcareSchoolAge,
             ALICE.exp.childcare=exp.childcareInfants+exp.childcarePreSchoolers+ALICE.exp.childcareSchoolAge)
  }
  
  # Caluculate Total ALICE Childcare across all ages
  data$ALICE.exp.childcare<-data$exp.childcareInfants*data$TotalInfants + data$exp.childcarePreSchoolers*data$TotalPreSchoolers + data$ALICE.exp.childcareSchoolAge*data$TotalSchoolAge
  data$exp.childcare<-data$exp.childcareInfants*data$TotalInfants + data$exp.childcarePreSchoolers*data$TotalPreSchoolers + data$exp.childcareSchoolAge*data$TotalSchoolAge
  
  # Childcare for each household member for Benefits Calculator
  data<-data%>%
    mutate(person1childcare = case_when(agePerson1 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson1 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson1 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person2childcare = case_when(agePerson2 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson2 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson2 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person3childcare = case_when(agePerson3 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson3 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson3 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person4childcare = case_when(agePerson4 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson4 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson4 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person5childcare = case_when(agePerson5 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson5 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson5 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person6childcare = case_when(agePerson6 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson6 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson6 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person7childcare = case_when(agePerson7 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson7 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson7 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person8childcare = case_when(agePerson8 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson8 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson8 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person9childcare = case_when(agePerson9 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson9 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson9 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person10childcare = case_when(agePerson10 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson10 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson10 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person11childcare = case_when(agePerson11 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson11 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson11 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0)
          ,person12childcare = case_when(agePerson12 %in% c(0:2) ~ exp.childcareInfants
                                        ,agePerson12 %in% c(3:4) ~ exp.childcarePreSchoolers
                                        ,agePerson12 %in% c(5:12) ~ exp.childcareSchoolAge
                                        ,TRUE ~ 0))
  
  #inflate cost to current Year - ALICE  
  data$ALICE.exp.childcare<-data$ALICE.exp.childcare*(1+(.021-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata)
  data$exp.childcare<-data$exp.childcare*(1+(parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata)
  
  data$ALICE.exp.childcare<-round(data$ALICE.exp.childcare,0)
  
  #inflate cost to current - PRD calls for childcare cost for each person
  data$person1childcare<-round(data$person1childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person2childcare<-round(data$person2childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person3childcare<-round(data$person3childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person4childcare<-round(data$person4childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person5childcare<-round(data$person5childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person6childcare<-round(data$person6childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person7childcare<-round(data$person7childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person8childcare<-round(data$person8childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person9childcare<-round(data$person9childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person10childcare<-round(data$person10childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person11childcare<-round(data$person11childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  data$person12childcare<-round(data$person12childcare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0)
  
  returnData<-data%>%
    select(person1childcare, person2childcare, person3childcare, person4childcare, person5childcare, person6childcare, person7childcare
           , person8childcare, person9childcare, person10childcare, person11childcare, person12childcare, ALICE.exp.childcare, exp.childcare)
  
  # remove yearofdata from main dataset before merging the next expense
  data<-data%>%select(-yearofdata)
  
  return(returnData)
}


# Transportation Expenses ----

function.transpExp.ALICE<-function(data){
  
  # Add the most recent expenses to the current year if we do not have most up-to-date expenses
  years<-unique(data$Year) # years in data set
  yearsinexpdata<- unique(exp.transportationData.ALICE$yearofdata) # years in exp data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data to years in expense data set
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in expenses data set
  # Create data for the future
  maxyearofdata<-max(exp.transportationData.ALICE$yearofdata) # collect latest year of expense data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest expense year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stcountyfips2010=unique(exp.transportationData.ALICE$stcountyfips2010), famsize=unique(exp.transportationData.ALICE$famsize), Year=futureYrs)
    # Collect latest expense data there is and merge w/data frame
    expand2<-exp.transportationData.ALICE[exp.transportationData.ALICE$yearofdata==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stcountyfips2010", "famsize"))#%>%drop_na()
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing expense data
    expandPastMiss<-expand.grid(stcountyfips2010=unique(exp.transportationData.ALICE$stcountyfips2010), famsize=unique(exp.transportationData.ALICE$famsize), Year=nonFutureYrs)
    # Merge on expense data and for each past/missing year, assign expense data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, exp.transportationData.ALICE, by=c("stcountyfips2010", "famsize"))
    expandPastMiss2$yeardiff<-expandPastMiss2$yearofdata-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff))
  }  # Attach copied future, historical, and missing expense data
  exp.transportationData.ALICE$Year<-exp.transportationData.ALICE$yearofdata
  if(length(futureYrs)>0) { exp.transportationData.ALICE<-exp.transportationData.ALICE%>%rbind(expand) }
  if(length(nonFutureYrs)>0) { exp.transportationData.ALICE<-exp.transportationData.ALICE%>%rbind(expandPastMiss2) }
  
  data<-left_join(data, exp.transportationData.ALICE, by=c("stcountyfips2010", "famsize", "Year"))

  data$TotalAdults=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=18 &  cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=64,na.rm=TRUE) 
  data$Total65plus=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=65, na.rm=TRUE) 
  data$TotalKids=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=17, na.rm=TRUE) 
  data$Totalpublic=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=5, na.rm=TRUE)
  
  #formula for survival
  if(budget.ALICE=="survival"){
    data <- data %>% 
      mutate(expense.car=((TotalKids*miles.kids)+(TotalAdults*miles.adult)+(Total65plus*miles.senior))*car.permile.survival+car.expense.survival,
             expense.transportation=(ifelse(use.public.trans == "Yes",(public.trans*Totalpublic), expense.car))
      )
  }
  
  #formula for stability
  if(budget.ALICE=="stability"){
    data <- data %>% 
      mutate(expense.car =((TotalKids*miles.kids)+(TotalAdults*miles.adult)+(Total65plus*miles.senior))*car.permile.stability+car.expense.stability,
             expense.transportation = ifelse(use.public.trans == "Yes",(public.trans*Totalpublic)+(expense.car*(1/3)), expense.car)
      )
  }
  
  #formula for CLIFF - we assume kids are < 19. EITC defines it under age 19 & USDA adult category starts at 19
  if(budget.ALICE=="survivalforcliff"){
    data <- data %>% 
      mutate(expense.car=((numkids*miles.kids)+(numadults*miles.adult)+(Total65plus*miles.senior))*car.permile.survival+car.expense.survival,
             expense.transportation=(ifelse(use.public.trans == "Yes",(public.trans*Totalpublic), expense.car))
      )
  }
  
  #inflation
  if (budget.ALICE=="survival" | budget.ALICE=="stability"){
    data$expense.transportation<-data$expense.transportation*(1+(.022-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
  }else if(budget.ALICE=="survivalforcliff"){
    data$expense.transportation<-data$expense.transportation*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata) 
  }
  
  data$expense.transportation<-round(data$expense.transportation,0)
 
   # remove yearofdata from main dataset before merging the next expense
  data<-data%>%select(-yearofdata)
  
  return(data$expense.transportation)
}

# Tech Expenses ----

function.techExp.ALICE<-function(data){

  # Add the most recent expenses to the current year if we do not have most up-to-date expenses
  years<-unique(data$Year) # years in data set
  yearsinexpdata<- unique(exp.techData.ALICE$yearofdata) # years in exp data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data to years in expense data set
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in expenses data set
  # Create data for the future
  maxyearofdata<-max(exp.techData.ALICE$yearofdata) # collect latest year of expense data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest expense year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(numadults=unique(exp.techData.ALICE$numadults), Year=futureYrs)
    # Collect latest expense data there is and merge w/data frame
    expand2<-exp.techData.ALICE[exp.techData.ALICE$yearofdata==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("numadults"))%>%drop_na()
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing expense data
    expandPastMiss<-expand.grid(numadults=unique(exp.techData.ALICE$numadults), Year=nonFutureYrs)
    # Merge on expense data and for each past/missing year, assign expense data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, exp.techData.ALICE, by=c("numadults"))
    expandPastMiss2$yeardiff<-expandPastMiss2$yearofdata-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff))
  }  # Attach copied future, historical, and missing expense data
  exp.techData.ALICE$Year<-exp.techData.ALICE$yearofdata
  if(length(futureYrs)>0) { exp.techData.ALICE<-exp.techData.ALICE%>%rbind(expand) }
  if(length(nonFutureYrs)>0) { exp.techData.ALICE<-exp.techData.ALICE%>%rbind(expandPastMiss2) } 
  
  data<-left_join(data, exp.techData.ALICE, by=c("numadults", "Year"))
 
  #formula for survival
  if(budget.ALICE=="survival"|budget.ALICE=="survivalforcliff"){
    data$expense.tech<-(data$expense.smartphone + data$expense.broadband)
  }
  
  #formula for stability
  if(budget.ALICE=="stability"){
    data$expense.tech<-(data$expense.smartphone + data$expense.broadband)
  }
  
  #inflation adjust
  #alice essentials says tech costs are flat, which means they are decreasing with time in real terms
  if (budget.ALICE=="survival" | budget.ALICE=="stability"){
    data$expense.tech<-data$expense.tech*(1+(0-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
  }else if(budget.ALICE=="survivalforcliff"){
    data$expense.tech<-data$expense.tech*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata) 
  }
  
  data$expense.tech<-round(data$expense.tech,0)
  
  # remove yearofdata from main dataset before merging the next expense
  data<-data%>%select(-yearofdata)
  
  return(data$expense.tech)
}

# Food Expenses ----

# USDA with feeding America variation - ALICE
function.foodExp.ALICE<-function(data){
  
  # Add the most recent expenses to the current year if we do not have most up-to-date expenses
  years<-unique(data$Year) # years in data set
  yearsinexpdata<- unique(exp.foodData.USDA$yearofdata) # years in exp data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data to years in expense data set
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in expenses data set
  # Create data for the future
  maxyearofdata<-max(exp.foodData.USDA$yearofdata) # collect latest year of expense data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest expense year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stcountyfips2010=unique(exp.foodData.USDA$stcountyfips2010), famsize=unique(exp.foodData.USDA$famsize), Year=futureYrs)
    # Collect latest expense data there is and merge w/data frame
    expand2<-exp.foodData.USDA[exp.foodData.USDA$yearofdata==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stcountyfips2010", "famsize"))#%>%drop_na()
    }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing expense data
    expandPastMiss<-expand.grid(stcountyfips2010=unique(exp.foodData.USDA$stcountyfips2010), famsize=unique(exp.foodData.USDA$famsize), Year=nonFutureYrs)
    # Merge on expense data and for each past/missing year, assign expense data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, exp.foodData.USDA, by=c("stcountyfips2010","famsize"))
    expandPastMiss2$yeardiff<-expandPastMiss2$yearofdata-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff)%>%
      select(-c(yeardiff, minyeardiff))
  }  # Attach copied future, historical, and missing expense data
  exp.foodData.USDA$Year<-exp.foodData.USDA$yearofdata
  exp.foodData.USDA<-subset(exp.foodData.USDA,!is.na(FIPS))
  
  if(length(futureYrs)>0){exp.foodData.USDA<-exp.foodData.USDA%>%rbind(expand)}
  if(length(nonFutureYrs)>0){exp.foodData.USDA<-exp.foodData.USDA%>%rbind(expandPastMiss2)}
  
  data<-left_join(data, exp.foodData.USDA, by=c("stcountyfips2010","famsize", "Year"))

 # data<-left_join(data, exp.foodData.USDA, by=c("stcountyfips2010","famsize","Year"))
  # #not all counties merge!!! some are missing!! View missing data
  #  data2<-data %>% 
  #    filter(is.na(Age_1_year_Thrifty))
  #   unique(data2$stcountyfips2010)
  # 
  
  #formula for survival
  if(budget.ALICE=="survival"){
    data<-data %>% 
      mutate(food1=case_when(agePerson1 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson1 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson1 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson1 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson1 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson1 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson1 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson1 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson1 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson1 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food2=case_when(agePerson2 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson2 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson2 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson2 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson2 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson2 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson2 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson2 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson2 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson2 >=71 ~ `Age_71plus_Thrifty`)) %>%
      
      mutate(food3=case_when(agePerson3 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson3 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson3 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson3 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson3 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson3 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson3 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson3 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson3 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson3 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food4=case_when(agePerson4 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson4 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson4 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson4 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson4 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson4 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson4 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson4 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson4 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson4 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food5=case_when(agePerson5 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson5 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson5 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson5 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson5 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson5 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson5 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson5 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson5 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson5 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food6=case_when(agePerson6 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson6 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson6 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson6 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson6 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson6 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson6 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson6 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson6 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson6 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food7=case_when(agePerson7 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson7 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson7 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson7 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson7 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson7 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson7 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson7 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson7 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson7 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food8=case_when(agePerson8 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson8 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson8 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson8 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson8 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson8 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson8 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson8 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson8 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson8 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food9=case_when(agePerson9 %in% c(0:1)~ `Age_1_year_Thrifty`,
                             agePerson9 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                             agePerson9 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                             agePerson9 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                             agePerson9 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                             agePerson9 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                             agePerson9 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                             agePerson9 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                             agePerson9 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                             agePerson9 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food10=case_when(agePerson10 %in% c(0:1)~ `Age_1_year_Thrifty`,
                              agePerson10 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                              agePerson10 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                              agePerson10 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                              agePerson10 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                              agePerson10 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                              agePerson10 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                              agePerson10 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                              agePerson10 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                              agePerson10 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food11=case_when(agePerson11 %in% c(0:1)~ `Age_1_year_Thrifty`,
                              agePerson11 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                              agePerson11 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                              agePerson11 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                              agePerson11 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                              agePerson11 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                              agePerson11 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                              agePerson11 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                              agePerson11 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                              agePerson11 >=71 ~ `Age_71plus_Thrifty`)) %>% 
      
      mutate(food12=case_when(agePerson12 %in% c(0:1)~ `Age_1_year_Thrifty`,
                              agePerson12 %in% c(2:3)~ `Age_2to3_years_Thrifty`,
                              agePerson12 %in% c(4:5)~ `Age_4to5_years_Thrifty`,
                              agePerson12 %in% c(6:8)~ `Age_6to8_years_Thrifty`,
                              agePerson12 %in% c(9:11)~ `Age_9to11_years_Thrifty`,
                              agePerson12 %in% c(12:13)~ `Age_12to13_years_Thrifty`,
                              agePerson12 %in% c(14:19)~ `Age_14to19_years_Thrifty`,
                              agePerson12 %in% c(20:50)~ `Age_20to50_years_Thrifty`,
                              agePerson12 %in% c(51:70)~ `Age_51to70_years_Thrifty`, 
                              agePerson12 >=71 ~ `Age_71plus_Thrifty`))
    
  }
  
  #formula for stability
  if(budget.ALICE=="stability"){
    data<-data %>% 
      mutate(food1=case_when(agePerson1 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson1 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson1 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson1 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson1 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson1 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson1 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson1 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson1 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson1 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food2=case_when(agePerson2 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson2 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson2 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson2 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson2 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson2 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson2 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson2 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson2 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson2 >=71 ~ `Age_71plus_Moderate`)) %>%
      
      mutate(food3=case_when(agePerson3 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson3 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson3 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson3 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson3 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson3 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson3 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson3 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson3 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson3 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food4=case_when(agePerson4 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson4 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson4 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson4 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson4 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson4 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson4 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson4 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson4 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson4 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food5=case_when(agePerson5 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson5 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson5 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson5 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson5 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson5 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson5 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson5 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson5 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson5 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food6=case_when(agePerson6 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson6 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson6 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson6 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson6 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson6 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson6 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson6 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson6 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson6 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food7=case_when(agePerson7 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson7 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson7 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson7 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson7 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson7 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson7 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson7 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson7 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson7 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food8=case_when(agePerson8 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson8 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson8 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson8 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson8 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson8 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson8 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson8 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson8 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson8 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food9=case_when(agePerson9 %in% c(0:1)~ `Age_1_year_Moderate`,
                             agePerson9 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                             agePerson9 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                             agePerson9 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                             agePerson9 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                             agePerson9 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                             agePerson9 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                             agePerson9 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                             agePerson9 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                             agePerson9 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food10=case_when(agePerson10 %in% c(0:1)~ `Age_1_year_Moderate`,
                              agePerson10 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                              agePerson10 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                              agePerson10 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                              agePerson10 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                              agePerson10 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                              agePerson10 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                              agePerson10 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                              agePerson10 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                              agePerson10 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food11=case_when(agePerson11 %in% c(0:1)~ `Age_1_year_Moderate`,
                              agePerson11 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                              agePerson11 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                              agePerson11 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                              agePerson11 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                              agePerson11 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                              agePerson11 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                              agePerson11 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                              agePerson11 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                              agePerson11 >=71 ~ `Age_71plus_Moderate`)) %>% 
      
      mutate(food12=case_when(agePerson12 %in% c(0:1)~ `Age_1_year_Moderate`,
                              agePerson12 %in% c(2:3)~ `Age_2to3_years_Moderate`,
                              agePerson12 %in% c(4:5)~ `Age_4to5_years_Moderate`,
                              agePerson12 %in% c(6:8)~ `Age_6to8_years_Moderate`,
                              agePerson12 %in% c(9:11)~ `Age_9to11_years_Moderate`,
                              agePerson12 %in% c(12:13)~ `Age_12to13_years_Moderate`,
                              agePerson12 %in% c(14:19)~ `Age_14to19_years_Moderate`,
                              agePerson12 %in% c(20:50)~ `Age_20to50_years_Moderate`,
                              agePerson12 %in% c(51:70)~ `Age_51to70_years_Moderate`, 
                              agePerson12 >=71 ~ `Age_71plus_Moderate`))
  }
  
  if(budget.ALICE=="survivalforcliff"){
    data<-data %>% 
      mutate(expense.food=case_when(famsize==1~(Thrifty_reference_family/4)*1.2,
                                    famsize==2~(Thrifty_reference_family/4)*1.1,
                                    famsize==3~(Thrifty_reference_family/4)*1.05,
                                    famsize==4~(Thrifty_reference_family/4),
                                    famsize==5~(Thrifty_reference_family/4)*.95,
                                    famsize==6~(Thrifty_reference_family/4)*.95,
                                    famsize==7~(Thrifty_reference_family/4)*0.9,
                                    famsize==8~(Thrifty_reference_family/4)*0.9,
                                    famsize==9~(Thrifty_reference_family/4)*0.9,
                                    famsize==10~(Thrifty_reference_family/4)*0.9,
                                    famsize==11~(Thrifty_reference_family/4)*0.9,
                                    famsize==12~(Thrifty_reference_family/4)*0.9))   
  }
  if (budget.ALICE=="survival"){
  data$expense.food<-rowSums(cbind(data$food1,data$food2, data$food3, data$food4, data$food5, data$food6, data$food7, data$food8, data$food9, data$food10, data$food11, data$food12), na.rm = TRUE)
  }else if(budget.ALICE=="survivalforcliff"){
  data$expense.food<-data$expense.food*data$famsize
  data$expense.food<-data$expense.food*12
  data$expense.food<-data$expense.food*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata) 
  }else if(budget.ALICE=="stability"){ 
  data$expense.food<-rowSums(cbind(data$food1,data$food2, data$food3, data$food4, data$food5, data$food6, data$food7, data$food8, data$food9, data$food10, data$food11, data$food12), na.rm = TRUE)+ data$Foodadd_stability
  }
  data$expense.food<-round(data$expense.food,0)
  
  # remove yearofdata from main dataset before merging the next expense
  data<-data%>%select(-yearofdata)
  
  return(data$expense.food)
}


# Cost of School Meals ----

function.schoolmealsExp<-function(data){
  
  # Add the most recent expenses to the current year if we do not have most up-to-date expenses
  years<-unique(data$Year) # years in data set
  yearsinexpdata<- unique(exp.foodData.schoolMeals$yearofdata) # years in exp data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data to years in expense data set
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in expenses data set
  # Create data for the future
  maxyearofdata<-max(exp.foodData.schoolMeals$yearofdata) # collect latest year of expense data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest expense year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(AKorHI=unique(exp.foodData.schoolMeals$AKorHI), Year=futureYrs)
    # Collect latest expense data there is and merge w/data frame
    expand2<-exp.foodData.schoolMeals[exp.foodData.schoolMeals$yearofdata==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("AKorHI"))%>%drop_na()
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing expense data
    expandPastMiss<-expand.grid(AKorHI=unique(exp.foodData.schoolMeals$AKorHI), Year=nonFutureYrs)
    # Merge on expense data and for each past/missing year, assign expense data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, exp.foodData.schoolMeals, by=c("AKorHI"))
    expandPastMiss2$yeardiff<-expandPastMiss2$yearofdata-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff))
  }  # Attach copied future, historical, and missing expense data
  exp.foodData.schoolMeals$Year<-exp.foodData.schoolMeals$yearofdata
  if(length(futureYrs)>0) { exp.foodData.schoolMeals<-exp.foodData.schoolMeals%>%rbind(expand) }
  if(length(nonFutureYrs)>0) { exp.foodData.schoolMeals<-exp.foodData.schoolMeals%>%rbind(expandPastMiss2) } 
  
  data<-left_join(data, exp.foodData.schoolMeals, by=c("AKorHI","Year"))
  
  data$expense.dailyschoolMeals<-data$dailyvalue.schoolLunch+data$dailyvalue.schoolBreakfast
  
  data$numkidsinschool=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=18 & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=5, na.rm=TRUE)

  #annualize
  data$expense.schoolmeals<-data$expense.dailyschoolMeals*parameters.defaults$numberofSchoolDays[1] 
  
  data$expense.schoolmeals<-data$expense.schoolmeals*data$numkidsinschool
  
  #inflation adjust
  if (budget.ALICE=="survival" | budget.ALICE=="stability"){
    data$expense.schoolmeals<-data$expense.schoolmeals*(1+(.055-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
  }else if(budget.ALICE=="survivalforcliff"){
    data$expense.schoolmeals<-data$expense.schoolmeals*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata) 
  }
  data$expense.schoolmeals<-round(data$expense.schoolmeals,2)
  
  # remove yearofdata from main dataset before merging the next expense
  data<-data%>%select(-yearofdata)
  
  # The cost for school meals is calculated using school age kids, >5 years old. Kids in PreK may be younger than 5 so the 'numkidsinschool' value is recalculated after
  # the PreK benefit is ran and includes these kids. 
  
  return(data$expense.schoolmeals)
}

# Cost of WIC ----

function.wicExp<-function(data){
  
  #no merging b/c only one value of data over time; no variation by geography
  data$value.infant<-exp.foodData.WIC$value.infant
  data$value.kidsage1to4<-exp.foodData.WIC$value.kidsage1to4
  data$value.women<-exp.foodData.WIC$value.women
  data$yearofdata<-exp.foodData.WIC$yearofdata
  
  # Step 1: Calculate number of eligible infants, children, and women
  data$numkidsage1to4=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=4 & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=1, na.rm=TRUE)
  data$numinfants=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)==0, na.rm=TRUE)
  data$mom<-0
  data$mom[data$numinfants>0]<-1 #all our calc require there to be one adult and don't ask gender, so assume mom is in house if there is a child present
  
  # Step 5: Calculate WIC value
  data$expense.WIC<- data$numinfants * data$value.infant + data$numkidsage1to4 * data$value.kidsage1to4 + data$mom*data$value.women 
  
  
  #inflation adjust
  if (budget.ALICE=="survival" | budget.ALICE=="stability"){
   data$expense.WIC<-data$expense.WIC*(1+(.055-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
  }else if(budget.ALICE=="survivalforcliff"){
    data$expense.WIC<-data$expense.WIC*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata) 
  }
  data$expense.WIC<-12*round(data$expense.WIC,2)
  
  return(data$expense.WIC)
}

# Healthcare Expenses ----

## Employer sponsored cost ----
#in BenefitsCalculator_functions, the cost of employer sponsored health care coverage is determined based on the famsize.
#when soemone in the family is on medicaid (and in the future, if they are on medicare, their famsize for matching here will be reduced)
function.healthcareExp.ALICE<-function(data
                                       , famsizevar){
  # For PRD, famsize may change depending on medicaid status as mentioned above
  data<-data%>%
    rename(famsizeToUse = famsizevar)
  
  # Add the most recent expenses to the current year if we do not have most up-to-date expenses
  years<-unique(data$Year) # years in data set
  yearsinexpdata<- unique(exp.healthcareData.ALICE$yearofdata) # years in exp data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data to years in expense data set
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in expenses data set
  # Create data for the future
  maxyearofdata<-max(exp.healthcareData.ALICE$yearofdata) # collect latest year of expense data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest expense year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stcountyfips2010=unique(exp.healthcareData.ALICE$stcountyfips2010), Year=futureYrs)
    # Collect latest expense data there is and merge w/data frame
    expand2<-exp.healthcareData.ALICE[exp.healthcareData.ALICE$yearofdata==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stcountyfips2010"))%>%drop_na()
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing expense data
    expandPastMiss<-expand.grid(stcountyfips2010=unique(exp.healthcareData.ALICE$stcountyfips2010), Year=nonFutureYrs)
    # Merge on expense data and for each past/missing year, assign expense data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, exp.healthcareData.ALICE, by=c("stcountyfips2010"))
    expandPastMiss2$yeardiff<-expandPastMiss2$yearofdata-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff))
  }  # Attach copied future, historical, and missing expense data
  exp.healthcareData.ALICE$Year<-exp.healthcareData.ALICE$yearofdata
  if(length(futureYrs)>0) { exp.healthcareData.ALICE<-exp.healthcareData.ALICE%>%rbind(expand) }
  if(length(nonFutureYrs)>0) { exp.healthcareData.ALICE<-exp.healthcareData.ALICE%>%rbind(expandPastMiss2) } 

  #for ALICE data, adjust famsize by Number of seniors so the seniors don't count towards employer family plan
  data$Total65plus<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=65, na.rm=TRUE) 
  data$famsize_forALICE<-data$famsize-data$Total65plus 
  data$TotalU65<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=64 ,na.rm=TRUE) 
  
  data<-left_join(data, exp.healthcareData.ALICE, by=c("stcountyfips2010", "Year"))
  
  #not all counties merge!!! some are missing!! View missing data
  # data2<-data %>%
  # filter(is.na(Single_employee))
  # unique(data2$stcountyfips2010)

  data <- data %>% 
    #total cost of health insurance (employer paid + employee paid premium)
    mutate(exp.healthcare.employer= case_when(famsizeToUse==1 ~ Single_employee+Single_employer, famsizeToUse==2 ~ Plusone_employee+ Plusone_employer, famsizeToUse>=3 ~Family_employee+Family_employer, TRUE~NA_real_)) %>% 
    mutate(exp.healthcare.employer.ALICE =case_when(famsize_forALICE==1 ~ Single_employee+Single_employer, famsize_forALICE==2 ~ Plusone_employee+ Plusone_employer, famsize_forALICE>=3 ~ Family_employee + Family_employer, TRUE~NA_real_)) %>% 
    #premium paid by the employee - low-income households are more likely to have someone in fair or poor health. The Household Survival Budget includes a poor-health multiplier, a conservative 30% increase to out-of-pocket costs.
    mutate(premium.employer= case_when(famsizeToUse==1 ~ Single_employee, famsizeToUse==2 ~ Plusone_employee, famsizeToUse>=3 ~Family_employee, TRUE~NA_real_)*1) %>% 
    mutate(premium.employer.ALICE =case_when(famsize_forALICE==1 ~ Single_employee, famsize_forALICE==2 ~ Plusone_employee, famsize_forALICE>=3 ~ Family_employee,famsize_forALICE==0 ~ 0, TRUE~NA_real_)) %>% 
    #premium.medicare
    mutate(premium.medicare= annual_premium_partB)  %>% 
    #out of pocket
    mutate(oop.notmedicare=annualOOP_survival) %>% 
    mutate(oop.medicare=annualOOP_partB) %>% 
    mutate(oop.add_for_elderlyordisabled=annualOOP.chronicconditions)
  
  
  if(budget.ALICE=="survival" | budget.ALICE=="survivalforcliff"){
    data<-data %>% 
      mutate(oop.health.family.ALICE=TotalU65*(annualOOP_survival)+Total65plus*(annualOOP_partB+annualOOP.chronicconditions+annual_premium_partB)) %>% 
      mutate(ALICE.expense.healthcare.family=premium.employer.ALICE*1+oop.health.family.ALICE)
  } 
  
  if(budget.ALICE=="stability"){
    data<-data %>% 
      mutate(oop.health.family.ALICE=TotalU65*(annualOOP_stability)+Total65plus*(annualOOP_partB+annualOOP.chronicconditions+annual_premium_partB)) %>% 
      mutate(ALICE.expense.healthcare.family=premium.employer.ALICE+oop.health.family.ALICE)
  }
  
  #maybe add something here for "survivalforcliff", maybe not
  
  # data$TotalAdults<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=18 &  cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=64,na.rm=TRUE) 
  # data$Total65plus<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=65, na.rm=TRUE) 
  # data$TotalKids<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=17 ,na.rm=TRUE) 
  # data$TotalU65<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=64 ,na.rm=TRUE) 
  # 
  # #formula for survival
  # if(budget.ALICE=="survival"){
  #   data <- data %>% 
  #     mutate(senior.health.expense = (annual.premium.partB +annual.OOP.partB+OOP.chronicconditions)*Total65plus,
  #            family.health.insurance = ifelse(TotalAdults == 1 & TotalU65 ==1, Single.employee,
  #                                             ifelse(TotalAdults>=1 & TotalU65 == 2,Plusone.employee,
  #                                                    ifelse(TotalAdults>=1 & TotalU65 >=3,Family.employee,0))),
  #            family.health.oop = TotalU65*annualOOP.survival,
  #            ALICE.expense.healthcare.family = senior.health.expense+family.health.insurance+family.health.oop,
  #            senior.health.medicare = annual.premium.partB,
  #            senior.health.oop = annual.OOP.partB+OOP.chronicconditions
  # 
  #     )
  # }
  # 
  # #formula for stability
  # if(budget.ALICE=="stability"){
  #   data <- data %>% 
  #     mutate(senior.health.expense = (annual.premium.partB +annual.OOP.partB+OOP.chronicconditions)*Total65plus,
  #            family.health.insurance = ifelse(TotalAdults == 1 & TotalU65 ==1, Single.employee,
  #                                             ifelse(TotalAdults>=1 & TotalU65 == 2,Plusone.employee,
  #                                                    ifelse(TotalAdults>=1 & TotalU65 >=3,Family.employee,0))),
  #            family.health.oop = TotalU65*annualOOP.stability,
  #            ALICE.expense.healthcare.family = senior.health.expense+family.health.insurance+family.health.oop,
  #            senior.health.medicare = annual.premium.partB,
  #            senior.health.oop = annual.OOP.partB+OOP.chronicconditions
  #     )
  # }
  
  #adjust for inflation
  if (budget.ALICE=="survival" | budget.ALICE=="stability"){
  data$ALICE.expense.healthcare.family<-round(data$ALICE.expense.healthcare.family*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata),0) 
  data$exp.healthcare.employer<-round(data$exp.healthcare.employer*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata),0) 
  data$premium.employer<-round(data$premium.employer*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata),0) 
  data$premium.medicare<-round(data$premium.medicare*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata),0) 
  data$oop.notmedicare<-round(data$oop.notmedicare*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata),0) 
  data$oop.medicare<-round(data$oop.medicare*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata),0) 
  data$oop.add_for_elderlyordisabled<-round(data$oop.add_for_elderlyordisabled*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata),0) 
  data$oop.health.family.ALICE<-round(data$oop.health.family.ALICE*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata),0)
  }else if(budget.ALICE=="survivalforcliff"){
    data$ALICE.expense.healthcare.family<-round(data$ALICE.expense.healthcare.family*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0) 
    data$exp.healthcare.employer<-round(data$exp.healthcare.employer*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0) 
    data$premium.employer<-round(data$premium.employer*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0) 
    data$premium.medicare<-round(data$premium.medicare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0) 
    data$oop.notmedicare<-round(data$oop.notmedicare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0) 
    data$oop.medicare<-round(data$oop.medicare*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0) 
    data$oop.add_for_elderlyordisabled<-round(data$oop.add_for_elderlyordisabled*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0) 
    data$oop.health.family.ALICE<-round(data$oop.health.family.ALICE*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata),0) 
    
  }
  returnData<-data %>% 
    select(ALICE.expense.healthcare.family,exp.healthcare.employer,premium.employer,premium.medicare,oop.notmedicare,oop.medicare,oop.add_for_elderlyordisabled,oop.health.family.ALICE) 
  
  # remove yearofdata from main dataset before merging the next expense
  data<-data%>%select(-yearofdata)
  
  return(returnData)
}


## Medicaid Costs ----- 
# calculated as a state government spending per enrollee
function.healthcareExp.Medicaid<-function(data
                                          , ageofpersonvar){
  
  colnames(data)[colnames(data)==ageofpersonvar]<-"ageofperson"
  
  data<-left_join(data, exp.healthcareData.Medicaid, by=c("stateFIPS"))
  
  # Assign value of medicaid based on whether person is a kid or an adult
  data<-data %>% 
    mutate(expense.medicaid=case_when(ageofperson >= 19 ~ expense.medicaidAdults
                                      ,ageofperson < 19 ~ expense.medicaidChildren
                                      , TRUE ~ 0))
  
  if (budget.ALICE=="survival" | budget.ALICE=="stability"){
    data$expense.medicaid<-data$expense.medicaid*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
  }else if(budget.ALICE=="survivalforcliff"){
    data$expense.medicaid<-data$expense.medicaid*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata) 
  }
  
  
  data$expense.medicaid<-round(data$expense.medicaid,0)
  
  return(data$expense.medicaid)
}

## ACA Costs ----
#- costs of the Second Lowest Costs Silverplan
function.healthcareExp.SLCS<-function(data
                                      , ageofpersonvar){
  
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
  
  if (budget.ALICE=="survival" | budget.ALICE=="stability"){
    data$expense.SLCS<-data$expense.SLCS*(1+(.033-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
  }else if(budget.ALICE=="survivalforcliff"){
    data$expense.SLCS<-data$expense.SLCS*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata) 
  }
  data$expense.SLCS<-round(data$expense.SLCS,0)

  return(data$expense.SLCS)
}


# Housing Expenses ----

function.housingExp.ALICE<-function(data){
  
  # Add the most recent expenses to the current year if we do not have most up-to-date expenses
  years<-unique(data$Year) # years in data set
  yearsinexpdata<- unique(exp.housingData$yearofdata) # years in exp data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data to years in expense data set
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in expenses data set
  # Create data for the future
  maxyearofdata<-max(exp.housingData$yearofdata) # collect latest year of expense data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest expense year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stcountyfips2010=unique(exp.housingData$stcountyfips2010), numadults=unique(exp.housingData$numadults), numkids=unique(exp.housingData$numkids), Year=futureYrs)
    # Collect latest expense data there is and merge w/data frame
    expand2<-exp.housingData[exp.housingData$yearofdata==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stcountyfips2010", "numadults", "numkids"))%>%drop_na()
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing expense data
    expandPastMiss<-expand.grid(stcountyfips2010=unique(exp.housingData$stcountyfips2010), numadults=unique(exp.housingData$numadults), numkids=unique(exp.housingData$numkids), Year=nonFutureYrs)
    # Merge on expense data and for each past/missing year, assign expense data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, exp.housingData, by=c("stcountyfips2010", "numadults", "numkids"))
    expandPastMiss2$yeardiff<-expandPastMiss2$yearofdata-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff))
  }  # Attach copied future, historical, and missing expense data
  exp.housingData$Year<-exp.housingData$yearofdata
  if(length(futureYrs)>0) { exp.housingData<-exp.housingData%>%rbind(expand) }
  if(length(nonFutureYrs)>0) { exp.housingData<-exp.housingData%>%rbind(expandPastMiss2) } 
  
  data<-left_join(data, exp.housingData, by=c("stcountyfips2010", "numadults", "numkids", "Year"))
  #not all counties merge!!! some are missing!! View missing data
  # data2<-data %>%
  #   filter(is.na(expense.rent))
  # unique(data2$stcountyfips2010)
  
  if(budget.ALICE=="survival"){
    data$exp.rent<-data$expense.rent*(1+(.032-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
    
    data$exp.rent<-round(data$expense.rent,0)
  
    data$exp.utilities<-data$expense.utilities*(1+(.032-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
    
    data$exp.utilities<-round(data$expense.utilities,0)
    
  }
  
  if(budget.ALICE=="stability"){
    data$exp.rent<-data$expense.rent.stability*(1+(.032-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
    
    data$exp.rent<-round(data$expense.rent.stability,0)
    
    data$exp.utilities<-data$expense.utilities*(1+(.032-parameters.defaults$inflationrate[1]))^(data$Year-data$yearofdata) 
    
    data$exp.utilities<-round(data$expense.utilities,0)
  }
  
  if(budget.ALICE=="survivalforcliff"){
    # In research, ruleYear is Year-1. ruleYear in CLIFF is static 
    data$exp.rent<-data$expense.rent*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata) 
    data$exp.rent<-round(data$expense.rent,0)
    
    data$exp.utilities<-data$expense.utilities*(1+(parameters.defaults$inflationrate[1]))^(data$ruleYear-data$yearofdata)
    data$exp.utilities<-round(data$expense.utilities,0)
    
  }
  returnData<-data %>% 
    select(exp.rent,exp.utilities)
  
  # remove yearofdata from main dataset before merging the next expense
  data<-data%>%select(-yearofdata)
  
  return(returnData)

 }

