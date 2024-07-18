 #-------------------------------------------------------------
# description: calculate the value of each public benefit in the PRD
# inputs: each benefit's 'parameters'.rdata file (e.g. snapData.rdata)
# output: values of each benefit & other output used in other programs
#-------------------------------------------------------------


# PUBLIC BENEFITS----


# Social Security Disability Insurance (SSDI)----

function.ssdiBenefit<-function(data){

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(ssdiData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(ssdiData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-ssdiData[ssdiData$ruleYear==maxyearofdata, ]
    expand2$ruleYear<-ifelse(expand2$ruleYear < expand$Year, expand$Year, expand2$ruleYear)
    expand<-expand%>%left_join(expand2, by=c("Year" = "ruleYear")) %>% rename("ruleYear"=Year)
  }# Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {ssdiData<-ssdiData %>% rbind(expand)}

  # We have historical rules
  data<-left_join(data, ssdiData, by=c("ruleYear"))
  data$value.ssdi<-0
  data$value.ssdi.mnth<-0
  data$ssdi_sga<-data$ssdi_sga*12 # make into a yearly value
  data$ssdi_blind_sga<-data$ssdi_blind_sga*12  # make into a yearly value
  data$disabledkids<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<18 & cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==1, na.rm=TRUE)

  #Step 1: Determine if the earnings of each household member are high enough to disqualify them from receiving SSDI (after the
  # expiration of any Trial Work Period and grace period)
  data$ssdiRecdMnth1<-ifelse(data$ssdiPIA1==0, 0,
                             ifelse(data$blind1==1 & data$income1>=data$ssdi_blind_sga, 0,
                                    ifelse(data$blind1==0 & data$income1>=data$ssdi_sga, 0
                                           ,data$ssdiPIA1)))
  data$ssdiRecdMnth2<-ifelse(data$ssdiPIA2==0, 0,
                             ifelse(data$blind2==1 & data$income2>=data$ssdi_blind_sga, 0,
                                    ifelse(data$blind2==0 & data$income2>=data$ssdi_sga, 0
                                           ,data$ssdiPIA2)))
  data$ssdiRecdMnth3<-ifelse(data$ssdiPIA3==0, 0,
                             ifelse(data$blind3==1 & data$income3>=data$ssdi_blind_sga, 0,
                                    ifelse(data$blind3==0 & data$income3>=data$ssdi_sga, 0 ,
                                           data$ssdiPIA3)))
  data$ssdiRecdMnth4<-ifelse(data$ssdiPIA4==0, 0,
                             ifelse(data$blind4==1 & data$income4>=data$ssdi_blind_sga, 0,
                                    ifelse(data$blind4==0 & data$income4>=data$ssdi_sga, 0 ,
                                           data$ssdiPIA4)))
  data$ssdiRecdMnth5<-ifelse(data$ssdiPIA5==0, 0,
                             ifelse(data$blind5==1 & data$income5>=data$ssdi_blind_sga, 0,
                                    ifelse(data$blind5==0 & data$income5>=data$ssdi_sga, 0 ,
                                           data$ssdiPIA5)))
  data$ssdiRecdMnth6<-ifelse(data$ssdiPIA6==0, 0,
                             ifelse(data$blind6==1 & data$income6>=data$ssdi_blind_sga, 0,
                                    ifelse(data$blind6==0 & data$income6>=data$ssdi_sga, 0 ,
                                           data$ssdiPIA6)))

  # Largest ssdi amount between parents in the home
  data$rank1<-case_when(data$married==1~as.numeric(rowMaxs(cbind(data$ssdiRecdMnth1,data$ssdiRecdMnth2),na.rm=TRUE))
                        ,data$married==0~as.numeric(data$ssdiRecdMnth1)
                        ,TRUE~as.numeric(data$ssdiRecdMnth1)) # when both values are the same amount

  # Count number of parents on SSDI
  data$countSSDI<-rowSums(cbind(data$ssdiRecdMnth1, data$ssdiRecdMnth2)>0, na.rm=TRUE)

  # Family maximum amount (not using Combined Family Maximum - potentially higher amount)
  data$famMaxAmount<-1.5*rowMaxs(cbind(data$rank1,0),na.rm=TRUE)


  # Auxiliary benefits eligible for a spouse if child is <16 or if any child is disabled before 22 (we don't check for this)
  data$checkUnder16<-case_when(data$agePerson2<16 | data$agePerson3<16 | data$agePerson4<16 | data$agePerson5<16 | data$agePerson6<16 | data$agePerson7<16 | data$agePerson8<16 | data$agePerson9<16 | data$agePerson10<16 | data$agePerson11<16 | data$agePerson12<16 ~ 1, TRUE~0)

  # Auxiliary maximum for calculating children auxiliary
  data$auxMax<-rowMaxs(cbind(data$famMaxAmount - (data$ssdiRecdMnth1 + data$ssdiRecdMnth2),0),na.rm=TRUE)

  # Calculation for potential spousal aux. benefits when both receive ssdi
  data$potentialBenSpouse1<-ifelse(data$ssdiRecdMnth1<.5*data$ssdiRecdMnth2, rowMaxs(cbind((.5*data$ssdiRecdMnth2)-data$ssdiRecdMnth1,0),na.rm=TRUE),0)
  data$potentialBenSpouse2<-ifelse(data$ssdiRecdMnth2<.5*data$ssdiRecdMnth1, rowMaxs(cbind((.5*data$ssdiRecdMnth1)-data$ssdiRecdMnth2,0),na.rm=TRUE),0)

  # Spouse receives some PIA amount low enough to also receive some partial auxiliary benefit
  data$withheldAmount1<-case_when(data$ssdiRecdMnth1<.5*data$ssdiRecdMnth2 ~ rowMaxs(cbind((.5*data$ssdiRecdMnth2)-(.5*data$ssdiRecdMnth2-data$ssdiRecdMnth1),0),na.rm=TRUE), TRUE~0)
  data$withheldAmount2<-case_when(data$ssdiRecdMnth2<.5*data$ssdiRecdMnth1 ~ rowMaxs(cbind((.5*data$ssdiRecdMnth1)-(.5*data$ssdiRecdMnth1-data$ssdiRecdMnth2),0),na.rm=TRUE), TRUE~0)

  # Count number of people receiving Auxiliary benefits
  data$receivesAux<- case_when(data$married==0 ~ data$numkids
                               ,data$married==1 & data$countSSDI==2 & rowSums(cbind(data$withheldAmount1,data$withheldAmount2),na.rm=TRUE)==0 ~ rowSums(cbind(data$numkids),na.rm=TRUE) # withheld == 0, then spouse (IF THERE IS ONE) does not receive auxiliary benefit
                               ,data$married==1 & data$countSSDI==2 & rowSums(cbind(data$withheldAmount1,data$withheldAmount2),na.rm=TRUE)>0 ~ rowSums(cbind(data$numkids,1),na.rm=TRUE) # witheld>0, then there is a spouse that receives auxiliary benefit
                               ,data$married==1 & data$countSSDI==1 ~ rowSums(cbind(data$numkids,1),na.rm=TRUE)) # Spouse and kids receive aux benefit

  # PIA + potential Auxiliary benefits for Person1 (does not check if child was disabled before age of 22)
  data$actualAuxandPIASpouse1<-case_when((data$married==1 & data$ssdiRecdMnth1==0 & data$ssdiRecdMnth2!=0) & (data$agePerson1>=62 | data$checkUnder16==1 | data$disabledkids!=0)~as.numeric(rowMaxs(cbind(data$receivesAux^-1*((0.5*data$ssdiRecdMnth2)+data$ssdiRecdMnth1),0),na.rm=TRUE)) # No PIA for person1 only for person2
                                         ,(data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2==0) & (data$agePerson1>=62 | data$checkUnder16==1 | data$disabledkids!=0)~ as.numeric(data$ssdiRecdMnth1) # PIA for person1 no PIA for person2
                                         ,(data$married==1 & data$numkids==0 & (data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2==0 | data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0) ~ as.numeric(data$ssdiRecdMnth1)) # No children No Auxiliary; only PIA for person1
                                         ,(data$married==0 & data$ssdiRecdMnth1!=0 ~ as.numeric(data$ssdiRecdMnth1)) # Not married No Auxiliary; only PIA
                                         ,(data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0 & data$withheldAmount1!=0) & (data$agePerson2>=62 | data$checkUnder16==1 | data$disabledkids!=0)~as.numeric(rowMaxs(cbind((data$receivesAux^-1*(data$famMaxAmount-data$ssdiRecdMnth2))-data$ssdiRecdMnth1+data$ssdiRecdMnth1,0),na.rm=TRUE)) # Person1 has lower PIA than Person2
                                         ,(data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0 & data$withheldAmount1==0) & (data$agePerson2>=62 | data$checkUnder16==1 | data$disabledkids!=0)~as.numeric(rowSums(cbind(data$potentialBenSpouse1,data$ssdiRecdMnth1),na.rm=TRUE))# Person1 has higher PIA than Person2
                                         ,TRUE~0
  )
  # PIA + potential Auxiliary benefits for Person2 (does not check if child was disabled before age of 22)
  data$actualAuxandPIASpouse2<-case_when((data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2==0) & (data$agePerson1>=62 | data$checkUnder16==1 | data$disabledkids!=0)~rowMaxs(cbind(data$receivesAux^-1*((0.5*data$ssdiRecdMnth1)+data$ssdiRecdMnth2),0),na.rm=TRUE) # No PIA for person2 only for person1
                                         ,(data$married==1 & data$ssdiRecdMnth1==0 & data$ssdiRecdMnth2!=0) & (data$agePerson1>=62 | data$checkUnder16==1 | data$disabledkids!=0)~ data$ssdiRecdMnth2 # PIA for person2 no PIA for person1
                                         ,(data$married==1 & data$numkids==0 & (data$ssdiRecdMnth1==0 & data$ssdiRecdMnth2!=0 | data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0) ~ data$ssdiRecdMnth2) # No children No Auxiliary; only PIA for person2
                                         ,(data$married==0 & data$ssdiRecdMnth2!=0 ~ data$ssdiRecdMnth2) # Not married No Auxiliary; only PIA
                                         ,(data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0 & data$withheldAmount2!=0) & (data$agePerson2>=62 | data$checkUnder16==1 | data$disabledkids!=0)~rowMaxs(cbind((data$receivesAux^-1*(data$famMaxAmount-data$ssdiRecdMnth1))-data$ssdiRecdMnth2+data$ssdiRecdMnth2,0),na.rm=TRUE) # Person2 has lower PIA than Person1
                                         ,(data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0 & data$withheldAmount2==0) & (data$agePerson2>=62 | data$checkUnder16==1 | data$disabledkids!=0)~rowSums(cbind(data$potentialBenSpouse2,data$ssdiRecdMnth2),na.rm=TRUE)# Person2 has higher PIA than Person1,
                                         ,TRUE~0
  )

  # Check whether the remaining family members qualify based on being a child of the SSDI recipient(s)
  data$potentialAuxBenKids<-case_when(data$married==0 & data$ssdiRecdMnth1!=0 & data$numkids>0~data$receivesAux^-1*rowMins(cbind(data$numkids*data$auxMax, data$numkids*(0.5*data$rank1)),na.rm=TRUE), # single parent
                                      data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2==0 & data$numkids>0~data$receivesAux^-1*rowMins(cbind(data$numkids*data$auxMax, data$numkids*(0.5*data$rank1)),na.rm=TRUE), # 2 parents only parent1 on ssdi  parent2 auxiliary benefit
                                      data$married==1 & data$ssdiRecdMnth1==0 & data$ssdiRecdMnth2!=0 & data$numkids>0~data$receivesAux^-1*rowMins(cbind(data$numkids*data$auxMax, data$numkids*(0.5*data$rank1)),na.rm=TRUE), # 2 parents only parent2 on ssdi parent1 auxiliary benefit
                                      data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0 & data$numkids>0~rowMaxs(cbind((data$numkids*data$receivesAux^-1*(data$famMaxAmount-data$rank1))+data$withheldAmount1+data$withheldAmount2,0),na.rm=TRUE), # both parents on ssdi;
                                      TRUE~0)

  # Family Max is 85% of AIME (since we don't determine initial eligibility this cannot be determined)
  data$famSSDIbenefit<-rowSums(cbind(data$actualAuxandPIASpouse1,data$actualAuxandPIASpouse2,data$potentialAuxBenKids,data$ssdiRecdMnth3,data$ssdiRecdMnth4,data$ssdiRecdMnth5,data$ssdiRecdMnth6),na.rm=TRUE)

  # Portion out aux benefits to family members bc SSDI income in important in determining individual benefits like SSI (counts as unearned income)
  # This has essentially been done using calculations above.
  data$portionedAuxKids<-rowMaxs(cbind(data$potentialAuxBenKids/data$numkids,0),na.rm=TRUE)*12
  data$portionedAuxAdlt1<-data$actualAuxandPIASpouse1*12
  data$portionedAuxAdlt2<-data$actualAuxandPIASpouse2*12

  data$value.ssdi.mnth<-data$famSSDIbenefit
  data$value.ssdi<-data$value.ssdi.mnth*12

  data.ssdi<-data %>%
    select(value.ssdi, portionedAuxAdlt1, portionedAuxAdlt2)

  return(data.ssdi)

}


# Supplemental Security Income Program (SSI)----

function.ssiBenefit<-function(data){

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(ssiData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(ssiData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(married=unique(ssiData$married), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-ssiData[ssiData$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("married")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }# Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {ssiData<-ssiData %>% rbind(expand)}

  # We have historical rules
  data<-left_join(data, ssiData, by=c("married","ruleYear"))
  data<-left_join(data, select(sspData, -c("ruleYear")), by=c("stateName")) # State Supplement Program only has values for 2022. Not merging by ruleYear
  data$value.ssi<-0
  data$value.ssi.mnth<-0

  # Step 1: Calculate number of disabled & non-disabled children
  data$disabledkids<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<18 & cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==1, na.rm=TRUE)
  data$non_disabledkids<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<18 & cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==0, na.rm=TRUE)
  data$disabledAdlts<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=18 & cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==1, na.rm=TRUE)

  # Distribute household assets & monthly disability work expense across all adults
  data$gift.distr<-data$income.gift/data$numadults
  data$investment.distr<-data$income.investment/data$numadults #ER 9/8/22: should divide by numadlts-disabledAdlts bc all interest bearing accounts go to nondisabled adlts
  data$disab.work.exp.distr<-data$disab.work.exp/data$disabledAdlts

  # Step 2a: Calculate annual and monthly total countable earned income for each person in the home.
  data$ann.earned.income1<-data$income1
  data$ann.earned.income2<-ifelse(data$agePerson2>18, data$income2, 0)
  data$ann.earned.income3<-ifelse(data$agePerson3>18, data$income3, 0)
  data$ann.earned.income4<-ifelse(data$agePerson4>18, data$income4, 0)
  data$ann.earned.income5<-ifelse(data$agePerson5>18, data$income5, 0)
  data$ann.earned.income6<-ifelse(data$agePerson6>18, data$income6, 0)
  data$ann.earned.income7<-ifelse(data$agePerson7>18, data$income7, 0)
  data$ann.earned.income8<-ifelse(data$agePerson8>18, data$income8, 0)
  data$ann.earned.income9<-ifelse(data$agePerson9>18, data$income9, 0)
  data$ann.earned.income10<-ifelse(data$agePerson10>18, data$income10, 0)
  data$ann.earned.income11<-ifelse(data$agePerson11>18, data$income11, 0)
  data$ann.earned.income12<-ifelse(data$agePerson12>18, data$income12, 0)

  # 2b. Monthly Countable Earned Income
  data$month.earned.income1<-data$ann.earned.income1 / 12
  data$month.earned.income2<-data$ann.earned.income2 / 12
  data$month.earned.income3<-data$ann.earned.income3 / 12
  data$month.earned.income4<-data$ann.earned.income4 / 12
  data$month.earned.income5<-data$ann.earned.income5 / 12
  data$month.earned.income6<-data$ann.earned.income6 / 12
  data$month.earned.income7<-data$ann.earned.income7 / 12
  data$month.earned.income8<-data$ann.earned.income8 / 12
  data$month.earned.income9<-data$ann.earned.income9 / 12
  data$month.earned.income10<-data$ann.earned.income10 / 12
  data$month.earned.income11<-data$ann.earned.income11 / 12
  data$month.earned.income12<-data$ann.earned.income12 / 12

  # Step 3a: Calculate annual and monthly total countable unearned income for each person in the home
  # annual countable unearned income - SSI counts child support as income for the child so it is not included here, instead see Step 11
  data<-data%>%
    mutate(ann.unearned.income1 = rowSums(cbind(data$gift.distr, data$investment.distr, data$portionedAuxAdlt1),na.rm=TRUE)

           ,ann.unearned.income2 = case_when(!is.na(data$agePerson2 & data$agePerson2>18)  ~ rowSums(cbind(data$gift.distr, data$investment.distr, data$portionedAuxAdlt2), na.rm=TRUE)
                                             ,(is.na(data$agePerson2) | data$agePerson2<=18) ~ 0 # children do not have unearned income
                                             ,TRUE ~ 0)
           ,ann.unearned.income3 = case_when(!is.na(data$agePerson3 & data$agePerson3>18) | is.na(data$agePerson3) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                             ,(is.na(data$agePerson3) | data$agePerson3<=18) ~ 0
                                             ,TRUE ~ 0)
           ,ann.unearned.income4 = case_when(!is.na(data$agePerson4 & data$agePerson4>18) | is.na(data$agePerson4) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                             ,(is.na(data$agePerson4) | data$agePerson4<=18) ~ 0
                                             ,TRUE ~ 0)
           ,ann.unearned.income5 = case_when(!is.na(data$agePerson5 & data$agePerson5>18) | is.na(data$agePerson5) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                             ,(is.na(data$agePerson5) | data$agePerson5<=18) ~ 0
                                             ,TRUE ~ 0)
           ,ann.unearned.income6 = case_when(!is.na(data$agePerson6 & data$agePerson6>18) | is.na(data$agePerson6) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                             ,(is.na(data$agePerson6) | data$agePerson6<=18) ~ 0
                                             ,TRUE ~ 0)
           ,ann.unearned.income7 = case_when(!is.na(data$agePerson7 & data$agePerson7>18) | is.na(data$agePerson7) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                             ,(is.na(data$agePerson7) | data$agePerson7<=18) ~ 0
                                             ,TRUE ~ 0)
           ,ann.unearned.income8 = case_when(!is.na(data$agePerson8 & data$agePerson8>18) | is.na(data$agePerson8) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                             ,(is.na(data$agePerson8) | data$agePerson8<=18) ~ 0
                                             ,TRUE ~ 0)
           ,ann.unearned.income9 = case_when(!is.na(data$agePerson9 & data$agePerson9>18) | is.na(data$agePerson9) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                             ,(is.na(data$agePerson9) | data$agePerson9<=18) ~ 0
                                             ,TRUE ~ 0)
           ,ann.unearned.income10 = case_when(!is.na(data$agePerson10 & data$agePerson10>18) | is.na(data$agePerson10) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                              ,(is.na(data$agePerson10) | data$agePerson10<=18) ~ 0
                                              ,TRUE ~ 0)
           ,ann.unearned.income11 = case_when(!is.na(data$agePerson11 & data$agePerson11>18) | is.na(data$agePerson11) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                              ,(is.na(data$agePerson11) | data$agePerson11<=18) ~ 0
                                              ,TRUE ~ 0)
           ,ann.unearned.income12 = case_when(!is.na(data$agePerson12 & data$agePerson12>18) | is.na(data$agePerson12) ~ rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
                                              ,(is.na(data$agePerson12) | data$agePerson12<=18) ~ 0
                                              ,TRUE ~ 0))


  # 3b. Monthly countable assets unearned income
  data$month.unearned.income1<- data$ann.unearned.income1 / 12
  data$month.unearned.income2<- data$ann.unearned.income2 / 12
  data$month.unearned.income3<- data$ann.unearned.income3 / 12
  data$month.unearned.income4<- data$ann.unearned.income4 / 12
  data$month.unearned.income5<- data$ann.unearned.income5 / 12
  data$month.unearned.income6<- data$ann.unearned.income6 / 12
  data$month.unearned.income7<- data$ann.unearned.income7 / 12
  data$month.unearned.income8<- data$ann.unearned.income8 / 12
  data$month.unearned.income9<- data$ann.unearned.income9 / 12
  data$month.unearned.income10<- data$ann.unearned.income10 / 12
  data$month.unearned.income11<- data$ann.unearned.income11 / 12
  data$month.unearned.income12<- data$ann.unearned.income12 / 12

  # Step 4: Calculate annual total countable assets -- THIS ASSUMES THESE ASSETS ARE ANNUAL
  data$countable.assets<-rowSums(cbind(data$assets.cash, data$assets.car1), na.rm = TRUE)

  # Step 5: Asset test -- used in the last step
  subset0<-data$countable.assets > data$asset_limit

  # Step 6: Income test and benefit calculation when all counted adults are on SSI
  data$included.unearned.income<-0
  data$remain.disregard<-0
  data$included.earned.income<-0
  data$ssi.income<-0
  data$value.ssi.mnth<-0

  # 6A: Single adult with a disability
  subset1<-data$married==0 & (data$disability1==1 & !is.na(data$disability1))
  data$included.unearned.income[subset1]<-rowMaxs(cbind((data$month.unearned.income1[subset1]-data$income_disregard[subset1]),0),na.rm=TRUE)
  data$remain.disregard[subset1]<-rowMaxs(cbind((data$income_disregard[subset1]-data$month.unearned.income1[subset1]),0),na.rm=TRUE)
  data$included.earned.income[subset1]<-(1-data$earnings_disregard_pct[subset1])*rowMaxs(cbind((data$month.earned.income1[subset1]-data$earnings_disregard_amt[subset1]-data$remain.disregard[subset1]),0),na.rm=TRUE)
  data$ssi.income[subset1]<-rowMaxs(cbind(data$included.unearned.income[subset1]+data$included.earned.income[subset1]-data$disab.work.exp[subset1],0),na.rm=TRUE)
  data$value.ssi.mnth[subset1]<-rowMaxs(cbind((data$fbr[subset1]+data$ssp_individual[subset1]-data$ssi.income[subset1]),0),na.rm=TRUE)

  # 6B: Two married adults where both have a disability
  subset2<-data$married==1 & (data$disability1==1 & !is.na(data$disability1)) & (data$disability2==1 & !is.na(data$disability2)) & (data$agePerson2>18 & !is.na(data$agePerson2))
  data$included.unearned.income[subset2]<-rowMaxs(cbind((data$month.unearned.income1[subset2]+data$month.unearned.income2[subset2]-data$income_disregard[subset2]),0),na.rm=TRUE)
  data$remain.disregard[subset2]<-rowMaxs(cbind((data$income_disregard[subset2]-data$month.unearned.income1[subset2]-data$month.unearned.income2[subset2]),0),na.rm=TRUE)
  data$included.earned.income[subset2]<-(1-data$earnings_disregard_pct[subset2])*rowMaxs(cbind((data$month.earned.income1[subset2]+data$month.earned.income2[subset2]-data$earnings_disregard_amt[subset2]-data$remain.disregard[subset2]),0),na.rm=TRUE)
  data$ssi.income[subset2]<-rowMaxs(cbind((data$included.unearned.income[subset2]+data$included.earned.income[subset2]-data$disab.work.exp[subset2]),0),na.rm=TRUE)
  data$value.ssi.mnth[subset2]<-rowMaxs(cbind((data$fbr[subset2]+data$ssp_couple[subset2]-data$ssi.income[subset2]),0),na.rm=TRUE)

  # Step 7: Income test and benefit calculation when for married adults, one is on SSI, the other is not
  data$ineligible.adlt.unearned.income<-0
  data$remaining.deemable.income<-0
  data$ineligible.adlt.earned.income<-0
  data$ssi.deemed.remain<-0
  data$eligible.adlt.unearned.income<-0

  subset3<-data$married==1 & (data$disability1==1 & !is.na(data$disability1)) & (data$disability2==0 & !is.na(data$disability2))

  data$deemed.nondisabled.child.allocation<-rowMaxs(cbind(data$non_disabledkids*data$nondisabled_child_allocation,0),na.rm=TRUE)

  data$eligible.adlt.earnings<-data$month.earned.income1
  data$ineligible.adlt.earnings<-data$month.earned.income2
  data$gross.ineligible.adlt.unearned.income<-data$month.unearned.income2
  data$eligible.adlt.unearned.income<-rowMaxs(cbind(data$month.unearned.income1,0),na.rm=TRUE)

  data$ineligible.adlt.unearned.income[subset3]<-rowMaxs(cbind(data$gross.ineligible.adlt.unearned.income[subset3]-data$deemed.nondisabled.child.allocation[subset3],0),na.rm=TRUE)
  data$remaining.deemable.income[subset3]<-rowMaxs(cbind(data$deemed.nondisabled.child.allocation[subset3]-data$gross.ineligible.adlt.unearned.income[subset3],0),na.rm=TRUE)
  data$ineligible.adlt.earned.income[subset3]<-rowMaxs(cbind(data$ineligible.adlt.earnings[subset3]-data$remaining.deemable.income[subset3],0),na.rm=TRUE)

  subset4<-data$ineligible.adlt.earned.income+data$ineligible.adlt.unearned.income<=data$fbr_difference & data$married==1 & (data$disability1==1 & !is.na(data$disability1)) & (data$disability2==0 & !is.na(data$disability2))
  data$ssi.income[subset4]<-rowMaxs(cbind(rowMaxs(cbind(data$eligible.adlt.unearned.income[subset4]-data$income_disregard[subset4],0),na.rm=TRUE)+rowMaxs(cbind(data$earnings_disregard_pct[subset4],0),na.rm=TRUE)*rowMaxs(cbind(data$eligible.adlt.earnings[subset4]-(rowMaxs(cbind(data$income_disregard[subset4]-data$eligible.adlt.unearned.income[subset4],0),na.rm=TRUE))-data$earnings_disregard_amt[subset4]-data$disab.work.exp[subset4],0),na.rm=TRUE),0),na.rm=TRUE)
  data$value.ssi.mnth[subset4]<-rowMaxs(cbind(data$fbr_individual[subset4]+data$ssp_spouse_as_fbr_individual[subset4]-data$ssi.income[subset4],0),na.rm=TRUE)

  subset5<-data$ineligible.adlt.earned.income+data$ineligible.adlt.unearned.income>data$fbr_difference & data$married==1 & (data$disability1==1 & !is.na(data$disability1)) & (data$disability2==0 & !is.na(data$disability2))
  data$included.unearned.income[subset5]<-rowMaxs(cbind(data$ineligible.adlt.unearned.income[subset5]+data$eligible.adlt.unearned.income[subset5]-data$income_disregard[subset5],0),na.rm=TRUE)
  data$remain.disregard[subset5]<-rowMaxs(cbind(data$income_disregard[subset5]-data$total.unearned.income[subset5],0),na.rm=TRUE)
  data$included.earned.income[subset5]<-(1-data$earnings_disregard_pct[subset5])*rowMaxs(cbind(data$ineligible.adlt.earned.income[subset5]+data$eligible.adlt.earnings[subset5]-data$earnings_disregard_amt[subset5]-data$remain.disregard[subset5],0),na.rm=TRUE)
  data$ssi.income[subset5]<-rowMaxs(cbind(data$included.unearned.income[subset5]+data$included.earned.income[subset5]-data$disab.work.exp[subset5],0),na.rm=TRUE)
  data$value.ssi.mnth[subset5]<-rowMaxs(cbind(data$fbr[subset5]+data$ssp_spouse_in_fbr_couple[subset5]-data$ssi.income[subset5],0),na.rm=TRUE)
  data$ssi.deemed.remain[subset5]<-rowMaxs(cbind(data$ssi.income[subset5]-data$fbr[subset5]+data$ssp_spouse_in_fbr_couple[subset5],0),na.rm=TRUE)

  # Married couple but only second adult has disability
  subset3.1<-data$married==1 & (data$disability1==0 & !is.na(data$disability1)) & (data$disability2==1 & !is.na(data$disability2))

  data$deemed.nondisabled.child.allocation<-rowMaxs(cbind(data$non_disabledkids*data$nondisabled_child_allocation,0),na.rm=TRUE)

  data$eligible.adlt.earnings<-data$month.earned.income2
  data$ineligible.adlt.earnings<-data$month.earned.income1
  data$gross.ineligible.adlt.unearned.income<-data$month.unearned.income1
  data$eligible.adlt.unearned.income<-rowMaxs(cbind(data$month.unearned.income2,0),na.rm=TRUE)

  data$ineligible.adlt.unearned.income[subset3.1]<-rowMaxs(cbind(data$gross.ineligible.adlt.unearned.income[subset3.1]-data$deemed.nondisabled.child.allocation[subset3.1],0),na.rm=TRUE)
  data$remaining.deemable.income[subset3.1]<-rowMaxs(cbind(data$deemed.nondisabled.child.allocation[subset3.1]-data$gross.ineligible.adlt.unearned.income[subset3.1],0),na.rm=TRUE)
  data$ineligible.adlt.earned.income[subset3.1]<-rowMaxs(cbind(data$ineligible.adlt.earnings[subset3.1]-data$remaining.deemable.income[subset3.1],0),na.rm=TRUE)

  subset4<-data$ineligible.adlt.earned.income+data$ineligible.adlt.unearned.income<=data$fbr_difference & data$married==1 & (data$disability1==0 & !is.na(data$disability1)) & (data$disability2==1 & !is.na(data$disability2))
  data$ssi.income[subset4]<-rowMaxs(cbind(rowMaxs(cbind(data$eligible.adlt.unearned.income[subset4]-data$income_disregard[subset4],0),na.rm=TRUE)+rowMaxs(cbind(data$earnings_disregard_pct[subset4],0),na.rm=TRUE)*rowMaxs(cbind(data$eligible.adlt.earnings[subset4]-(rowMaxs(cbind(data$income_disregard[subset4]-data$eligible.adlt.unearned.income[subset4],0),na.rm=TRUE))-data$earnings_disregard_amt[subset4]-data$disab.work.exp[subset4],0),na.rm=TRUE),0),na.rm=TRUE)
  data$value.ssi.mnth[subset4]<-rowMaxs(cbind(data$fbr_individual[subset4]+data$ssp_spouse_as_fbr_individual[subset4]-data$ssi.income[subset4],0),na.rm=TRUE)

  subset5<-data$ineligible.adlt.earned.income+data$ineligible.adlt.unearned.income>data$fbr_difference & data$married==1 & (data$disability1==0 & !is.na(data$disability1)) & (data$disability2==1 & !is.na(data$disability2))
  data$included.unearned.income[subset5]<-rowMaxs(cbind(data$ineligible.adlt.unearned.income[subset5]+data$eligible.adlt.unearned.income[subset5]-data$income_disregard[subset5],0),na.rm=TRUE)
  data$remain.disregard[subset5]<-rowMaxs(cbind(data$income_disregard[subset5]-data$total.unearned.income[subset5],0),na.rm=TRUE)
  data$included.earned.income[subset5]<-(1-data$earnings_disregard_pct[subset5])*rowMaxs(cbind(data$ineligible.adlt.earned.income[subset5]+data$eligible.adlt.earnings[subset5]-data$earnings_disregard_amt[subset5]-data$remain.disregard[subset5],0),na.rm=TRUE)
  data$ssi.income[subset5]<-rowMaxs(cbind(data$included.unearned.income[subset5]+data$included.earned.income[subset5]-data$disab.work.exp[subset5],0),na.rm=TRUE)
  data$value.ssi.mnth[subset5]<-rowMaxs(cbind(data$fbr[subset5]+data$ssp_spouse_in_fbr_couple[subset5]-data$ssi.income[subset5],0),na.rm=TRUE)
  data$ssi.deemed.remain[subset5]<-rowMaxs(cbind(data$ssi.income[subset5]-data$fbr[subset5]+data$ssp_spouse_in_fbr_couple[subset5],0),na.rm=TRUE)

  # Step 7.5: Check for other adults with disability
    # Calculate SSI value for each individual when no one is married - extension of Step 6A
  subset2.1A<-(data$disability2==1 & !is.na(data$disability2)) & (data$agePerson2>18 & !is.na(data$agePerson2)) & data$married==0
  data$included.unearned.income[subset2.1A]<-rowMaxs(cbind((data$month.unearned.income2[subset2.1A]-data$income_disregard[subset2.1A]),0),na.rm=TRUE)
  data$remain.disregard[subset2.1A]<-rowMaxs(cbind((data$income_disregard[subset2.1A]-data$month.unearned.income2[subset2.1A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.1A]<-(1-data$earnings_disregard_pct[subset2.1A])*rowMaxs(cbind((data$month.earned.income2[subset2.1A]-data$earnings_disregard_amt[subset2.1A]-data$remain.disregard[subset2.1A]),0),na.rm=TRUE)
  data$ssi.income_2[subset2.1A]<-rowMaxs(cbind((data$included.unearned.income[subset2.1A]+data$included.earned.income[subset2.1A]),0),na.rm=TRUE)
  data$value.ssi.mnth_2[subset2.1A]<-rowMaxs(cbind((data$fbr[subset2.1A]+data$ssp_individual[subset2.1A]-data$ssi.income_2[subset2.1A]),0),na.rm=TRUE)

  subset2.2A<-(data$disability3==1 & !is.na(data$disability3)) & (data$agePerson3>18 & !is.na(data$agePerson3)) & data$married==0
  data$included.unearned.income[subset2.2A]<-rowMaxs(cbind((data$month.unearned.income3[subset2.2A]-data$income_disregard[subset2.2A]),0),na.rm=TRUE)
  data$remain.disregard[subset2.2A]<-rowMaxs(cbind((data$income_disregard[subset2.2A]-data$month.unearned.income3[subset2.2A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.2A]<-(1-data$earnings_disregard_pct[subset2.2A])*rowMaxs(cbind((data$month.earned.income3[subset2.2A]-data$earnings_disregard_amt[subset2.2A]-data$remain.disregard[subset2.2A]),0),na.rm=TRUE)
  data$ssi.income_3[subset2.2A]<-rowMaxs(cbind((data$included.unearned.income[subset2.2A]+data$included.earned.income[subset2.2A]),0),na.rm=TRUE)
  data$value.ssi.mnth_3[subset2.2A]<-rowMaxs(cbind((data$fbr[subset2.2A]+data$ssp_individual[subset2.2A]-data$ssi.income_3[subset2.2A]),0),na.rm=TRUE)

  subset2.3A<-(data$disability4==1 & !is.na(data$disability4)) & (data$agePerson4>18 & !is.na(data$agePerson4)) & data$married==0
  data$included.unearned.income[subset2.3A]<-rowMaxs(cbind((data$month.unearned.income4[subset2.3A]-data$income_disregard[subset2.3A]),0),na.rm=TRUE)
  data$remain.disregard[subset2.3A]<-rowMaxs(cbind((data$income_disregard[subset2.3A]-data$month.unearned.income4[subset2.3A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.3A]<-(1-data$earnings_disregard_pct[subset2.3A])*rowMaxs(cbind((data$month.earned.income4[subset2.3A]-data$earnings_disregard_amt[subset2.3A]-data$remain.disregard[subset2.3A]),0),na.rm=TRUE)
  data$ssi.income_4[subset2.3A]<-rowMaxs(cbind((data$included.unearned.income[subset2.3A]+data$included.earned.income[subset2.3A]),0),na.rm=TRUE)
  data$value.ssi.mnth_4[subset2.3A]<-rowMaxs(cbind((data$fbr[subset2.3A]+data$ssp_individual[subset2.3A]-data$ssi.income_4[subset2.3A]),0),na.rm=TRUE)

  subset2.4A<-(data$disability5==1 & !is.na(data$disability5)) & (data$agePerson5>18 & !is.na(data$agePerson5)) & data$married==0
  data$included.unearned.income[subset2.4A]<-rowMaxs(cbind((data$month.unearned.income5[subset2.4A]-data$income_disregard[subset2.4A]),0),na.rm=TRUE)
  data$remain.disregard[subset2.4A]<-rowMaxs(cbind((data$income_disregard[subset2.4A]-data$month.unearned.income5[subset2.4A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.4A]<-(1-data$earnings_disregard_pct[subset2.4A])*rowMaxs(cbind((data$month.earned.income5[subset2.4A]-data$earnings_disregard_amt[subset2.4A]-data$remain.disregard[subset2.4A]),0),na.rm=TRUE)
  data$ssi.income_5[subset2.4A]<-rowMaxs(cbind((data$included.unearned.income[subset2.4A]+data$included.earned.income[subset2.4A]),0),na.rm=TRUE)
  data$value.ssi.mnth_5[subset2.4A]<-rowMaxs(cbind((data$fbr[subset2.4A]+data$ssp_individual[subset2.4A]-data$ssi.income_5[subset2.4A]),0),na.rm=TRUE)

  subset2.5A<-(data$disability6==1 & !is.na(data$disability6)) & (data$agePerson6>18 & !is.na(data$agePerson6)) & data$married==0
  data$included.unearned.income[subset2.5A]<-rowMaxs(cbind((data$month.unearned.income6[subset2.5A]-data$income_disregard[subset2.5A]),0),na.rm=TRUE)
  data$remain.disregard[subset2.5A]<-rowMaxs(cbind((data$income_disregard[subset2.5A]-data$month.unearned.income6[subset2.5A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.5A]<-(1-data$earnings_disregard_pct[subset2.5A])*rowMaxs(cbind((data$month.earned.income6[subset2.5A]-data$earnings_disregard_amt[subset2.5A]-data$remain.disregard[subset2.5A]),0),na.rm=TRUE)
  data$ssi.income_6[subset2.5A]<-rowMaxs(cbind((data$included.unearned.income[subset2.5A]+data$included.earned.income[subset2.5A]),0),na.rm=TRUE)
  data$value.ssi.mnth_6[subset2.5A]<-rowMaxs(cbind((data$fbr[subset2.5A]+data$ssp_individual[subset2.5A]-data$ssi.income_6[subset2.5A]),0),na.rm=TRUE)

  # Add individual SSI values to get monthly household SSI value
  data$ssi.income<-rowSums(cbind(data$ssi.income,data$ssi.income_2[subset2.1A],data$ssi.income_3[subset2.2A],data$ssi.income_4[subset2.3A],data$ssi.income_5[subset2.4A],data$ssi.income_6[subset2.5A],(-data$disab.work.exp)),na.rm=TRUE)
  data$value.ssi.mnth<-rowSums(cbind(data$value.ssi.mnth, data$value.ssi.mnth_2, data$value.ssi.mnth_3, data$value.ssi.mnth_4, data$value.ssi.mnth_5, data$value.ssi.mnth_6),na.rm=TRUE)

    # Calculations when all adults in the household have a disability and married is true - extension of Step 6B or 7 which depends on marital status
  subset2.1B<-(data$disability3==1 & !is.na(data$disability3)) & (data$agePerson3>18 & !is.na(data$agePerson3)) & data$married==1
  data$included.unearned.income[subset2.1B]<-rowMaxs(cbind((data$month.unearned.income3[subset2.1B]-data$income_disregard[subset2.1B]),0),na.rm=TRUE)
  data$remain.disregard[subset2.1B]<-rowMaxs(cbind((data$income_disregard[subset2.1B]-data$month.unearned.income3[subset2.1B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.1B]<-(1-data$earnings_disregard_pct[subset2.1B])*rowMaxs(cbind((data$month.earned.income3[subset2.1B]-data$earnings_disregard_amt[subset2.1B]-data$remain.disregard[subset2.1B]),0),na.rm=TRUE)
  data$ssi.income_3[subset2.1B]<-rowMaxs(cbind((data$included.unearned.income[subset2.1B]+data$included.earned.income[subset2.1B]),0),na.rm=TRUE)
  data$value.ssi.mnth_3[subset2.1B]<-rowMaxs(cbind((data$fbr_individual[subset2.1B]+data$ssp_spouse_as_fbr_individual[subset2.1B]-data$ssi.income_3[subset2.1B]),0),na.rm=TRUE)

  subset2.2B<-(data$disability4==1 & !is.na(data$disability4)) & (data$agePerson4>18 & !is.na(data$agePerson4)) & data$married==1
  data$included.unearned.income[subset2.2B]<-rowMaxs(cbind((data$month.unearned.income4[subset2.2B]-data$income_disregard[subset2.2B]),0),na.rm=TRUE)
  data$remain.disregard[subset2.2B]<-rowMaxs(cbind((data$income_disregard[subset2.2B]-data$month.unearned.income4[subset2.2B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.2B]<-(1-data$earnings_disregard_pct[subset2.2B])*rowMaxs(cbind((data$month.earned.income4[subset2.2B]-data$earnings_disregard_amt[subset2.2B]-data$remain.disregard[subset2.2B]),0),na.rm=TRUE)
  data$ssi.income_4[subset2.2B]<-rowMaxs(cbind((data$included.unearned.income[subset2.2B]+data$included.earned.income[subset2.2B]),0),na.rm=TRUE)
  data$value.ssi.mnth_4[subset2.2B]<-rowMaxs(cbind((data$fbr_individual[subset2.2B]+data$ssp_spouse_as_fbr_individual[subset2.2B]-data$ssi.income_4[subset2.2B]),0),na.rm=TRUE)

  subset2.3B<-(data$disability5==1 & !is.na(data$disability5)) & (data$agePerson5>18 & !is.na(data$agePerson5)) & data$married==1
  data$included.unearned.income[subset2.3B]<-rowMaxs(cbind((data$month.unearned.income5[subset2.3B]-data$income_disregard[subset2.3B]),0),na.rm=TRUE)
  data$remain.disregard[subset2.3B]<-rowMaxs(cbind((data$income_disregard[subset2.3B]-data$month.unearned.income5[subset2.3B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.3B]<-(1-data$earnings_disregard_pct[subset2.3B])*rowMaxs(cbind((data$month.earned.income5[subset2.3B]-data$earnings_disregard_amt[subset2.3B]-data$remain.disregard[subset2.3B]),0),na.rm=TRUE)
  data$ssi.income_5[subset2.3B]<-rowMaxs(cbind((data$included.unearned.income[subset2.3B]+data$included.earned.income[subset2.3B]),0),na.rm=TRUE)
  data$value.ssi.mnth_5[subset2.3B]<-rowMaxs(cbind((data$fbr_individual[subset2.3B]+data$ssp_spouse_as_fbr_individual[subset2.3B]-data$ssi.income_5[subset2.3B]),0),na.rm=TRUE)

  subset2.4B<-(data$disability6==1 & !is.na(data$disability6)) & (data$agePerson6>18 & !is.na(data$agePerson6)) & data$married==1
  data$included.unearned.income[subset2.4B]<-rowMaxs(cbind((data$month.unearned.income6[subset2.4B]-data$income_disregard[subset2.4B]),0),na.rm=TRUE)
  data$remain.disregard[subset2.4B]<-rowMaxs(cbind((data$income_disregard[subset2.4B]-data$month.unearned.income6[subset2.4B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.4B]<-(1-data$earnings_disregard_pct[subset2.4B])*rowMaxs(cbind((data$month.earned.income6[subset2.4B]-data$earnings_disregard_amt[subset2.4B]-data$remain.disregard[subset2.4B]),0),na.rm=TRUE)
  data$ssi.income_6[subset2.4B]<-rowMaxs(cbind((data$included.unearned.income[subset2.4B]+data$included.earned.income[subset2.4B]),0),na.rm=TRUE)
  data$value.ssi.mnth_6[subset2.4B]<-rowMaxs(cbind((data$fbr_individual[subset2.4B]+data$ssp_spouse_as_fbr_individual[subset2.4B]-data$ssi.income_6[subset2.4B]),0),na.rm=TRUE)

  # Add individual SSI values to get monthly household SSI value
  data$ssi.income<-rowSums(cbind(data$ssi.income, data$ssi.income_3[subset2.1B], data$ssi.income_4[subset2.2B], data$ssi.income_5[subset2.3B], data$ssi.income_6[subset2.4B],(-data$disab.work.exp)),na.rm=TRUE)
  data$value.ssi.mnth<-rowSums(cbind(data$value.ssi.mnth, data$value.ssi.mnth_3[subset2.1B], data$value.ssi.mnth_4[subset2.2B], data$value.ssi.mnth_5[subset2.3B], data$value.ssi.mnth_6[subset2.4B]),na.rm=TRUE)

  # Step 8: Determine the total annual SSI amount and ANY person in the household receiving SSI.Whether or not an adult receives SSI is important for determining non-MAGI Medicaid eligibility.
  data$ssi.recd<-12*data$value.ssi.mnth
  data$adlt1.ssi<-ifelse(data$ssi.recd>0 & (data$disability1==1 & !is.na(data$disability1)), data$adlt1.ssi<-1, data$adlt1.ssi<-0)
  data$adlt2.ssi<-ifelse(data$ssi.recd>0 & (data$disability2==1 & !is.na(data$disability2)), data$adlt2.ssi<-1, data$adlt2.ssi<-0)

  data$parent1.ssi<-ifelse(data$ssi.recd>0 & (data$disability1==1 & !is.na(data$disability1)), data$parent1.ssi<-1, data$parent1.ssi<-0)
  data$parent2.ssi<-ifelse(data$ssi.recd>0 & (data$disability2==1 & !is.na(data$disability2)), data$parent2.ssi<-1, data$parent2.ssi<-0)

  # Step 9: Deem assets for determining child eligibility
  data$total.countable.assets<-ifelse(data$parent1.ssi==0 & data$parent2.ssi==1,rowSums(cbind(data$ann.earned.income1,data$ann.unearned.income1,data$cash.distr,data$assets.car1),na.rm=TRUE),
                                      ifelse(data$parent1.ssi==1 & data$parent2.ssi==0,rowSums(cbind(data$ann.earned.income2,data$ann.unearned.income2,data$cash.distr,data$assets.car1),na.rm=TRUE),
                                             ifelse(data$parent1.ssi==0 & data$parent2.ssi==0,rowSums(cbind(data$ann.earned.income1,data$ann.earned.income2,data$ann.unearned.income1,data$ann.unearned.income2,data$cash,data$assets.car1),na.rm=TRUE),0)))

  data$asset.test<-ifelse(data$disabledkids>0 & ((data$total.countable.assets-data$asset_limit)/data$disabledkids)<data$asset_limit_child, data$asset.test<-1, data$asset.test<-0)

  # Step 10: Count number of SSI-eligible parents of eligible child and factor in allocations
  data$num.parents<-ifelse(data$married==1,2,1)
  data$parent1.tanf<-ifelse(data$value.tanf>0 & data$married==1, 1, 0)
  data$parent2.tanf<-ifelse(data$value.tanf>0 & data$married==1, 1, 0) # If family receives tanf and married, then assume both parents receive tanf

  data$deemable.parents.ssi<-rowMaxs(cbind(data$num.parents-rowMaxs(cbind(data$parent1.ssi,data$parent1.tanf,0))-rowMaxs(cbind(data$parent2.ssi,data$parent2.tanf,0)),0))
  data$num.parents.ssi<-data$parent1.ssi+data$parent2.ssi

  subset6<-data$deemable.parents.ssi==0 | data$num.parents.ssi>0
  data$deemed.income[subset6]<-0

  subset7<-data$num.parents==2 & rowSums(cbind(data$disability1+data$disability2),na.rm=TRUE)==1 & data$num.parents.ssi==0 # DID NOT INCLUDE MARRIED==1 BC ASSUME NUM.PARENTS==2 THEN THEY ARE MARRIED
  data$deemed.income[subset7]<-data$ssi.deemed.remain[subset7]

  subset8<-data$parent1.ssi==0 & data$parent1.tanf==0
  data$mnthly.unearned.income[subset8]<-rowMaxs(cbind(data$month.unearned.income1[subset8],0),na.rm=TRUE)
  data$mnthly.earned.income[subset8]<-rowMaxs(cbind(data$month.earned.income1[subset8],0),na.rm=TRUE)

  subset9<-data$parent2.ssi==0 & data$parent2.tanf==0
  data$mnthly.unearned.income[subset9]<-rowSums(cbind(data$mnthly.unearned.income[subset9],data$month.unearned.income2[subset9]),na.rm=TRUE)
  data$mnthly.earned.income[subset9]<-rowSums(cbind(data$mnthly.earned.income[subset9],data$month.earned.income2[subset9]),na.rm=TRUE)

  data$deemable.unearned.income<-rowMaxs(cbind(data$mnthly.unearned.income-(data$non_disabled_child_allocation*data$non_disabledkids),0),na.rm=TRUE)
  data$allocation.remainder<-rowMaxs(cbind((data$non_disabled_child_allocation*data$non_disabledkids)-data$mnthly.unearned.income,0),na.rm = TRUE)
  data$deemable.earned.income<-rowMaxs(cbind(data$mnthly.earned.income-data$allocation.remainder,0),na.rm=TRUE)
  data$deemed.unearned.income<-rowMaxs(cbind(data$deemable.unearned.income-data$income_disregard,0),na.rm=TRUE)
  data$deemed.disregard.remainder<-rowMaxs(cbind(data$income_disregard-data$deemable.unearned.income,0),na.rm=TRUE)
  data$deemed.earned.income<-rowMaxs(cbind(1-data$earnings_disregard_pct,0),na.rm=TRUE)*rowMaxs(cbind(data$deemable.earned.income-data$deemed.disregard.remainder-data$earnings_disregard_amt,0),na.rm=TRUE)
  data$deemed.income<-rowMaxs(cbind(data$deemed.unearned.income+data$deemed.earned.income-data$parental_allocation,0),na.rm=TRUE)

  # Step 11: Deem income to disabled children
  data$deemed.income.perchild<-rowMaxs(cbind((data$deemed.income + ((data$income.child_support/data$numkids)*.66))/data$disabledkids,0),na.rm=TRUE)

  # Step 12: Calculate SSI benefit for each disabled child in the home
  data$child.ssi.recd<-0

  person1<-case_when(data$agePerson1<18 & data$disability1==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person2<-case_when(data$agePerson2<18 & data$disability2==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person3<-case_when(data$agePerson3<18 & data$disability3==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person4<-case_when(data$agePerson4<18 & data$disability4==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person5<-case_when(data$agePerson5<18 & data$disability5==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person6<-case_when(data$agePerson6<18 & data$disability6==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person7<-case_when(data$agePerson7<18 & data$disability7==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person8<-case_when(data$agePerson8<18 & data$disability8==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person9<-case_when(data$agePerson9<18 & data$disability9==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person10<-case_when(data$agePerson10<18 & data$disability10==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person11<-case_when(data$agePerson11<18 & data$disability11==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)
  person12<-case_when(data$agePerson12<18 & data$disability12==1~rowMaxs(cbind(rowMaxs(cbind(data$fbr_difference,0))-rowMaxs(cbind(data$deemed.income.perchild-data$income_disregard,0)),0)),TRUE~0)

  data$child.ssi.recd<-person1+person2+person3+person4+person5+person6+person7+person8+person9+person10+person11+person12

  # Add child SSI to SSI received by adults in the household
  # Use Step 5 result: if countable asset<asset limit then value.ssi=0
  data$totalvalue.ssi.mnth<-case_when(subset0==TRUE~0,
                                 subset0==FALSE~data$value.ssi.mnth+data$child.ssi.recd)
  data$value.ssi<-data$totalvalue.ssi.mnth*12

  # create lag of the value of ssi. Once value of ssi reaches zero, disabled ppl may still qualify for medicaid
  data$hadssi1<-case_when(data$disability1==1 & !is.na(data$disability1) & data$value.ssi>0 ~ 0
                          ,data$disability1==1 & !is.na(data$disability1) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi2<-case_when(data$disability2==1 & !is.na(data$disability2) & data$value.ssi>0 ~ 0,
                          data$disability2==1 & !is.na(data$disability2) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi3<-case_when(data$disability3==1 & !is.na(data$disability3) & data$value.ssi>0 ~ 0,
                          data$disability3==1 & !is.na(data$disability3) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi4<-case_when(data$disability4==1 & !is.na(data$disability4) & data$value.ssi>0 ~ 0,
                          data$disability4==1 & !is.na(data$disability4) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi5<-case_when(data$disability5==1 & !is.na(data$disability5) & data$value.ssi>0 ~ 0,
                          data$disability5==1 & !is.na(data$disability5) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi6<-case_when(data$disability6==1 & !is.na(data$disability6) & data$value.ssi>0 ~ 0,
                          data$disability6==1 & !is.na(data$disability6) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi7<-case_when(data$disability7==1 & !is.na(data$disability7) & data$value.ssi>0 ~ 0,
                          data$disability7==1 & !is.na(data$disability7) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi8<-case_when(data$disability8==1 & !is.na(data$disability8) & data$value.ssi>0 ~ 0,
                          data$disability8==1 & !is.na(data$disability8) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi9<-case_when(data$disability9==1 & !is.na(data$disability9) & data$value.ssi>0 ~ 0,
                          data$disability9==1 & !is.na(data$disability9) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi10<-case_when(data$disability10==1 & !is.na(data$disability10) & data$value.ssi>0 ~ 0,
                          data$disability10==1 & !is.na(data$disability10) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi11<-case_when(data$disability11==1 & !is.na(data$disability11) & data$value.ssi>0 ~ 0,
                          data$disability11==1 & !is.na(data$disability11) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssi12<-case_when(data$disability12==1 & !is.na(data$disability12) & data$value.ssi>0 ~ 0,
                          data$disability12==1 & !is.na(data$disability12) & data$value.ssi<=0 ~ 1
                          ,TRUE ~ 0)

  # individual ssi amounts for purpose of adjusting other programs
  data$value.ssiAdlt1<-case_when(data$disabledAdlts>=1 & subset0==FALSE ~ (data$value.ssi.mnth/data$disabledAdlts)*12, TRUE~0)
  data$value.ssiAdlt2<-case_when(data$disabledAdlts>=2 & subset0==FALSE ~ (data$value.ssi.mnth/data$disabledAdlts)*12, TRUE~0)
  data$value.ssiAdlt3<-case_when(data$disabledAdlts>=3 & subset0==FALSE ~ (data$value.ssi.mnth/data$disabledAdlts)*12, TRUE~0)
  data$value.ssiAdlt4<-case_when(data$disabledAdlts>=4 & subset0==FALSE ~ (data$value.ssi.mnth/data$disabledAdlts)*12, TRUE~0)
  data$value.ssiAdlt5<-case_when(data$disabledAdlts>=5 & subset0==FALSE ~ (data$value.ssi.mnth/data$disabledAdlts)*12, TRUE~0)
  data$value.ssiAdlt6<-case_when(data$disabledAdlts>=6 & subset0==FALSE ~ (data$value.ssi.mnth/data$disabledAdlts)*12, TRUE~0)

  data$value.ssiChild1<-case_when(data$disabledkids>=1 & subset0==FALSE ~ (data$child.ssi.recd/data$disabledkids)*12, TRUE~0)
  data$value.ssiChild2<-case_when(data$disabledkids>=2 & subset0==FALSE ~ (data$child.ssi.recd/data$disabledkids)*12, TRUE~0)
  data$value.ssiChild3<-case_when(data$disabledkids>=3 & subset0==FALSE ~ (data$child.ssi.recd/data$disabledkids)*12, TRUE~0)
  data$value.ssiChild4<-case_when(data$disabledkids>=4 & subset0==FALSE ~ (data$child.ssi.recd/data$disabledkids)*12, TRUE~0)
  data$value.ssiChild5<-case_when(data$disabledkids>=5 & subset0==FALSE ~ (data$child.ssi.recd/data$disabledkids)*12, TRUE~0)
  data$value.ssiChild6<-case_when(data$disabledkids>=6 & subset0==FALSE ~ (data$child.ssi.recd/data$disabledkids)*12, TRUE~0)

  data.ssi<-data%>%
    select(value.ssi,value.ssiAdlt1,value.ssiAdlt2,value.ssiAdlt3,value.ssiAdlt4,value.ssiAdlt5,value.ssiAdlt6
           ,value.ssiChild1,value.ssiChild2,value.ssiChild3,value.ssiChild4,value.ssiChild5,value.ssiChild6
           ,hadssi1,hadssi2,hadssi3,hadssi4,hadssi5,hadssi6,hadssi7,hadssi8,hadssi9,hadssi10,hadssi11,hadssi12)

  #### TEST SSI OUTPUT ####
  # outputTest<-function(data){
  #   returnData<-data %>%
  #     select(countyortownName, stateAbbrev, income1, income2, income3, income4, income5, income6, income7, agePerson1,agePerson2
  #            ,agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, disability1, disability2,disability3, disability4
  #            ,disability5, disability6, disability7, married, assets.cash, assets.car1, assets.car2, assets.car3, income.gift
  #            ,income.child_support, income.investment,disab.work.exp,included.earned.income, included.unearned.income, remain.disregard,
  #            eligible.adlt.earnings, ineligible.adlt.earned.income, gross.ineligible.adlt.unearned.income, eligible.adlt.unearned.income, ineligible.adlt.unearned.income,
  #            remaining.deemable.income, ineligible.adlt.earned.income, included.unearned.income, ssi.deemed.remain, num.parents ,deemable.parents.ssi,
  #            num.parents.ssi, deemed.income, adlt1.ssi, adlt2.ssi, deemable.unearned.income, allocation.remainder, deemable.earned.income,
  #            deemed.disregard.remainder, deemed.earned.income, deemed.income.perchild, child.ssi.recd, value.ssi
  #            )
  #
  #   write.csv(returnData,paste0(getwd(),"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/Output/SSI_Output_ManyChildrenOneParents.csv"),row.names=FALSE)
  # }
  # outputTest(data)

  return(data.ssi)

}

# Supplemental Nutrition Assistance Program (SNAP)----

function.snapBenefit<-function(data){

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(snapData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(snapData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stateFIPS=unique(snapData$stateFIPS), famsize=unique(snapData$famsize), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-snapData[snapData$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stateFIPS","famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }# Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {snapData<-snapData %>% rbind(expand)}

    # We have historical rules
    data<-left_join(data, snapData, by=c("ruleYear","stateFIPS", "famsize"))

    # First, calculate total countable income
    data$income.gross<-rowSums(cbind(data$income,data$income.gift,data$income.child_support,data$income.investment,data$value.tanf,data$value.ssi,data$value.ssdi),na.rm=TRUE)
    # Determine if anyone in the household is elderly (above 60) or has a disability
    data$disabled_count<-rowSums(cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==1, na.rm=TRUE)
    data$elderly_count<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>60, na.rm=TRUE)
    data$kid_count<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<19, na.rm=TRUE)

    # Step I: Calculate Earned Income Deduction
    data$EarnedIncomeDeduction<-0.2*data$income #earned income only

    # Step II: Calculate adjusted income
    data$adjustedincome<-rowMaxs(cbind(data$income.gross-data$EarnedIncomeDeduction-12*data$StandardDeduction-data$netexp.childcare,0),na.rm=TRUE)

    # Step III: Calculate Utility Deductions
    data$UtilityDeduction<-0
    subset<-which((data$netexp.utilities>0 | data$HeatandEatState=="Yes") & data$HCSUA=="Mandatory") #liheap >0 should be in list of "|" statements runs after this program so will always be 0
    data$UtilityDeduction[subset]<-12*data$HCSUAValue[subset]

    subset<-(data$netexp.utilities>0 |  data$HeatandEatState=="Yes") & data$HCSUA=="Optional" #liheap >0 should be in list of "|" statements runs after this program so will always be 0
    data$UtilityDeduction[subset]<-rowMaxs(cbind(12*data$HCSUAValue[subset],data$netexp.utilities[subset]))


    # # Step V: Calculate Medical Expense Deduction (those on SSI,SSDI, and elderly can deduct their medical expenses)
    data$MedicalDeduction.person1<-case_when( (data$value.ssiAdlt1>0 | data$ssdiPIA1>0 | data$agePerson1 >60) ~ data$oop.add_for_elderlyordisabled
                                             , TRUE ~ 0)
    data$MedicalDeduction.person2<-case_when( (data$value.ssiAdlt2>0 | data$ssdiPIA2>0 | data$agePerson2 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$MedicalDeduction.person3<-case_when( (data$value.ssiAdlt3>0 | data$ssdiPIA3>0 | data$agePerson3 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$MedicalDeduction.person4<-case_when( (data$value.ssiAdlt4>0 | data$ssdiPIA4>0 | data$agePerson4 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$MedicalDeduction.person5<-case_when( (data$value.ssiAdlt5>0 | data$ssdiPIA5>0 | data$agePerson5 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$MedicalDeduction.person6<-case_when( (data$value.ssiAdlt6>0 | data$ssdiPIA6>0 | data$agePerson6 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)

    data$MedicalDeduction.person7<-case_when( (data$value.ssiChild1>0 | data$agePerson7 >60) ~ data$oop.add_for_elderlyordisabled # child age > 60 is impossible but it doesn't crash
                                              , TRUE ~ 0)
    data$MedicalDeduction.person8<-case_when( (data$value.ssiChild2>0 | data$agePerson8 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$MedicalDeduction.person9<-case_when( (data$value.ssiChild3>0 | data$agePerson9 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$MedicalDeduction.person10<-case_when( (data$value.ssiChild4>0 | data$agePerson10 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$MedicalDeduction.person11<-case_when( (data$value.ssiChild5>0 | data$agePerson11 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$MedicalDeduction.person12<-case_when( (data$value.ssiChild6>0 | data$agePerson12 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)

    data<-data %>%
      mutate(MedicalDeduction = MedicalDeduction.person1+MedicalDeduction.person2+MedicalDeduction.person3+MedicalDeduction.person4+MedicalDeduction.person5+MedicalDeduction.person6+MedicalDeduction.person7+MedicalDeduction.person8+MedicalDeduction.person9+MedicalDeduction.person10+MedicalDeduction.person11+MedicalDeduction.person12)

    data$MedicalDeduction<-rowMaxs(cbind(data$MedicalDeduction-data$MedicalExpenseDeductionFloor*12,0)) # Floor

    # Step IV: Calculate Net Income
    data$netincome<-rowMaxs(cbind(0,data$adjustedincome-rowMins(cbind(data$netexp.rentormortgage + data$UtilityDeduction + data$MedicalDeduction - 0.5*data$adjustedincome,data$MaxShelterDeduction*12))))

    # Step V-VI: Determine eligibility and calculate SNAP value

    #adjust for NY special rule that says those with dependent care expenses have a gross threhsold of 200% of FPL threshold ; others have 150%FPL)
    data$GrossIncomeEligibility[data$stateFIPS==36 & data$netexp.childcare >0]<- data$FPL[data$stateFIPS==36 & data$netexp.childcare >0]*2
    data$GrossIncomeEligibility[data$stateFIPS==36 & data$netexp.childcare ==0]<- data$FPL[data$stateFIPS==36 & data$netexp.childcare ==0]*1.5
    #adjust for NH speical rule that says ONLY those with dependents are eligbile for BBCE rules
    data$GrossIncomeEligibility[data$stateFIPS==33 & data$kid_count >0]<- data$FPL[data$stateFIPS==33 & data$kid_count >0]*1.85
    data$AssetTest_nonelddis[data$stateFIPS==33 & data$kid_count >0]<-999999
    data$AssetTest_Elder_Dis_over200FPL[data$stateFIPS==33 & data$kid_count >0]<-999999
    data$AssetTest_Elder_Dis_under200FPL[data$stateFIPS==33 & data$kid_count >0]<-999999

    data$GrossIncomeEligibility[data$stateFIPS==33 & data$kid_count ==0]<- data$FPL[data$stateFIPS==33 & data$kid_count ==0]*1.3
    data$AssetTest_nonelddis[data$stateFIPS==33 & data$kid_count==0]<-data$AssetTestFed_nonelddis[data$stateFIPS==33 & data$kid_count==0]
    data$AssetTest_Elder_Dis_over200FPL[data$stateFIPS==33 & data$kid_count ==0]<-data$AssetTestFed_Elder_Dis[data$stateFIPS==33 & data$kid_count ==0]
    data$AssetTest_Elder_Dis_under200FPL[data$stateFIPS==33 & data$kid_count ==0]<-data$AssetTestFed_Elder_Dis[data$stateFIPS==33 & data$kid_count ==0]
      # Determine categorical eligibility through SSI & TANF
    data$ssi_recipients_count<-rowSums(cbind(data$value.ssiAdlt1, data$value.ssiAdlt2, data$value.ssiAdlt3, data$value.ssiAdlt4, data$value.ssiAdlt5, data$value.ssiAdlt6, data$value.ssiChild1, data$value.ssiChild2, data$value.ssiChild3, data$value.ssiChild4, data$value.ssiChild5, data$value.ssiChild6)>0, na.rm=TRUE)
    data$not_categ_elig_ssi<-data$ssi_recipients_count<data$famsize # if not everyone in the family receives SSI

    data$not_categ_elig_tanf<-data$value.tanf==0 # if family doesn't receive TANF

    # Determine if the family FAILS income tests
    data$fail_grossIncomeTest<-data$income.gross>data$GrossIncomeEligibility & (data$not_categ_elig_tanf==TRUE & data$not_categ_elig_ssi==TRUE)
    #some states waive net income tests
    data$fail_netIncomeTest_nonelddis<-(data$disabled_count==0 & data$elderly_count==0) & data$netincome>data$NetIncomeEligibility_nonelddis
    data$fail_netIncomeTest_Elder_Dis<-(data$disabled_count>0 | data$elderly_count>0) & data$netincome>data$NetIncomeEligibility_Elder_Dis

    # Determine if the family FAILS asset tests
    data$fail_assetTest_regular<-(data$disabled_count==0 & data$elderly_count==0) & data$totalassets>data$AssetTest_nonelddis & (data$not_categ_elig_tanf==TRUE & data$not_categ_elig_ssi==TRUE) # regular asset test for families WITHOUT elderly/disabled
    data$fail_assetTest_Elderly_Disabled_under200FPL<-(data$disabled_count>0 | data$elderly_count>0) & data$income.gross <= 2*data$FPL & data$totalassets>data$AssetTest_Elder_Dis_under200FPL & (data$not_categ_elig_tanf==TRUE & data$not_categ_elig_ssi==TRUE) # regular asset test for families WITH elderly/disabled
    data$fail_assetTest_Elderly_Disabled_over200FPL<-(data$disabled_count>0 | data$elderly_count>0) & data$income.gross > 2*data$FPL & data$totalassets>data$AssetTest_Elder_Dis_over200FPL & (data$not_categ_elig_tanf==TRUE & data$not_categ_elig_ssi==TRUE) # special asset test for families WITH elderly/disabled and income > 200% FPL

    # Calculate the benefit if all income and asset tests are satisfied
    subset<-which(data$fail_grossIncomeTest==FALSE & data$fail_netIncomeTest_Elder_Dis==FALSE & data$fail_netIncomeTest_nonelddis==FALSE & data$fail_assetTest_regular==FALSE & data$fail_assetTest_Elderly_Disabled_under200FPL==FALSE & data$fail_assetTest_Elderly_Disabled_over200FPL==FALSE)

    data$snapValue<-0
    data$snapValue[subset]<-rowMins(cbind(rowMaxs(cbind(12*data$MaxBenefit[subset]-0.3*data$netincome[subset],12*data$MinBenefit[subset]),na.rm = TRUE),12*data$MaxBenefit[subset]),na.rm = TRUE)

    data$snapValue<-round(data$snapValue,0)

    return(data$snapValue)

  }


# Special Supplemental Nutrition Program for Women, Infants and Children (WIC)----

function.wicBenefit<-function(data){

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(wicData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(wicData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(AKorHI=unique(wicData$AKorHI), famsize=unique(wicData$famsize), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-wicData[wicData$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("AKorHI","famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }# Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {wicData<-wicData %>% rbind(expand)}

  # We have historical rules
  data<-left_join(data, wicData, by=c("famsize", "AKorHI", "ruleYear"))

  data$income.countable= data$income + data$income.gift + data$value.ssi + data$value.ssdi

  # Step 1: Calculate number of eligible infants, children, and women
  data$numkidsage1to4=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=4 & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=1, na.rm=TRUE)
  data$numinfants=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)==0, na.rm=TRUE)
  data$mom<-0
  data$mom[data$numinfants>0]<-1 #all our calc require there to be one adult and don't ask gender, so assume mom is in house if there is a child present

  #Step2: Determine categorical eligibility
  data$categorically.eligible<-0
  data$categorically.eligible[data$value.snap>0]<-1
  data$categorically.eligible[data$value.tanf>0]<-1
  data$categorically.eligible[data$value.medicaid.adult>0]<-1

  # Turn off for now. There is no categorical eligibility for children who on CHIP. We cannot currently discern between children on CHIP and on Medicaid for Children
  # By turning this on we are likely overestimating WIC eligibility
  #data$categorically.eligible[data$value.medicaid.child>0]<-1

  # Double check with Seth
  #data$categorically.eligible[data$value.ssi>0]<-1
  #data$categorically.eligible[data$value.ssdi>0]<-1

  # Step 3: Determine income eligibility
  data$income.eligible<-FALSE
  data$income.eligible[data$categorically.eligible == 1] <-TRUE
  data$income.eligible[data$income.countable < data$IncomeEligibility]<-TRUE

  # Step 5: Calculate WIC value
  data$value.WIC<- data$exp.wic
  data$value.WIC[data$income.eligible==FALSE]<-0

  return(data$value.WIC)

}


# Section 8 Housing Choice Voucher----

function.section8Benefit<-function(data){

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(section8Data$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(section8Data$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stcountyfips2010=as.character(unique(section8Data$stcountyfips2010)), numadults=unique(section8Data$numadults), numkids=unique(section8Data$numkids), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-section8Data[section8Data$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stcountyfips2010","numadults", "numkids"))%>%drop_na() %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing benefit data
    expandPastMiss<-expand.grid(stcountyfips2010=unique(section8Data$stcountyfips2010),numadults=unique(section8Data$numadults), numkids=unique(section8Data$numkids), Year=nonFutureYrs)
    # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, section8Data, by=c("stcountyfips2010", "numadults", "numkids"))
    expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
  }  # Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {section8Data<-section8Data %>% rbind(expand)}
  if(length(nonFutureYrs)>0) {section8Data<-section8Data %>% rbind(expandPastMiss2)}
  # Vars in section8Data are already in the data
  section8Data<-section8Data%>%select(-c("stateName","stateAbbrev","townFIPS","stateFIPS"))

    data<-left_join(data, section8Data, by=c("ruleYear","stcountyfips2010","countyortownName","numadults", "numkids"))

    data$income.countable= data$income + data$income.gift + data$value.tanf + data$value.ssi + data$value.ssdi + data$income.child_support

    # $400 Annual Flat Deduction for a disabled family
    data$disabilityDeduction<-ifelse(data$disability1==1 | data$disability2==1| data$disability3==1 | data$disability4==1 | data$disability5==1 | data$disability6==1 |
                                       data$ssdiPIA1>0 | data$ssdiPIA2>0 | data$ssdiPIA3>0 | data$ssdiPIA4>0 | data$ssdiPIA5>0 | data$ssdiPIA6>0
                                     ,400, 0)

    # Calculate Medical Expense Deduction (those on SSI,SSDI, and elderly can deduct their medical expenses)
    # unreimbursed expenses that allow a disabled adult family member to be employed
    data$exp.disability_work.person1<-case_when( (data$value.ssiAdlt1>0 | data$ssdiPIA1>0 | data$agePerson1 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$exp.disability_work.person2<-case_when( (data$value.ssiAdlt2>0 | data$ssdiPIA2>0 | data$agePerson2 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$exp.disability_work.person3<-case_when( (data$value.ssiAdlt3>0 | data$ssdiPIA3>0 | data$agePerson3 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$exp.disability_work.person4<-case_when( (data$value.ssiAdlt4>0 | data$ssdiPIA4>0 | data$agePerson4 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$exp.disability_work.person5<-case_when( (data$value.ssiAdlt5>0 | data$ssdiPIA5>0 | data$agePerson5 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$exp.disability_work.person6<-case_when( (data$value.ssiAdlt6>0 | data$ssdiPIA6>0 | data$agePerson6 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$exp.disability_work.person7<-case_when( (data$value.ssiChild1>0 | data$agePerson7 >60) ~ data$oop.add_for_elderlyordisabled # child age > 60 is impossible but it doesn't crash
                                              , TRUE ~ 0)
    data$exp.disability_work.person8<-case_when( (data$value.ssiChild2>0 | data$agePerson8 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$exp.disability_work.person9<-case_when( (data$value.ssiChild3>0 | data$agePerson9 >60) ~ data$oop.add_for_elderlyordisabled
                                              , TRUE ~ 0)
    data$exp.disability_work.person10<-case_when( (data$value.ssiChild4>0 | data$agePerson10 >60) ~ data$oop.add_for_elderlyordisabled
                                               , TRUE ~ 0)
    data$exp.disability_work.person11<-case_when( (data$value.ssiChild5>0 | data$agePerson11 >60) ~ data$oop.add_for_elderlyordisabled
                                               , TRUE ~ 0)
    data$exp.disability_work.person12<-case_when( (data$value.ssiChild6>0 | data$agePerson12 >60) ~ data$oop.add_for_elderlyordisabled
                                               , TRUE ~ 0)
    data<-data %>%
      mutate(exp.disability_work = exp.disability_work.person1+exp.disability_work.person2+exp.disability_work.person3+exp.disability_work.person4+exp.disability_work.person5+exp.disability_work.person6+exp.disability_work.person7+exp.disability_work.person8+exp.disability_work.person9+exp.disability_work.person10+exp.disability_work.person11+exp.disability_work.person12)

    # Calculate earnings of all disabled family members
    data$earnings_disabled_members<-rowMaxs(cbind(data$income1*data$disability1+data$income2*data$disability2+data$income3*data$disability3+data$income4*data$disability4+data$income5*data$disability5+data$income6*data$disability6, 0), na.rm = TRUE)

    # Calculate Disability Work Expense Deduction - cannot exceed the total earnings of all disabled family members
    data$DisabilityWorkExpDeduction<-rowMins(cbind(data$earnings_disabled_members, data$exp.disability_work))

    # Calculate other unreimbursed medical expenses
    data$exp.medical.unreimbursed<-0 # assume zero for now

    # Medical deductions is calculated as DisabilityWorkExpDeduction + unreimbursed medical expenses above 3% of gross income
    data$MedicalExpDeduction<-rowMaxs(cbind(data$DisabilityWorkExpDeduction + data$exp.medical.unreimbursed-data$income.countable*.03,0))

    data$netexp.childcare[is.na(data$netexp.childcare)] <- 0 # TURN NAS INTO ZEROES !
    # Step I: Calculate Adjusted Income
    data$adjustedincome<-rowMaxs(cbind(data$income.countable - data$numkids*data$DependentDeduction - data$netexp.childcare - data$disabilityDeduction - data$MedicalExpDeduction,0)) # HERE - subtract $400/m and subtract medical expense like SNAP (everything above 3% of gross income)

    # Step II: Determine Total Tenant Payment
    data$ttp<-rowMaxs(cbind(0.1*data$income.countable,0.3*data$adjustedincome))

    # Step III: Determine benefit value (voucher also covers GROSS rent, hence add utilities to rent)
    data$section8value<-0
    data$section8value<-rowMaxs(cbind(rowMins(cbind(data$exp.rentormortgage+data$exp.utilities,data$MaxBenefit))-data$ttp,0))

    data$section8value[data$ownorrent!="rent"]<-0

    data$section8value<-round(data$section8value,0)

    return(data$section8value)

  }


# Connecticut Rental Assistance Program (RAP) ----

function.RAPBenefit<-function(data){

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(section8Data$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(section8Data$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(countyortownName=as.character(unique(section8Data$countyortownName)), stateAbbrev=unique(section8Data$stateAbbrev), numadults=unique(section8Data$numadults), numkids=unique(section8Data$numkids), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-section8Data[section8Data$ruleYear==maxyearofdata, ]
    # For New England townships, grab the one that matches the data
    expand2<-expand2%>%filter(countyortownName==unique(data$countyortownName), stateAbbrev==unique(data$stateAbbrev))
    expand<-expand%>%left_join(expand2, by=c("countyortownName","stateAbbrev", "numadults", "numkids"))%>%drop_na() %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing benefit data
    expandPastMiss<-expand.grid(countyortownName=unique(section8Data$countyortownName),stateAbbrev=unique(section8Data$stateAbbrev),numadults=unique(section8Data$numadults), numkids=unique(section8Data$numkids), Year=nonFutureYrs)
    # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, section8Data, by=c("countyortownName", "stateAbbrev", "numadults", "numkids"))
    expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
  }  # Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {section8Data<-section8Data %>% rbind(expand)}
  if(length(nonFutureYrs)>0) {section8Data<-section8Data %>% rbind(expandPastMiss2)}
  section8Data<-section8Data%>%select(-c("stateName","countyortownName","stateAbbrev","townFIPS","stateFIPS"))

    data$income.countable = data$income

    # Step I: Calculate Adjusted Income
    data$adjustedincome<-rowMaxs(cbind(data$income.countable - data$numkids*data$DependentDeduction - data$netexp.childcare,0),na.rm = TRUE)

    # Step II: Determine Total Tenant Payment
    data$ttp<-rowMaxs(cbind(0.1*data$income.countable,0.4*data$adjustedincome),na.rm=TRUE)

    # Step III: Determine benefit value
    data$section8value<-0
    data$section8value<-rowMaxs(cbind(rowMins(cbind(data$exp.rentormortgage,data$MaxBenefit))-data$ttp,0),na.rm=TRUE)

    data$section8value[data$ownorrent!="rent"]<-0

    data$section8value<-round(data$section8value,0)

    return(data$section8value)

  }


# DC Specific Housing Program - Family Rehousing and Stabilization Program (FRSP) ----

function.FRSPBenefit<-function(data
                               , shareOfRent = 0.3 # User input - varies from 40 to 60%
                               , CareerMapIndicator1){ 
  
  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(section8Data$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(section8Data$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stcountyfips2010=as.character(unique(section8Data$stcountyfips2010)), numadults=unique(section8Data$numadults), numkids=unique(section8Data$numkids), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-section8Data[section8Data$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stcountyfips2010","numadults", "numkids"))%>%drop_na() %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing benefit data
    expandPastMiss<-expand.grid(stcountyfips2010=unique(section8Data$stcountyfips2010),numadults=unique(section8Data$numadults), numkids=unique(section8Data$numkids), Year=nonFutureYrs)
    # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, section8Data, by=c("stcountyfips2010", "numadults", "numkids"))
    expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
  }  # Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {section8Data<-section8Data %>% rbind(expand)}
  if(length(nonFutureYrs)>0) {section8Data<-section8Data %>% rbind(expandPastMiss2)}
  # Vars in section8Data are already in the data
  section8Data<-section8Data%>%select(-c("stateName","stateAbbrev","townFIPS","stateFIPS"))
  
  data<-left_join(data, section8Data, by=c("ruleYear","stcountyfips2010","countyortownName","numadults", "numkids"))

  data$income.countable = data$income
  
  # Step I: Calculate Adjusted Income
  data$adjustedincome<-rowMaxs(cbind(data$income.countable - data$numkids*data$DependentDeduction,0))
  
  # Step II: Determine Total Tenant Payment
  data$ttp<-shareOfRent*data$adjustedincome
  
  # Step III: Determine benefit value
  data$section8value<-0
  data$section8value<-rowMaxs(cbind(rowMins(cbind(data$exp.rentormortgage+data$exp.utilities,data$MaxBenefit))-data$ttp,0))
  
  data$section8value[data$ownorrent!="rent"]<-0
  
  data$section8value<-round(data$section8value,0)
  
  
  # FRSP participants only receive the benefit for one year
  years <- unique(data$Year)
  if(nrow(data)>1 & length(years)>1 & CareerMapIndicator1==FALSE){
    data<-data %>%
      mutate(min_Year=min(Year)) %>%
      mutate(section8value=ifelse(Year>(min_Year), 0, section8value))
    
    # Career MAP participants receive the benefit for 4 years (since 1 program year already elapsed)
  }else if (nrow(data)>1 & length(years)>1 & CareerMapIndicator1==TRUE){
    data<-data %>%
      mutate(min_Year=min(Year)) %>%
      mutate(section8value=ifelse(Year>(min_Year+3), 0, section8value))
  } else{
    abc <- 0
  }
  
  return(data$section8value)
  
}
# DC Specific Housing Program - CareerMAP ----
# function.careerMAP is called in the CLIFF tools. Not available to be called directly from the PRD
function.careerMap<-function(data, data_init){
  
  # Career MAP hold harmless fund available for up to 4 years only
  years<-unique(data$Year)
  minYear<-min(years)
  careerMapLength <- 3  # years careermap lasts
  
  temp <-data[data$Year %in% seq(minYear, minYear + careerMapLength), ]
  
  # Calculate initial value of selected benefits
  temp$value.snap_max<-data_init$value.snap[1]
  temp$value.tanf_max<-data_init$value.tanf[1]
  #  temp$netexp.childcare_min<-data_init$netexp.childcare[1]
  # temp$netexp.healthcare_min<-data_init$netexp.healthcare[1]
  
  # SNAP HHF
  temp$hhf_snap_full<-rowMaxs(cbind(temp$value.snap_max-temp$value.snap,0)) # full HHF
  temp$hhf_snap_rent<-rowMins(cbind(temp$hhf_snap_full,temp$netexp.rentormortgage)) # rent reduction
  temp$hhf_snap_cash<-temp$hhf_snap_full-temp$hhf_snap_rent # cash
  
  # Reduce rent and increase FRSP value by the amount of HHF
  temp$netexp.rentormortgage<-temp$netexp.rentormortgage-temp$hhf_snap_rent
  temp$value.section8<-temp$value.section8+temp$hhf_snap_rent
  
  # TANF HHF
  temp$hhf_tanf_full<-rowMaxs(cbind(temp$value.tanf_max-temp$value.tanf,0)) # full HHF
  temp$hhf_tanf_rent<-rowMins(cbind(temp$hhf_tanf_full,temp$netexp.rentormortgage)) # rent reduction
  temp$hhf_tanf_cash<-temp$hhf_tanf_full-temp$hhf_tanf_rent # cash
  
  # Reduce rent and increase FRSP value by the amount of HHF
  temp$netexp.rentormortgage<-temp$netexp.rentormortgage-temp$hhf_tanf_rent
  temp$value.section8<-temp$value.section8+temp$hhf_tanf_rent
  
  # Childcare expense HHF, applied only if receiving CCDF
  
  if(any(temp$value.CCDF > 0) == TRUE) {
    temp$hhf_childcare_full<-rowMaxs(cbind(temp$netexp.childcare,0)) # full HHF
  } else { temp$hhf_childcare_full <- 0 }
  
  # Healthcare expense HHF
  
  if(temp$healthcare.source[2] != "Medicaid/CHIP") {
    temp$hhf_healthcare_full<-rowMaxs(cbind(temp$netexp.healthcare,0)) # full HHF
    
    # Remove out-of-pocket misc. healthcare expenses since program only covers premiums
    temp$hhf_healthcare_full <- temp$hhf_healthcare_full - temp$oop.health.family.ALICE
    
  } else{temp$hhf_healthcare_full <- 0}

  # Total value of cash HHF
  temp$value.hhf<-temp$hhf_snap_cash+temp$hhf_tanf_cash+temp$hhf_childcare_full+temp$hhf_healthcare_full
  
  #  cap annual HHF at $10,000 per year.
  temp$value.hhf <- ifelse(temp$value.hhf >= 10000, 10000, temp$value.hhf)
  
  # Override selected values up to 5 years
  subset<-data$Year %in% seq(minYear, minYear + careerMapLength)
  
  data$netexp.rentormortgage[subset]<-temp$netexp.rentormortgage
  data$value.section8[subset]<-temp$value.section8
  data$value.hhf[subset]<-temp$value.hhf
  
  return(data)
  
}



# Low Income Home Energy Assistance Program (LIHEAP)----

function.liheapBenefit<-function(data){

  data$value.liheap<-0 # initialize

  # District of Columbia (stateFIPS==11)----

  if(11 %in% unique(data$stateFIPS)){ # make sure that state is in the list

    temp<-data[data$stateFIPS==11,]

    # One third of the utility is the energy spending (cite)
    temp$exp.utilities<-0.3*temp$exp.utilities

    temp<-left_join(temp, liheapData, by=c("stateFIPS", "famsize"))

    # Step I: Countable income
    temp$income<-temp$income+temp$value.ssi+temp$value.tanf+temp$value.snap

    temp$value.liheap[temp$income>=0 & temp$income<=temp$Bin1Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=0 & temp$income<=temp$Bin1Max], temp$MaxBenefit1[temp$income>=0 & temp$income<=temp$Bin1Max]))
    temp$value.liheap[temp$income>=temp$Bin1Max & temp$income<=temp$Bin2Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin1Max & temp$income<=temp$Bin2Max], temp$MaxBenefit2[temp$income>=temp$Bin1Max & temp$income<=temp$Bin2Max]))
    temp$value.liheap[temp$income>=temp$Bin2Max & temp$income<=temp$Bin3Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin2Max & temp$income<=temp$Bin3Max], temp$MaxBenefit3[temp$income>=temp$Bin2Max & temp$income<=temp$Bin3Max]))
    temp$value.liheap[temp$income>=temp$Bin3Max & temp$income<=temp$Bin4Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin3Max & temp$income<=temp$Bin4Max], temp$MaxBenefit4[temp$income>=temp$Bin3Max & temp$income<=temp$Bin4Max]))
    temp$value.liheap[temp$income>=temp$Bin4Max & temp$income<=temp$Bin5Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin4Max & temp$income<=temp$Bin5Max], temp$MaxBenefit5[temp$income>=temp$Bin4Max & temp$income<=temp$Bin5Max]))
    temp$value.liheap[temp$income>=temp$Bin5Max & temp$income<=temp$Bin6Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin5Max & temp$income<=temp$Bin6Max], temp$MaxBenefit6[temp$income>=temp$Bin5Max & temp$income<=temp$Bin6Max]))
    temp$value.liheap[temp$income>=temp$Bin6Max & temp$income<=temp$Bin7Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin6Max & temp$income<=temp$Bin7Max], temp$MaxBenefit7[temp$income>=temp$Bin6Max & temp$income<=temp$Bin7Max]))
    temp$value.liheap[temp$income>=temp$Bin7Max & temp$income<=temp$Bin8Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin7Max & temp$income<=temp$Bin8Max], temp$MaxBenefit8[temp$income>=temp$Bin7Max & temp$income<=temp$Bin8Max]))
    temp$value.liheap[temp$income>=temp$Bin8Max & temp$income<=temp$Bin9Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin8Max & temp$income<=temp$Bin9Max], temp$MaxBenefit9[temp$income>=temp$Bin8Max & temp$income<=temp$Bin9Max]))
    temp$value.liheap[temp$income>=temp$Bin9Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin9Max], temp$MaxBenefit10[temp$income>=temp$Bin9Max]))

    # Apply upper limit OR categorical eligibility
    subset<-(temp$income>temp$Bin10Max & temp$value.snap==0 & temp$value.tanf==0 & temp$value.ssi==0)
    temp$value.liheap[subset]<-0

    data$value.liheap[data$stateFIPS==11]<-temp$value.liheap

  }

  if(9 %in% unique(data$stateFIPS)){ # make sure that state is in the list

    temp<-data[data$stateFIPS==9,]

    # One third of the utility is the energy spending (cite)
    temp$exp.utilities<-0.3*temp$exp.utilities

    temp<-left_join(temp, liheapData, by=c("stateFIPS", "famsize"))

    # Step I: Countable income
    temp$income<-rowSums(cbind(temp$income,temp$value.ssi,temp$value.tanf,temp$value.snap,0),na.rm=TRUE)

    temp$value.liheap<-NA

    temp$value.liheap[temp$income>=0 & temp$income<=temp$Bin1Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=0 & temp$income<=temp$Bin1Max], temp$MaxBenefit1[temp$income>=0 & temp$income<=temp$Bin1Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin1Max & temp$income<=temp$Bin2Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin1Max & temp$income<=temp$Bin2Max], temp$MaxBenefit2[temp$income>=temp$Bin1Max & temp$income<=temp$Bin2Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin2Max & temp$income<=temp$Bin3Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin2Max & temp$income<=temp$Bin3Max], temp$MaxBenefit3[temp$income>=temp$Bin2Max & temp$income<=temp$Bin3Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin3Max & temp$income<=temp$Bin4Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin3Max & temp$income<=temp$Bin4Max], temp$MaxBenefit4[temp$income>=temp$Bin3Max & temp$income<=temp$Bin4Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin4Max & temp$income<=temp$Bin5Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin4Max & temp$income<=temp$Bin5Max], temp$MaxBenefit5[temp$income>=temp$Bin4Max & temp$income<=temp$Bin5Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin5Max & temp$income<=temp$Bin6Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin5Max & temp$income<=temp$Bin6Max], temp$MaxBenefit6[temp$income>=temp$Bin5Max & temp$income<=temp$Bin6Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin6Max & temp$income<=temp$Bin7Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin6Max & temp$income<=temp$Bin7Max], temp$MaxBenefit7[temp$income>=temp$Bin6Max & temp$income<=temp$Bin7Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin7Max & temp$income<=temp$Bin8Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin7Max & temp$income<=temp$Bin8Max], temp$MaxBenefit8[temp$income>=temp$Bin7Max & temp$income<=temp$Bin8Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin8Max & temp$income<=temp$Bin9Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin8Max & temp$income<=temp$Bin9Max], temp$MaxBenefit9[temp$income>=temp$Bin8Max & temp$income<=temp$Bin9Max]),na.rm=TRUE)
    temp$value.liheap[temp$income>=temp$Bin9Max]<-rowMins(cbind(temp$exp.utilities[temp$income>=temp$Bin9Max], temp$MaxBenefit10[temp$income>=temp$Bin9Max]),na.rm=TRUE)

    # Apply upper limit OR categorical eligibility
    subset<-(temp$income>temp$Bin10Max & temp$value.snap==0 & temp$value.tanf==0 & temp$value.ssi==0)
    temp$value.liheap[subset]<-0

    data$value.liheap[data$stateFIPS==9]<-temp$value.liheap

  }

  return(data$value.liheap)

}




# FATES CCDF ----

function.CCDFcopayFATES<-function(data
                                   , contelig.ccdf = TRUE){

   data$income <- data$income+data$income.gift

   data$totcopay<-NA

   data$InitialEligibility<-NA

   #count number of kids who still need childcare, after head start & preK taken into account
   # Calculate number of children in care (separately for age < 5 & age b/w 5 and 12)
   data$numkidsincare0to4<-rowSums(cbind(data$netexp.childcareperson1 >0 & data$agePerson1<=4
                                         ,data$netexp.childcareperson2 > 0 & data$agePerson2<=4
                                         ,data$netexp.childcareperson3 > 0 & data$agePerson3<=4
                                         ,data$netexp.childcareperson4 > 0 & data$agePerson4<=4
                                         ,data$netexp.childcareperson5 > 0 & data$agePerson5<=4
                                         ,data$netexp.childcareperson6 > 0 & data$agePerson6<=4
                                         ,data$netexp.childcareperson7 > 0 & data$agePerson7<=4
                                         ,data$netexp.childcareperson8 > 0 & data$agePerson8<=4
                                         ,data$netexp.childcareperson9 > 0 & data$agePerson9<=4
                                         ,data$netexp.childcareperson10 > 0 & data$agePerson10<=4
                                         ,data$netexp.childcareperson11 > 0 & data$agePerson11<=4
                                         ,data$netexp.childcareperson12 > 0 & data$agePerson12<=4),na.rm=TRUE)

   data$numkidsincare5to12<-rowSums(cbind(data$netexp.childcareperson1>0 & data$agePerson1>=5 & data$agePerson1<=12
                                          ,data$netexp.childcareperson2>0 & data$agePerson2>=5 & data$agePerson2<=12
                                          ,data$netexp.childcareperson3>0 & data$agePerson3>=5 & data$agePerson3<=12
                                          ,data$netexp.childcareperson4>0 & data$agePerson4>=5 & data$agePerson4<=12
                                          ,data$netexp.childcareperson5>0 & data$agePerson5>=5 & data$agePerson5<=12
                                          ,data$netexp.childcareperson6>0 & data$agePerson6>=5 & data$agePerson6<=12
                                          ,data$netexp.childcareperson7>0 & data$agePerson7>=5 & data$agePerson7<=12
                                          ,data$netexp.childcareperson8>0 & data$agePerson8>=5 & data$agePerson8<=12
                                          ,data$netexp.childcareperson9>0 & data$agePerson9>=5 & data$agePerson9<=12
                                          ,data$netexp.childcareperson10>0 & data$agePerson10>=5 & data$agePerson10<=12
                                          ,data$netexp.childcareperson11>0 & data$agePerson11>=5 & data$agePerson11<=12
                                          ,data$netexp.childcareperson12>0 & data$agePerson12>=5 & data$agePerson12<=12),na.rm=TRUE)

   data<- data %>%

     mutate(child1_0to4 = case_when(agePerson1>=0 & agePerson1<=4 ~1, TRUE ~ 0),
            child2_0to4 = case_when(agePerson2>=0 & agePerson2<=4 ~1, TRUE ~ 0),
            child3_0to4 = case_when(agePerson3>=0 & agePerson3<=4 ~1, TRUE ~ 0),
            child4_0to4 = case_when(agePerson4>=0 & agePerson4<=4 ~1, TRUE ~ 0),
            child5_0to4 = case_when(agePerson5>=0 & agePerson5<=4 ~1, TRUE ~ 0),
            child6_0to4 = case_when(agePerson6>=0 & agePerson6<=4 ~1, TRUE ~ 0),
            child7_0to4 = case_when(agePerson7>=0 & agePerson7<=4 ~1, TRUE ~ 0),
            child8_0to4 = case_when(agePerson8>=0 & agePerson8<=4 ~1, TRUE ~ 0),
            child9_0to4 = case_when(agePerson9>=0 & agePerson9<=4 ~1, TRUE ~ 0),
            child10_0to4 = case_when(agePerson10>=0 & agePerson10<=4 ~1, TRUE ~ 0),
            child11_0to4 = case_when(agePerson11>=0 & agePerson11<=4 ~1, TRUE ~ 0),
            child12_0to4 = case_when(agePerson12>=0 & agePerson12<=4 ~1, TRUE ~ 0)) %>%
     mutate(child1_5to12 = case_when(agePerson1>=5 & agePerson1<=12 ~1, TRUE ~ 0),
            child2_5to12 = case_when(agePerson2>=5 & agePerson2<=12 ~1, TRUE ~ 0),
            child3_5to12 = case_when(agePerson3>=5 & agePerson3<=12 ~1, TRUE ~ 0),
            child4_5to12 = case_when(agePerson4>=5 & agePerson4<=12 ~1, TRUE ~ 0),
            child5_5to12 = case_when(agePerson5>=5 & agePerson5<=12 ~1, TRUE ~ 0),
            child6_5to12 = case_when(agePerson6>=5 & agePerson6<=12 ~1, TRUE ~ 0),
            child7_5to12 = case_when(agePerson7>=5 & agePerson7<=12 ~1, TRUE ~ 0),
            child8_5to12 = case_when(agePerson8>=5 & agePerson8<=12 ~1, TRUE ~ 0),
            child9_5to12 = case_when(agePerson9>=5 & agePerson9<=12 ~1, TRUE ~ 0),
            child10_5to12 = case_when(agePerson10>=5 & agePerson10<=12 ~1, TRUE ~ 0),
            child11_5to12 = case_when(agePerson11>=5 & agePerson11<=12 ~1, TRUE ~ 0),
            child12_5to12 = case_when(agePerson12>=5 & agePerson12<=12 ~1, TRUE ~ 0)) %>%

     mutate(netexpchildcare0to4= netexp.childcareperson1*child1_0to4 + netexp.childcareperson2*child2_0to4 +netexp.childcareperson3*child3_0to4+netexp.childcareperson4*child4_0to4+ netexp.childcareperson5*child5_0to4 + netexp.childcareperson6*child6_0to4+ netexp.childcareperson7*child7_0to4+ netexp.childcareperson8*child8_0to4+ netexp.childcareperson9*child9_0to4+ netexp.childcareperson10*child10_0to4+ netexp.childcareperson11*child11_0to4+ netexp.childcareperson12*child12_0to4,
            childcareexp0to4= childcare.exp.person1*child1_0to4 + childcare.exp.person2*child2_0to4 +childcare.exp.person3*child3_0to4+childcare.exp.person4*child4_0to4+ childcare.exp.person5*child5_0to4 + childcare.exp.person6*child6_0to4 + childcare.exp.person7*child7_0to4 + childcare.exp.person8*child8_0to4 + childcare.exp.person9*child9_0to4 + childcare.exp.person10*child10_0to4 + childcare.exp.person11*child11_0to4 + childcare.exp.person12*child12_0to4,
            netexpchildcare5to12= netexp.childcareperson1*child1_5to12 + netexp.childcareperson2*child2_5to12 +netexp.childcareperson3*child3_5to12+netexp.childcareperson4*child4_5to12+ netexp.childcareperson5*child5_5to12 + netexp.childcareperson6*child6_5to12 + netexp.childcareperson7*child7_5to12 + netexp.childcareperson8*child8_5to12 + netexp.childcareperson9*child9_5to12 + netexp.childcareperson10*child10_5to12 + netexp.childcareperson11*child11_5to12 + netexp.childcareperson12*child12_5to12,
            childcareexp5to12= childcare.exp.person1*child1_5to12 + childcare.exp.person2*child2_5to12 +childcare.exp.person3*child3_5to12+childcare.exp.person4*child4_5to12+ childcare.exp.person5*child5_5to12 + childcare.exp.person6*child6_5to12 + childcare.exp.person7*child7_5to12 + childcare.exp.person8*child8_5to12 + childcare.exp.person9*child9_5to12 + childcare.exp.person10*child10_5to12 + childcare.exp.person11*child11_5to12 + childcare.exp.person12*child12_5to12,
            daysofcareneeded0to4 = netexpchildcare0to4/childcareexp0to4 * (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
            daysofcareneeded5to12 = netexpchildcare5to12/childcareexp5to12 * (0.5*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]))) #school age kids get pt care in summer & school year

   data$daysofcareneeded0to4[is.na(data$daysofcareneeded0to4)]<-0
   data$daysofcareneeded5to12[is.na(data$daysofcareneeded5to12)]<-0

   # Florida -----
   # Description:
   # Variation at the county level
   # Fixed copay per child
   # Daily frequency
   # Discount for a second child
   if(12 %in% unique(data$stateFIPS)){ # make sure that state is in the list

     temp<-data[data$stateFIPS==12,]

     # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
     years<-unique(data$ruleYear) # years in data set
     yearsinexpdata<- unique(ccdfData_FL$ruleYear) # rule years in benefit data
     yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
     yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
     # Create data for the future
     maxyearofdata<-max(ccdfData_FL$ruleYear) # collect latest year of benefit data
     futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
     if(length(futureYrs)>0){
       # Create data frame with future years
       expand<-expand.grid(stateFIPS=unique(ccdfData_FL$stateFIPS), AKorHI=unique(ccdfData_FL$AKorHI), famsize=unique(ccdfData_FL$famsize), countyortownName=unique(ccdfData_FL$countyortownName), Year=futureYrs)
       # Collect latest benefit data there is and merge w/data frame
       expand2<-ccdfData_FL[ccdfData_FL$ruleYear==maxyearofdata, ]
       expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
     }
     # Create data for past and gap years (missing data) - not the future
     nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
     if(length(nonFutureYrs)>0){
       #Create data frame with past years and year for which we are missing benefit data
       expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_FL$stateFIPS), AKorHI=unique(ccdfData_FL$AKorHI), famsize=unique(ccdfData_FL$famsize), countyortownName=unique(ccdfData_FL$countyortownName), Year=nonFutureYrs)
       # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
       expandPastMiss2<-left_join(expandPastMiss, ccdfData_FL, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName"))
       expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
       expandPastMiss2<-expandPastMiss2%>%
         group_by(Year)%>%
         mutate(minyeardiff = min(yeardiff))
       expandPastMiss2<-expandPastMiss2 %>%
         filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
     }  # Attach copied future, historical, and missing benefit data
     if(length(futureYrs)>0) {ccdfData_FL<-ccdfData_FL %>% rbind(expand)}
     if(length(nonFutureYrs)>0) {ccdfData_FL<-ccdfData_FL %>% rbind(expandPastMiss2)}

     #----------------------------------
     # Step 1: Assign copays
     #----------------------------------
     temp<-left_join(temp, ccdfData_FL, by=c("stateFIPS", "AKorHI", "countyortownName", "famsize","ruleYear"))

     # Adjust for the income disregard
     temp$income<-temp$income-12*temp$IncomeDisregard

     temp$FTcopay<-NA

     temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
     temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
     temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
     temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
     temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
     temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
     temp$FTcopay[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$CopayBin7[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
     temp$FTcopay[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]<-temp$CopayBin8[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]
     temp$FTcopay[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]<-temp$CopayBin9[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]
     temp$FTcopay[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]<-temp$CopayBin10[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]
     temp$FTcopay[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]<-temp$CopayBin11[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]
     temp$FTcopay[temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max]<-temp$CopayBin12[temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max]
     temp$FTcopay[temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max]<-temp$CopayBin13[temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max]
     temp$FTcopay[temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max]<-temp$CopayBin14[temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max]
     temp$FTcopay[temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max]<-temp$CopayBin15[temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max]
     temp$FTcopay[temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max]<-temp$CopayBin16[temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max]
     temp$FTcopay[temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max]<-temp$CopayBin17[temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max]

     #right now only for fates (function below)... but this may change!
     #IF Martin or St Lucie counties, also assign Bins 17-20:
     temp$FTcopay[temp$countyortownName %in% c("St. Lucie County","Martin County") & temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
     temp$FTcopay[temp$countyortownName %in% c("St. Lucie County","Martin County") & temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
     temp$FTcopay[temp$countyortownName %in% c("St. Lucie County","Martin County") & temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]



     #----------------------------------
     # Step 2: Calculate total copays
     #----------------------------------

     # Initialize
     temp$totcopay<-NA


     temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)

     # Set copay to zero if no children
     temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0

     # Implement discount for multiple children (only for the youngest child)
     # UNDER CONSTRUCTION

     temp$totcopay[is.na(temp$FTcopay)]<-NA
     # Note: code produces NAs for a good reason, because family is ineligible for CCDF
     # Copay is NOT zero for ineligible, but there are people who pay 0 copay

     # Adjust overage depending on whether states allow to charge it
     temp$childcare.overage[temp$OverageOption=="No"]<-0

     #View(temp[,c("InitialEligibility.y","income","totcopay","countyortownName", "FTcopay", "daysofcareneeded0to4", "daysofcareneeded5to12", "netexp.childcare")])

     # Merge back
     data$childcare.overage[data$stateFIPS==12]<-temp$childcare.overage
     data$totcopay[data$stateFIPS==12]<-temp$totcopay
     data$InitialEligibility[data$stateFIPS==12]<-temp$InitialEligibility.y
   }

    #View(data[,c("InitialEligibility","income","totcopay","countyortownName", "daysofcareneeded0to4", "daysofcareneeded5to12", "netexp.childcare")])


   data$totcopay<-as.numeric(data$totcopay)

   # Calculate value of CCDF as a portion of remaining net expenses covered by the subsidy
   data$value.CCDF<-rowMaxs(cbind(data$netexp.childcare-data$totcopay-data$childcare.overage,0))


   data$value.CCDF[is.na(data$value.CCDF)]<-0


   #determine initial elig  & use this elig if contelig=FALSE & its the first yr receivng the program
   #NOTE! need to do this by unique id if using a larger dataset w/ multiple ppl
   data<- data %>%
     mutate(incomeeligible_initial_ccdf=case_when(income<InitialEligibility ~1, TRUE~0),
            firstyearofCCDF=min(Year[value.CCDF>0 & incomeeligible_initial_ccdf==1], 9999, na.rm = TRUE), #min function cant handle missing values, so set to 9999 if person is never eligible for head start
            incomeeligible_CCDF=case_when(`contelig.ccdf`==TRUE ~1 , Year>firstyearofCCDF ~1, `contelig.ccdf`==FALSE & firstyearofCCDF!=9999 & Year<=firstyearofCCDF ~ incomeeligible_initial_ccdf, TRUE~0),
            value.CCDF=value.CCDF*incomeeligible_CCDF)


   #FATES program can only last a max of 3 years <-

   numyrsindataset<-unique(data$Year) #only run if n ot cross section dataset

   if(length(numyrsindataset)>1){
   data<-data %>%
     mutate(lagyear=lag(Year, k=1),
            FATES_not0=case_when(value.CCDF > 0 ~ 1),
            yrsofFATES=case_when(Year>lagyear ~ cumsum(!is.na(FATES_not0))),
            value.CCDF=case_when(yrsofFATES>3 ~ 0))
   }

   return(data$value.CCDF)



  } #end function.CCDF.fates


# Head Start ----

function.headstart<-function(data
                               , ageofPersonvar
                               , expchildcarevar
                               , headstart_ftorpt = "FT"
                               , contelig.headstart = TRUE
                               , contelig.earlyheadstart = TRUE){

    colnames(data)[colnames(data)==ageofPersonvar]<-"ageofchild"
    colnames(data)[colnames(data)==expchildcarevar]<-"exp.child"

    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(headstartData$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(headstartData$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(famsize=unique(headstartData$famsize), AKorHI=unique(headstartData$AKorHI), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-headstartData[headstartData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("famsize", "AKorHI"))%>%drop_na() %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(famsize=unique(headstartData$famsize), AKorHI=unique(headstartData$AKorHI), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, headstartData, by=c("famsize", "AKorHI"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {headstartData<-headstartData %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {headstartData<-headstartData %>% rbind(expandPastMiss2)}

    data<-data %>%
      left_join(headstartData, by=c("ruleYear", "famsize", "AKorHI"))

    data<-data %>%
      #initial eligibility vs cont eligiblity limit
      mutate(countableincome=income + value.ssi + value.ssdi + value.tanf) # + value.socsec (after we'll add social security)

     # Early Head Start Eligibility
      #Ditto for early  head start, except care is in summer too
     data<- data %>%
            mutate(ageeligible_earlyheadstart=case_when(!is.na(ageofchild) & ageofchild %in% c(0,1,2)~1,TRUE~0),
            earlyheadstart=case_when(ageeligible_earlyheadstart==1  & `headstart_ftorpt` == "FT" ~  1, #summer time care for early head start
                                ageeligible_earlyheadstart==1 & `headstart_ftorpt` == "PT" ~ .5,
                                TRUE ~ 0))

      #determine initial elig  & use this elig if contelig=FALSE & its the first yr receivng the program
      #NOTE! need to do this by unique id if using a larger dataset w/ multiple records per person
     data<- data %>%
         mutate(incomeeligible_initial_earlyheadstart = case_when(countableincome<IncomeEligibilityLimit|value.ssi>0|value.tanf>0
                                                                ~1, TRUE~0))
     # check to see if householdid exists - householdid created in TANF functions
     if ("householdid" %in% names(data) & length(data$householdid)>1){
       data<-data %>%
         group_by(householdid)}

     data<-data%>%
       mutate(firstyearofearlyheadstart = min(Year[incomeeligible_initial_earlyheadstart==1 & ageeligible_earlyheadstart==1], 9999, na.rm = TRUE)) %>% #min function cant handle missing values, so set to 9999 if person is never eligible for earlh head start
       ungroup()

     data<-data%>%
       mutate(incomeeligible_earlyheadstart=case_when(`contelig.earlyheadstart`==TRUE ~1
                                                        , `contelig.earlyheadstart`==FALSE & Year>firstyearofearlyheadstart ~1
                                                        , `contelig.earlyheadstart`==FALSE & firstyearofearlyheadstart!=9999 & Year<=firstyearofearlyheadstart ~ incomeeligible_initial_earlyheadstart
                                                        , TRUE~0),
                   earlyheadstart=earlyheadstart*incomeeligible_earlyheadstart,
                   value.earlyheadstart=earlyheadstart*exp.child)



     # Head Start Eligibility
     data<-data %>%
       #initial eligibility vs cont eligiblity limit
       mutate(
         #head start for 3 &4 yr olds typpically provides care during school year but not summer.
         #person can choose if the care during the school year is 'PT' or 'FT'
         ageeligible_headstart=case_when(!is.na(ageofchild) & ageofchild %in% c(3,4)~1,TRUE~0),

         headstart=case_when(ageeligible_headstart==1  & `headstart_ftorpt` == "FT" ~
                               (parameters.defaults$numberofSchoolDays[1]/(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),  #FT durign school yr covered, not summer
                             ageeligible_headstart==1 & `headstart_ftorpt` == "PT" ~
                               (parameters.defaults$numberofSchoolDays[1]*.5/(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])), #part time durign school yr covered, not summer
                             TRUE ~ 0))

     data<-data %>%
       mutate(incomeeligible_initial_headstart=case_when(countableincome<IncomeEligibilityLimit|value.ssi>0|value.tanf>0 ~1, TRUE~0))

     #determine initial elig  & use this elig if contelig=FALSE & its the first yr receivng the program
     #NOTE! need to do this by unique id if using a larger dataset w/ multiple records per person

     # # check to see if householdid exists
     if ("householdid" %in% names(data) & length(data$householdid)>1){
       data<-data%>%
         group_by(householdid)}

     data<-data %>%
       mutate(firstyearearlyheadstart_receive=min(Year[value.earlyheadstart>0], 9999, na.rm = TRUE) # Determine whether kid received Early Head Start at any point in the past
              ,firstyearofheadstart = min(Year[incomeeligible_initial_headstart==1 & ageeligible_headstart==1], 9999, na.rm = TRUE)) %>% #min function cant handle missing values, so set to 9999 if person is never eligible for earlh head start
       ungroup()

         #apply initial eliibility if flag set to TRUE or if need to redetermine kid's eligibility from Early HS to HS
        data<-data%>%
          mutate(incomeeligible_headstart=case_when((`contelig.headstart`==TRUE & firstyearearlyheadstart_receive == 9999) ~ 1
                                            , (`contelig.headstart`==FALSE | firstyearearlyheadstart_receive != 9999) & Year>firstyearofheadstart ~ 1
                                            , (`contelig.headstart`==FALSE | firstyearearlyheadstart_receive != 9999) & firstyearofheadstart!=9999 & Year<=firstyearofheadstart ~ incomeeligible_initial_headstart
                                            , TRUE ~ 0),
              headstart=headstart*incomeeligible_headstart,
              value.headstart=headstart*exp.child)

      returnData<-data %>%
              select(value.earlyheadstart, value.headstart)

      return(returnData)
  }


# Pre K program -----

function.prek<-function(data
                        , agePersonvar
                        , headstartPersonvar
                        , headstart_ftorpt = "PT"
                        , childcare.expvar
                        , preK_ftorpt = "PT"
                        , schoolagesummercare="PT"
                        , contelig.headstart = TRUE
                        , contelig.earlyheadstart = TRUE){


  data<-data %>%
    rename("agePerson" = agePersonvar
           ,"value.headstart"=headstartPersonvar
           ,"childcare.exp.person"=childcare.expvar)

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(schoolmealData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(schoolmealData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(AKorHI=unique(schoolmealData$AKorHI), famsize=unique(schoolmealData$famsize), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-schoolmealData[schoolmealData$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("AKorHI","famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }# Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {schoolmealData<-schoolmealData %>% rbind(expand)}

  # We have historical rules for school meals
  data<-data %>%
    left_join(preKData, by=c("stateName", "famsize"))  %>%
    left_join(schoolmealData, by=c("ruleYear","AKorHI","famsize")) %>%
    mutate(preKPerson=0) #initiate each person not to be eligible for preK

  #From UW:
  #180	average number of instruction days	https://nces.ed.gov/programs/statereform/tab5_14.asp
  #262	average number of workdays	https://www.opm.gov/policy-data-oversight/pay-leave/pay-administration/fact-sheets/computing-hourly-rates-of-pay-using-the-2087-hour-divisor/
  # 69%	Percentage of workdays in school (before/after school care assumed)
  #31%	Percentage of summer/vacation school days (full time care assumed)

  #FOR Cat elig later: determine if person is income elig for head start (regardles if they click the button):

  data$value.HeadStartperson<-function.headstart(data=data
                                                 , ageofPersonvar="agePerson"
                                                 , expchildcarevar="childcare.exp.person"
                                                 , headstart_ftorpt = `headstart_ftorpt`
                                                 , contelig.headstart = `contelig.headstart`
                                                 , contelig.earlyheadstart = `contelig.earlyheadstart`)[[2]]
  #############
  #Income Elig#
  #############

  #missing income means no program or cat elig only
  data$income.countable <- data$income + data$income.gift

  #compute portion of PREK costs covered by prek: 3&4 year olds are assumed to need 'full day care' during the school year (not full day care + after school care like 5-12)
  #preK will cover the school day FT costs if FT & 1/2 the school day FT costs if part day
  data<-data %>%
    mutate(preKPerson=case_when(!is.na(inc_elig_4) & income.countable < inc_elig_4 & agePerson==4 & preK_ftorpt== "FT" ~ (parameters.defaults$numberofSchoolDays[1]/
                                                                                                               (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                !is.na(inc_elig_4) & income.countable < inc_elig_4 & agePerson==4 & preK_ftorpt== "PT" ~ (parameters.defaults$numberofSchoolDays[1]*(1/2)/
                                                                                                                  (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                !is.na(inc_elig_3) & income.countable < inc_elig_3 & agePerson==3 & preK_ftorpt== "FT" ~ (parameters.defaults$numberofSchoolDays[1]*(1)/
                                                                                                                  (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                !is.na(inc_elig_3) & income.countable < inc_elig_3 & agePerson==3 & preK_ftorpt== "PT" ~ (parameters.defaults$numberofSchoolDays[1]*(1/2)/
                                                                                                                  (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
  ##########
  #Cat Elig#
  ##########

   #person will go to headstart after preK (during the school year).
  #otherwise if headstart is full-time then they dont go to preK (executed at end of code)
                                !is.na(cat_elig_4) & cat_elig_4=="headstart" & value.headstart>0 & `headstart_ftorpt` == "PT" &  agePerson==4 ~ (parameters.defaults$numberofSchoolDays[1]*(1/2)/
                                                                                                                                                 (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                !is.na(cat_elig_3) & cat_elig_3=="headstart" & value.headstart>0 & `headstart_ftorpt` == "PT" &  agePerson==3 ~ (parameters.defaults$numberofSchoolDays[1]*(1/2)/
                                                                                                                                                 (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                #if person is elig for headstart but not receiving it
                                !is.na(cat_elig_4) & cat_elig_4=="headstart" & value.headstart==0 & value.HeadStartperson >0 & agePerson==4 ~ (parameters.defaults$numberofSchoolDays[1]/
                                                                                                                                                 (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                !is.na(cat_elig_3) & cat_elig_3=="headstart" & value.headstart==0 & value.HeadStartperson >0 & agePerson==3 ~ (parameters.defaults$numberofSchoolDays[1]/
                                                                                                                                                 (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                TRUE ~ preKPerson))

  #For school lunch use income definition and cat elig rules (TANF, head start, child ssi) but not cat. eligiblity for SNAP bc SNAP takes in childcare exp so it would be an endless loop.
  #calculate cost assumpting preK program is FT (assumign school year is 195 days)
  data<-data %>%
  mutate(income.countable.schoollunch=income+income.gift + income.child_support + value.ssdi +value.ssi,  # + value.socsec (after we'll add social security)
          preKPerson=(case_when(!is.na(cat_elig_4) & cat_elig_4 %in% c("free or reduced price school lunch") & (income.countable.schoollunch<=IncomeBin2Max|value.tanf>0|value.headstart>0) & agePerson==4  ~ (parameters.defaults$numberofSchoolDays[1]/
                                                                                                                                                                                      (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                !is.na(cat_elig_3) & cat_elig_3 %in% c("free or reduced price school lunch") & (income.countable.schoollunch<=IncomeBin2Max|value.tanf>0|value.headstart>0) & agePerson==3 ~ (parameters.defaults$numberofSchoolDays[1]/
                                                                                                                                                                                     (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                !is.na(cat_elig_4) & cat_elig_4 %in% c("free lunch") & (income.countable.schoollunch<=IncomeBin1Max|value.tanf>0|value.headstart>0) & agePerson==4  ~ (parameters.defaults$numberofSchoolDays[1]/
                                                                                                                                                                                                                              (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                !is.na(cat_elig_3) & cat_elig_3 %in% c("free lunch") & (income.countable.schoollunch<=IncomeBin1Max|value.tanf>0|value.headstart>0) & agePerson==3 ~ (parameters.defaults$numberofSchoolDays[1]/
                                                                                                                                                                                                                           (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),

                                TRUE ~ preKPerson)),
    #ppl who receive head start only get PT value of PreK if headstart is PT. IF head start is full time, then we assume they are in head start all day during the school year and don't go to prek
          preKPerson=(case_when(preK_ftorpt=="PT" & !is.na(cat_elig_4) & value.headstart==0 & preKPerson >0 ~ (parameters.defaults$numberofSchoolDays[1]*(1/2)/
                                                                                                                 (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                      preK_ftorpt=="PT" & !is.na(cat_elig_3) & value.headstart==0 & preKPerson >0 ~ (parameters.defaults$numberofSchoolDays[1]*(1/2)/
                                                                                                                       (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])),
                                      value.headstart>0 &  `headstart_ftorpt` == "FT"   ~ 0,
                                      value.headstart>0 &  `headstart_ftorpt` == "PT" & preKPerson > 0 ~ (parameters.defaults$numberofSchoolDays[1]*(1/2)/
                                                                                                            (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])) ,

                                      TRUE ~ preKPerson)),
        value.preKPerson= childcare.exp.person*preKPerson)

  return(data$value.preKPerson)
}


# Medicaid ----

function.medicaid<-function(data
                              , ageofpersonvar
                              , disabilityofpersonvar
                              , hadssivar){

    colnames(data)[colnames(data)==ageofpersonvar]<-"ageofperson"
    colnames(data)[colnames(data)==disabilityofpersonvar]<-"disabilityofperson"
    colnames(data)[colnames(data)==hadssivar]<-"hadssi"

    ###############################
    # State Custom Logic
    ###############################
    # Minnesota has different definition of "infant" (up to age 2)
    data$ageofperson[data$ageofperson==2 & data$stateFIPS==27]<-0
    data$ageofperson[data$ageofperson==1 & data$stateFIPS==27]<-0

    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(medicaidData$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(medicaidData$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(medicaidData$stateFIPS), famsize=unique(medicaidData$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-medicaidData[medicaidData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS","famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }# Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {medicaidData<-medicaidData %>% rbind(expand)}


    # We have historical rules
    data<-data %>%
      left_join(medicaidData, by=c("ruleYear", "famsize", "stateFIPS")) %>%
      mutate(incomelimit=case_when(ageofperson==0~ incomelimit.child.Age0,
                                   ageofperson %in% c(1:5)~incomelimit.child.Age1to5,
                                   ageofperson %in% c(5:18)~incomelimit.child.Age6to18,
                                   ageofperson >=19 & hasdependent==1 ~ incomelimit.Adult.withDependent
                                   ,ageofperson >=19 & hasdependent==0 ~ incomelimit.Adult.noDependent)) %>%

      # Assign copay for a child based on income bin
      # NOTE: premiums are monthly - need to transform into annual
      mutate(Premium.Child=case_when(income<=IncomeBin1Max ~ Premium.Child.Bin1*12,
                                   income>IncomeBin1Max & income<=IncomeBin2Max ~ Premium.Child.Bin2*12,
                                   income>IncomeBin2Max & income<=IncomeBin3Max ~ Premium.Child.Bin3*12,
                                   income>IncomeBin3Max & income<=IncomeBin4Max ~ Premium.Child.Bin4*12,
                                   income>IncomeBin4Max & income<=IncomeBin5Max ~ Premium.Child.Bin5*12)) %>%

      # Assign copay values for adult/child
      mutate(premium.medicaid=case_when(ageofperson >= 19 & (income<=incomelimit  | (disabilityofperson==1 & value.ssi>0) | (hadssi==1 & income < medicaid.while.working.threshold)) ~ Premium.Adult*12
                                      , ageofperson < 19 & (income<=incomelimit | (disabilityofperson==1 & value.ssi > 0) | (hadssi==1 & income < medicaid.while.working.threshold)) ~ Premium.Child
                                      , TRUE ~ NA_real_))

    returnData<-data %>%
      select(premium.medicaid, PremiumType.CHIP)

    return(returnData)
  }


# Employer Sponsored Health Insurance ----

# function.EmployerHealthcare<-function(data
#                                         , famsizevar
#                                         , currentyr = 2021){
#
#     # Rename variables if necessary
#     colnames(data)[colnames(data)==famsizevar]<-"famsize.enrolled"
#     colnames(employerHealthcareData)[colnames(employerHealthcareData)=="famsize"]<-"famsize.enrolled"
#
#     data<-left_join(data, employerHealthcareData, by=c("stateFIPS", "famsize.enrolled"))
#
#     data$premium.healthcare.byfamsize<-data$premium.healthcare.byfamsize*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
#
#     #annualize and round
#     data$premium.healthcare.byfamsize<-round(data$premium.healthcare.byfamsize,0)*12
#
#     return(data$premium.healthcare.byfamsize)
#   }



# Affordable Care Act ACA ----

function.aca<-function(data){

    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(acaData$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(acaData$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(famsize=unique(acaData$famsize), AKorHI=unique(acaData$AKorHI), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-acaData[acaData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("famsize", "AKorHI")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }# Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {acaData<-acaData %>% rbind(expand)}

    data<-left_join(data, acaData, by=c("ruleYear", "famsize", "AKorHI"))
    data<-left_join(data,employerHealthcareData, by=c("stateFIPS", "famsize")) # Merge empl healthcare to obtain costs of the individual plan

    # Inflate/deflate
    data$premium.healthcare.individual<-12*data$premium.healthcare.individual*(1+parameters.defaults$inflationrate[1])^(data$ruleYear-data$yearofdata)

    data$premium.aca<-NA_real_

    data_preACA<-data[data$ruleYear<2014,]
    data_postACA<-data[data$ruleYear>=2014,]

    subset<-data_postACA$income>=data_postACA$IncomeLowerBound & data_postACA$income<=data_postACA$IncomeBin1Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin1Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeLowerBound[subset])*(data_postACA$ShareOfIncomesBin1Final[subset]-data_postACA$ShareOfIncomesBin1Initial[subset])/(data_postACA$IncomeBin1Max[subset]-data_postACA$IncomeLowerBound[subset]))

    subset<-data_postACA$income>data_postACA$IncomeBin1Max & data_postACA$income<=data_postACA$IncomeBin2Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin2Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin1Max[subset])*(data_postACA$ShareOfIncomesBin2Final[subset]-data_postACA$ShareOfIncomesBin2Initial[subset])/(data_postACA$IncomeBin2Max[subset]-data_postACA$IncomeBin1Max[subset]))

    subset<-data_postACA$income>data_postACA$IncomeBin2Max & data_postACA$income<=data_postACA$IncomeBin3Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin3Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin2Max[subset])*(data_postACA$ShareOfIncomesBin3Final[subset]-data_postACA$ShareOfIncomesBin3Initial[subset])/(data_postACA$IncomeBin3Max[subset]-data_postACA$IncomeBin2Max[subset]))

    subset<-data_postACA$income>data_postACA$IncomeBin3Max & data_postACA$income<=data_postACA$IncomeBin4Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin4Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin3Max[subset])*(data_postACA$ShareOfIncomesBin4Final[subset]-data_postACA$ShareOfIncomesBin4Initial[subset])/(data_postACA$IncomeBin4Max[subset]-data_postACA$IncomeBin3Max[subset]))

    subset<-data_postACA$income>data_postACA$IncomeBin4Max & data_postACA$income<=data_postACA$IncomeBin5Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin5Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin4Max[subset])*(data_postACA$ShareOfIncomesBin5Final[subset]-data_postACA$ShareOfIncomesBin5Initial[subset])/(data_postACA$IncomeBin5Max[subset]-data_postACA$IncomeBin4Max[subset]))

    subset<-data_postACA$income>data_postACA$IncomeBin5Max & data_postACA$income<=data_postACA$IncomeBin6Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin6Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin5Max[subset])*(data_postACA$ShareOfIncomesBin6Final[subset]-data_postACA$ShareOfIncomesBin6Initial[subset])/(data_postACA$IncomeBin6Max[subset]-data_postACA$IncomeBin5Max[subset]))

    subset<-data_postACA$income>data_postACA$IncomeBin6Max & data_postACA$income<=data_postACA$IncomeBin7Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin7Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin6Max[subset])*(data_postACA$ShareOfIncomesBin7Final[subset]-data_postACA$ShareOfIncomesBin7Initial[subset])/(data_postACA$IncomeBin7Max[subset]-data_postACA$IncomeBin6Max[subset]))


    # Implement affordability logic
    subset<-data_postACA$premium.healthcare.individual<=data_postACA$Affordability*data_postACA$income & data_postACA$empl_healthcare==1 # Considered affordable
    data_postACA$premium.aca[subset]<-NA_real_

    ###############################
    # State Custom Logic
    ###############################

    # Vermont (FIPS=50)
    # Vermont provides 1.5% reduction in costs share for families with income [100% FPL, 300% FPL]
    subset<-data_postACA$stateFIPS==50 & data_postACA$income>=data_postACA$IncomeLowerBound & data_postACA$income<=data_postACA$IncomeBin5Max
    data_postACA$premium.aca[subset]<-data_postACA$premium.aca[subset]-data_postACA$income[subset]*0.015 # Essentially the premium is reduced by 1.5% of income

    # Implement affordability logic
    subset<-data_postACA$stateFIPS==50 & data_postACA$premium.healthcare.individual<=data_postACA$Affordability*data_postACA$income & data_postACA$empl_healthcare==1 # Considered affordable
    data_postACA$premium.aca[subset]<-NA_real_

    data[data$ruleYear<2014,]<-data_preACA
    data[data$ruleYear>=2014,]<-data_postACA

    data$premium.aca<-round(data$premium.aca,0)

    return(data$premium.aca)
  }


# Medicare - IN PROGRESS ----
function.medicare<-function(data){

  # for testing - already done in Benefitscalcuator_functions file
  data%>%mutate(FilingStatus=(case_when(numadults==1 & numkids==0~1
                                 ,numadults==1 & numkids>0 ~ 3 # Head of the household
                                 ,numadults>=2~2
                                 ,TRUE~1)))
  # for testing
  data<-data%>%rename("year"=Year)

  data<-left_join(data, medicareMaritalstatData, by=c("year"))
  data%>%mutate
  (data$index_year_filing_status<-case_when(data$index_year_filing_status=="single"~1
                                            ,data$index_year_filing_status=="head_of_household"~3
                                            ,data$index_year_filing_status=="joint"~2
                                            ,data$index_year_filing_status=="married_filing_separate"~2))
  # if medicare is selected we proabbly have to ask which part they want to model
  # Part A SSDI eligibility: eligible if currently on or previously received SSDI
  data$PartAeligble1<-ifelse(data$ssdiPIA1>0 | hadssdi1==1, 1, 0)
  data$PartAeligble2<-ifelse(data$ssdiPIA2>0 | hadssdi2==1, 1, 0)
  data$PartAeligble3<-ifelse(data$ssdiPIA3>0 | hadssdi3==1, 1, 0)
  data$PartAeligble4<-ifelse(data$ssdiPIA4>0 | hadssdi4==1, 1, 0)
  data$PartAeligble5<-ifelse(data$ssdiPIA5>0 | hadssdi5==1, 1, 0)
  data$PartAeligble6<-ifelse(data$ssdiPIA6>0 | hadssdi6==1, 1, 0)
  # SSDI recipients recevie 'premium-free part A'
  data$medicareQuartersPaid<-35 # temporary - question about quarters they have paid medicare taxes (or our calculation of this numnber) is only asked if disability checkbox is selected

  data$PartApremium1<-ifelse(PartAeligble1==1 & data$ssdiRecdMnth1>0, 0,
                                 ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin1, data$part_a_premium_bin1,
                                        ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin2, data$part_a_premium_bin2,
                                               )))

  data$PartApremium2<-ifelse(PartAeligble1==1 & data$ssdiRecdMnth2>0, 0,
                                 ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin1, data$part_a_premium_bin1,
                                        ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin2, data$part_a_premium_bin2,
                                        )))

  data$PartApremium3<-ifelse(PartAeligble1==3 & data$ssdiRecdMnth3>0, 0,
                                 ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin1, data$part_a_premium_bin1,
                                        ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin2, data$part_a_premium_bin2,
                                        )))

  data$PartApremium4<-ifelse(PartAeligble4==1 & data$ssdiRecdMnth4>0, 0,
                                 ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin1, data$part_a_premium_bin1,
                                        ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin2, data$part_a_premium_bin2,
                                        )))

  data$PartApremium5<-ifelse(PartAeligble5==1 & data$ssdiRecdMnth5>0, 0,
                                 ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin1, data$part_a_premium_bin1,
                                        ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin2, data$part_a_premium_bin2,
                                        )))

  data$PartApremium6<-ifelse(PartAeligble6==1 & data$ssdiRecdMnth6>0, 0,
                                 ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin1, data$part_a_premium_bin1,
                                        ifelse(data$medicareQuartersPaid < data$medicare_quarters_paid_maxbin2, data$part_a_premium_bin2,
                                        )))

  # People who are eligible for Medicare Part A become eligible for Medicare Part B - may need to add income>incomebin1 & income<incomebin2
  # Should we ask about filing status and income in 2020 for each person or assume it's the same as now (and only person1&2 can be married)
  single<-data$index_year_finling_status==1
  data$PartBpremium1[single]<-case_when(data$PartAeligble1[single]==1 & data$income1[single]<=data$part_b_premium_incomebin1[single] ~ data$part_b_premium_bin1[single]
                                   , data$PartAeligble1[single]==1 & data$income1[single]<=data$part_b_premium_incomebin2[single] ~ data$part_b_premium_bin2[single]
                                   , data$PartAeligble1[single]==1 & data$income1[single]<=data$part_b_premium_incomebin3[single] ~ data$part_b_premium_bin3[single]
                                   , data$PartAeligble1[single]==1 & data$income1[single]<=data$part_b_premium_incomebin4[single] ~ data$part_b_premium_bin4[single]
                                   , data$PartAeligble1[single]==1 & data$income1[single]<=data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin5[single]
                                   , data$PartAeligble1[single]==1 & data$income1[single]>data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin6[single])

  data$PartBpremium2[single]<-case_when(data$PartAeligble1[single]==1 & data$income2[single]<=data$part_b_premium_incomebin1[single] ~ data$part_b_premium_bin1[single]
                                    , data$PartAeligble1[single]==1 & data$income2[single]<=data$part_b_premium_incomebin2[single] ~ data$part_b_premium_bin2[single]
                                    , data$PartAeligble1[single]==1 & data$income2[single]<=data$part_b_premium_incomebin3[single] ~ data$part_b_premium_bin3[single]
                                    , data$PartAeligble1[single]==1 & data$income2[single]<=data$part_b_premium_incomebin4[single] ~ data$part_b_premium_bin4[single]
                                    , data$PartAeligble1[single]==1 & data$income2[single]<=data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin5[single]
                                    , data$PartAeligble1[single]==1 & data$income2[single]>data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin6[single])

  data$PartBpremium3[single]<-case_when(data$PartAeligble3[single]==1 & data$income3[single]<=data$part_b_premium_incomebin1[single] ~ data$part_b_premium_bin1[single]
                                    , data$PartAeligble3[single]==1 & data$income3[single]<=data$part_b_premium_incomebin2[single] ~ data$part_b_premium_bin2[single]
                                    , data$PartAeligble3[single]==1 & data$income3[single]<=data$part_b_premium_incomebin3[single] ~ data$part_b_premium_bin3[single]
                                    , data$PartAeligble3[single]==1 & data$income3[single]<=data$part_b_premium_incomebin4[single] ~ data$part_b_premium_bin4[single]
                                    , data$PartAeligble3[single]==1 & data$income3[single]<=data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin5[single]
                                    , data$PartAeligble3[single]==1 & data$income3[single]>data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin6[single])

  data$PartBpremium4[single]<-case_when(data$PartAeligble4[single]==1 & data$income4[single]<=data$part_b_premium_incomebin1[single] ~ data$part_b_premium_bin1[single]
                                    , data$PartAeligble4[single]==1 & data$income4[single]<=data$part_b_premium_incomebin2[single] ~ data$part_b_premium_bin2[single]
                                    , data$PartAeligble4[single]==1 & data$income4[single]<=data$part_b_premium_incomebin3[single] ~ data$part_b_premium_bin3[single]
                                    , data$PartAeligble4[single]==1 & data$income4[single]<=data$part_b_premium_incomebin4[single] ~ data$part_b_premium_bin4[single]
                                    , data$PartAeligble4[single]==1 & data$income4[single]<=data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin5[single]
                                    , data$PartAeligble4[single]==1 & data$income4[single]>data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin6[single])

  data$PartBpremium5[single]<-case_when(data$PartAeligble5[single]==1 & data$income5[single]<=data$part_b_premium_incomebin1[single] ~ data$part_b_premium_bin1[single]
                                    , data$PartAeligble5[single]==1 & data$income5[single]<=data$part_b_premium_incomebin2[single] ~ data$part_b_premium_bin2[single]
                                    , data$PartAeligble5[single]==1 & data$income5[single]<=data$part_b_premium_incomebin3[single] ~ data$part_b_premium_bin3[single]
                                    , data$PartAeligble5[single]==1 & data$income5[single]<=data$part_b_premium_incomebin4[single] ~ data$part_b_premium_bin4[single]
                                    , data$PartAeligble5[single]==1 & data$income5[single]<=data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin5[single]
                                    , data$PartAeligble5[single]==1 & data$income5[single]>data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin6[single])

  data$PartBpremium6[single]<-case_when(data$PartAeligble6[single]==1 & data$income6[single]<=data$part_b_premium_incomebin1[single] ~ data$part_b_premium_bin1[single]
                                    , data$PartAeligble6[single]==1 & data$income6[single]<=data$part_b_premium_incomebin2[single] ~ data$part_b_premium_bin2[single]
                                    , data$PartAeligble6[single]==1 & data$income6[single]<=data$part_b_premium_incomebin3[single] ~ data$part_b_premium_bin3[single]
                                    , data$PartAeligble6[single]==1 & data$income6[single]<=data$part_b_premium_incomebin4[single] ~ data$part_b_premium_bin4[single]
                                    , data$PartAeligble6[single]==1 & data$income6[single]<=data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin5[single]
                                    , data$PartAeligble6[single]==1 & data$income6[single]>data$part_b_premium_incomebin5[single] ~ data$part_b_premium_bin6[single])
  filejointly<-data$index_year_finling_status==2
  data$PartBpremium1[filejointly]<-case_when(data$PartAeligble1[filejointly]==1 & data$income1[filejointly]<=data$part_b_premium_incomebin1[filejointly] ~ data$part_b_premium_bin1[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income1[filejointly]<=data$part_b_premium_incomebin2[filejointly] ~ data$part_b_premium_bin2[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income1[filejointly]<=data$part_b_premium_incomebin3[filejointly] ~ data$part_b_premium_bin3[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income1[filejointly]<=data$part_b_premium_incomebin4[filejointly] ~ data$part_b_premium_bin4[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income1[filejointly]<=data$part_b_premium_incomebin5[filejointly] ~ data$part_b_premium_bin5[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income1[filejointly]>data$part_b_premium_incomebin5[filejointly] ~ data$part_b_premium_bin6[filejointly])

  data$PartBpremium2[filejointly]<-case_when(data$PartAeligble1[filejointly]==1 & data$income2[filejointly]<=data$part_b_premium_incomebin1[filejointly] ~ data$part_b_premium_bin1[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income2[filejointly]<=data$part_b_premium_incomebin2[filejointly] ~ data$part_b_premium_bin2[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income2[filejointly]<=data$part_b_premium_incomebin3[filejointly] ~ data$part_b_premium_bin3[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income2[filejointly]<=data$part_b_premium_incomebin4[filejointly] ~ data$part_b_premium_bin4[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income2[filejointly]<=data$part_b_premium_incomebin5[filejointly] ~ data$part_b_premium_bin5[filejointly]
                                    , data$PartAeligble1[filejointly]==1 & data$income2[filejointly]>data$part_b_premium_incomebin5[filejointly] ~ data$part_b_premium_bin6[filejointly])

}

# Value of School Meals ----

function.schoolmeals<-function(data){

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(schoolmealData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(schoolmealData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(AKorHI=unique(schoolmealData$AKorHI), famsize=unique(schoolmealData$famsize), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-schoolmealData[schoolmealData$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("AKorHI","famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }# Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {schoolmealData<-schoolmealData %>% rbind(expand)}

    data<-left_join(data, schoolmealData, by=c("ruleYear", "famsize", "AKorHI"))

    data$value.schoolmeals<-0

    data$copay<-NA

    #value of school lunch depends on income
    data$income.countable=data$income+data$income.gift + data$income.child_support+data$value.ssdi + data$value.ssi + data$value.tanf
    data$copay[data$income.countable<=data$IncomeBin1Max]<-data$CopayBin1[data$income.countable<=data$IncomeBin1Max]
    data$copay[data$income.countable>data$IncomeBin1Max & data$income.countable<=data$IncomeBin2Max]<-data$CopayBin2[data$income.countable>data$IncomeBin1Max & data$income.countable<=data$IncomeBin2Max]
    data$copay<-data$copay*parameters.defaults$numberofSchoolDays[1] #Annualize

    # Universal free school meals for these states
    data$copay[data$ruleYear==2021] <- 0 # Pandemic-era rule for free meals
    data$copay[data$ruleYear>=2022 & data$stateAbbrev %in% c("CA", "ME", "MA")] <- 0
    data$copay[data$ruleYear>=2023 & data$stateAbbrev %in% c("CO", "IL", "MN", "NM", "VT")] <- 0
    data$copay[(data$ruleYear==2023 | data$ruleYear==2024) & data$stateAbbrev %in% c("MI", "GA", "NV")] <- 0

    #categorical elibiblity:

    #Step2: Determine categorical eligibility
    data$categorically.eligible<-0
    data$categorically.eligible[data$value.snap>0]<-1
    data$categorically.eligible[data$value.tanf>0]<-1
    data$categorically.eligible[data$value.HeadStart>0]<-1
    data$categorically.eligible[data$value.earlyHeadStart>0]<-1
    data$categorically.eligible[data$value.ssi>0]<-1

    data$copay[data$categorically.eligible==1]<-0

    data$copay<-data$copay*data$numkidsinschool
    data$value.schoolmeals<-data$exp.schoolMeals-data$copay

    data$value.schoolmeals[is.na(data$value.schoolmeals)]<-0

    data$value.schoolmeals[data$value.schoolmeals<0]<-0

    data$value.schoolmeals<-ifelse(data$numkidsinschool==0, 0, data$value.schoolmeals)

    data$value.schoolmeals<-round(data$value.schoolmeals,2)

    # NOTE! The value of free school meals depends on the cost assigned to school meals (exp.schoolMeals) which uses the number of kids in school
    # older than 5. numkidsinschool is recalculated after the PreK function bc kids in PreK may be younger than 5.

    return(data$value.schoolmeals)
  }


# TAXES AND TAX CREDITS----

# State Sales Tax ----

function.statesalestax<-function(data
                               , taxableexpensesvar
                               , foodexpensesvar){

    colnames(data)[colnames(data)==taxableexpensesvar]<-"taxableexpenses"
    colnames(data)[colnames(data)==foodexpensesvar]<-"foodexpenses"

    data<-left_join(data, salestaxData, by=c("stateFIPS"))

    data$value.salestax<-data$taxableexpenses*data$SalesTaxRate+data$foodexpenses*data$SalesTaxRate.Food

    return(data$value.salestax)
  }


# Federal Personal Income Tax ----

calculate_taxableamtofSSDI<-function(value.ssdi
                                      , income.base
                                      , baseamount
                                      , ssdi_taxable_test){

    if(.5* (value.ssdi) + (income.base) > baseamount) {
      Line12_ssdi = max(0,(max (0, .5 * (value.ssdi) + (income.base) - baseamount))- ssdi_taxable_test)
      Line14_ssdi = min(ssdi_taxable_test, (max(0,.5 * (value.ssdi) + (income.base) - (baseamount)*.5)))
      Line15_ssdi = min( .5 * (value.ssdi) ,Line14_ssdi)
      Line16_ssdi = Line12_ssdi*.85
      Line17_ssdi = Line15_ssdi  + Line16_ssdi
      TaxableamtofSSDI = min(Line17_ssdi, .5 * (value.ssdi))
    }else{
      TaxableamtofSSDI = 0
    }

    return(TaxableamtofSSDI)
  }




function.fedinctax<-function(data
                               , incomevar){

    colnames(data)[colnames(data)==incomevar]<-"income.base"

    data$income.base<-data$income.base+data$income.investment # For now, tax investment income at a income tax rate

    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(fedinctaxData$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(fedinctaxData$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(FilingStatus=unique(fedinctaxData$FilingStatus), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-fedinctaxData[fedinctaxData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("FilingStatus")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }# Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {fedinctaxData<-fedinctaxData %>% rbind(expand)}

    data<-left_join(data, fedinctaxData, by=c("ruleYear", "FilingStatus"))

    data$value.fedinctax<-0

    # Seven different tax brackets
    data$value.tax1<-0
    data$value.tax2<-0
    data$value.tax3<-0
    data$value.tax4<-0
    data$value.tax5<-0
    data$value.tax6<-0
    data$value.tax7<-0

    #step 1: Determine taxable amount of SSDI benefits

    data$TaxableamtofSSDI<-0
    if(sum(data$value.ssdi)>0 & !is.na(sum(data$value.ssdi))){

      data$TaxableamtofSSDI<- apply(data[ ,c("value.ssdi" # select relevant columns
                                            , "income.base"
                                            , "baseAmount_SSDI"
                                            , "SSDI_taxable_test")], 1 # apply function row-wise (to each row)
                                    , function(x) calculate_taxableamtofSSDI(x[1],x[2],x[3],x[4])) # apply function

    }

    data$income.base<-rowMaxs(cbind((data$income.base+data$TaxableamtofSSDI-data$Standard),0),na.rm=TRUE) # adjust countable income


    # Calculate income tax for each bracket separately
    data$taxableincome.bin1<-rowMaxs(cbind((data$income.base-0)-rowMaxs(cbind(data$income.base-data$IncomeBin1Max,0)),0))
    data$taxableincome.bin2<-rowMaxs(cbind((data$income.base-data$IncomeBin1Max)-rowMaxs(cbind(data$income.base-data$IncomeBin2Max,0)),0))
    data$taxableincome.bin3<-rowMaxs(cbind((data$income.base-data$IncomeBin2Max)-rowMaxs(cbind(data$income.base-data$IncomeBin3Max,0)),0))
    data$taxableincome.bin4<-rowMaxs(cbind((data$income.base-data$IncomeBin3Max)-rowMaxs(cbind(data$income.base-data$IncomeBin4Max,0)),0))
    data$taxableincome.bin5<-rowMaxs(cbind((data$income.base-data$IncomeBin4Max)-rowMaxs(cbind(data$income.base-data$IncomeBin5Max,0)),0))
    data$taxableincome.bin6<-rowMaxs(cbind((data$income.base-data$IncomeBin5Max)-rowMaxs(cbind(data$income.base-data$IncomeBin6Max,0)),0))
    data$taxableincome.bin7<-rowMaxs(cbind((data$income.base-data$IncomeBin6Max),0))

    data$value.tax1<-data$TaxRate1*data$taxableincome.bin1
    data$value.tax2<-data$TaxRate2*data$taxableincome.bin2
    data$value.tax3<-data$TaxRate3*data$taxableincome.bin3
    data$value.tax4<-data$TaxRate4*data$taxableincome.bin4
    data$value.tax5<-data$TaxRate5*data$taxableincome.bin5
    data$value.tax6<-data$TaxRate6*data$taxableincome.bin6
    data$value.tax7<-data$TaxRate7*data$taxableincome.bin7



    data$value.fedinctax<-data$value.tax1+data$value.tax2+data$value.tax3+data$value.tax4+data$value.tax5+data$value.tax6+data$value.tax7
    data$value.fedinctax<-round(data$value.fedinctax,0)

    return(data$value.fedinctax)

  }


# State Income Tax----

function.stateinctax<-function(data
                               , incomevar
                               , fedincometaxvar
                               , fedtaxcreditsvar){

  data<-data %>%
    rename("income.base" = incomevar
           ,"fedincometax" = fedincometaxvar
           , "fedtaxcredits" = fedtaxcreditsvar)

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(stateinctaxData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(stateinctaxData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stateName=unique(stateinctaxData$stateName), FilingStatus=unique(stateinctaxData$FilingStatus), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-stateinctaxData[stateinctaxData$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stateName","FilingStatus")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing benefit data
    expandPastMiss<-expand.grid(stateName=unique(stateinctaxData$stateName), FilingStatus=unique(stateinctaxData$FilingStatus), Year=nonFutureYrs)
    # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, stateinctaxData, by=c("stateName", "FilingStatus"))
    expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
  }  # Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {stateinctaxData<-stateinctaxData %>% rbind(expand)}
  if(length(nonFutureYrs)>0) {stateinctaxData<-stateinctaxData %>% rbind(expandPastMiss2)}

  data<-left_join(data, stateinctaxData, by=c("ruleYear", "stateFIPS", "stateName", "FilingStatus"))

  # 2025 ----
  # Tax foundation data footnote: c)
  if(2025 %in% unique(data$ruleYear) & "AL" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$AL_PhaseOutThreshold1 & data$stateAbbrev=="AL" & data$ruleYear==2025

    # Apply phase out if condition is met, phase out not to fall below minimum
    data$Standard[subset0] <- rowMaxs(cbind(data$Standard[subset0] - (data$income.base[subset0] - data$AL_PhaseOutThreshold1[subset0])*data$AL_PhaseOut1[subset0], 0), na.rm=TRUE)

    # Conditions for reducing Dependent Exemption
    subset1 <- data$income.base > data$AL_DepExReductionThreshold1 & data$stateAbbrev=="AL" & data$ruleYear==2025

    data$DependentExemption[subset1] <- data$AL_ReducedDependentExemption1[subset1]

    subset2 <- data$income.base > data$AL_DepExReductionThreshold2 & data$stateAbbrev=="AL" & data$ruleYear==2025

    data$DependentExemption[subset2] <- data$AL_ReducedDependentExemption2[subset2]
  }

  # f)
  if(2025 %in% unique(data$ruleYear) & "AZ" %in% unique(data$stateAbbrev)){

    # Dependet credit depends on kids
    numkidsunder17 <- rowSums(cbind(data$agePerson1, data$agePerson2,data$agePerson3,data$agePerson4,data$agePerson5,data$agePerson6,data$agePerson7,data$agePerson8,data$agePerson9,data$agePerson10,data$agePerson11,data$agePerson12) < data$AZ_FullCreditAgeLimit, na.rm=TRUE)
    numkidsover17 <- rowSums(cbind(data$agePerson1, data$agePerson2,data$agePerson3,data$agePerson4,data$agePerson5,data$agePerson6,data$agePerson7,data$agePerson8,data$agePerson9,data$agePerson10,data$agePerson11,data$agePerson12) >= data$AZ_FullCreditAgeLimit & cbind(data$agePerson1, data$agePerson2,data$agePerson3,data$agePerson4,data$agePerson5,data$agePerson6,data$agePerson7,data$agePerson8,data$agePerson9,data$agePerson10,data$agePerson11,data$agePerson12) < data$AZ_ReducedCreditAgeLimit, na.rm=TRUE)

    subset0 <- data$stateAbbrev=="AZ" & data$ruleYear==2025

    # Adjust the Dependent Exemption to find the amount per child with the appropriate credit applied based on age
    data$DependentExemption[subset0] <- (numkidsunder17[subset0]*data$DependentExemption[subset0] + numkidsover17[subset0]*data$AZ_ReducedCredit[subset0])/data$numkids[subset0]

  }

  # g & ll)
  if(2025 %in% unique(data$ruleYear) & "AR" %in% unique(data$stateAbbrev)){

    subset0 <- data$income.base < data$AR_Threshold1 & data$stateAbbrev=="AR" & data$ruleYear==2025

    data <- data %>%
      # tax brackets change when individual income is below limit. There are no special tax brackets for being married and under threshold limit.
      mutate(IncomeBin1Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin1MaxNew
                                       ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin1Max)
             ,IncomeBin2Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4)  ~ AR_IncomeBin2MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin2Max)
             ,IncomeBin3Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin3MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin3Max)
             ,IncomeBin4Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin4MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin4Max)
             ,IncomeBin5Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin5MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin5Max)
             ,IncomeBin6Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin6MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin6Max)

             #tax rates when individual income is below limit
             ,TaxRate1 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate1New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate1)
             ,TaxRate2 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate2New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate2)
             ,TaxRate3 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate3New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate3)
             ,TaxRate4 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate4New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate4)
             ,TaxRate5 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate5New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate5))

    # ll) # Reduce amount of tax due by deducting bracket adjustment - essentially works as a tax credit so add to Personal Exemption which also works as a credit in AR
    subset1 <- (data$income.base >= data$AR_Threshold1) & (data$income.base < data$AR_Threshold2) & data$FilingStatus %in% c(1,3,4) & data$stateAbbrev=="AR"

    data$PersonalExemption[subset1] <- data$PersonalExemption[subset1] + data$AR_BracketAdjustment[subset1] - (data$income.base[subset1] - data$AR_Threshold1[subset1])*data$AR_Phaseout1[subset1]

  }

  # k)
  if(2025 %in% unique(data$ruleYear) & ("CA" %in% unique(data$stateAbbrev))){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$CA_ExemptionThreshold1 & data$stateAbbrev=="CA" & data$ruleYear==2025

    # Apply phase out if condition is met, same phaseouts applied to both personal & dependent
    data$PersonalExemption[subset0] <- rowMaxs(cbind(data$PersonalExemption[subset0] - (data$income.base[subset0] - data$CA_ExemptionThreshold1[subset0])*data$CA_ExemptPhaseout1[subset0], 0), na.rm=TRUE)

    data$DependentExemption[subset0] <- rowMaxs(cbind(data$DependentExemption[subset0] - (data$income.base[subset0] - data$CA_ExemptionThreshold1[subset0])*data$CA_ExemptPhaseout1[subset0], 0), na.rm=TRUE)

  }

  # r. (footnote p also pertains to CT. This is done within the income tax calculation below)
  if(2025 %in% unique(data$ruleYear) & "CT" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$CT_ExemptThreshold1 & data$stateAbbrev=="CT" & data$ruleYear==2025

    # Apply phase out if condition is met
    data$PersonalExemption[subset0] <- rowMaxs(cbind(data$PersonalExemption[subset0] - (data$income.base[subset0] - data$CT_ExemptThreshold1[subset0])*data$CT_ExemptPhaseout1[subset0], 0), na.rm = TRUE)

  }
  # v)
  if(2025 %in% unique(data$ruleYear) & "IL" %in% unique(data$stateAbbrev)){

    # Exemptions go to $0 at threshold
    data$PersonalExemption[data$income.base > data$IL_ExemptThreshold1 & data$stateAbbrev=="IL"] <- 0
    data$DependentExemption[data$income.base > data$IL_ExemptThreshold1 & data$stateAbbrev=="IL"] <- 0

  }

  # z & aa)
  if(2025 %in% unique(data$ruleYear) & "MD" %in% unique(data$stateAbbrev)){

    # Standard deduction is a percentage of AGI with caps and minimums
    percentofAGI <- data$MD_PercentofAGI*data$income.base

    # Choose which Standard Deduction to use
    data$Standard <- case_when(percentofAGI <= data$MD_StandardMinimum & data$stateAbbrev=="MD" ~ data$MD_StandardMinimum

                               # Standard Deduction presented in the stateinctaxData is the maximum amount
                               ,percentofAGI > data$Standard & data$stateAbbrev=="MD" ~ data$Standard

                               ,TRUE ~ data$Standard)

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$MD_ExemptThreshold1 & data$stateAbbrev=="MD" & data$ruleYear==2025

    # Apply phase outs if conditions are met
    data$PersonalExemption[subset0] <- rowMaxs(cbind(data$PersonalExemption[subset0] - (data$income.base[subset0] - data$MD_ExemptThreshold1[subset0])*data$MD_ExemptPhaseout1[subset0], 0), na.rm=TRUE)

    data$DependentExemption[subset0] <- rowMaxs(cbind(data$DependentExemption[subset0] - (data$income.base[subset0] - data$MD_ExemptThreshold1[subset0])*data$MD_ExemptPhaseout1[subset0], 0), na.rm=TRUE)

  }

  # cc)
  if(2025 %in% unique(data$ruleYear) & "MN" %in% unique(data$stateAbbrev)){

    # Amount to reduce Standard Deduction: either a percentage of AGI or a percentage of the Standard Deduction
    subset0 <- data$income.base > data$MN_StandDeductThreshold & data$stateAbbrev=="MN" & data$ruleYear==2025

    reductionAmount <- rowMins(cbind(data$MN_PercentofAGI[subset0]*data$income.base[subset0], data$MN_PercentofStandDeduct[subset0]*data$Standard[subset0]), na.rm = TRUE)

    data$Standard[subset0] <- rowMaxs(cbind(data$Standard[subset0] - reductionAmount[subset0], 0),na.rm=TRUE)
  }

  # ee)
  if(2025 %in% unique(data$ruleYear) & "OH" %in% unique(data$stateAbbrev)){

    # Adjust Personal & Dependent Exemptions depending on income thresholds
    data$PersonalExemption <- case_when(data$income.base > data$OH_Threshold2 & data$stateAbbrev=="OH" & data$ruleYear==2025 ~ data$OH_Exemption2
                                        ,data$income.base > data$OH_Threshold1 & data$income.base <= data$OH_Threshold2 & data$stateAbbrev=="OH" & data$ruleYear==2025  ~ data$OH_Exemption1
                                        ,data$income.base <= data$OH_Threshold1 & data$stateAbbrev=="OH" & data$ruleYear==2025 ~ data$PersonalExemption
                                        ,TRUE ~ data$PersonalExemption)

    data$DependentExemption <- case_when(data$income.base > data$OH_Threshold2 & data$stateAbbrev=="OH" & data$ruleYear==2025 ~ data$OH_Exemption2
                                         ,data$income.base > data$OH_Threshold1 & data$income.base <= data$OH_Threshold2 & data$stateAbbrev=="OH" & data$ruleYear==2025~ data$OH_Exemption1
                                         ,data$income.base <= data$OH_Threshold1 & data$stateAbbrev=="OH" & data$ruleYear==2025~ data$DependentExemption
                                         , TRUE ~ data$DependentExemption)
  }

  # ff)
  if(2025 %in% unique(data$ruleYear) & "OR" %in% unique(data$stateAbbrev)){

    # Exemptions are $0 above the threshold
    data$PersonalExemption[data$income.base > data$OR_Threshold1 & data$stateAbbrev=="OR"] <- 0
    data$DependentExemption[data$income.base > data$OR_Threshold1 & data$stateAbbrev=="OR"] <- 0

  }

  # gg)
  if(2025 %in% unique(data$ruleYear) & "RI" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$RI_Threshold1 & data$stateAbbrev=="RI" & data$ruleYear==2025

    # Apply phase out if condition is met
    data$PersonalExemption[subset0] <- rowMaxs(cbind((data$PersonalExemption[subset0] - (data$income.base[subset0] - data$RI_Threshold1[subset0])*data$RI_Phaseout1[subset0]), 0), na.rm=TRUE)

    data$Standard[data$income.base > data$RI_Threshold2 & data$stateAbbrev=="RI"] <- 0
    data$PersonalExemption[data$income.base > data$RI_Threshold2 & data$stateAbbrev=="RI"] <- 0
    data$DependentExemption[data$income.base > data$RI_Threshold2 & data$stateAbbrev=="RI"] <- 0
  }

  # hh)
  if(2025 %in% unique(data$ruleYear) & "UT" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$UT_Threshold1 & data$stateAbbrev=="UT" & data$ruleYear==2025

    # Apply phase outs if conditions are met
    data$PersonalExemption[subset0] <- rowMaxs(cbind(data$PersonalExemption[subset0] - (data$income.base[subset0] - data$UT_Threshold1[subset0])*data$UT_Phaseout[subset0], 0), na.rm=TRUE)

  }

  # ii) VT: calculated at the end of the function

  # jj)
  if(2025 %in% unique(data$ruleYear) & "WI" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$WI_Threshold1 & data$stateAbbrev=="WI" & data$ruleYear==2025

    # Apply phase outs if conditions are met
    data$Standard[subset0] <- rowMaxs(cbind(data$Standard[subset0] - (data$income.base[subset0] - data$WI_Threshold1[subset0])*data$WI_Phaseout[subset0], 0), na.rm=TRUE)

  }

  # kk)
  if(2025 %in% unique(data$ruleYear) & "NM" %in% unique(data$stateAbbrev)){

    subset0 <- data$stateAbbrev=="NM" & data$ruleYear==2025

    # Adjust Dependent Exemption to find appropriate amount per child - NM provides exemption for all but one child
    data$DependentExemption[subset0] <- (data$DependentExemption[subset0]*data$numkids[subset0]-1)/data$numkids[subset0]

  }


  # 2024  ----
  #c)
  if(2024 %in% unique(data$ruleYear) & "AL" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$AL_PhaseOutThreshold1 & data$stateAbbrev=="AL" & data$ruleYear==2024

    # Apply phase out if condition is met, phase out not to fall below minimum
    data$Standard[subset0] <- rowMaxs(cbind(data$Standard[subset0] - (data$income.base[subset0] - data$AL_PhaseOutThreshold1[subset0])*data$AL_PhaseOut1[subset0], 0), na.rm=TRUE)

    # Conditions for reducing Dependent Exemption
    subset1 <- data$income.base > data$AL_DepExReductionThreshold1 & data$stateAbbrev=="AL" & data$ruleYear==2024

    data$DependentExemption[subset1] <- data$AL_ReducedDependentExemption1[subset1]

    subset2 <- data$income.base > data$AL_DepExReductionThreshold2 & data$stateAbbrev=="AL" & data$ruleYear==2024

    data$DependentExemption[subset2] <- data$AL_ReducedDependentExemption2[subset2]
  }

  # g)
  if(2024 %in% unique(data$ruleYear) & "AZ" %in% unique(data$stateAbbrev)){

    # Dependet credit depends on kids
    numkidsunder17 <- rowSums(cbind(data$agePerson1, data$agePerson2,data$agePerson3,data$agePerson4,data$agePerson5,data$agePerson6,data$agePerson7,data$agePerson8,data$agePerson9,data$agePerson10,data$agePerson11,data$agePerson12) < data$AZ_FullCreditAgeLimit, na.rm=TRUE)
    numkidsover17 <- rowSums(cbind(data$agePerson1, data$agePerson2,data$agePerson3,data$agePerson4,data$agePerson5,data$agePerson6,data$agePerson7,data$agePerson8,data$agePerson9,data$agePerson10,data$agePerson11,data$agePerson12) >= data$AZ_FullCreditAgeLimit & cbind(data$agePerson1, data$agePerson2,data$agePerson3,data$agePerson4,data$agePerson5,data$agePerson6,data$agePerson7,data$agePerson8,data$agePerson9,data$agePerson10,data$agePerson11,data$agePerson12) < data$AZ_ReducedCreditAgeLimit, na.rm=TRUE)

    subset0 <- data$stateAbbrev=="AZ" & data$ruleYear==2024

    # Adjust the Dependent Exemption to find the amount per child with the appropriate credit applied based on age
    data$DependentExemption[subset0] <- (numkidsunder17[subset0]*data$DependentExemption[subset0] + numkidsover17[subset0]*data$AZ_ReducedCredit[subset0])/data$numkids[subset0]

  }

  # h & oo)
  if(2024 %in% unique(data$ruleYear) & "AR" %in% unique(data$stateAbbrev)){

    subset0 <- data$income.base < data$AR_Threshold1 & data$stateAbbrev=="AR" & data$ruleYear==2024

    data <- data %>%
      # tax brackets change when individual income is below limit. There are no special tax brackets for being married and under threshold limit.
      mutate(IncomeBin1Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin1MaxNew
                                       ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin1Max)
             ,IncomeBin2Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4)  ~ AR_IncomeBin2MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin2Max)
             ,IncomeBin3Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin3MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin3Max)
             ,IncomeBin4Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin4MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin4Max)
             ,IncomeBin5Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin5MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin5Max)
             ,IncomeBin6Max = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_IncomeBin6MaxNew
                                        ,subset0==FALSE | FilingStatus %in% c(2) ~ IncomeBin6Max)

             #tax rates when individual income is below limit
             ,TaxRate1 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate1New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate1)
             ,TaxRate2 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate2New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate2)
             ,TaxRate3 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate3New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate3)
             ,TaxRate4 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate4New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate4)
             ,TaxRate5 = case_when(subset0==TRUE & FilingStatus %in% c(1,3,4) ~ AR_TaxRate5New
                                   ,subset0==FALSE | FilingStatus %in% c(2) ~ TaxRate5))

    # oo) # Reduce amount of tax due by deducting bracket adjustment - essentially works as a tax credit so add to Personal Exemption which also works as a credit in AR
    subset1 <- (data$income.base >= data$AR_Threshold1) & (data$income.base < data$AR_Threshold2) & data$FilingStatus %in% c(1,3,4) & data$stateAbbrev=="AR"

    data$PersonalExemption[subset1] <- data$PersonalExemption[subset1] + data$AR_BracketAdjustment[subset1] - (data$income.base[subset1] - data$AR_Threshold1[subset1])*data$AR_Phaseout1[subset1]

  }

  # l)
  if(2024 %in% unique(data$ruleYear) & ("CA" %in% unique(data$stateAbbrev))){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$CA_ExemptionThreshold1 & data$stateAbbrev=="CA" & data$ruleYear==2024

    # Apply phase out if condition is met, same phaseouts applied to both personal & dependent
    data$PersonalExemption[subset0] <- rowMaxs(cbind(data$PersonalExemption[subset0] - (data$income.base[subset0] - data$CA_ExemptionThreshold1[subset0])*data$CA_ExemptPhaseout1[subset0], 0), na.rm=TRUE)

    data$DependentExemption[subset0] <- rowMaxs(cbind(data$DependentExemption[subset0] - (data$income.base[subset0] - data$CA_ExemptionThreshold1[subset0])*data$CA_ExemptPhaseout1[subset0], 0), na.rm=TRUE)

  }

  # s)
  if(2024 %in% unique(data$ruleYear) & "CT" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$CT_ExemptThreshold1 & data$stateAbbrev=="CT" & data$ruleYear==2024

    # Apply phase out if condition is met
    data$PersonalExemption[subset0] <- rowMaxs(cbind(data$PersonalExemption[subset0] - (data$income.base[subset0] - data$CT_ExemptThreshold1[subset0])*data$CT_ExemptPhaseout1[subset0], 0), na.rm = TRUE)

  }
  # x)
  if(2024 %in% unique(data$ruleYear) & "IL" %in% unique(data$stateAbbrev)){

    # Exemptions go to $0 at threshold
    data$PersonalExemption[data$income.base > data$IL_ExemptThreshold1 & data$stateAbbrev=="IL"] <- 0
    data$DependentExemption[data$income.base > data$IL_ExemptThreshold1 & data$stateAbbrev=="IL"] <- 0

  }
  # aa) need to think about how to apply phaseouts

  # bb & cc)
  if(2024 %in% unique(data$ruleYear) & "MD" %in% unique(data$stateAbbrev)){

    # Standard deduction is a percentage of AGI with caps and minimums
    percentofAGI <- data$MD_PercentofAGI*data$income.base

    # Choose which Standard Deduction to use
    data$Standard <- case_when(percentofAGI <= data$MD_StandardMinimum & data$stateAbbrev=="MD" ~ data$MD_StandardMinimum

                               # Standard Deduction presented in the stateinctaxData is the maximum amount
                               ,percentofAGI > data$Standard & data$stateAbbrev=="MD" ~ data$Standard

                               ,TRUE ~ data$Standard)

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$MD_ExemptThreshold1 & data$stateAbbrev=="MD" & data$ruleYear==2024

    # Apply phase outs if conditions are met
    data$PersonalExemption[subset0] <- rowMaxs(cbind(data$PersonalExemption[subset0] - (data$income.base[subset0] - data$MD_ExemptThreshold1[subset0])*data$MD_ExemptPhaseout1[subset0], 0), na.rm=TRUE)

    data$DependentExemption[subset0] <- rowMaxs(cbind(data$DependentExemption[subset0] - (data$income.base[subset0] - data$MD_ExemptThreshold1[subset0])*data$MD_ExemptPhaseout1[subset0], 0), na.rm=TRUE)

  }

  # ee)
  if(2024 %in% unique(data$ruleYear) & "MN" %in% unique(data$stateAbbrev)){

    # Amount to reduce Standard Deduction: either a percentage of AGI or a percentage of the Standard Deduction
    subset0 <- data$income.base > data$MN_StandDeductThreshold & data$stateAbbrev=="MN" & data$ruleYear==2024

    reductionAmount <- rowMins(cbind(data$MN_PercentofAGI[subset0]*data$income.base[subset0], data$MN_PercentofStandDeduct[subset0]*data$Standard[subset0]), na.rm = TRUE)

    data$Standard[subset0] <- rowMaxs(cbind(data$Standard[subset0] - reductionAmount[subset0], 0),na.rm=TRUE)
  }

  # ff)
  if(2024 %in% unique(data$ruleYear) & "MT" %in% unique(data$stateAbbrev)){

    # Standard deduction is a percentage of AGI with caps and minimums
    percentofAGI <- data$MT_PercentofAGI*data$income.base

    # Choose which Standard Deduction to use
    data$Standard <- case_when(percentofAGI <= data$MT_StandardMinimum & data$stateAbbrev=="MT" ~ data$MT_StandardMinimum

                               # Standard Deduction presented in the stateinctaxData is the maximum amount
                               ,percentofAGI > data$Standard & data$stateAbbrev=="MT"~ data$Standard

                               ,TRUE ~ data$Standard)
  }

  # hh)
  if(2024 %in% unique(data$ruleYear) & "OH" %in% unique(data$stateAbbrev)){

    # Adjust Personal & Dependent Exemptions depending on income thresholds
    data$PersonalExemption <- case_when(data$income.base > data$OH_Threshold2 & data$stateAbbrev=="OH" & data$ruleYear==2024 ~ data$OH_Exemption2
                                        ,data$income.base > data$OH_Threshold1 & data$income.base <= data$OH_Threshold2 & data$stateAbbrev=="OH" & data$ruleYear==2024  ~ data$OH_Exemption1
                                        ,data$income.base <= data$OH_Threshold1 & data$stateAbbrev=="OH" & data$ruleYear==2024 ~ data$PersonalExemption
                                        ,TRUE ~ data$PersonalExemption)

    data$DependentExemption <- case_when(data$income.base > data$OH_Threshold2 & data$stateAbbrev=="OH" & data$ruleYear==2024 ~ data$OH_Exemption2
                                         ,data$income.base > data$OH_Threshold1 & data$income.base <= data$OH_Threshold2 & data$stateAbbrev=="OH" & data$ruleYear==2024~ data$OH_Exemption1
                                         ,data$income.base <= data$OH_Threshold1 & data$stateAbbrev=="OH" & data$ruleYear==2024~ data$DependentExemption
                                         , TRUE ~ data$DependentExemption)
  }

  # ii)
  if(2024 %in% unique(data$ruleYear) & "OR" %in% unique(data$stateAbbrev)){

    # Exemptions are $0 above the threshold
    data$PersonalExemption[data$income.base > data$OR_Threshold1 & data$stateAbbrev=="OR"] <- 0
    data$DependentExemption[data$income.base > data$OR_Threshold1 & data$stateAbbrev=="OR"] <- 0

  }

  # jj)
  if(2024 %in% unique(data$ruleYear) & "RI" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$RI_Threshold1 & data$stateAbbrev=="RI" & data$ruleYear==2024

    # Apply phase out if condition is met
    data$PersonalExemption[subset0] <- rowMaxs(cbind((data$PersonalExemption[subset0] - (data$income.base[subset0] - data$RI_Threshold1[subset0])*data$RI_Phaseout1[subset0]), 0), na.rm=TRUE)

    data$Standard[data$income.base > data$RI_Threshold2 & data$stateAbbrev=="RI"] <- 0
    data$PersonalExemption[data$income.base > data$RI_Threshold2 & data$stateAbbrev=="RI"] <- 0
    data$DependentExemption[data$income.base > data$RI_Threshold2 & data$stateAbbrev=="RI"] <- 0
  }

  # kk)
  if(2024 %in% unique(data$ruleYear) & "UT" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$UT_Threshold1 & data$stateAbbrev=="UT" & data$ruleYear==2024

    # Apply phase outs if conditions are met
    data$PersonalExemption[subset0] <- rowMaxs(cbind(data$PersonalExemption[subset0] - (data$income.base[subset0] - data$UT_Threshold1[subset0])*data$UT_Phaseout[subset0], 0), na.rm=TRUE)

  }

  # ll) calculated at the end of the function

  # mm)
  if(2024 %in% unique(data$ruleYear) & "WI" %in% unique(data$stateAbbrev)){

    # Condition for standard deduction phase out
    subset0 <- data$income.base > data$WI_Threshold1 & data$stateAbbrev=="WI" & data$ruleYear==2024

    # Apply phase outs if conditions are met
    data$Standard[subset0] <- rowMaxs(cbind(data$Standard[subset0] - (data$income.base[subset0] - data$WI_Threshold1[subset0])*data$WI_Phaseout[subset0], 0), na.rm=TRUE)

  }

  # nn)
  if(2024 %in% unique(data$ruleYear) & "NM" %in% unique(data$stateAbbrev)){

    subset0 <- data$stateAbbrev=="NM" & data$ruleYear==2024

    # Adjust Dependent Exemption to find appropriate amount per child - NM provides exemption for all but one child
    data$DependentExemption[subset0] <- (data$DependentExemption[subset0]*data$numkids[subset0]-1)/data$numkids[subset0]

  }


  # Net Federal Income Tax ----
  data$fedincometax<-rowMaxs(cbind(data$fedincometax-data$fedtaxcredits,0))

  # HI has the most tax brackets (12) so tax data should have up at least 12+1 brackets to account for top coded value
  data<-data %>%
    mutate(value.stateinctax=0
           ,value.tax1=0
           ,value.tax2=0
           ,value.tax3=0
           ,value.tax4=0
           ,value.tax5=0
           ,value.tax6=0
           ,value.tax7=0
           ,value.tax8=0
           ,value.tax9=0
           ,value.tax10=0
           ,value.tax11=0
           ,value.tax12=0
           ,value.tax13=0)

  # Some states allow full deduction of fed income tax and some have caps
  data<-data %>%
    mutate(deductfedtax=case_when(FederalIncomeTaxDeductible=="Yes" & stateAbbrev %in% c("MO", "MT", "OR") &  fedincometax > FedIncTaxLimit ~ FedIncTaxLimit # Limited
                                  ,FederalIncomeTaxDeductible=="Yes" & stateAbbrev %in% c("MO", "MT", "OR") &  fedincometax <= FedIncTaxLimit ~ fedincometax
                                  ,FederalIncomeTaxDeductible=="Yes" & stateAbbrev %in% c("AL", "IA") ~ fedincometax # Full federal tax is deductible
                                  ,FederalIncomeTaxDeductible=="No"~0)

           # Used to reduce taxable income - deduct personal/dependent exemptions
           ,deductPExemption=case_when(PersonalExemptionAsCredit=="No"~1
                                       ,PersonalExemptionAsCredit=="Yes"~0)
           ,deductDExemption = case_when(DependExemptionAsCredit=="No"~1
                                         ,DependExemptionAsCredit=="Yes"~0)

           # Used to reduce tax liability - personal/dependent exemptions as tax credits
           ,pExemptAsCredit = case_when(PersonalExemptionAsCredit=="Yes"~1
                                        ,PersonalExemptionAsCredit=="No"~0)
           ,dExemptAsCredit = case_when(DependExemptionAsCredit=="Yes"~1
                                        ,DependExemptionAsCredit=="No"~0))

  data$income.base<-data$income.base+data$income.investment-data$Standard-data$PersonalExemption*data$deductPExemption-data$DependentExemption*data$numkids*data$deductDExemption-data$deductfedtax # adjust countable income

  # Calculate income tax for each bracket separately
  data$taxableincome.bin1<-rowMaxs(cbind((data$income.base-data$IncomeBin1Max)-rowMaxs(cbind(data$income.base-data$IncomeBin2Max,0)),0))
  data$taxableincome.bin2<-rowMaxs(cbind((data$income.base-data$IncomeBin2Max)-rowMaxs(cbind(data$income.base-data$IncomeBin3Max,0)),0))
  data$taxableincome.bin3<-rowMaxs(cbind((data$income.base-data$IncomeBin3Max)-rowMaxs(cbind(data$income.base-data$IncomeBin4Max,0)),0))
  data$taxableincome.bin4<-rowMaxs(cbind((data$income.base-data$IncomeBin4Max)-rowMaxs(cbind(data$income.base-data$IncomeBin5Max,0)),0))
  data$taxableincome.bin5<-rowMaxs(cbind((data$income.base-data$IncomeBin5Max)-rowMaxs(cbind(data$income.base-data$IncomeBin6Max,0)),0))
  data$taxableincome.bin6<-rowMaxs(cbind((data$income.base-data$IncomeBin6Max)-rowMaxs(cbind(data$income.base-data$IncomeBin7Max,0)),0))
  data$taxableincome.bin7<-rowMaxs(cbind((data$income.base-data$IncomeBin7Max)-rowMaxs(cbind(data$income.base-data$IncomeBin8Max,0)),0))
  data$taxableincome.bin8<-rowMaxs(cbind((data$income.base-data$IncomeBin8Max)-rowMaxs(cbind(data$income.base-data$IncomeBin9Max,0)),0))
  data$taxableincome.bin9<-rowMaxs(cbind((data$income.base-data$IncomeBin9Max)-rowMaxs(cbind(data$income.base-data$IncomeBin10Max,0)),0))
  data$taxableincome.bin10<-rowMaxs(cbind((data$income.base-data$IncomeBin10Max)-rowMaxs(cbind(data$income.base-data$IncomeBin11Max,0)),0))
  data$taxableincome.bin11<-rowMaxs(cbind((data$income.base-data$IncomeBin11Max)-rowMaxs(cbind(data$income.base-data$IncomeBin12Max,0)),0))
  data$taxableincome.bin12<-rowMaxs(cbind((data$income.base-data$IncomeBin12Max)-rowMaxs(cbind(data$income.base-data$IncomeBin13Max,0)),0))

  # initalize
  data$AdditionalTaxTotal <- NA
  if("CT" %in% unique(data$stateAbbr) & 2025 %in% unique(data$ruleYear)){
    subset0 <- data$income.base > data$CT_TaxThreshold1 & data$ruleYear==2025 & data$stateAbbrev=="CT"

    # Amount by which the taxable income is reduced from lower tax bracket and applied to higher tax bracket
    data$firstReduction[subset0] <- rowMaxs(cbind(floor((data$income.base[subset0] - data$CT_TaxThreshold1[subset0])/data$CT_TaxInterval1[subset0])*data$CT_IncomeReduction[subset0], 0), na.rm = TRUE)

    # Amount is moved from lower tax bracket to higher tax bracket
    data$taxableincome.bin1[subset0] <- rowMaxs(cbind(data$taxableincome.bin1[subset0] - data$firstReduction[subset0], 0), na.rm = TRUE)
    data$taxableincome.bin2[subset0] <- rowMaxs(cbind(data$taxableincome.bin2[subset0] + data$firstReduction[subset0], 0), na.rm = TRUE)

    # Add the additional tax liability not to surpass the capped amount
    susbet1 <- (data$income.base >= data$CT_TaxThreshold2) & data$ruleYear==2025 & data$stateAbbrev=="CT"
    data$AdditionalTaxAmt1<-0 # initialize
    data$AdditionalTaxAmt1[subset1] <- rowMins(cbind(floor((data$income.base[subset1] - data$CT_TaxThreshold2[subset1])/data$CT_TaxInterval2[subset1])*data$CT_AddTaxAmt1[subset1], data$CT_AddTaxMax1[subset1]), na.rm = TRUE)

    susbet2 <- (data$income.base >= data$CT_TaxThreshold3) & data$ruleYear==2025 & data$stateAbbrev=="CT"
    data$AdditionalTaxAmt2<-0 # initialize
    data$AdditionalTaxAmt2[subset2] <- floor((data$income.base[subset2] - data$CT_TaxThreshold3[subset2])/data$CT_TaxInterval3[subset2])*data$CT_AddTaxAmt2[subset2]

    susbet3 <- (data$income.base >= data$CT_TaxThreshold4) & data$ruleYear==2025 & data$stateAbbrev=="CT"
    data$AdditionalTaxAmt3<-0 # initialize
    data$AdditionalTaxAmt3[subset3] <- floor((data$income.base[subset3] - data$CT_TaxThreshold4[subset3])/data$CT_TaxInterval4[subset3])*data$CT_AddTaxAmt3[subset3]

    # Ensure that NA values are handled properly
    data$AdditionalTaxAmt1[is.na(data$AdditionalTaxAmt1)] <- 0
    data$AdditionalTaxAmt2[is.na(data$AdditionalTaxAmt2)] <- 0
    data$AdditionalTaxAmt3[is.na(data$AdditionalTaxAmt3)] <- 0

    # Add additional tax amounts together. The sum of Amounts 2 & 3 are capped. Amount 1 was capped above
    subset4 <- (subset1==TRUE | subset2==TRUE | subset3==TRUE) & data$ruleYear==2025
    data$AdditionalTaxTotal[subset4] <- rowMaxs(cbind(data$AdditionalTaxAmt1 + rowMins(cbind(data$AdditionalTaxAmt2 + data$AdditionalTaxAmt3, data$CT_AddTaxMax2), na.rm = TRUE), 0), na.rm = TRUE)

  }

  if("CT" %in% unique(data$stateAbbr) & 2024 %in% unique(data$ruleYear)){
    subset0 <- (data$income.base > data$CT_TaxThreshold1) & data$ruleYear==2024 & data$stateAbbrev=="CT"

    # Amount by which the taxable income is reduced from lower tax bracket and applied to higher tax bracket
    data$firstReduction[subset0] <- rowMaxs(cbind(floor((data$income.base[subset0] - data$CT_TaxThreshold1[subset0])/data$CT_TaxInterval1[subset0])*data$CT_IncomeReduction[subset0], 0), na.rm = TRUE)

    # Amount is moved from lower tax bracket to higher tax bracket
    data$taxableincome.bin1[subset0] <- rowMaxs(cbind(data$taxableincome.bin1[subset0] - data$firstReduction[subset0], 0), na.rm = TRUE)
    data$taxableincome.bin2[subset0] <- rowMaxs(cbind(data$taxableincome.bin2[subset0] + data$firstReduction[subset0], 0), na.rm = TRUE)

    # Additional tax for high income earners between thresholds
    subset1 <- (data$income.base >= data$CT_TaxThreshold2) & (data$income.base < data$CT_TaxThreshold3) & data$ruleYear==2024 & data$stateAbbrev=="CT"
    data$AdditionalTaxAmt1<-0 # initialize
    data$AdditionalTaxAmt1[subset1] <- floor((data$income.base[subset1] - data$CT_TaxThreshold2[subset1])/data$CT_TaxInterval2[subset1])*data$CT_AddTaxAmt1[subset1]

    # Additional tax for high income earners above high threshold
    subset2 <- (data$income.base >= data$CT_TaxThreshold3) & data$ruleYear==2024 & data$stateAbbrev=="CT"
    data$AdditionalTaxAmt2<-0 # initialize
    data$AdditionalTaxAmt2[subset2] <- rowMaxs(cbind(floor((data$income.base[subset2] - data$CT_TaxThreshold3[subset2])/data$CT_TaxInterval3[subset2])*data$CT_AddTaxAmt2[subset2], 0), na.rm = TRUE)

    # Ensure that NA values are properly handled
    data$AdditionalTaxAmt1[is.na(data$AdditionalTaxAmt1)] <- 0
    data$AdditionalTaxAmt2[is.na(data$AdditionalTaxAmt2)] <- 0

    # Add the additional tax liability but make sure their is a cap
    subset3 <- (subset1==TRUE | subset2==TRUE) & data$ruleYear==2024 & data$stateAbbrev=="CT"
    data$AdditionalTaxTotal[subset3] <- rowMins(cbind(rowMaxs(cbind(data$AdditionalTaxAmt1[subset3] + data$AdditionalTaxAmt2[subset3], 0), na.rm = TRUE), data$CT_AddTaxMax1[subset3]), na.rm = TRUE)

  }

  # Calculate income tax for each bracket separately
  data$value.tax1<-data$TaxRate1*data$taxableincome.bin1
  data$value.tax2<-data$TaxRate2*data$taxableincome.bin2
  data$value.tax3<-data$TaxRate3*data$taxableincome.bin3
  data$value.tax4<-data$TaxRate4*data$taxableincome.bin4
  data$value.tax5<-data$TaxRate5*data$taxableincome.bin5
  data$value.tax6<-data$TaxRate6*data$taxableincome.bin6
  data$value.tax7<-data$TaxRate7*data$taxableincome.bin7
  data$value.tax8<-data$TaxRate8*data$taxableincome.bin8
  data$value.tax9<-data$TaxRate9*data$taxableincome.bin9
  data$value.tax10<-data$TaxRate10*data$taxableincome.bin10
  data$value.tax11<-data$TaxRate11*data$taxableincome.bin11
  data$value.tax12<-data$TaxRate12*data$taxableincome.bin12

  data$value.stateinctax<-(data$value.tax1+data$value.tax2+data$value.tax3
                           +data$value.tax4+data$value.tax5+data$value.tax6
                           +data$value.tax7+data$value.tax8+data$value.tax9
                           +data$value.tax10+data$value.tax11+data$value.tax12)

  # reduce tax liability by states that use exemptions as a tax credit
  data$value.stateinctax<- rowMaxs(cbind(data$value.stateinctax-data$PersonalExemption*data$pExemptAsCredit-data$DependentExemption*data$numkids*data$dExemptAsCredit, 0))

  # Additional taxes added in Connecticut which are calculated above
  if("CT" %in% unique(data$stateAbbrev)){
    subset0 <- data$stateAbbrev == "CT"
    data$value.stateinctax[subset0] <- rowMaxs(cbind(data$value.stateinctax[subset0] + data$AdditionalTaxTotal[subset0], 0), na.rm = TRUE)
  }

  # tax provision for Vermont found in the Tax Foundation raw data footnote
  if(2024 %in% unique(data$ruleYear) & "VT" %in% unique(data$stateAbbrev)){
    subset0 <- data$income.base > data$VT_Threshold1 & data$stateAbbrev=="VT" & data$ruleYear==2024
    data$value.stateinctax[subset0] <- rowMaxs(cbind(data$VT_PercentofAGI[subset0]*data$income.base[subset0], data$value.stateinctax[subset0]), na.rm=TRUE)
  }

  data$value.stateinctax<-round(data$value.stateinctax,0)

  return(data$value.stateinctax)
}

# Local income Tax ----

function.localinctax<-function(data
                               , incomevar){

  temp<-data

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(localinctaxData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(localinctaxData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(stcountyfips2010=unique(localinctaxData$stcountyfips2010), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-localinctaxData[localinctaxData$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("stcountyfips2010"))%>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing benefit data
    expandPastMiss<-expand.grid(stcountyfips2010=unique(localinctaxData$stcountyfips2010), Year=nonFutureYrs)
    # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, localinctaxData, by=c("stcountyfips2010"))
    expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
  }  # Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {localinctaxData<-localinctaxData %>% rbind(expand)}
  if(length(nonFutureYrs)>0) {localinctaxData<-localinctaxData %>% rbind(expandPastMiss2)}

  temp<-left_join(temp, localinctaxData, by=c("stcountyfips2010","ruleYear"))

  #check for data that is missing
  #setdiff(data$stcountyfips2010, temp$stcountyfips2010)
  #setdiff(temp$stcountyfips2010, data$stcountyfips2010)
  #dim(temp)[1]==dim(localinctaxData)[1]

  temp<-temp %>%
    rename("income.base" = incomevar)

  temp$localinctaxValue<-as.numeric(temp$income.base)*as.numeric(temp$localTaxRate)

  return(temp$localinctaxValue)

}

# Federal Child Tax Credit (CTC) ----
function.fedctc<-function(data
                            , incomevar
                            , totalfederaltaxvar){

    data<-data %>%
      rename("income.base" = incomevar
             ,"totalfederaltax" = totalfederaltaxvar)

    data$value.fedctc<-0

    if(2024 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2024,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2024,]<-temp


      }


    if(2023 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2023,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2023,]<-temp
    }


    if(2022 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2022,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2022,]<-temp
    }

    if(2021 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2021,]

      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      temp$numkidsunder7=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=5 & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0

      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc[subset2]<-temp$numkidsunder17[subset2]*temp$CreditBin1[subset2]+600*temp$numkidsunder7[subset2]

      subset3<-temp$income.base>temp$IncomeBin2Max & temp$income.base<=temp$IncomeBin3Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(((temp$numkidsunder17[subset3]*temp$CreditBin1[subset3]+600*temp$numkidsunder7[subset3])-temp$PhaseOutSlope1[subset3]*(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])),temp$numkidsunder17[subset3]*temp$CreditBin2[subset3]))

      subset4<-temp$income.base>temp$IncomeBin3Max
      temp$value.fedctc[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$CreditBin2[subset4]-(temp$income.base[subset4]-temp$IncomeBin3Max[subset4])*temp$PhaseOutSlope2[subset4],0))

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2021,]<-temp
    }

    if(2020 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2020,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2020,]<-temp
    }

    if(2019 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2019,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2019,]<-temp

    }

    if(2018 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2018,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2018,]<-temp
    }

    if(2017 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2017,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2017,]<-temp
    }

    if(2016 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2016,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2016,]<-temp
    }

    if(2015 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2015,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2015,]<-temp
    }

    if(2014 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2014,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2014,]<-temp
    }

    if(2013 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2013,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2013,]<-temp
    }

    if(2012 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2012,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2012,]<-temp
    }

    if(2011 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2011,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2011,]<-temp
    }

    if(2010 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2010,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2010,]<-temp
    }

    if(2009 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2009,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2009,]<-temp
    }

    if(2008 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2008,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2008,]<-temp
    }

    if(2007 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2007,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2007,]<-temp
    }

    if(2006 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2006,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2006,]<-temp
    }

    if(2005 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2005,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2005,]<-temp
    }

    if(2004 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2004,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2004,]<-temp
    }

    if(2003 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2003,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2003,]<-temp
    }

    if(2002 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2002,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2002,]<-temp
    }

    if(2001 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2001,]

      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      # Case 1: Income below phase-in threshold
      subset1<-temp$income.base<=temp$IncomeBin1Max

      temp$value.fedctc.refundable[subset1]<-0
      temp$value.fedctc.nonrefundable[subset1]<-0

      # Case 2: Income between phase-in and phase-out thresholds, no tax liability
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax<=0

      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(temp$PhaseInRefundability[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),
                                                           temp$numkidsunder17[subset2]*temp$RefundableCredit[subset2]))
      temp$value.fedctc.nonrefundable[subset2]<-0

      # Case 3: Income between phase-in and phase-out thresholds, positive tax liability below max total CTC credit
      subset3<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max & temp$totalfederaltax>0 & temp$totalfederaltax<(temp$IncomeBin2Max*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset3]<-rowMins(cbind(rowMins(cbind(temp$PhaseInRefundability[subset3]*(temp$income.base[subset3]-temp$IncomeBin1Max[subset3]),
                                                                         temp$numkidsunder17[subset3]*temp$RefundableCredit[subset3])),
                                                           (temp$CreditBin1[subset3]*temp$numkidsunder17[subset3])-temp$totalfederaltax[subset3]))
      temp$value.fedctc.nonrefundable[subset3]<-temp$totalfederaltax[subset3]

      # Case 4: Income between phase-in and phase-out thresholds, tax liability above max total CTC credit
      subset4<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$CreditBin1 & temp$totalfederaltax>=(temp$RefundableCredit*temp$numkidsunder17)

      temp$value.fedctc.refundable[subset4]<-0
      temp$value.fedctc.nonrefundable[subset4]<-rowMaxs(cbind(temp$numkidsunder17[subset4]*temp$RefundableCredit[subset4],0))

      # Case 5: Income above phase-out threshold
      subset5<-temp$income.base>temp$IncomeBin2Max

      temp$value.fedctc.refundable[subset5]<-0
      temp$value.fedctc.nonrefundable[subset5]<-rowMaxs(cbind(temp$CreditBin1[subset5]-(temp$income.base[subset5]-temp$IncomeBin2Max[subset5])*temp$PhaseOutSlope1[subset5],0))

      # Add refundable and non-refundable portions of CTC
      temp$value.fedctc<-rowMaxs(cbind(temp$value.fedctc.refundable+temp$value.fedctc.nonrefundable, 0), na.rm=TRUE)

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2001,]<-temp
    }

    if(2000 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==2000,]

      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      subset1<-temp$income.base<=temp$IncomeBin2Max # Below this value CTC is non-refundable
      temp$value.fedctc[subset1]<-0

      subset2<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset2]<-rowMaxs(cbind(temp$CreditBin1[subset2]-(temp$income.base[subset2]-temp$IncomeBin2Max[subset2])*temp$PhaseOutSlope1[subset2],0))

      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==2000,]<-temp
    }

    if(1999 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==1999,]

      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      subset1<-temp$income.base<=temp$IncomeBin2Max # Below this value CTC is non-refundable
      temp$value.fedctc[subset1]<-0

      subset2<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset2]<-rowMaxs(cbind(temp$CreditBin1[subset2]-(temp$income.base[subset2]-temp$IncomeBin2Max[subset2])*temp$PhaseOutSlope1[subset2],0))

      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==1999,]<-temp
    }

    if(1998 %in% unique(data$ruleYear)){ # make sure that year is in the list

      temp<-data[data$ruleYear==1998,]

      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))

      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)

      subset1<-temp$income.base<=temp$IncomeBin2Max # Below this value CTC is non-refundable
      temp$value.fedctc[subset1]<-0

      subset2<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset2]<-rowMaxs(cbind(temp$CreditBin1[subset2]-(temp$income.base[subset2]-temp$IncomeBin2Max[subset2])*temp$PhaseOutSlope1[subset2],0))

      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17

      # Make sure the variables names are the same
      temp<-temp %>%
        select(colnames(data),"value.fedctc")

      # Merge back
      data[data$ruleYear==1998,]<-temp
    }

    data$value.fedctc<-round(data$value.fedctc,0)

    return(data$value.fedctc)
  }



# State Child Tax Credit (CTC) ----

function.statectc<-function(data
                              , incomevar
                              , stateincometaxvar
                              , federalctcvar){

    data<-data %>%
      rename( "income.base" = incomevar
             ,"stateincometax" = stateincometaxvar
             ,"federalctc" = federalctcvar)

    data_main<-left_join(data, statectcData, by=c("stateFIPS", "FilingStatus"))

    # Initialize
    data_main$numeligiblekids<-0
    data_main$value.statectc<-0

    # Work on the subset of states that have CTC
    states_with_ctc<-unique(statectcData$stateFIPS)
    data<-data_main[data_main$stateFIPS %in% states_with_ctc,]

    # Calculate number of eligible dependents
    data$numeligiblekids<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=data$AgeofDependentMax & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=data$AgeofDependentMin, na.rm=TRUE)

    subset1<- data$income.base<=data$IncomeBin1Max
    data$value.statectc[subset1]<-rowMaxs(cbind(data$ValueBin1[subset1], data$federalctc[subset1]*data$PercentOfFederalBin1[subset1]))

    subset2<- data$income.base>data$IncomeBin1Max & data$income.base<=data$IncomeBin2Max
    data$value.statectc[subset2]<-rowMaxs(cbind(data$ValueBin2[subset2], data$federalctc[subset2]*data$PercentOfFederalBin2[subset2]))

    subset3<- data$income.base>data$IncomeBin2Max & data$income.base<=data$IncomeBin3Max
    data$value.statectc[subset3]<-rowMaxs(cbind(data$ValueBin3[subset3], data$federalctc[subset3]*data$PercentOfFederalBin3[subset3]))


    # Adjust for refundability
    subset<-data$Refundable=="No"
    data$value.statectc[subset]<-rowMins(cbind(data$value.statectc[subset],data$stateincometax[subset]))

    # Adjust for number of children to claim
    data$value.statectc<-data$value.statectc*data$numeligiblekids

    data$value.statectc<-round(data$value.statectc,0)

    # Plug states with CTC back
    data_main[data_main$stateFIPS %in% states_with_ctc,]<-data

    return(data_main$value.statectc)
  }


# Federal Earned Income Tax Credit (EITC) ----

function.fedeitc<-function(data
                             , incomevar
                             , investmentincomevar
                             , ageofRespondentvar # Main respondent
                             , ageofSpousevar){   # Spouse (if exists)

    # Rename variable to make it consistent with using dataset
    data<-data %>%
      rename(  "income.base" = incomevar,
               "investmentincome" = investmentincomevar
               ,"agePerson1" = ageofRespondentvar
               ,"agePerson2" = ageofSpousevar)

    # Create two variables: AGI and earned income (later that will go to the InitialTransformations)
    execute<-"Federal_EITC"

    data$income.base.AGI<-data$income.base
    data$income.base.earned<-data$income.base

    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(fedeitcData$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(fedeitcData$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(FilingStatus=unique(fedeitcData$FilingStatus), numkids=unique(fedeitcData$numkids), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-fedeitcData[fedeitcData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("FilingStatus", "numkids"))%>%drop_na() %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(FilingStatus=unique(fedeitcData$FilingStatus), numkids=unique(fedeitcData$numkids), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, fedeitcData, by=c("FilingStatus", "numkids"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {fedeitcData<-fedeitcData %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {fedeitcData<-fedeitcData %>% rbind(expandPastMiss2)}

    # Step I: merge parameters by filing status and number of children
    data<-left_join(data, fedeitcData, by=c("FilingStatus", "numkids", "ruleYear"))

    # Compute value of the based on Earned Income
    data$value.fedeitc<-0

    subset1<-which(data$income.base.earned<data$IncomeBin1Max)
    data$value.fedeitc[subset1]<-0
    assign(paste("subset1",execute,sep="_"),subset1,envir=.GlobalEnv)

    subset2<-which(data$income.base.earned>=data$IncomeBin1Max & data$income.base.earned<data$IncomeBin2Max)
    data$value.fedeitc[subset2]<-0+(data$income.base.earned[subset2]-data$IncomeBin1Max[subset2])*data$PhaseInRate[subset2]
    assign(paste("subset2",execute,sep="_"),subset2,envir=.GlobalEnv)

    subset3<-which(data$income.base.earned>=data$IncomeBin2Max & data$income.base.earned<data$IncomeBin3Max)
    data$value.fedeitc[subset3]<-data$MaxCredit[subset3]
    assign(paste("subset3",execute,sep="_"),subset3,envir=.GlobalEnv)


    subset4<-which(data$income.base.earned>=data$IncomeBin3Max & data$income.base.earned<data$IncomeBin4Max)
    data$value.fedeitc[subset4]<-data$MaxCredit[subset4]-(data$income.base.earned[subset4]-data$IncomeBin3Max[subset4])*data$PhaseOutRate[subset4]
    assign(paste("subset4",execute,sep="_"),subset4,envir=.GlobalEnv)

    subset5<-which(data$income.base.earned>=data$IncomeBin4Max)
    data$value.fedeitc[subset5]<-0
    assign(paste("subset5",execute,sep="_"),subset5,envir=.GlobalEnv)



    # Recalculate EITC based on AGI if required
    subset4a<-which(data$income.base.AGI!=data$income.base.earned & data$income.base.AGI>=data$IncomeBin3Max & data$income.base.AGI<data$IncomeBin4Max)
    data$value.fedeitc[subset4a]<-data$MaxCredit[subset4a]-(data$income.base.AGI[subset4a]-data$IncomeBin3Max[subset4a])*data$PhaseOutRate[subset4a]
    assign(paste("subset4a",execute,sep="_"),subset4a,envir=.GlobalEnv)

    subset5a<-which(data$income.base.AGI!=data$income.base.earned & data$income.base.AGI>=data$IncomeBin4Max)
    data$value.fedeitc[subset5a]<-0
    assign(paste("subset5a",execute,sep="_"),subset5a,envir=.GlobalEnv)


    # Apply special rule for those who do not claim a child
    subset1<-which(data$numkids==0 & (data$FilingStatus==1 | data$FilingStatus==3) & (data$agePerson1<data$ageLimitMin | data$agePerson1>data$ageLimitMax))
    data$value.fedeitc[subset1]<-0
    assign(paste("subset1a",execute,sep="_"),subset1,envir=.GlobalEnv)


    subset2<-which(data$numkids==0 & (data$FilingStatus==2) & ((data$agePerson1<data$ageLimitMin & data$agePerson2<data$ageLimitMin) | (data$agePerson1>data$ageLimitMax & data$agePerson2>data$ageLimitMax)))
    data$value.fedeitc[subset2]<-0
    assign(paste("subset2a",execute,sep="_"),subset2,envir=.GlobalEnv)


    # Invoke investment income test
    data$value.fedeitc[data$investmentincome>data$InvestmentIncomeEligibility]<-0


    data$value.fedeitc<-round(data$value.fedeitc,0)

    return(data$value.fedeitc)
  }



# FOR POLICY SIMULATIONS - Federal Earned Income Tax Credit (EITC)-----
# Return Federal EITC parameters

function.fedeitcIncomeLimits<-function(data
                                       , filingstatusvar
                                       , numberofkidsvar
                                       , incomevar
                                       , investmentincomevar
                                       , ageofRespondentvar # Main respondent
                                       , ageofSpousevar
                                       , ruleYearvar){   # Spouse (if exists)

  # Rename variable to make it consistent with using dataset
  data<-data %>%
    rename(  "income.base" = incomevar
             ,"FilingStatus" = filingstatusvar
             ,"investmentincome" = investmentincomevar
             ,"numkids" = numberofkidsvar
             ,"agePerson1" = ageofRespondentvar
             ,"agePerson2" = ageofSpousevar
             ,"ruleYear" = ruleYearvar)

  # Create two variables: AGI and earned income (later that will go to the InitialTransformations)
  data$income.base.AGI<-data$income.base
  data$income.base.earned<-data$income.base

  # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
  years<-unique(data$ruleYear) # years in data set
  yearsinexpdata<- unique(fedeitcData$ruleYear) # rule years in benefit data
  yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
  yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
  # Create data for the future
  maxyearofdata<-max(fedeitcData$ruleYear) # collect latest year of benefit data
  futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
  if(length(futureYrs)>0){
    # Create data frame with future years
    expand<-expand.grid(FilingStatus=unique(fedeitcData$FilingStatus), numkids=unique(fedeitcData$numkids), Year=futureYrs)
    # Collect latest benefit data there is and merge w/data frame
    expand2<-fedeitcData[fedeitcData$ruleYear==maxyearofdata, ]
    expand<-expand%>%left_join(expand2, by=c("FilingStatus", "numkids"))%>%drop_na() %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
  }
  # Create data for past and gap years (missing data) - not the future
  nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
  if(length(nonFutureYrs)>0){
    #Create data frame with past years and year for which we are missing benefit data
    expandPastMiss<-expand.grid(FilingStatus=unique(fedeitcData$FilingStatus), numkids=unique(fedeitcData$numkids), Year=nonFutureYrs)
    # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
    expandPastMiss2<-left_join(expandPastMiss, fedeitcData, by=c("FilingStatus", "numkids"))
    expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
    expandPastMiss2<-expandPastMiss2%>%
      group_by(Year)%>%
      mutate(minyeardiff = min(yeardiff))
    expandPastMiss2<-expandPastMiss2 %>%
      filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
  }  # Attach copied future, historical, and missing benefit data
  if(length(futureYrs)>0) {fedeitcData<-fedeitcData %>% rbind(expand)}
  if(length(nonFutureYrs)>0) {fedeitcData<-fedeitcData %>% rbind(expandPastMiss2)}


  # Step I: merge parameters by filing status and number of children
  data<-left_join(data, fedeitcData, by=c("FilingStatus", "numkids", "ruleYear"))

  # Compute value of the based on Earned Income
  data$value.fedeitc<-0

  subset1<-which(data$income.base.earned<data$IncomeBin1Max)
  data$value.fedeitc[subset1]<-0

  subset2<-which(data$income.base.earned>=data$IncomeBin1Max & data$income.base.earned<data$IncomeBin2Max)
  data$value.fedeitc[subset2]<-0+(data$income.base.earned[subset2]-data$IncomeBin1Max[subset2])*data$PhaseInRate[subset2]

  subset3<-which(data$income.base.earned>=data$IncomeBin2Max & data$income.base.earned<data$IncomeBin3Max)
  data$value.fedeitc[subset3]<-data$MaxCredit[subset3]

  subset4<-which(data$income.base.earned>=data$IncomeBin3Max & data$income.base.earned<data$IncomeBin4Max)
  data$value.fedeitc[subset4]<-data$MaxCredit[subset4]-(data$income.base.earned[subset4]-data$IncomeBin3Max[subset4])*data$PhaseOutRate[subset4]

  subset5<-which(data$income.base.earned>=data$IncomeBin4Max)
  data$value.fedeitc[subset5]<-0


  # Recalculate EITC based on AGI if required
  subset4a<-which(data$income.base.AGI!=data$income.base.earned & data$income.base.AGI>=data$IncomeBin3Max & data$income.base.AGI<data$IncomeBin4Max)
  data$value.fedeitc[subset4a]<-data$MaxCredit[subset4a]-(data$income.base.AGI[subset4a]-data$IncomeBin3Max[subset4a])*data$PhaseOutRate[subset4a]

  subset5a<-which(data$income.base.AGI!=data$income.base.earned & data$income.base.AGI>=data$IncomeBin4Max)
  data$value.fedeitc[subset5a]<-0


  # Apply special rule for those who do not claim a child
  subset1<-which(data$numkids==0 & (data$FilingStatus==1 | data$FilingStatus==3) & (data$agePerson1<data$ageLimitMin | data$agePerson1>data$ageLimitMax))
  data$value.fedeitc[subset1]<-0

  subset2<-which(data$numkids==0 & (data$FilingStatus==2) & ((data$agePerson1<data$ageLimitMin & data$agePerson2<data$ageLimitMin) | (data$agePerson1>data$ageLimitMax & data$agePerson2>data$ageLimitMax)))
  data$value.fedeitc[subset2]<-0


  # Invoke investment income test
  data$value.fedeitc[data$investmentincome>data$InvestmentIncomeEligibility]<-0


  data$value.fedeitc<-round(data$value.fedeitc,0)

  return(cbind(data$MaxCredit,data$IncomeBin2Max,data$IncomeBin3Max,data$PhaseOutRate))
}


# State Earned Income Tax Credit (EITC) ----

function.stateeitc<-function(data
                               , incomevar
                               , investmentincomevar
                               , federaleitcvar
                               , stateincometaxvar
                               , ageofRespondentvar
                               , ageofSpousevar
                               , ageofYoungestChildvar){

    data<-data %>%
      rename("income.base" = incomevar
             ,"investmentincome" = investmentincomevar
             ,"federaleitc" = federaleitcvar
             ,"stateincometax" = stateincometaxvar
             ,"agePerson1" = ageofRespondentvar
             ,"agePerson2" = ageofSpousevar
             ,"ageChild1" = ageofYoungestChildvar)

    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(stateeitcData$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(stateeitcData$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(stateeitcData$stateFIPS), numkids=unique(stateeitcData$numkids), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-stateeitcData[stateeitcData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "numkids"))%>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(stateeitcData$stateFIPS), numkids=unique(stateeitcData$numkids), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, stateeitcData, by=c("stateFIPS", "numkids"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {stateeitcData<-stateeitcData %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {stateeitcData<-stateeitcData %>% rbind(expandPastMiss2)}

    # Step I: merge parameters by filing status and number of children
   # data_main<-left_join(data, stateeitcData[,colnames(stateeitcData)!="ruleYear"], by=c("stateFIPS", "numkids"))
    data_main<-left_join(data, stateeitcData, by=c("stateFIPS", "numkids", "ruleYear"))

    # Create two variables: AGI and earned income (later that will go to the InitialTransformations)
    data_main$income.base.AGI<-data_main$income.base
    data_main$income.base.earned<-data_main$income.base

    # Compute value of the credit following the steps from Calculations
    data_main$value.stateeitc<-0

    # Work on the subset of states that have CTC
    states_with_eitc<-unique(stateeitcData$stateFIPS)
    data<-data_main[data_main$stateFIPS %in% states_with_eitc,]

    # Compute value of the credit following the steps from Calculations
    data$value.stateeitc<-data$PercentOfFederal*data$federaleitc

    # Adjust for refundability
    subset<-which(data$Refundable=="No")
    data$value.stateeitc[subset]<-rowMins(cbind(data$value.stateeitc[subset], data$stateincometax[subset]),na.rm=TRUE)

    # State-specific rules

    #-------------------------------------
    #1. California (stateFIPS==6)
    #-------------------------------------
    if(6 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data[data$stateFIPS==6,]

    temp$value.stateeitc<-0
    temp$value.stateeitc<-temp$PercentOfFederal*temp$federaleitc

    # Adjust for refundability
    subset<-which(temp$Refundable=="No")
    temp$value.stateeitc[subset]<-rowMins(cbind(temp$value.stateeitc[subset],temp$stateincometax[subset]))

    #Invoke CA Income eligibility
    subset<-which(temp$income.base.earned>30000)
    temp$value.stateeitc[subset]<-0

    # Replace back
    data$value.stateeitc[data$stateFIPS==6]<-temp$value.stateeitc
    }

    #-------------------------------------
    #2. District of Columbia (stateFIPS==11)
    #-------------------------------------
    if(11 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==11 & data$numkids==0,] # special program for people without depndents

      if(length(temp$stateFIPS)!=0){
      temp$value.stateeitc.additional<-0

      # Step 1: calculate EITC base value (based on earned income)
      temp$value.eitc.base<-rowMins(cbind(0.0765*temp$income.base.earned,538))

      # Step 2: Calculate total EITC (with phase-out)
      temp$income.countable<-rowMaxs(cbind(temp$income.base.earned,temp$income.base)) #max b/w earnings and gross income

      subset<-which(temp$income.countable<19489)
      temp$value.stateeitc.additional[subset]<-temp$value.eitc.base[subset]

      subset<-which(temp$income.countable>=19489)
      temp$value.stateeitc.additional[subset]<-rowMaxs(cbind(0,temp$value.eitc.base[subset]-0.0848*(temp$income.countable[subset]-19489)))

      # Make sure that age requirements for Federal EITC are met
      subset1<-which(temp$numkids==0 & (temp$FilingStatus==1 | temp$FilingStatus==3) & (temp$agePerson1<19 | temp$agePerson1>999))
      temp$value.stateeitc.additional[subset1]<-0

      subset2<-which(temp$numkids==0 & (temp$FilingStatus==2) & ((temp$agePerson1<19 & temp$agePerson2<19) | (temp$agePerson1>999 & temp$agePerson2>999)))
      temp$value.stateeitc.additional[subset2]<-0

      # Check if family satisfies investment income requirements
      subset3<-which(temp$investmentincome<3650)
      temp$value.stateeitc.additional[subset3]<-0

      # Add this special EITC to the DC state EITC
      temp$value.stateeitc<-temp$value.stateeitc+temp$value.stateeitc.additional

      # Replace back
      data$value.stateeitc[data$stateFIPS==11 & data$numkids==0]<-temp$value.stateeitc
      }
    }

    #-------------------------------------
    #2. Ohio (stateFIPS==39)
    #-------------------------------------
    if(39 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data[data$stateFIPS==39,]

    temp$value.stateeitc<-0
    temp$value.stateeitc<-temp$PercentOfFederal*temp$federaleitc

    # Adjust for refundability
    subset<-which(temp$Refundable=="No")
    temp$value.stateeitc[subset]<-rowMins(cbind(temp$value.stateeitc[subset],temp$stateincometax[subset]))

    #OH has different rules for refundability
    subset<-which(temp$income.base>20000)
    temp$value.stateeitc[subset]<-rowMins(cbind(temp$value.stateeitc[subset],0.5*temp$stateincometax[subset]))

    # Replace back
    data$value.stateeitc[data$stateFIPS==39]<-temp$value.stateeitc
    }

    #-------------------------------------
    #3. Oregon (stateFIPS==41)
    #-------------------------------------
    if(41 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data[data$stateFIPS==41,]

    # Families with children under the age of 3 have different percent of federal
    subset<-which(temp$ageChild1<3)
    temp$PercentOfFederal[subset]<-0.12

    # Calculate tax credit amount in a standard way
    temp$value.stateeitc<-0
    temp$value.stateeitc<-temp$PercentOfFederal*temp$federaleitc

    # Adjust for refundability
    subset<-which(temp$Refundable=="No")
    temp$value.stateeitc[subset]<-rowMins(cbind(temp$value.stateeitc[subset],temp$stateincometax[subset]))

    # Replace back
    data$value.stateeitc[data$stateFIPS==41]<-temp$value.stateeitc
    }


    #-------------------------------------
    #4. Maryland (stateFIPS==24)
    #-------------------------------------
    if(24 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==24,]

      # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
      years<-unique(data$ruleYear) # years in data set
      yearsinexpdata<- unique(fedeitcData$ruleYear) # rule years in benefit data
      yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
      yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
      # Create data for the future
      maxyearofdata<-max(fedeitcData$ruleYear) # collect latest year of benefit data
      futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
      if(length(futureYrs)>0){
        # Create data frame with future years
        expand<-expand.grid(FilingStatus=unique(fedeitcData$FilingStatus), numkids=unique(fedeitcData$numkids), Year=futureYrs)
        # Collect latest benefit data there is and merge w/data frame
        expand2<-fedeitcData[fedeitcData$ruleYear==maxyearofdata, ]
        expand<-expand%>%left_join(expand2, by=c("FilingStatus", "numkids"))%>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
      }
      # Create data for past and gap years (missing data) - not the future
      nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
      if(length(nonFutureYrs)>0){
        #Create data frame with past years and year for which we are missing benefit data
        expandPastMiss<-expand.grid(FilingStatus=unique(fedeitcData$FilingStatus), numkids=unique(fedeitcData$numkids), Year=nonFutureYrs)
        # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
        expandPastMiss2<-left_join(expandPastMiss, fedeitcData, by=c("FilingStatus", "numkids"))
        expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
        expandPastMiss2<-expandPastMiss2%>%
          group_by(Year)%>%
          mutate(minyeardiff = min(yeardiff))
        expandPastMiss2<-expandPastMiss2 %>%
          filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
      }  # Attach copied future, historical, and missing benefit data
      if(length(futureYrs)>0) {fedeitcData<-fedeitcData %>% rbind(expand)}
      if(length(nonFutureYrs)>0) {fedeitcData<-fedeitcData %>% rbind(expandPastMiss2)}

      # Step 1: Need to recalulate federal EITC, because Maryland has minimum age requirement disregard
      # Recalculate Federal tax credit
      temp<-left_join(temp, fedeitcData, by=c("FilingStatus", "numkids", "ruleYear"))

      # Compute value of the based on Earned Income
      temp$federaleitc.rep<-0

      subset1<-which(temp$income.base.earned<temp$IncomeBin1Max)
      temp$federaleitc.rep[subset1]<-0

      subset2<-which(temp$income.base.earned>=temp$IncomeBin1Max & temp$income.base.earned<temp$IncomeBin2Max)
      temp$federaleitc.rep[subset2]<-0+(temp$income.base.earned[subset2]-temp$IncomeBin1Max[subset2])*temp$PhaseInRate[subset2]

      subset3<-which(temp$income.base.earned>=temp$IncomeBin2Max & temp$income.base.earned<temp$IncomeBin3Max)
      temp$federaleitc.rep[subset3]<-temp$MaxCredit[subset3]

      subset4<-which(temp$income.base.earned>=temp$IncomeBin3Max & temp$income.base.earned<temp$IncomeBin4Max)
      temp$federaleitc.rep[subset4]<-temp$MaxCredit[subset4]-(temp$income.base.earned[subset4]-temp$IncomeBin3Max[subset4])*temp$PhaseOutRate[subset4]

      subset5<-which(temp$income.base.earned>=temp$IncomeBin4Max)
      temp$federaleitc.rep[subset5]<-0


      # Recalculate EITC based on AGI if required
      subset4a<-which(temp$income.base.AGI!=temp$income.base.earned & temp$income.base.AGI>=temp$IncomeBin3Max & temp$income.base.AGI<temp$IncomeBin4Max)
      temp$federaleitc.rep[subset4a]<-temp$MaxCredit[subset4a]-(temp$income.base.AGI[subset4a]-temp$IncomeBin3Max[subset4a])*temp$PhaseOutRate[subset4a]

      subset5a<-which(temp$income.base.AGI!=temp$income.base.earned & temp$income.base.AGI>=temp$IncomeBin4Max)
      temp$federaleitc.rep[subset5a]<-0


      # Invoke investment income test
      temp$federaleitc.rep[temp$investmentincome>temp$InvestmentIncomeEligibility]<-0


      temp$federaleitc<-temp$federaleitc.rep # Taking into account "Potential" Federal EITC for those who do not qualify due to age requirements

      # Apply refundable credit at 28% of federal EITC
      temp$value.stateeitc.refundable<-0
      temp$value.stateeitc.refundable<-0.28*temp$federaleitc

      # Apply nonrefundable credit at 50% of federal EITC
      temp$value.stateeitc.nonrefundable<-rowMins(cbind(0.5*temp$federaleitc,rowMaxs(cbind(temp$stateincometax-temp$value.stateeitc.refundable,0))))

      # Total value of credit
      temp$value.stateeitc<-temp$value.stateeitc.refundable+temp$value.stateeitc.nonrefundable

      # Get rid of all other variables before merging back
      #temp<-temp[,colnames(temp) %in% colnames(data)]

      # Replace back
      data$value.stateeitc[data$stateFIPS==24]<-temp$value.stateeitc

    }

    # Apply special rule for those who do not claim a child (except Maryland)
    subset1<-which(data$numkids==0 & (data$FilingStatus==1 | data$FilingStatus==3) & data$agePerson1<25 & data$stateFIPS != 24)
    data$value.stateeitc[subset1]<-0

    subset2<-which(data$numkids==0 & (data$FilingStatus==2) & (data$agePerson1<25 & data$agePerson2<25) & data$stateFIPS != 24)
    data$value.stateeitc[subset2]<-0


    data$value.stateeitc<-round(data$value.stateeitc,0)

    data_main[data_main$stateFIPS %in% states_with_eitc,]<-data

    return(data_main$value.stateeitc)
  }



# Federal Child and Dependent Care Tax Credit (CDCTC)----

function.fedcdctc<-function(data
                              , incomevar
                              , qualifyingexpensesvar
                              , totalfederaltaxvar){

    data<-data %>%
      rename(  "income.base" = incomevar
              ,"qualifyingExpenses" = qualifyingexpensesvar
              ,"totalfederaltax" = totalfederaltaxvar
              )

    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(fedcdctcData$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(fedcdctcData$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(NumberOfKidsUnder13=unique(fedcdctcData$NumberOfKidsUnder13), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-fedcdctcData[fedcdctcData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("NumberOfKidsUnder13"))%>%drop_na() %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(fedcdctcData$stateFIPS), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, fedcdctcData, by=c("NumberOfKidsUnder13"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {fedcdctcData<-fedcdctcData %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {fedcdctcData<-fedcdctcData %>% rbind(expandPastMiss2)}

    # Calculate number of dependents under the age of 13
    data$NumberOfKidsUnder13<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=12 & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=0, na.rm=TRUE)

    data<-left_join(data, fedcdctcData, by=c("NumberOfKidsUnder13", "ruleYear"))

    # Separate between earned income and AGI
    data$income.base.earned<-data$income.base
    data$income.base.AGI<-data$income.base

    # LIMITATION
    # Right now the function doesn't check for spouses income to be >1

    data$value.fedcdctc<-0

    subset1<-which(data$income.base.AGI <= data$IncomeBin1Max) # Receive Maximum Credit
    data$value.fedcdctc[subset1]<-data$MaxCredit[subset1]*rowMins(cbind(rowMins(cbind(data$qualifyingExpenses[subset1],data$MaxExpense[subset1])),data$income.base.AGI[subset1]))

    subset2<-which(data$income.base.AGI > data$IncomeBin1Max) # Receive Phase-Out Credit bounded by the minimum credit from below
    data$value.fedcdctc[subset2]<-(rowMaxs(cbind((data$MaxCredit[subset2]-data$PhaseOutRate[subset2]*(data$income.base.AGI[subset2]-data$IncomeBin1Max[subset2])),data$MinCredit[subset2])))*rowMins(cbind(rowMins(cbind(data$qualifyingExpenses[subset2],data$MaxExpense[subset2])),data$income.base.AGI[subset2]))

    # Adjust if CDCTC is non-refundable
    subset<-which(data$Refundable=="No")
    data$value.fedcdctc[subset]<-rowMins(cbind(data$value.fedcdctc[subset],data$totalfederaltax[subset]))

    # Earned Income Test
    subset<-which(data$income.base.earned==0)
    data$value.fedcdctc[subset]<-0

    data$value.fedcdctc<-round(data$value.fedcdctc,0)

    return(data$value.fedcdctc)
  }


# State Child and Dependent Care Tax Credit (CDCTC)----

function.statecdctc<-function(data
                                , qualifyingexpensesvar
                                , incomevar
                                , stateincometaxvar
                                , federalcdctcvar){

    data<-data %>%
      rename( "income.base" = incomevar
              ,"qualifyingExpenses" = qualifyingexpensesvar
              ,"stateincometax" = stateincometaxvar
              ,"federalcdctc" = federalcdctcvar)

    # Calculate number of dependents under the age of 13
    data$NumberOfKidsUnder13<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=13 & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=0, na.rm=TRUE)

    data_main<-left_join(data, statecdctcData, by=c("stateFIPS"))

    # Initialize
    data_main$value.statecdctc<-0

    # Work on the subset of states that have CDCTC
    states_with_cdctc<-unique(statecdctcData$stateFIPS)
    data<-data_main[data_main$stateFIPS %in% states_with_cdctc,]

    subset1<- which(data$income.base<=data$IncomeBin1Max)
    data$value.statecdctc[subset1]<-rowMaxs(cbind(data$PercentOfFederalBin1[subset1]*data$federalcdctc[subset1], data$PercentOfExpensesBin1[subset1]*data$qualifyingExpenses[subset1]))

    subset2<- which(data$income.base>data$IncomeBin1Max & data$income.base<=data$IncomeBin2Max)
    data$value.statecdctc[subset2]<-rowMaxs(cbind(data$PercentOfFederalBin2[subset2]*data$federalcdctc[subset2], data$PercentOfExpensesBin2[subset2]*data$qualifyingExpenses[subset2]))

    subset3<- which(data$income.base>data$IncomeBin2Max & data$income.base<=data$IncomeBin3Max)
    data$value.statecdctc[subset3]<-rowMaxs(cbind(data$PercentOfFederalBin3[subset3]*data$federalcdctc[subset3], data$PercentOfExpensesBin3[subset3]*data$qualifyingExpenses[subset3]))

    subset4<- which(data$income.base>data$IncomeBin3Max & data$income.base<=data$IncomeBin4Max)
    data$value.statecdctc[subset4]<-rowMaxs(cbind(data$PercentOfFederalBin4[subset4]*data$federalcdctc[subset4], data$PercentOfExpensesBin4[subset4]*data$qualifyingExpenses[subset4]))

    subset5<- which(data$income.base>data$IncomeBin4Max & data$income.base<=data$IncomeBin5Max)
    data$value.statecdctc[subset5]<-rowMaxs(cbind(data$PercentOfFederalBin5[subset5]*data$federalcdctc[subset5], data$PercentOfExpensesBin5[subset5]*data$qualifyingExpenses[subset5]))

    subset6<- which(data$income.base>data$IncomeBin5Max & data$income.base<=data$IncomeBin6Max)
    data$value.statecdctc[subset6]<-rowMaxs(cbind(data$PercentOfFederalBin6[subset6]*data$federalcdctc[subset6], data$PercentOfExpensesBin6[subset6]*data$qualifyingExpenses[subset6]))

    subset7<- which(data$income.base>data$IncomeBin6Max & data$income.base<=data$IncomeBin7Max)
    data$value.statecdctc[subset7]<-rowMaxs(cbind(data$PercentOfFederalBin7[subset7]*data$federalcdctc[subset7], data$PercentOfExpensesBin7[subset7]*data$qualifyingExpenses[subset7]))

    subset8<-which(data$income.base>data$IncomeBin7Max & data$income.base<=data$IncomeBin8Max)
    data$value.statecdctc[subset8]<-rowMaxs(cbind(data$PercentOfFederalBin8[subset8]*data$federalcdctc[subset8], data$PercentOfExpensesBin8[subset8]*data$qualifyingExpenses[subset8]))

    subset9<-which(data$income.base>data$IncomeBin8Max & data$income.base<=data$IncomeBin9Max)
    data$value.statecdctc[subset9]<-rowMaxs(cbind(data$PercentOfFederalBin9[subset9]*data$federalcdctc[subset9], data$PercentOfExpensesBin9[subset9]*data$qualifyingExpenses[subset9]))

    subset10<-which(data$income.base>data$IncomeBin9Max & data$income.base<=data$IncomeBin10Max)
    data$value.statecdctc[subset10]<-rowMaxs(cbind(data$PercentOfFederalBin10[subset10]*data$federalcdctc[subset10], data$PercentOfExpensesBin10[subset10]*data$qualifyingExpenses[subset10]))

    subset11<- which(data$income.base>data$IncomeBin10Max & data$income.base<=data$IncomeBin11Max)
    data$value.statecdctc[subset11]<-rowMaxs(cbind(data$PercentOfFederalBin11[subset11]*data$federalcdctc[subset11], data$PercentOfExpensesBin11[subset11]*data$qualifyingExpenses[subset11]))


    # Adjust for refundability
    subset<-which(data$Refundable=="No")
    data$value.statecdctc[subset]<-rowMins(cbind(data$value.statecdctc[subset],data$stateincometax[subset]))

    # State-specific rules

    #-------------------------------------
    # District of Columbia (stateFIPS==11)
    #-------------------------------------
    if(11 %in% unique(data$stateFIPS)){ # make sure that state is in the list

      temp<-data[data$stateFIPS==11,]

      # SPECIAL RULE:  DC has an additional "Keep Child Care Affordable Tax Credit" (also known as Early Learning Credit - ELC)

      # Initialize (per each kid)
      temp$value.statecdctc.elc.kid1<-0
      temp$value.statecdctc.elc.kid2<-0
      temp$value.statecdctc.elc.kid3<-0
      temp$value.statecdctc.elc.kid4<-0
      temp$value.statecdctc.elc.kid5<-0
      temp$value.statecdctc.elc.kid6<-0
      temp$value.statecdctc.elc.kid7<-0
      temp$value.statecdctc.elc.kid8<-0
      temp$value.statecdctc.elc.kid9<-0
      temp$value.statecdctc.elc.kid10<-0
      temp$value.statecdctc.elc.kid11<-0
      temp$value.statecdctc.elc.kid12<-0

      # Child 1 (income limit is satisfied and age is below 4)
      subset.kid1<-which(temp$income.base<=151900 & (!is.na(temp$agePerson1) & temp$agePerson1<=3))
      temp$value.statecdctc.elc.kid1[subset.kid1]<-rowMins(cbind(temp$netexp.childcareperson1[subset.kid1],1010)) # Tax credit is a min of a qualifying expense or a maximum credit per child

      # Child 2
      subset.kid2<-which(temp$income.base<=151900 & (!is.na(temp$agePerson2) & temp$agePerson2<=3))
      temp$value.statecdctc.elc.kid2[subset.kid2]<-rowMins(cbind(temp$netexp.childcareperson2[subset.kid2],1010))

      # Child 3
      subset.kid3<-which(temp$income.base<=151900 & (!is.na(temp$agePerson3) & temp$agePerson3<=3))
      temp$value.statecdctc.elc.kid3[subset.kid3]<-rowMins(cbind(temp$netexp.childcareperson3[subset.kid3],1010))

      # Child 4
      subset.kid4<-which(temp$income.base<=151900 & (!is.na(temp$agePerson4) & temp$agePerson4<=3))
      temp$value.statecdctc.elc.kid4[subset.kid4]<-rowMins(cbind(temp$netexp.childcareperson4[subset.kid4],1010))

      # Child 5
      subset.kid5<-which(temp$income.base<=151900 & (!is.na(temp$agePerson5) & temp$agePerson5<=3))
      temp$value.statecdctc.elc.kid5[subset.kid5]<-rowMins(cbind(temp$netexp.childcareperson5[subset.kid5],1010))

      # Child 6
      subset.kid6<-which(temp$income.base<=151900 & (!is.na(temp$agePerson6) & temp$agePerson6<=3))
      temp$value.statecdctc.elc.kid6[subset.kid6]<-rowMins(cbind(temp$netexp.childcareperson6[subset.kid6],1010))

      # Child 7
      subset.kid7<-which(temp$income.base<=151900 & (!is.na(temp$agePerson7) & temp$agePerson7<=3))
      temp$value.statecdctc.elc.kid7[subset.kid7]<-rowMins(cbind(temp$netexp.childcareperson7[subset.kid7],1010))

      # Child 8
      subset.kid8<-which(temp$income.base<=151900 & (!is.na(temp$agePerson8) & temp$agePerson8<=3))
      temp$value.statecdctc.elc.kid8[subset.kid8]<-rowMins(cbind(temp$netexp.childcareperson8[subset.kid8],1010))

      # Child 9
      subset.kid9<-which(temp$income.base<=151900 & (!is.na(temp$agePerson9) & temp$agePerson9<=3))
      temp$value.statecdctc.elc.kid9[subset.kid9]<-rowMins(cbind(temp$netexp.childcareperson9[subset.kid9],1010))

      # Child 10
      subset.kid10<-which(temp$income.base<=151900 & (!is.na(temp$agePerson10) & temp$agePerson10<=3))
      temp$value.statecdctc.elc.kid10[subset.kid10]<-rowMins(cbind(temp$netexp.childcareperson10[subset.kid10],1010))

      # Child 11
      subset.kid11<-which(temp$income.base<=151900 & (!is.na(temp$agePerson11) & temp$agePerson11<=3))
      temp$value.statecdctc.elc.kid11[subset.kid11]<-rowMins(cbind(temp$netexp.childcareperson11[subset.kid11],1010))

      # Child 12
      subset.kid12<-which(temp$income.base<=151900 & (!is.na(temp$agePerson12) & temp$agePerson12<=3))
      temp$value.statecdctc.elc.kid12[subset.kid12]<-rowMins(cbind(temp$netexp.childcareperson12[subset.kid12],1010))

      # Total value of the credit
      temp$value.statecdctc.elc<-temp$value.statecdctc.elc.kid1+temp$value.statecdctc.elc.kid2+temp$value.statecdctc.elc.kid3+temp$value.statecdctc.elc.kid4+temp$value.statecdctc.elc.kid5+temp$value.statecdctc.elc.kid6+temp$value.statecdctc.elc.kid7+temp$value.statecdctc.elc.kid8+temp$value.statecdctc.elc.kid9+temp$value.statecdctc.elc.kid10+temp$value.statecdctc.elc.kid11+temp$value.statecdctc.elc.kid12

      # Add it back to the total state CDCTC
      temp$value.statecdctc<-temp$value.statecdctc+temp$value.statecdctc.elc

      # Replace back
      data$value.statecdctc[data$stateFIPS==11]<-temp$value.statecdctc
    }


    #-------------------------------------
    #2. Louisiana (stateFIPS==22)
    #-------------------------------------
    if(22 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==22,]

      # SPECIAL RULE: In Louisiana, the tax credit is refundable if the federal AGI is $25,000 or less.

      subset1<- temp$income.base<=temp$IncomeBin1Max
      temp$value.statecdctc[subset1]<-rowMaxs(cbind(temp$PercentOfFederalBin1[subset1]*temp$federalcdctc[subset1], temp$PercentOfExpensesBin1[subset1]*temp$qualifyingExpenses[subset1]))

      subset2<- temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.statecdctc[subset2]<-rowMaxs(cbind(temp$PercentOfFederalBin2[subset2]*temp$federalcdctc[subset2], temp$PercentOfExpensesBin2[subset2]*temp$qualifyingExpenses[subset2]))

      subset3<- temp$income.base>temp$IncomeBin2Max & temp$income.base<=temp$IncomeBin3Max
      temp$value.statecdctc[subset3]<-rowMaxs(cbind(temp$PercentOfFederalBin3[subset3]*temp$federalcdctc[subset3], temp$PercentOfExpensesBin3[subset3]*temp$qualifyingExpenses[subset3]))

      subset4<- temp$income.base>temp$IncomeBin3Max & temp$income.base<=temp$IncomeBin4Max
      temp$value.statecdctc[subset4]<-rowMaxs(cbind(temp$PercentOfFederalBin4[subset4]*temp$federalcdctc[subset4], temp$PercentOfExpensesBin4[subset4]*temp$qualifyingExpenses[subset4]))

      subset5<- temp$income.base>temp$IncomeBin4Max & temp$income.base<=temp$IncomeBin5Max
      temp$value.statecdctc[subset5]<-rowMaxs(cbind(temp$PercentOfFederalBin5[subset5]*temp$federalcdctc[subset5], temp$PercentOfExpensesBin5[subset5]*temp$qualifyingExpenses[subset5]))

      subset6<- temp$income.base>temp$IncomeBin5Max & temp$income.base<=temp$IncomeBin6Max
      temp$value.statecdctc[subset6]<-rowMaxs(cbind(temp$PercentOfFederalBin6[subset6]*temp$federalcdctc[subset6], temp$PercentOfExpensesBin6[subset6]*temp$qualifyingExpenses[subset6]))

      subset7<- temp$income.base>temp$IncomeBin6Max & temp$income.base<=temp$IncomeBin7Max
      temp$value.statecdctc[subset7]<-rowMaxs(cbind(temp$PercentOfFederalBin7[subset7]*temp$federalcdctc[subset7], temp$PercentOfExpensesBin6[subset7]*temp$qualifyingExpenses[subset7]))

      subset8<- data$income.base>data$IncomeBin7Max & data$income.base<=data$IncomeBin8Max
      data$value.statecdctc[subset8]<-rowMaxs(cbind(data$PercentOfFederalBin8[subset8]*data$federalcdctc[subset8], data$PercentOfExpensesBin8[subset8]*data$qualifyingExpenses[subset8]))

      subset9<- data$income.base>data$IncomeBin8Max & data$income.base<=data$IncomeBin9Max
      data$value.statecdctc[subset9]<-rowMaxs(cbind(data$PercentOfFederalBin9[subset9]*data$federalcdctc[subset9], data$PercentOfExpensesBin9[subset9]*data$qualifyingExpenses[subset9]))

      subset10<- data$income.base>data$IncomeBin9Max & data$income.base<=data$IncomeBin10Max
      data$value.statecdctc[subset10]<-rowMaxs(cbind(data$PercentOfFederalBin10[subset10]*data$federalcdctc[subset10], data$PercentOfExpensesBin10[subset10]*data$qualifyingExpenses[subset10]))

      subset11<- data$income.base>data$IncomeBin10Max & data$income.base<=data$IncomeBin11Max
      data$value.statecdctc[subset11]<-rowMaxs(cbind(data$PercentOfFederalBin11[subset11]*data$federalcdctc[subset11], data$PercentOfExpensesBin11[subset11]*data$qualifyingExpenses[subset11]))

      # Adjust for refundability
      subset<-temp$income.base>25000
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))

      # Replace back
      data[data$stateFIPS==22,]<-temp
    }


    #-------------------------------------
    #3. Maine (stateFIPS==23)
    #-------------------------------------
    if(23 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==23,]

      # SPECIAL RULE: In Maine, the tax credit is refundable up to $500.

      subset1<- temp$income.base<=temp$IncomeBin1Max
      temp$value.statecdctc[subset1]<-rowMaxs(cbind(temp$PercentOfFederalBin1[subset1]*temp$federalcdctc[subset1], temp$PercentOfExpensesBin1[subset1]*temp$qualifyingExpenses[subset1]))

      subset2<- temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.statecdctc[subset2]<-rowMaxs(cbind(temp$PercentOfFederalBin2[subset2]*temp$federalcdctc[subset2], temp$PercentOfExpensesBin2[subset2]*temp$qualifyingExpenses[subset2]))

      subset3<- temp$income.base>temp$IncomeBin2Max & temp$income.base<=temp$IncomeBin3Max
      temp$value.statecdctc[subset3]<-rowMaxs(cbind(temp$PercentOfFederalBin3[subset3]*temp$federalcdctc[subset3], temp$PercentOfExpensesBin3[subset3]*temp$qualifyingExpenses[subset3]))

      subset4<- temp$income.base>temp$IncomeBin3Max & temp$income.base<=temp$IncomeBin4Max
      temp$value.statecdctc[subset4]<-rowMaxs(cbind(temp$PercentOfFederalBin4[subset4]*temp$federalcdctc[subset4], temp$PercentOfExpensesBin4[subset4]*temp$qualifyingExpenses[subset4]))

      subset5<- temp$income.base>temp$IncomeBin4Max & temp$income.base<=temp$IncomeBin5Max
      temp$value.statecdctc[subset5]<-rowMaxs(cbind(temp$PercentOfFederalBin5[subset5]*temp$federalcdctc[subset5], temp$PercentOfExpensesBin5[subset5]*temp$qualifyingExpenses[subset5]))

      subset6<- temp$income.base>temp$IncomeBin5Max & temp$income.base<=temp$IncomeBin6Max
      temp$value.statecdctc[subset6]<-rowMaxs(cbind(temp$PercentOfFederalBin6[subset6]*temp$federalcdctc[subset6], temp$PercentOfExpensesBin6[subset6]*temp$qualifyingExpenses[subset6]))

      subset7<- temp$income.base>temp$IncomeBin6Max & temp$income.base<=temp$IncomeBin7Max
      temp$value.statecdctc[subset7]<-rowMaxs(cbind(temp$PercentOfFederalBin7[subset7]*temp$federalcdctc[subset7], temp$PercentOfExpensesBin6[subset7]*temp$qualifyingExpenses[subset7]))

      subset8<- data$income.base>data$IncomeBin7Max & data$income.base<=data$IncomeBin8Max
      data$value.statecdctc[subset8]<-rowMaxs(cbind(data$PercentOfFederalBin8[subset8]*data$federalcdctc[subset8], data$PercentOfExpensesBin8[subset8]*data$qualifyingExpenses[subset8]))

      subset9<- data$income.base>data$IncomeBin8Max & data$income.base<=data$IncomeBin9Max
      data$value.statecdctc[subset9]<-rowMaxs(cbind(data$PercentOfFederalBin9[subset9]*data$federalcdctc[subset9], data$PercentOfExpensesBin9[subset9]*data$qualifyingExpenses[subset9]))

      subset10<- data$income.base>data$IncomeBin9Max & data$income.base<=data$IncomeBin10Max
      data$value.statecdctc[subset10]<-rowMaxs(cbind(data$PercentOfFederalBin10[subset10]*data$federalcdctc[subset10], data$PercentOfExpensesBin10[subset10]*data$qualifyingExpenses[subset10]))

      subset11<- data$income.base>data$IncomeBin10Max & data$income.base<=data$IncomeBin11Max
      data$value.statecdctc[subset11]<-rowMaxs(cbind(data$PercentOfFederalBin11[subset11]*data$federalcdctc[subset11], data$PercentOfExpensesBin11[subset11]*data$qualifyingExpenses[subset11]))

      # Adjust for refundability
      subset<-temp$value.statecdctc>500
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))

      # Replace back
      data[data$stateFIPS==23,]<-temp
    }


    #-------------------------------------
    #4. Nebraska (stateFIPS==31)
    #-------------------------------------
    if(31 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==31,]

      # SPECIAL RULE: The Nebraska tax credit is refundable for families with income below $29,000.

      subset1<- temp$income.base<=temp$IncomeBin1Max
      temp$value.statecdctc[subset1]<-rowMaxs(cbind(temp$PercentOfFederalBin1[subset1]*temp$federalcdctc[subset1], temp$PercentOfExpensesBin1[subset1]*temp$qualifyingExpenses[subset1]))

      subset2<- temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.statecdctc[subset2]<-rowMaxs(cbind(temp$PercentOfFederalBin2[subset2]*temp$federalcdctc[subset2], temp$PercentOfExpensesBin2[subset2]*temp$qualifyingExpenses[subset2]))

      subset3<- temp$income.base>temp$IncomeBin2Max & temp$income.base<=temp$IncomeBin3Max
      temp$value.statecdctc[subset3]<-rowMaxs(cbind(temp$PercentOfFederalBin3[subset3]*temp$federalcdctc[subset3], temp$PercentOfExpensesBin3[subset3]*temp$qualifyingExpenses[subset3]))

      subset4<- temp$income.base>temp$IncomeBin3Max & temp$income.base<=temp$IncomeBin4Max
      temp$value.statecdctc[subset4]<-rowMaxs(cbind(temp$PercentOfFederalBin4[subset4]*temp$federalcdctc[subset4], temp$PercentOfExpensesBin4[subset4]*temp$qualifyingExpenses[subset4]))

      subset5<- temp$income.base>temp$IncomeBin4Max & temp$income.base<=temp$IncomeBin5Max
      temp$value.statecdctc[subset5]<-rowMaxs(cbind(temp$PercentOfFederalBin5[subset5]*temp$federalcdctc[subset5], temp$PercentOfExpensesBin5[subset5]*temp$qualifyingExpenses[subset5]))

      subset6<- temp$income.base>temp$IncomeBin5Max & temp$income.base<=temp$IncomeBin6Max
      temp$value.statecdctc[subset6]<-rowMaxs(cbind(temp$PercentOfFederalBin6[subset6]*temp$federalcdctc[subset6], temp$PercentOfExpensesBin6[subset6]*temp$qualifyingExpenses[subset6]))

      subset7<- temp$income.base>temp$IncomeBin6Max & temp$income.base<=temp$IncomeBin7Max
      temp$value.statecdctc[subset7]<-rowMaxs(cbind(temp$PercentOfFederalBin7[subset7]*temp$federalcdctc[subset7], temp$PercentOfExpensesBin6[subset7]*temp$qualifyingExpenses[subset7]))

      subset8<- data$income.base>data$IncomeBin7Max & data$income.base<=data$IncomeBin8Max
      data$value.statecdctc[subset8]<-rowMaxs(cbind(data$PercentOfFederalBin8[subset8]*data$federalcdctc[subset8], data$PercentOfExpensesBin8[subset8]*data$qualifyingExpenses[subset8]))

      subset9<- data$income.base>data$IncomeBin8Max & data$income.base<=data$IncomeBin9Max
      data$value.statecdctc[subset9]<-rowMaxs(cbind(data$PercentOfFederalBin9[subset9]*data$federalcdctc[subset9], data$PercentOfExpensesBin9[subset9]*data$qualifyingExpenses[subset9]))

      subset10<- data$income.base>data$IncomeBin9Max & data$income.base<=data$IncomeBin10Max
      data$value.statecdctc[subset10]<-rowMaxs(cbind(data$PercentOfFederalBin10[subset10]*data$federalcdctc[subset10], data$PercentOfExpensesBin10[subset10]*data$qualifyingExpenses[subset10]))

      subset11<- data$income.base>data$IncomeBin10Max & data$income.base<=data$IncomeBin11Max
      data$value.statecdctc[subset11]<-rowMaxs(cbind(data$PercentOfFederalBin11[subset11]*data$federalcdctc[subset11], data$PercentOfExpensesBin11[subset11]*data$qualifyingExpenses[subset11]))

      # Adjust for refundability
      subset<-temp$income.base>temp$IncomeBin8Max
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))

      # Replace back
      data[data$stateFIPS==31,]<-temp
    }

    data$value.statecdctc<-round(data$value.statecdctc,0)

    # Plug states with CTC back
    data_main[data_main$stateFIPS %in% states_with_cdctc,]<-data

    return(data_main$value.statecdctc)
  }


# Federal Insurance Contribution Act (FICA) Tax----

function.ficatax<-function(data
                               , employmentincomevar){

    colnames(data)[colnames(data)==employmentincomevar]<-"income.base"

    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ficataxData$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ficataxData$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(FilingStatus=unique(ficataxData$FilingStatus), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ficataxData[ficataxData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("FilingStatus")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }# Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ficataxData<-ficataxData %>% rbind(expand)}

    # We have historical rules
    data<-left_join(data, ficataxData, by=c("ruleYear","FilingStatus"))

    data$tax.ss<-rowMins(cbind(data$income.base,data$SocialSecurityTaxBase))*data$SocialSecurityTaxRate

    # Calculate income tax for each bracket separately
    data$taxableincome.bin1<-rowMaxs(cbind((data$income.base-0)-rowMaxs(cbind(data$income.base-data$IncomeBin1Max,0)),0))
    data$taxableincome.bin2<-rowMaxs(cbind((data$income.base-data$IncomeBin1Max),0))

    data$value.medicaretax<-data$taxableincome.bin1*data$MedicareTaxRateBin1+data$taxableincome.bin2*data$MedicareTaxRateBin2

    data$value.ficatax<-data$tax.ss+data$value.medicaretax

    data$value.ficatax<-round(data$value.ficatax,0)

    return(data$value.ficatax)
  }






