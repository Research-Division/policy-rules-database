# CCDF function (new) ----

function.CCDFcopay<-function(data
                             , contelig.ccdf = TRUE
){
  
  if(min(data$ruleYear)<=2024){
    
  
  data$income <- data$income+data$income.gift
  
  data$totcopay<-NA
  
  data$InitialEligibility<-NA
  
  data$netexp.childcareperson1 <- data$netexp.childcare
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
           child12_5to12 = case_when(agePerson12>=5 & agePerson12<=12 ~1, TRUE ~ 0))
  
  data<- data %>%   mutate(netexpchildcare0to4= netexp.childcareperson1*child1_0to4 + netexp.childcareperson2*child2_0to4 +netexp.childcareperson3*child3_0to4+netexp.childcareperson4*child4_0to4+ netexp.childcareperson5*child5_0to4 + netexp.childcareperson6*child6_0to4+ netexp.childcareperson7*child7_0to4+ netexp.childcareperson8*child8_0to4+ netexp.childcareperson9*child9_0to4+ netexp.childcareperson10*child10_0to4+ netexp.childcareperson11*child11_0to4+ netexp.childcareperson12*child12_0to4,
                           childcareexp0to4= childcare.exp.person1*child1_0to4 + childcare.exp.person2*child2_0to4 +childcare.exp.person3*child3_0to4+childcare.exp.person4*child4_0to4+ childcare.exp.person5*child5_0to4 + childcare.exp.person6*child6_0to4 + childcare.exp.person7*child7_0to4 + childcare.exp.person8*child8_0to4 + childcare.exp.person9*child9_0to4 + childcare.exp.person10*child10_0to4 + childcare.exp.person11*child11_0to4 + childcare.exp.person12*child12_0to4,
                           netexpchildcare5to12= netexp.childcareperson1*child1_5to12 + netexp.childcareperson2*child2_5to12 +netexp.childcareperson3*child3_5to12+netexp.childcareperson4*child4_5to12+ netexp.childcareperson5*child5_5to12 + netexp.childcareperson6*child6_5to12 + netexp.childcareperson7*child7_5to12 + netexp.childcareperson8*child8_5to12 + netexp.childcareperson9*child9_5to12 + netexp.childcareperson10*child10_5to12 + netexp.childcareperson11*child11_5to12 + netexp.childcareperson12*child12_5to12,
                           childcareexp5to12= childcare.exp.person1*child1_5to12 + childcare.exp.person2*child2_5to12 +childcare.exp.person3*child3_5to12+childcare.exp.person4*child4_5to12+ childcare.exp.person5*child5_5to12 + childcare.exp.person6*child6_5to12 + childcare.exp.person7*child7_5to12 + childcare.exp.person8*child8_5to12 + childcare.exp.person9*child9_5to12 + childcare.exp.person10*child10_5to12 + childcare.exp.person11*child11_5to12 + childcare.exp.person12*child12_5to12,
                           daysofcareneeded0to4 = netexpchildcare0to4/childcareexp0to4 * (parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
                           daysofcareneeded5to12 = netexpchildcare5to12/childcareexp5to12 * (0.5*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]))) #school age kids get pt care in summer & school year
  
  data$daysofcareneeded0to4[is.na(data$daysofcareneeded0to4)]<-0
  data$daysofcareneeded5to12[is.na(data$daysofcareneeded5to12)]<-0
  
  
  # Alabama ----
  
  # Description:
  # Copay is a dollar amount per child
  # Daily frequency
  if(1 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==1,]
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_AL$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_AL$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_AL$stateFIPS), AKorHI=unique(ccdfData_AL$AKorHI), famsize=unique(ccdfData_AL$famsize), numkidsInCare=unique(ccdfData_AL$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_AL[ccdfData_AL$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_AL$stateFIPS), AKorHI=unique(ccdfData_AL$AKorHI), famsize=unique(ccdfData_AL$famsize), numkidsInCare=unique(ccdfData_AL$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_AL, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_AL<-ccdfData_AL %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_AL<-ccdfData_AL %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_AL, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare", "ruleYear"))
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    
    # Adjust for the income disregard
    #temp$income<-temp$income-12*temp$IncomeDisregard
    
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
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    temp$totcopay<-NA
    
    temp$totcopay<-temp$FTcopay*52
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==1]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==1]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==1]<-temp$InitialEligibility.y
  }
  
  
  # ALASKA ----
  
  
  if(2 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_AK$stateFIPS <- 2
    
    temp<-data[data$stateFIPS==2,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_AK$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_AK$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_AK$stateFIPS), AKorHI=unique(ccdfData_AK$AKorHI), famsize=unique(ccdfData_AK$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_AK[ccdfData_AK$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_AK$stateFIPS), AKorHI=unique(ccdfData_AK$AKorHI), famsize=unique(ccdfData_AK$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_AK, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_AK<-ccdfData_AK %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_AK<-ccdfData_AK %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_AK, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[which(temp$income>=0 & temp$income<=temp$Bin1Max)]<-temp$CopayBin1[which(temp$income>=0 & temp$income<=temp$Bin1Max)]
    temp$FTcopay[which(temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max)]<-temp$CopayBin2[which(temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max)]
    temp$FTcopay[which(temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max)]<-temp$CopayBin3[which(temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max)]
    temp$FTcopay[which(temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max)]<-temp$CopayBin4[which(temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max)]
    temp$FTcopay[which(temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max)]<-temp$CopayBin5[which(temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max)]
    temp$FTcopay[which(temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max)]<-temp$CopayBin6[which(temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max)]
    temp$FTcopay[which(temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max)]<-temp$CopayBin7[which(temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max)]
    temp$FTcopay[which(temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max)]<-temp$CopayBin8[which(temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max)]
    temp$FTcopay[which(temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max)]<-temp$CopayBin9[which(temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max)]
    temp$FTcopay[which(temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max)]<-temp$CopayBin10[which(temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max)]
    temp$FTcopay[which(temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max)]<-temp$CopayBin11[which(temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max)]
    temp$FTcopay[which(temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max)]<-temp$CopayBin12[which(temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max)]
    temp$FTcopay[which(temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max)]<-temp$CopayBin13[which(temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max)]
    temp$FTcopay[which(temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max)]<-temp$CopayBin14[which(temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max)]
    temp$FTcopay[which(temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max)]<-temp$CopayBin15[which(temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max)]
    temp$FTcopay[which(temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max)]<-temp$CopayBin16[which(temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max)]
    temp$FTcopay[which(temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max)]<-temp$CopayBin17[which(temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max)]
    temp$FTcopay[which(temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max)]<-temp$CopayBin18[which(temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max)]
    temp$FTcopay[which(temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max)]<-temp$CopayBin19[which(temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max)]
    temp$FTcopay[which(temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max)]<-temp$CopayBin20[which(temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max)]
    temp$FTcopay[which(temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max)]<-temp$CopayBin21[which(temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max)]
    temp$FTcopay[which(temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max)]<-temp$CopayBin22[which(temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max)]
    temp$FTcopay[which(temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max)]<-temp$CopayBin23[which(temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max)]
    temp$FTcopay[which(temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max)]<-temp$CopayBin24[which(temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max)]
    temp$FTcopay[which(temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max)]<-temp$CopayBin25[which(temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max)]
    temp$FTcopay[which(temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max)]<-temp$CopayBin26[which(temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max)]
    temp$FTcopay[which(temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max)]<-temp$CopayBin27[which(temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max)]
    temp$FTcopay[which(temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max)]<-temp$CopayBin28[which(temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max)]
    temp$FTcopay[which(temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max)]<-temp$CopayBin29[which(temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max)]
    temp$FTcopay[which(temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max)]<-temp$CopayBin30[which(temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max)]
    temp$FTcopay[which(temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max)]<-temp$CopayBin31[which(temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max)]
    temp$FTcopay[which(temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max)]<-temp$CopayBin32[which(temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max)]
    temp$FTcopay[which(temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max)]<-temp$CopayBin33[which(temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max)]
    temp$FTcopay[which(temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max)]<-temp$CopayBin34[which(temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max)]
    temp$FTcopay[which(temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max)]<-temp$CopayBin35[which(temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max)]
    temp$FTcopay[which(temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max)]<-temp$CopayBin36[which(temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max)]
    temp$FTcopay[which(temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max)]<-temp$CopayBin37[which(temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max)]
    temp$FTcopay[which(temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max)]<-temp$CopayBin38[which(temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max)]
    temp$FTcopay[which(temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max)]<-temp$CopayBin39[which(temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max)]
    temp$FTcopay[which(temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max)]<-temp$CopayBin40[which(temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max)]
    temp$FTcopay[which(temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max)]<-temp$CopayBin41[which(temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max)]
    temp$FTcopay[which(temp$income>temp$Bin41Max & temp$income<=temp$Bin42Max)]<-temp$CopayBin42[which(temp$income>temp$Bin41Max & temp$income<=temp$Bin42Max)]
    temp$FTcopay[which(temp$income>temp$Bin42Max & temp$income<=temp$Bin43Max)]<-temp$CopayBin43[which(temp$income>temp$Bin42Max & temp$income<=temp$Bin43Max)]
    temp$FTcopay[which(temp$income>temp$Bin43Max & temp$income<=temp$Bin44Max)]<-temp$CopayBin44[which(temp$income>temp$Bin43Max & temp$income<=temp$Bin44Max)]
    temp$FTcopay[which(temp$income>temp$Bin44Max & temp$income<=temp$Bin45Max)]<-temp$CopayBin45[which(temp$income>temp$Bin44Max & temp$income<=temp$Bin45Max)]
    temp$FTcopay[which(temp$income>temp$Bin45Max & temp$income<=temp$Bin46Max)]<-temp$CopayBin46[which(temp$income>temp$Bin45Max & temp$income<=temp$Bin46Max)]
    temp$FTcopay[which(temp$income>temp$Bin46Max & temp$income<=temp$Bin47Max)]<-temp$CopayBin47[which(temp$income>temp$Bin46Max & temp$income<=temp$Bin47Max)]
    temp$FTcopay[which(temp$income>temp$Bin47Max & temp$income<=temp$Bin48Max)]<-temp$CopayBin48[which(temp$income>temp$Bin47Max & temp$income<=temp$Bin48Max)]
    temp$FTcopay[which(temp$income>temp$Bin48Max & temp$income<=temp$Bin49Max)]<-temp$CopayBin49[which(temp$income>temp$Bin48Max & temp$income<=temp$Bin49Max)]
    temp$FTcopay[which(temp$income>temp$Bin49Max & temp$income<=temp$Bin50Max)]<-temp$CopayBin50[which(temp$income>temp$Bin49Max & temp$income<=temp$Bin50Max)]
    temp$FTcopay[which(temp$income>temp$Bin50Max & temp$income<=temp$Bin51Max)]<-temp$CopayBin51[which(temp$income>temp$Bin50Max & temp$income<=temp$Bin51Max)]
    temp$FTcopay[which(temp$income>temp$Bin51Max & temp$income<=temp$Bin52Max)]<-temp$CopayBin52[which(temp$income>temp$Bin51Max & temp$income<=temp$Bin52Max)]
    temp$FTcopay[which(temp$income>temp$Bin52Max & temp$income<=temp$Bin53Max)]<-temp$CopayBin53[which(temp$income>temp$Bin52Max & temp$income<=temp$Bin53Max)]
    temp$FTcopay[which(temp$income>temp$Bin53Max & temp$income<=temp$Bin54Max)]<-temp$CopayBin54[which(temp$income>temp$Bin53Max & temp$income<=temp$Bin54Max)]
    temp$FTcopay[which(temp$income>temp$Bin54Max & temp$income<=temp$Bin55Max)]<-temp$CopayBin55[which(temp$income>temp$Bin54Max & temp$income<=temp$Bin55Max)]
    temp$FTcopay[which(temp$income>temp$Bin55Max & temp$income<=temp$Bin56Max)]<-temp$CopayBin56[which(temp$income>temp$Bin55Max & temp$income<=temp$Bin56Max)]
    temp$FTcopay[which(temp$income>temp$Bin56Max & temp$income<=temp$Bin57Max)]<-temp$CopayBin57[which(temp$income>temp$Bin56Max & temp$income<=temp$Bin57Max)]
    temp$FTcopay[which(temp$income>temp$Bin57Max & temp$income<=temp$Bin58Max)]<-temp$CopayBin58[which(temp$income>temp$Bin57Max & temp$income<=temp$Bin58Max)]
    temp$FTcopay[which(temp$income>temp$Bin58Max & temp$income<=temp$Bin59Max)]<-temp$CopayBin59[which(temp$income>temp$Bin58Max & temp$income<=temp$Bin59Max)]
    temp$FTcopay[which(temp$income>temp$Bin59Max & temp$income<=temp$Bin60Max)]<-temp$CopayBin60[which(temp$income>temp$Bin59Max & temp$income<=temp$Bin60Max)]
    temp$FTcopay[which(temp$income>temp$Bin60Max & temp$income<=temp$Bin61Max)]<-temp$CopayBin61[which(temp$income>temp$Bin60Max & temp$income<=temp$Bin61Max)]
    temp$FTcopay[which(temp$income>temp$Bin61Max & temp$income<=temp$Bin62Max)]<-temp$CopayBin62[which(temp$income>temp$Bin61Max & temp$income<=temp$Bin62Max)]
    temp$FTcopay[which(temp$income>temp$Bin62Max & temp$income<=temp$Bin63Max)]<-temp$CopayBin63[which(temp$income>temp$Bin62Max & temp$income<=temp$Bin63Max)]
    temp$FTcopay[which(temp$income>temp$Bin63Max & temp$income<=temp$Bin64Max)]<-temp$CopayBin64[which(temp$income>temp$Bin63Max & temp$income<=temp$Bin64Max)]
    temp$FTcopay[which(temp$income>temp$Bin64Max & temp$income<=temp$Bin65Max)]<-temp$CopayBin65[which(temp$income>temp$Bin64Max & temp$income<=temp$Bin65Max)]
    temp$FTcopay[which(temp$income>temp$Bin65Max & temp$income<=temp$Bin66Max)]<-temp$CopayBin66[which(temp$income>temp$Bin65Max & temp$income<=temp$Bin66Max)]
    temp$FTcopay[which(temp$income>temp$Bin66Max & temp$income<=temp$Bin67Max)]<-temp$CopayBin67[which(temp$income>temp$Bin66Max & temp$income<=temp$Bin67Max)]
    temp$FTcopay[which(temp$income>temp$Bin67Max & temp$income<=temp$Bin68Max)]<-temp$CopayBin68[which(temp$income>temp$Bin67Max & temp$income<=temp$Bin68Max)]
    temp$FTcopay[which(temp$income>temp$Bin68Max & temp$income<=temp$Bin69Max)]<-temp$CopayBin69[which(temp$income>temp$Bin68Max & temp$income<=temp$Bin69Max)]
    temp$FTcopay[which(temp$income>temp$Bin69Max & temp$income<=temp$Bin70Max)]<-temp$CopayBin70[which(temp$income>temp$Bin69Max & temp$income<=temp$Bin70Max)]
    temp$FTcopay[which(temp$income>temp$Bin70Max & temp$income<=temp$Bin71Max)]<-temp$CopayBin71[which(temp$income>temp$Bin70Max & temp$income<=temp$Bin71Max)]
    temp$FTcopay[which(temp$income>temp$Bin71Max & temp$income<=temp$Bin72Max)]<-temp$CopayBin72[which(temp$income>temp$Bin71Max & temp$income<=temp$Bin72Max)]
    temp$FTcopay[which(temp$income>temp$Bin72Max & temp$income<=temp$Bin73Max)]<-temp$CopayBin73[which(temp$income>temp$Bin72Max & temp$income<=temp$Bin73Max)]
    temp$FTcopay[which(temp$income>temp$Bin73Max & temp$income<=temp$Bin74Max)]<-temp$CopayBin74[which(temp$income>temp$Bin73Max & temp$income<=temp$Bin74Max)]
    temp$FTcopay[which(temp$income>temp$Bin74Max & temp$income<=temp$Bin75Max)]<-temp$CopayBin75[which(temp$income>temp$Bin74Max & temp$income<=temp$Bin75Max)]
    temp$FTcopay[which(temp$income>temp$Bin75Max & temp$income<=temp$Bin76Max)]<-temp$CopayBin76[which(temp$income>temp$Bin75Max & temp$income<=temp$Bin76Max)]
    temp$FTcopay[which(temp$income>temp$Bin76Max & temp$income<=temp$Bin77Max)]<-temp$CopayBin77[which(temp$income>temp$Bin76Max & temp$income<=temp$Bin77Max)]
    temp$FTcopay[which(temp$income>temp$Bin77Max & temp$income<=temp$Bin78Max)]<-temp$CopayBin78[which(temp$income>temp$Bin77Max & temp$income<=temp$Bin78Max)]
    temp$FTcopay[which(temp$income>temp$Bin78Max & temp$income<=temp$Bin79Max)]<-temp$CopayBin79[which(temp$income>temp$Bin78Max & temp$income<=temp$Bin79Max)]
    temp$FTcopay[which(temp$income>temp$Bin79Max & temp$income<=temp$Bin80Max)]<-temp$CopayBin80[which(temp$income>temp$Bin79Max & temp$income<=temp$Bin80Max)]
    temp$FTcopay[which(temp$income>temp$Bin80Max & temp$income<=temp$Bin81Max)]<-temp$CopayBin81[which(temp$income>temp$Bin80Max & temp$income<=temp$Bin81Max)]
    temp$FTcopay[which(temp$income>temp$Bin81Max & temp$income<=temp$Bin82Max)]<-temp$CopayBin82[which(temp$income>temp$Bin81Max & temp$income<=temp$Bin82Max)]
    temp$FTcopay[which(temp$income>temp$Bin82Max & temp$income<=temp$Bin83Max)]<-temp$CopayBin83[which(temp$income>temp$Bin82Max & temp$income<=temp$Bin83Max)]
    temp$FTcopay[which(temp$income>temp$Bin83Max & temp$income<=temp$Bin84Max)]<-temp$CopayBin84[which(temp$income>temp$Bin83Max & temp$income<=temp$Bin84Max)]
    temp$FTcopay[which(temp$income>temp$Bin84Max & temp$income<=temp$Bin85Max)]<-temp$CopayBin85[which(temp$income>temp$Bin84Max & temp$income<=temp$Bin85Max)]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==2]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==2]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==2]<-temp$InitialEligibility.y
  }
  
  
  
  # ARIZONA ----
  
  # Description:
  # Copay is a fixed dollar amount per child
  # Daily frequency
  if(4 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_AZ$stateFIPS <- 4
    
    temp<-data[data$stateFIPS==4,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_AZ$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_AZ$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_AZ$stateFIPS), AKorHI=unique(ccdfData_AZ$AKorHI), famsize=unique(ccdfData_AZ$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_AZ[ccdfData_AZ$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_AZ$stateFIPS), AKorHI=unique(ccdfData_AZ$AKorHI), famsize=unique(ccdfData_AZ$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_AZ, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_AZ<-ccdfData_AZ %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_AZ<-ccdfData_AZ %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_AZ, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    
    temp$totcopay<-NA
    
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==4]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==4]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==4]<-temp$InitialEligibility.y
  }
  
  
  # Arkansas ----
  
  # Description:
  # Copay is a percentage of total income
  # Monthly frequency
  if(5 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==5,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_AR$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_AR$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_AR$stateFIPS), AKorHI=unique(ccdfData_AR$AKorHI), famsize=unique(ccdfData_AR$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_AR[ccdfData_AR$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_AR$stateFIPS), AKorHI=unique(ccdfData_AR$AKorHI), famsize=unique(ccdfData_AR$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_AR, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_AR<-ccdfData_AR %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_AR<-ccdfData_AR %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_AR, by=c("stateFIPS", "AKorHI", "famsize", "ruleYear"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==5]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==5]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==5]<-temp$InitialEligibility.y
  }
  
  
  
  # CALIFORNIA ----
  
  # Description:
  # Copay is a fixed amount per family
  # Monthly frequency
  #! need to add the full time versus part time rule (pt copay is half of ft copay)
  if(6 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_CA$stateFIPS <- 6
    
    temp<-data[data$stateFIPS==6,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_CA$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_CA$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_CA$stateFIPS), AKorHI=unique(ccdfData_CA$AKorHI), famsize=unique(ccdfData_CA$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_CA[ccdfData_CA$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_CA$stateFIPS), AKorHI=unique(ccdfData_CA$AKorHI), famsize=unique(ccdfData_CA$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_CA, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_CA<-ccdfData_CA %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_CA<-ccdfData_CA %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_CA, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    temp$FTcopay[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]<-temp$CopayBin31[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]
    temp$FTcopay[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]<-temp$CopayBin32[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]
    temp$FTcopay[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]<-temp$CopayBin33[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]
    temp$FTcopay[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]<-temp$CopayBin34[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]
    temp$FTcopay[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]<-temp$CopayBin35[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]
    temp$FTcopay[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]<-temp$CopayBin36[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]
    temp$FTcopay[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]<-temp$CopayBin37[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]
    temp$FTcopay[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]<-temp$CopayBin38[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]
    temp$FTcopay[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]<-temp$CopayBin39[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]
    temp$FTcopay[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]<-temp$CopayBin40[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    #set copays to 0 if family has no kids that are not in head start
    temp$FTcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    
    #calculation # of months needed
    temp<- temp %>%
      mutate(monthsofcareneeded0to4=12*daysofcareneeded0to4/(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
             monthsofcareneeded5to12=12*daysofcareneeded5to12/ (0.5*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])))#school age kids get pt care in summer
    
    temp$monthsofcareneeded=rowMaxs(cbind(temp$monthsofcareneeded0to4,temp$monthsofcareneeded5to12))
    
    # Calculate total copay (12 months needed)
    temp$FTcopay[!is.na(temp$FTcopay)]<-as.numeric(temp$FTcopay[!is.na(temp$FTcopay)])*temp$monthsofcareneeded[!is.na(temp$FTcopay)]
    
    
    #copay is per family so no need to calculate totalcopay
    
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Make sure the variables names are the same
    temp$totcopay <- temp$FTcopay #totcopay is initilaized to NA in beginning
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==6]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==6]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==6]<-temp$InitialEligibility.y
  }
  
  # Colorado ----
  
  if(8 %in% unique(data$stateFIPS)){
    
    ccdfData_CO$stateFIPS <- 8
    
    temp <- data[data$stateFIPS==8,]
    
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_CO$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_CO$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_CO$stateFIPS), AKorHI=unique(ccdfData_CO$AKorHI), famsize=unique(ccdfData_CO$famsize), countyortownName=unique(ccdfData_CO$countyortownName), numkidsInCare=unique(ccdfData_CO$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_CO[ccdfData_CO$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize","countyortownName", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_CO$stateFIPS), AKorHI=unique(ccdfData_CO$AKorHI), famsize=unique(ccdfData_CO$famsize), countyortownName=unique(ccdfData_CO$countyortownName), numkidsInCare=unique(ccdfData_CO$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_CO, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_CO<-ccdfData_CO %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_CO<-ccdfData_CO %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_CO, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "countyortownName", "numkidsInCare"))
    
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    temp$COPAY <- 0
    temp$x <- 0
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max] # Add $15 if user is over 100% FPL and have more than one kid in care
    
    temp$COPAY[temp$income>=0 & temp$income<=temp$Bin1Max] <- temp$income[temp$income>=0 & temp$income<=temp$Bin1Max]*temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$COPAY[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max] <- temp$Bin1Max[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]*0.01 + temp$x[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]*temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    
    temp$COPAY[temp$numkidsInCare > 1 & !is.na(temp$numkidsInCare) & temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max] <- temp$COPAY[temp$numkidsInCare > 1 & !is.na(temp$numkidsInCare) & temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max] + (temp$numkidsInCare[temp$numkidsInCare > 1 & !is.na(temp$numkidsInCare) & temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]*15)
    
    
    
    temp$totcopay<-rowMins(cbind(temp$COPAY,temp$netexp.childcare))
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Set copay to zero if no children
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==8]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==8]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==8]<-temp$InitialEligibility.y
  }
  
  # CONNECTICUT ----
  
  # Description:
  # Copay is a percentage of total income
  # Weekly frequency
  # Discount for a second child (UNDER CONSTRUCTION)
  if(9 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ############ TEMP FIX FOR CT
    
    ccdfData_CT$stateFIPS <- 9
    
    temp<-data[data$stateFIPS==9,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_CT$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_CT$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_CT$stateFIPS), AKorHI=unique(ccdfData_CT$AKorHI), famsize=unique(ccdfData_CT$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_CT[ccdfData_CT$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_CT$stateFIPS), AKorHI=unique(ccdfData_CT$AKorHI), famsize=unique(ccdfData_CT$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_CT, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_CT<-ccdfData_CT %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_CT<-ccdfData_CT %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_CT, by=c("ruleYear",  "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==9]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==9]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==9]<-temp$InitialEligibility.y
  }
  
  # DELAWARE ----
  
  if(10 %in% unique(data$stateFIPS)){
    
    ################ TEMP TO FIX DE
    
    temp <- data[data$stateFIPS==10,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_DE$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_DE$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_DE$stateFIPS), AKorHI=unique(ccdfData_DE$AKorHI), famsize=unique(ccdfData_DE$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_DE[ccdfData_DE$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)# %>%drop_na()
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_DE$stateFIPS), AKorHI=unique(ccdfData_DE$AKorHI), famsize=unique(ccdfData_DE$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_DE, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_DE<-ccdfData_DE %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_DE<-ccdfData_DE %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_DE, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==10]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==10]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==10]<-temp$InitialEligibility.y
  }
  
  
  # DISTRICT OF COLUMBIA ----
  
  # Description:
  # Fixed copay per child
  # Daily frequency
  # 65% discount for the second child, no fee charged after second child (conditional logic implemented for this feature)
  if(11 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_DC$OverageOption <- "No"
    ccdfData_DC$stateFIPS <- 11
    
    temp<-data[data$stateFIPS==11,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_DC$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_DC$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_DC$stateFIPS), AKorHI=unique(ccdfData_DC$AKorHI), famsize=unique(ccdfData_DC$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_DC[ccdfData_DC$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_DC$stateFIPS), AKorHI=unique(ccdfData_DC$AKorHI), famsize=unique(ccdfData_DC$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_DC, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_DC<-ccdfData_DC %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_DC<-ccdfData_DC %>% rbind(expandPastMiss2)}
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_DC, by=c("stateFIPS", "AKorHI", "famsize", "ruleYear"))
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    temp$FTcopay[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]<-temp$CopayBin31[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]
    temp$FTcopay[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]<-temp$CopayBin32[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]
    temp$FTcopay[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]<-temp$CopayBin33[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]
    temp$FTcopay[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]<-temp$CopayBin34[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]
    temp$FTcopay[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]<-temp$CopayBin35[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]
    temp$FTcopay[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]<-temp$CopayBin36[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]
    temp$FTcopay[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]<-temp$CopayBin37[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]
    
    
    # If a family receives TANF, copays are waived
    temp$FTcopay[temp$value.tanf>0]<-0
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    temp$totcopay<-NA
    
    # Determine how much care child need
    # age < 5 requires full-time school time care and full-time summer care
    # age 5 to 12 requires part-time school time care and full-time summer care
    
    # Scenario 1: single child that need care
    subset1<-temp$numkidsincare0to4+temp$numkidsincare5to12<=1
    temp$totcopay[subset1]<-temp$FTcopay[subset1]*(temp$daysofcareneeded0to4[subset1]+temp$daysofcareneeded5to12[subset1])
    
    # Apply 65% Discount for a second (youngest) child
    
    # Both kids are young
    subset2<-temp$numkidsincare5to12==0 & temp$numkidsincare0to4>=2
    temp$totcopay[subset2]<-0.5*temp$FTcopay[subset2]*(temp$daysofcareneeded0to4[subset2])+0.35*0.5*temp$FTcopay[subset2]*(temp$daysofcareneeded0to4[subset2])
    
    # One kid is older
    subset3<-temp$numkidsincare0to4==0 & temp$numkidsincare5to12>=2
    temp$totcopay[subset3]<-0.5*temp$FTcopay[subset3]*(temp$daysofcareneeded5to12[subset3])+0.35*0.5*temp$FTcopay[subset3]*(temp$daysofcareneeded5to12[subset3])
    
    # All other situations
    subset4<-subset1==FALSE & subset2==FALSE & subset3==FALSE
    temp$totcopay[subset4]<-temp$FTcopay[subset4]*(temp$daysofcareneeded5to12[subset4]) + 0.35*temp$FTcopay[subset4]*(temp$daysofcareneeded0to4[subset4])
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==11]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==11]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==11]<-temp$InitialEligibility.y
  }
  
  
  
  # FLORIDA ----
  
  # ORIGINAL LEGISLATION: WHEN FAMILY IS INITIALLY APPLYING TO CCDF, IF CHILD IS 5 OR ABOVE, THEN THEY DON'T QUALIFY. ONCE ON IT, THEN THEY'RE ON CCDF TILL KID IS 13 OR INCOME EXCEEDS LIMIT
  
  # NEW LEGISLATION:
  
  # IF FAMILY IS ON TANF, THEN THEY QUALIFY AS LONG AS KID(S) IS BELOW 13
  # IF FAMILY QUALIFIES SO LONG AS THEY HAVE A CHILD UNDER 5, EVEN IF OTHER KIDS ARE 5 OR ABOVE
  
  
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
    temp<-left_join(temp, ccdfData_FL, by=c("stateFIPS", "AKorHI", "countyortownName", "famsize", "ruleYear"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[which(temp$income>=0 & temp$income<=temp$Bin1Max)]<-temp$CopayBin1[which(temp$income>=0 & temp$income<=temp$Bin1Max)]
    temp$FTcopay[which(temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max)]<-temp$CopayBin2[which(temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max)]
    temp$FTcopay[which(temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max)]<-temp$CopayBin3[which(temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max)]
    temp$FTcopay[which(temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max)]<-temp$CopayBin4[which(temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max)]
    temp$FTcopay[which(temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max)]<-temp$CopayBin5[which(temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max)]
    temp$FTcopay[which(temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max)]<-temp$CopayBin6[which(temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max)]
    temp$FTcopay[which(temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max)]<-temp$CopayBin7[which(temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max)]
    temp$FTcopay[which(temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max)]<-temp$CopayBin8[which(temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max)]
    temp$FTcopay[which(temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max)]<-temp$CopayBin9[which(temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max)]
    temp$FTcopay[which(temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max)]<-temp$CopayBin10[which(temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max)]
    temp$FTcopay[which(temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max)]<-temp$CopayBin11[which(temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max)]
    temp$FTcopay[which(temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max)]<-temp$CopayBin12[which(temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max)]
    temp$FTcopay[which(temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max)]<-temp$CopayBin13[which(temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max)]
    temp$FTcopay[which(temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max)]<-temp$CopayBin14[which(temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max)]
    temp$FTcopay[which(temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max)]<-temp$CopayBin15[which(temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max)]
    temp$FTcopay[which(temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max)]<-temp$CopayBin16[which(temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max)]
    temp$FTcopay[which(temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max)]<-temp$CopayBin17[which(temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max)]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    temp$totcopay<-NA
    
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    # if(contelig.ccdf == FALSE){
    
    #    temp$totcopay[temp$daysofcareneeded0to4 == 0 & temp$daysofcareneeded5to12 != 0 & temp$value.tanf == 0 & temp$Year == min(temp$Year)] <- temp$netexp.childcare[temp$daysofcareneeded0to4 == 0 & temp$daysofcareneeded5to12 != 0 & temp$value.tanf == 0 & temp$Year == min(temp$Year)]
    
    #    if(temp$totcopay[temp$Year == min(temp$Year)] == temp$netexp.childcare[temp$Year == min(temp$Year)]){
    #      temp$totcopay <- temp$netexp.childcare
    #    }
    #  }
    
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
  
  
  # GEORGIA ----
  
  # Description:
  # Copay is a percentage of total net income
  # Weekly frequency
  # Discount for a second child (UNDER CONSTRUCTION)
  if(13 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    #   ccdfData_GA$stateFIPS <- 13
    #  ccdfData_GA$IncomeDisregard <- 0
    #  ccdfData_GA$InitialEligibility <- c(19814, 25910, 32007, 38103, 44199, 50296, 56392, 62489, 68585, 74682, 80778, 86875)
    #  ccdfData_GA$ContinuousEligibility <- c(33683, 44047, 54411, 64775, 75139, 85503, 95867, 106231, 116595, 126959, 137323, 147687)
    #  ccdfData_GA$Bin4Max <- ccdfData_GA$ContinuousEligibility
    
    # ER: code below does similar thing
    # if(min(data$ruleYear)==2021){
    # ccdfData_GA$ruleYear[ccdfData_GA$ruleYear == min(ccdfData_GA$ruleYear)] <- 2021
    # }
    
    temp<-data[data$stateFIPS==13,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_GA$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_GA$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_GA$stateFIPS), AKorHI=unique(ccdfData_GA$AKorHI), famsize=unique(ccdfData_GA$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_GA[ccdfData_GA$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_GA$stateFIPS), AKorHI=unique(ccdfData_GA$AKorHI), famsize=unique(ccdfData_GA$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_GA, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_GA<-ccdfData_GA %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_GA<-ccdfData_GA %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_GA, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    #  temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==13]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==13]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==13]<-temp$InitialEligibility.y
  }
  
  
  # HAWAII ----
  
  # Description:
  # Copay is a percentage of the child care subsidy payment earned by family (UNDER CONSTRUCTION)
  # Monthly frequency
  # Per family
  if(15 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==15,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_HI$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_HI$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_HI$stateFIPS), AKorHI=unique(ccdfData_HI$AKorHI), famsize=unique(ccdfData_HI$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_HI[ccdfData_HI$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_HI$stateFIPS), AKorHI=unique(ccdfData_HI$AKorHI), famsize=unique(ccdfData_HI$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_HI, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_HI<-ccdfData_HI %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_HI<-ccdfData_HI %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_HI, by=c("stateFIPS", "AKorHI", "famsize", "ruleYear"))
    
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
    
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    ## IMPORTANT: temp$income should be replaced with the child care subsidy earned by the family
    
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==15]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==15]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==15]<-temp$InitialEligibility.y
  }
  
  
  
  # IOWA ----
  
  # Description:
  # Copay is a fixed dollar amount per child
  # Daily frequency
  if(19 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_IA$stateFIPS <- 19
    
    
    
    temp<-data[data$stateFIPS==19,]
    
    
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_IA$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_IA$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_IA$stateFIPS), AKorHI=unique(ccdfData_IA$AKorHI), famsize=unique(ccdfData_IA$famsize), numkidsInCare=unique(ccdfData_IA$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_IA[ccdfData_IA$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_IA$stateFIPS), AKorHI=unique(ccdfData_IA$AKorHI), famsize=unique(ccdfData_IA$famsize), numkidsInCare=unique(ccdfData_IA$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_IA, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_IA<-ccdfData_IA %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_IA<-ccdfData_IA %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_IA, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    temp$totcopay<-NA
    
    # Determine how much care child need
    # age < 5 requires full-time school time care and full-time summer care
    # age 5 to 12 requires part-time school time care and full-time summer care
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==19]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==19]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==19]<-temp$InitialEligibility.y
  }
  
  # IDAHO ----
  
  if(16 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_ID$stateFIPS <- 16
    
    temp<-data[data$stateFIPS==16,]
    
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_ID$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_ID$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_ID$stateFIPS), AKorHI=unique(ccdfData_ID$AKorHI), famsize=unique(ccdfData_ID$famsize), numkidsInCare=unique(ccdfData_ID$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_ID[ccdfData_ID$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_ID$stateFIPS), AKorHI=unique(ccdfData_ID$AKorHI), famsize=unique(ccdfData_ID$famsize), numkidsInCare=unique(ccdfData_ID$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_ID, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_ID<-ccdfData_ID %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_ID<-ccdfData_ID %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_ID, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==16]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==16]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==16]<-temp$InitialEligibility.y
  }
  
  
  # ILLINOIS ----
  
  # Description:
  # Copay is a fixed amount per family
  # Monthly frequency
  # Discount in case when all kids need part-time care
  if(17 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_IL$stateFIPS <- 17
    
    
    temp<-data[data$stateFIPS==17,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_IL$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_IL$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_IL$stateFIPS), AKorHI=unique(ccdfData_IL$AKorHI), famsize=unique(ccdfData_IL$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_IL[ccdfData_IL$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_IL$stateFIPS), AKorHI=unique(ccdfData_IL$AKorHI), famsize=unique(ccdfData_IL$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_IL, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_IL<-ccdfData_IL %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_IL<-ccdfData_IL %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_IL, by=c("stateFIPS", "AKorHI", "famsize", "ruleYear"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-0.25*temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    
    # CopayBin21 explanation:
    # Family Income in the following ranges at the time of redetermination will result in a 3-month eligibility extension, known as
    # a Graduated Phase Out for more information, see CCAP Policy 02.03.01 https://www.dhs.state.il.us/page.aspx?item=10568
    
    # If results start to look incorrect for any scenario that achieves copaybin21, just eliminate from this code for the time being
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    #set copays to 0 if family has no kids that are not in head start
    temp$FTcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    # Adjust for a part-time care (school age kids only)
    temp$FTcopay[temp$numkidsincare0to4==0 & temp$numkidsincare5to12>0]<-0.5*temp$FTcopay[temp$numkidsincare0to4==0 & temp$numkidsincare5to12>0]
    
    #calculation # of months needed
    temp<- temp %>%
      mutate(monthsofcareneeded0to4=12*daysofcareneeded0to4/(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1]),
             monthsofcareneeded5to12=12*daysofcareneeded5to12/ (0.5*(parameters.defaults$numberofSummerChildcareDays[1]+parameters.defaults$numberofSchoolDays[1])))#school age kids get pt care in summer & school year
    
    temp$monthsofcareneeded=rowMaxs(cbind(temp$monthsofcareneeded0to4,temp$monthsofcareneeded5to12))
    
    # Calculate total copay (12 months needed)
    temp$FTcopay[!is.na(temp$FTcopay)]<-as.numeric(temp$FTcopay[!is.na(temp$FTcopay)])*temp$monthsofcareneeded[!is.na(temp$FTcopay)]
    
    
    #copay is per family so no need to calculate totalcopay
    
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Make sure the variables names are the same
    temp$totcopay <- temp$FTcopay #totcopay is initilaized to NA in beginning
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==17]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==17]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==17]<-temp$InitialEligibility.y
  }
  
  
  # INDIANA ----
  
  # Description:
  # Copay is a percentage of total income
  # Monthly frequency
  if(18 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_IN$stateFIPS <- 18
    ccdfData_IN$AssetTest <- 1000000
    #ccdfData_IN <- ccdfData_IN[ccdfData_IN$famsize < 8,]
    
    temp<-data[data$stateFIPS==18,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_IN$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_IN$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_IN$stateFIPS), AKorHI=unique(ccdfData_IN$AKorHI), famsize=unique(ccdfData_IN$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_IN[ccdfData_IN$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_IN$stateFIPS), AKorHI=unique(ccdfData_IN$AKorHI), famsize=unique(ccdfData_IN$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_IN, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_IN<-ccdfData_IN %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_IN<-ccdfData_IN %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_IN, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==18]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==18]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==18]<-temp$InitialEligibility.y
  }
  
  
  
  # KANSAS ----
  
  # Description:
  # Fixed copay dollar amount per family
  # Monthly frequency
  if(20 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_KS$stateFIPS <- 20
    
    
    temp<-data[data$stateFIPS==20,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_KS$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_KS$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_KS$stateFIPS), AKorHI=unique(ccdfData_KS$AKorHI), famsize=unique(ccdfData_KS$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_KS[ccdfData_KS$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_KS$stateFIPS), AKorHI=unique(ccdfData_KS$AKorHI), famsize=unique(ccdfData_KS$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_KS, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_KS<-ccdfData_KS %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_KS<-ccdfData_KS %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_KS, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==20]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==20]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==20]<-temp$InitialEligibility.y
  }
  
  
  
  # KENTUCKY ----
  
  # Description:
  # Copay is a fixed amount per child
  # Copay amount varies by number of children that family has in care
  # Daily frequency
  if(21 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_KY$stateFIPS <- 21
    
    temp<-data[data$stateFIPS==21,]
    
    
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_KY$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_KY$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_KY$stateFIPS), AKorHI=unique(ccdfData_KY$AKorHI), famsize=unique(ccdfData_KY$famsize), numkidsInCare=unique(ccdfData_KY$numkidsInCare),Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_KY[ccdfData_KY$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_KY$stateFIPS), AKorHI=unique(ccdfData_KY$AKorHI), famsize=unique(ccdfData_KY$famsize), numkidsInCare=unique(ccdfData_KY$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_KY, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_KY<-ccdfData_KY %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_KY<-ccdfData_KY %>% rbind(expandPastMiss2)}
    
    #----------------------------------``
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_KY, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    
    temp$totcopay<-NA
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==21]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==21]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==21]<-temp$InitialEligibility.y
  }
  
  
  
  # Louisiana ----
  
  # Description:
  # Copay is a dollar amount per child, same for each child
  # Daily frequency
  # No fee after certain number of children (under construction)
  if(22 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ################ TEMP FIX FOR LA
    
    ccdfData_LA$stateFIPS <- 22
    #ccdfData_LA$InitialEligibility <- c(22123, 28930, 35737, 42544, 49351, 56159, 62966, 69733, 76580, 83387, 90194, 97002)
    #ccdfData_LA$ContinuousEligibility <- c(33108, 43296, 53483, 63670, 73857, 84045, 94232, 104419, 114606, 124793, 134981, 145168)
    #ccdfData_LA$InitialEligibility <- as.numeric(ccdfData_LA$InitialEligibility)
    #ccdfData_LA$Bin5Max <- ccdfData_LA$ContinuousEligibility
    #ccdfData_LA$Bin5Max[1] <- 0
    #ccdfData_LA$Bin1Max <- ccdfData_LA$ContinuousEligibility*0.43
    ##ccdfData_LA$Bin2Max <- ccdfData_LA$ContinuousEligibility*0.59
    #ccdfData_LA$Bin3Max <- ccdfData_LA$ContinuousEligibility*0.647
    #ccdfData_LA$Bin4Max <- ccdfData_LA$ContinuousEligibility*0.823
    #ccdfData_LA$Bin1Max[1] <- 0
    #ccdfData_LA$Bin2Max[1] <- 0
    #ccdfData_LA$Bin3Max[1] <- 0
    #ccdfData_LA$Bin4Max[1] <- 0
    
    temp<-data[data$stateFIPS==22,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_LA$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_LA$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_LA$stateFIPS), AKorHI=unique(ccdfData_LA$AKorHI), famsize=unique(ccdfData_LA$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_LA[ccdfData_LA$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_LA$stateFIPS), AKorHI=unique(ccdfData_LA$AKorHI), famsize=unique(ccdfData_LA$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_LA, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_LA<-ccdfData_LA %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_LA<-ccdfData_LA %>% rbind(expandPastMiss2)}
    
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_LA, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    #  temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    
    temp$totcopay<-NA
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==22]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==22]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==22]<-temp$InitialEligibility.y
  }
  
  
  
  # MAINE ----
  
  # Description:
  # Copay is a percentage of total income
  # Monthly frequency
  if(23 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    #ccdfData_ME$stateFIPS <- 23
    #ccdfData_ME$InitialEligibility[11:12] <- c(112662,114960)
    #ccdfData_ME$ContinuousEligibility[11:12] <- c(112662,114960)
    
    temp<-data[data$stateFIPS==23,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_ME$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_ME$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_ME$stateFIPS), AKorHI=unique(ccdfData_ME$AKorHI), famsize=unique(ccdfData_ME$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_ME[ccdfData_ME$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_ME$stateFIPS), AKorHI=unique(ccdfData_ME$AKorHI), famsize=unique(ccdfData_ME$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_ME, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_ME<-ccdfData_ME %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_ME<-ccdfData_ME %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_ME, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==23]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==23]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==23]<-temp$InitialEligibility.y
  }
  
  # MARYLAND ----
  
  # Description:
  # Copay is a fixed amount based on the number of children
  # Copay amount varies by number of children that family has in care
  # Copay amount varies by region in state -> UNDER CONSTRUCTION
  # Weekly frequency
  if(24 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    # ccdfData_MD$stateFIPS <- 24
    #  ccdfData_MD$AssetTest <- 1000000
    
    temp<-data[data$stateFIPS==24,]
    
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_MD$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_MD$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_MD$stateFIPS), AKorHI=unique(ccdfData_MD$AKorHI), famsize=unique(ccdfData_MD$famsize), numkidsInCare=unique(ccdfData_MD$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_MD[ccdfData_MD$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_MD$stateFIPS), AKorHI=unique(ccdfData_MD$AKorHI), famsize=unique(ccdfData_MD$famsize), numkidsInCare=unique(ccdfData_MD$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_MD, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_MD<-ccdfData_MD %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_MD<-ccdfData_MD %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_MD, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare", "ruleYear"))
    
    # Adjust for the income disregard
    #   temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1_RegionBC[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2_RegionBC[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3_RegionBC[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4_RegionBC[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5_RegionBC[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6_RegionBC[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$CopayBin7_RegionBC[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    temp$FTcopay[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]<-temp$CopayBin8_RegionBC[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]
    temp$FTcopay[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]<-temp$CopayBin9_RegionBC[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]
    temp$FTcopay[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]<-temp$CopayBin10_RegionBC[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]
    temp$FTcopay[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]<-temp$CopayBin11_RegionBC[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    # Calculate total copay (12 months needed)
    temp$FTcopay[!is.na(temp$FTcopay)]<-as.numeric(temp$FTcopay[!is.na(temp$FTcopay)])
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Make sure the variables names are the same
    temp$totcopay <- temp$FTcopay*52 #totcopay is initilaized to NA in beginning
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==24]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==24]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==24]<-temp$InitialEligibility.y
  }
  
  
  # MASSACHUSETTS ----
  
  # Description:
  # Copay is a percentage of total income (above poverty level)
  # Monthly frequency
  if(25 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==25,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_MA$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_MA$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_MA$stateFIPS), AKorHI=unique(ccdfData_MA$AKorHI), famsize=unique(ccdfData_MA$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_MA[ccdfData_MA$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_MA$stateFIPS), AKorHI=unique(ccdfData_MA$AKorHI), famsize=unique(ccdfData_MA$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_MA, by=c("stateFIPS", "famsize", "AKorHI"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_MA<-ccdfData_MA %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_MA<-ccdfData_MA %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_MA, by=c("stateFIPS", "AKorHI", "famsize", "ruleYear"))
    
    # Adjust for the income disregard
    #  temp$income<-temp$income-12*temp$IncomeDisregard
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # MA calculates their copayments by Percentage of (family income minus bin 1 max)
    
    temp$income_above_FPL<-rowMaxs(cbind(temp$income-temp$Bin1Max,0)) # Make sure it's not negative
    
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income_above_FPL*temp$FTcopay,temp$netexp.childcare))
    
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==25]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==25]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==25]<-temp$InitialEligibility.y
  }
  
  
  
  # MICHIGAN ----
  
  # Description:
  # Copay is a dollar amount per child, same for each child, up to a maximum
  # Biweekly (every 2 weeks) frequency
  # No fee after a maximum copay is reached (under construction)
  if(26 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_MI$stateFIPS <- 26
    #ccdfData_MI <- ccdfData_MI[ccdfData_MI$famsize < 8,]
    
    temp<-data[data$stateFIPS==26,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_MI$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_MI$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_MI$stateFIPS), AKorHI=unique(ccdfData_MI$AKorHI), famsize=unique(ccdfData_MI$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_MI[ccdfData_MI$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_MI$stateFIPS), AKorHI=unique(ccdfData_MI$AKorHI), famsize=unique(ccdfData_MI$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_MI, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_MI<-ccdfData_MI %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_MI<-ccdfData_MI %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_MI, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Calculate total copay
    temp$totcopay<-temp$FTcopay*(temp$numkidsincare0to4+temp$numkidsincare5to12)*26 # bi-weekly frequency
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==26]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==26]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==26]<-temp$InitialEligibility.y
  }
  
  # MINNESOTA ----
  
  # Description:
  # Copay is a dollar amount per family
  # Biweekly (every 2 weeks) frequency
  if(27 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_MN$stateFIPS <- 27
    
    temp<-data[data$stateFIPS==27,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_MN$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_MN$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_MN$stateFIPS), AKorHI=unique(ccdfData_MN$AKorHI), famsize=unique(ccdfData_MN$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_MN[ccdfData_MN$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_MN$stateFIPS), AKorHI=unique(ccdfData_MN$AKorHI), famsize=unique(ccdfData_MN$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_MN, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_MN<-ccdfData_MN %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_MN<-ccdfData_MN %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_MN, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    temp$FTcopay[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]<-temp$CopayBin31[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]
    temp$FTcopay[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]<-temp$CopayBin32[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]
    #temp$FTcopay[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]<-temp$CopayBin33[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    # Calculate total copay (26 periods needed)
    temp$FTcopay[!is.na(temp$FTcopay)]<-as.numeric(temp$FTcopay[!is.na(temp$FTcopay)])
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Make sure the variables names are the same
    temp$totcopay <- temp$FTcopay*26 #totcopay is initilaized to NA in beginning
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==27]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==27]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==27]<-temp$InitialEligibility.y
  }
  
  
  # MISSISSIPPI
  
  if(28 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_MS$stateFIPS <- 28
    
    temp<-data[data$stateFIPS==28,]
    
    years<-unique(data$Year)
    maxYear<-max(ccdfData_MS$ruleYear)
    for (yr in years){
      if(yr %notin% ccdfData_MS$ruleYear & yr > maxYear){
        copyLatestYear<-ccdfData_MS[rep(ccdfData_MS$ruleYear==maxYear), ]
        copyLatestYear$ruleYear[copyLatestYear$ruleYear == maxYear]<-yr
        ccdfData_MS<-ccdfData_MS%>%
          rbind(copyLatestYear)
      }}
    # Adds historical rules if we do not have them
    minYear<-min(ccdfData_MS$ruleYear)
    for (yr in years){
      if(yr %notin% ccdfData_MS$ruleYear & yr < minYear){
        copyEarliestYear<-ccdfData_MS[rep(ccdfData_MS$ruleYear==minYear), ]
        copyEarliestYear$ruleYear[copyEarliestYear$ruleYear == minYear]<-yr
        ccdfData_MS<-ccdfData_MS%>%
          rbind(copyEarliestYear)
      }}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_MS, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    temp$FTcopay[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]<-temp$CopayBin31[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]
    temp$FTcopay[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]<-temp$CopayBin32[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]
    temp$FTcopay[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]<-temp$CopayBin33[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]
    temp$FTcopay[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]<-temp$CopayBin34[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]
    temp$FTcopay[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]<-temp$CopayBin35[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]
    temp$FTcopay[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]<-temp$CopayBin36[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]
    temp$FTcopay[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]<-temp$CopayBin37[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]
    temp$FTcopay[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]<-temp$CopayBin38[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]
    temp$FTcopay[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]<-temp$CopayBin39[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]
    temp$FTcopay[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]<-temp$CopayBin40[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]
    temp$FTcopay[temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max]<-temp$CopayBin41[temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max]
    temp$FTcopay[temp$income>temp$Bin41Max & temp$income<=temp$Bin42Max]<-temp$CopayBin42[temp$income>temp$Bin41Max & temp$income<=temp$Bin42Max]
    temp$FTcopay[temp$income>temp$Bin42Max & temp$income<=temp$Bin43Max]<-temp$CopayBin43[temp$income>temp$Bin42Max & temp$income<=temp$Bin43Max]
    temp$FTcopay[temp$income>temp$Bin43Max & temp$income<=temp$Bin44Max]<-temp$CopayBin44[temp$income>temp$Bin43Max & temp$income<=temp$Bin44Max]
    temp$FTcopay[temp$income>temp$Bin44Max & temp$income<=temp$Bin45Max]<-temp$CopayBin45[temp$income>temp$Bin44Max & temp$income<=temp$Bin45Max]
    temp$FTcopay[temp$income>temp$Bin45Max & temp$income<=temp$Bin46Max]<-temp$CopayBin46[temp$income>temp$Bin45Max & temp$income<=temp$Bin46Max]
    temp$FTcopay[temp$income>temp$Bin46Max & temp$income<=temp$Bin47Max]<-temp$CopayBin47[temp$income>temp$Bin46Max & temp$income<=temp$Bin47Max]
    temp$FTcopay[temp$income>temp$Bin47Max & temp$income<=temp$Bin48Max]<-temp$CopayBin48[temp$income>temp$Bin47Max & temp$income<=temp$Bin48Max]
    temp$FTcopay[temp$income>temp$Bin48Max & temp$income<=temp$Bin49Max]<-temp$CopayBin49[temp$income>temp$Bin48Max & temp$income<=temp$Bin49Max]
    temp$FTcopay[temp$income>temp$Bin49Max & temp$income<=temp$Bin50Max]<-temp$CopayBin50[temp$income>temp$Bin49Max & temp$income<=temp$Bin50Max]
    temp$FTcopay[temp$income>temp$Bin50Max & temp$income<=temp$Bin51Max]<-temp$CopayBin51[temp$income>temp$Bin50Max & temp$income<=temp$Bin51Max]
    temp$FTcopay[temp$income>temp$Bin51Max & temp$income<=temp$Bin52Max]<-temp$CopayBin52[temp$income>temp$Bin51Max & temp$income<=temp$Bin52Max]
    temp$FTcopay[temp$income>temp$Bin52Max & temp$income<=temp$Bin53Max]<-temp$CopayBin53[temp$income>temp$Bin52Max & temp$income<=temp$Bin53Max]
    temp$FTcopay[temp$income>temp$Bin53Max & temp$income<=temp$Bin54Max]<-temp$CopayBin54[temp$income>temp$Bin53Max & temp$income<=temp$Bin54Max]
    temp$FTcopay[temp$income>temp$Bin54Max & temp$income<=temp$Bin55Max]<-temp$CopayBin55[temp$income>temp$Bin54Max & temp$income<=temp$Bin55Max]
    temp$FTcopay[temp$income>temp$Bin55Max & temp$income<=temp$Bin56Max]<-temp$CopayBin56[temp$income>temp$Bin55Max & temp$income<=temp$Bin56Max]
    temp$FTcopay[temp$income>temp$Bin56Max & temp$income<=temp$Bin57Max]<-temp$CopayBin57[temp$income>temp$Bin56Max & temp$income<=temp$Bin57Max]
    temp$FTcopay[temp$income>temp$Bin57Max & temp$income<=temp$Bin58Max]<-temp$CopayBin58[temp$income>temp$Bin57Max & temp$income<=temp$Bin58Max]
    temp$FTcopay[temp$income>temp$Bin58Max & temp$income<=temp$Bin59Max]<-temp$CopayBin59[temp$income>temp$Bin58Max & temp$income<=temp$Bin59Max]
    temp$FTcopay[temp$income>temp$Bin59Max & temp$income<=temp$Bin60Max]<-temp$CopayBin60[temp$income>temp$Bin59Max & temp$income<=temp$Bin60Max]
    temp$FTcopay[temp$income>temp$Bin60Max & temp$income<=temp$Bin61Max]<-temp$CopayBin61[temp$income>temp$Bin60Max & temp$income<=temp$Bin61Max]
    temp$FTcopay[temp$income>temp$Bin61Max & temp$income<=temp$Bin62Max]<-temp$CopayBin62[temp$income>temp$Bin61Max & temp$income<=temp$Bin62Max]
    temp$FTcopay[temp$income>temp$Bin62Max & temp$income<=temp$Bin63Max]<-temp$CopayBin63[temp$income>temp$Bin62Max & temp$income<=temp$Bin63Max]
    temp$FTcopay[temp$income>temp$Bin63Max & temp$income<=temp$Bin64Max]<-temp$CopayBin64[temp$income>temp$Bin63Max & temp$income<=temp$Bin64Max]
    temp$FTcopay[temp$income>temp$Bin64Max & temp$income<=temp$Bin65Max]<-temp$CopayBin65[temp$income>temp$Bin64Max & temp$income<=temp$Bin65Max]
    temp$FTcopay[temp$income>temp$Bin65Max & temp$income<=temp$Bin66Max]<-temp$CopayBin66[temp$income>temp$Bin65Max & temp$income<=temp$Bin66Max]
    temp$FTcopay[temp$income>temp$Bin66Max & temp$income<=temp$Bin67Max]<-temp$CopayBin67[temp$income>temp$Bin66Max & temp$income<=temp$Bin67Max]
    temp$FTcopay[temp$income>temp$Bin67Max & temp$income<=temp$Bin68Max]<-temp$CopayBin68[temp$income>temp$Bin67Max & temp$income<=temp$Bin68Max]
    temp$FTcopay[temp$income>temp$Bin68Max & temp$income<=temp$Bin69Max]<-temp$CopayBin69[temp$income>temp$Bin68Max & temp$income<=temp$Bin69Max]
    temp$FTcopay[temp$income>temp$Bin69Max & temp$income<=temp$Bin70Max]<-temp$CopayBin70[temp$income>temp$Bin69Max & temp$income<=temp$Bin70Max]
    temp$FTcopay[temp$income>temp$Bin70Max & temp$income<=temp$Bin71Max]<-temp$CopayBin71[temp$income>temp$Bin70Max & temp$income<=temp$Bin71Max]
    temp$FTcopay[temp$income>temp$Bin71Max & temp$income<=temp$Bin72Max]<-temp$CopayBin72[temp$income>temp$Bin71Max & temp$income<=temp$Bin72Max]
    temp$FTcopay[temp$income>temp$Bin72Max & temp$income<=temp$Bin73Max]<-temp$CopayBin73[temp$income>temp$Bin72Max & temp$income<=temp$Bin73Max]
    temp$FTcopay[temp$income>temp$Bin73Max & temp$income<=temp$Bin74Max]<-temp$CopayBin74[temp$income>temp$Bin73Max & temp$income<=temp$Bin74Max]
    temp$FTcopay[temp$income>temp$Bin74Max & temp$income<=temp$Bin75Max]<-temp$CopayBin75[temp$income>temp$Bin74Max & temp$income<=temp$Bin75Max]
    temp$FTcopay[temp$income>temp$Bin75Max & temp$income<=temp$Bin76Max]<-temp$CopayBin76[temp$income>temp$Bin75Max & temp$income<=temp$Bin76Max]
    temp$FTcopay[temp$income>temp$Bin76Max & temp$income<=temp$Bin77Max]<-temp$CopayBin77[temp$income>temp$Bin76Max & temp$income<=temp$Bin77Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==28]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==28]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==28]<-temp$InitialEligibility.y
  }
  
  
  # Missouri-----
  
  # Description:
  # Copay is a fixed dollar amount per child
  # Daily frequency
  if(29 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==29,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_MO$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_MO$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_MO$stateFIPS), AKorHI=unique(ccdfData_MO$AKorHI), famsize=unique(ccdfData_MO$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_MO[ccdfData_MO$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_MO$stateFIPS), AKorHI=unique(ccdfData_MO$AKorHI), famsize=unique(ccdfData_MO$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_MO, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_MO<-ccdfData_MO %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_MO<-ccdfData_MO %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_MO, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1_FT[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2_FT[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3_FT[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4_FT[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5_FT[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6_FT[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$CopayBin7_FT[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    temp$FTcopay[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]<-temp$CopayBin8_FT[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]
    temp$FTcopay[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]<-temp$CopayBin9_FT[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]
    temp$FTcopay[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]<-temp$CopayBin10_FT[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]
    temp$FTcopay[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]<-temp$CopayBin11_FT[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Determine how much care child need
    # age < 5 requires full-time school time care and full-time summer care
    # age 5 to 12 requires part-time school time care and full-time summer care
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==29]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==29]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==29]<-temp$InitialEligibility.y
  }
  
  
  
  # MONTANA ----
  
  # Description:
  # Copay is a percentage of total income
  # Weekly frequency
  # Discount for a second child (UNDER CONSTRUCTION)
  if(30 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_MT$stateFIPS <- 30
    
    temp<-data[data$stateFIPS==30,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_MT$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_MT$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_MT$stateFIPS), AKorHI=unique(ccdfData_MT$AKorHI), famsize=unique(ccdfData_MT$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_MT[ccdfData_MT$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_MT$stateFIPS), AKorHI=unique(ccdfData_MT$AKorHI), famsize=unique(ccdfData_MT$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_MT, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_MT<-ccdfData_MT %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_MT<-ccdfData_MT %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_MT, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==30]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==30]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==30]<-temp$InitialEligibility.y
  }
  
  # NEBRASKA ----
  
  
  if(31 %in% unique(data$stateFIPS)){
    
    ccdfData_NE$stateFIPS <- 31
    #ccdfData_NE <- ccdfData_NE[ccdfData_NE$famsize <= 12,]
    
    temp <- data[data$stateFIPS==31,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_NE$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_NE$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_NE$stateFIPS), AKorHI=unique(ccdfData_NE$AKorHI), famsize=unique(ccdfData_NE$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_NE[ccdfData_NE$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_NE$stateFIPS), AKorHI=unique(ccdfData_NE$AKorHI), famsize=unique(ccdfData_NE$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_NE, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_NE<-ccdfData_NE %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_NE<-ccdfData_NE %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_NE, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==31]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==31]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==31]<-temp$InitialEligibility.y
  }
  
  # NEVADA ----
  
  
  if(32 %in% unique(data$stateFIPS)){
    
    temp <- data[data$stateFIPS==32,]
    
    ### DOUBLE CHECK THAT THE CCDFDATA_NV DATASET IN THE GLOBAL ENVIRONMENT HAS THESE CORRECT
    # ccdfData_NV$stateFIPS <- 32
    #  ccdfData_NV$AssetTest <- 1000000
    #  ccdfData_NV$IncomeDisregard <- 0
    
    #### SAME GOES FOR PROVIDERCOST_NV
    #  providercost_NV$stateFIPS <- 32
    
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(providercost_NV$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(providercost_NV$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(providercost_NV$stateFIPS), countyortownName=unique(providercost_NV$countyortownName), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-providercost_NV[providercost_NV$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "countyortownName")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(providercost_NV$stateFIPS), countyortownName=unique(providercost_NV$countyortownName), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, providercost_NV, by=c("stateFIPS", "countyortownName"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {providercost_NV<-providercost_NV %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {providercost_NV<-providercost_NV %>% rbind(expandPastMiss2)}
    
    #  if(ruleYear > 2022){
    #   temp$ruleYear <- 2022
    #   }
    #temp<-left_join(temp, providercost_NV, by=c("stateFIPS", "countyortownName", "ruleYear")) # EI: ruleYear is not working here !!
    #  providercost_NV$ruleYear<-providercost_NV$yearofdata
    
    
    
    
    temp<-left_join(temp, providercost_NV, by=c("stateFIPS", "countyortownName", "ruleYear","stateName","stateAbbrev","AKorHI"))
    
    # Determine Expense based on Age of Children
    temp<-temp %>%
      mutate(sprPerson1=(case_when(agePerson1 %in% c(0)~ftdailyrate.infant,
                                   agePerson1 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson1 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson1 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson2=(case_when(agePerson2 %in% c(0)~ftdailyrate.infant,
                                   agePerson2 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson2 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson2 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson3=(case_when(agePerson3 %in% c(0)~ftdailyrate.infant,
                                   agePerson3 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson3 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson3 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson4=(case_when(agePerson4 %in% c(0)~ftdailyrate.infant,
                                   agePerson4 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson4 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson4 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson5=(case_when(agePerson5 %in% c(0)~ftdailyrate.infant,
                                   agePerson5 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson5 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson5 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson6=(case_when(agePerson6 %in% c(0)~ftdailyrate.infant,
                                   agePerson6 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson6 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson6 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson7=(case_when(agePerson7 %in% c(0)~ftdailyrate.infant,
                                   agePerson7 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson7 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson7 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson8=(case_when(agePerson8 %in% c(0)~ftdailyrate.infant,
                                   agePerson8 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson8 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson8 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson9=(case_when(agePerson9 %in% c(0)~ftdailyrate.infant,
                                   agePerson9 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson9 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson9 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson10=(case_when(agePerson10 %in% c(0)~ftdailyrate.infant,
                                    agePerson10 %in% c(1:2)~ftdailyrate.toddler,
                                    agePerson10 %in% c(3:5)~ftdailyrate.preschool,
                                    agePerson10 %in% c(6:12)~ftdailyrate.schoolage,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson11=(case_when(agePerson11 %in% c(0)~ftdailyrate.infant,
                                    agePerson11 %in% c(1:2)~ftdailyrate.toddler,
                                    agePerson11 %in% c(3:5)~ftdailyrate.preschool,
                                    agePerson11 %in% c(6:12)~ftdailyrate.schoolage,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson12=(case_when(agePerson12 %in% c(0)~ftdailyrate.infant,
                                    agePerson12 %in% c(1:2)~ftdailyrate.toddler,
                                    agePerson12 %in% c(3:5)~ftdailyrate.preschool,
                                    agePerson12 %in% c(6:12)~ftdailyrate.schoolage,
                                    TRUE~0)
      )
      )
    
    # sprTotal=sprPerson1+sprPerson2+sprPerson3+sprPerson4+sprPerson5+sprPerson6+sprPerson7
    temp<-temp %>%
      mutate(annualcost1=(case_when(agePerson1 < 5 ~ sprPerson1*daysofcareneeded0to4,
                                    agePerson1 > 4 & agePerson1 < 13 ~ sprPerson1*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost2=(case_when(agePerson2 < 5 ~ sprPerson2*daysofcareneeded0to4,
                                    agePerson2 > 4 & agePerson2 < 13 ~ sprPerson2*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost3=(case_when(agePerson3 < 5 ~ sprPerson3*daysofcareneeded0to4,
                                    agePerson3 > 4 & agePerson3 < 13 ~ sprPerson3*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost4=(case_when(agePerson4 < 5 ~ sprPerson4*daysofcareneeded0to4,
                                    agePerson4 > 4 & agePerson4 < 13 ~ sprPerson4*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost5=(case_when(agePerson5 < 5 ~ sprPerson5*daysofcareneeded0to4,
                                    agePerson5 > 4 & agePerson5 < 13 ~ sprPerson5*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost6=(case_when(agePerson6 < 5 ~ sprPerson6*daysofcareneeded0to4,
                                    agePerson6 > 4 & agePerson6 < 13 ~ sprPerson6*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost7=(case_when(agePerson7 < 5 ~ sprPerson7*daysofcareneeded0to4,
                                    agePerson7 > 4 & agePerson7 < 13 ~ sprPerson7*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost8=(case_when(agePerson8 < 5 ~ sprPerson8*daysofcareneeded0to4,
                                    agePerson8 > 4 & agePerson8 < 13 ~ sprPerson8*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost9=(case_when(agePerson9 < 5 ~ sprPerson9*daysofcareneeded0to4,
                                    agePerson9 > 4 & agePerson9 < 13 ~ sprPerson9*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost10=(case_when(agePerson10 < 5 ~ sprPerson10*daysofcareneeded0to4,
                                     agePerson10 > 4 & agePerson10 < 13 ~ sprPerson10*daysofcareneeded5to12,
                                     TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost11=(case_when(agePerson11 < 5 ~ sprPerson11*daysofcareneeded0to4,
                                     agePerson11 > 4 & agePerson11 < 13 ~ sprPerson11*daysofcareneeded5to12,
                                     TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost12=(case_when(agePerson12 < 5 ~ sprPerson12*daysofcareneeded0to4,
                                     agePerson12 > 4 & agePerson12 < 13 ~ sprPerson12*daysofcareneeded5to12,
                                     TRUE~0)
      )
      )
    
    temp$annualCost<-temp$annualcost1+temp$annualcost2+temp$annualcost3+temp$annualcost4+temp$annualcost5+temp$annualcost6+temp$annualcost7+temp$annualcost8+temp$annualcost9+temp$annualcost10+temp$annualcost11+temp$annualcost12
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_NV$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_NV$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_NV$stateFIPS), famsize=unique(ccdfData_NV$famsize), countyortownName=unique(ccdfData_NV$countyortownName), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_NV[ccdfData_NV$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "famsize", "countyortownName")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_NV$stateFIPS), famsize=unique(ccdfData_NV$famsize), countyortownName=unique(ccdfData_NV$countyortownName), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_NV, by=c("stateFIPS","famsize", "countyortownName"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_NV<-ccdfData_NV %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_NV<-ccdfData_NV %>% rbind(expandPastMiss2)}
    
    #   if(ruleYear > 2022){
    #     temp$ruleYear <- 2022
    #    }
    
    temp<-left_join(temp, ccdfData_NV, by=c("stateFIPS","stateName", "famsize", "countyortownName", "AKorHI", "ruleYear"))
    
    # temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$ShareofCost1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$ShareofCost2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$ShareofCost3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$ShareofCost4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$ShareofCost5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$ShareofCost6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$ShareofCost7[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    temp$FTcopay[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]<-temp$ShareofCost8[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]
    temp$FTcopay[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]<-temp$ShareofCost9[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]
    temp$FTcopay[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]<-temp$ShareofCost10[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    temp$totcopay[temp$ruleYear<=2022] <- temp$annualCost[temp$ruleYear<=2022]*(1-temp$FTcopay[temp$ruleYear<=2022])
    
    temp$totcopay[temp$ruleYear>=2023] <- temp$income[temp$ruleYear>=2023]*temp$FTcopay[temp$ruleYear>=2023]
    
    
    
    temp$totcopay<-rowMins(cbind(temp$totcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==32]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==32]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==32]<-temp$InitialEligibility.y
  }
  
  # NEW MEXICO ----
  
  # Description:
  # Copay is a dollar amount per child
  # Monthly frequency
  if(35 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_NM$stateFIPS <- 35
    
    temp<-data[data$stateFIPS==35,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_NM$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_NM$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_NM$stateFIPS), AKorHI=unique(ccdfData_NM$AKorHI), famsize=unique(ccdfData_NM$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_NM[ccdfData_NM$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_NM$stateFIPS), AKorHI=unique(ccdfData_NM$AKorHI), famsize=unique(ccdfData_NM$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_NM, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_NM<-ccdfData_NM %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_NM<-ccdfData_NM %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_NM, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    temp$FTcopay[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]<-temp$CopayBin31[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]
    temp$FTcopay[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]<-temp$CopayBin32[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]
    temp$FTcopay[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]<-temp$CopayBin33[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]
    temp$FTcopay[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]<-temp$CopayBin34[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]
    temp$FTcopay[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]<-temp$CopayBin35[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]
    temp$FTcopay[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]<-temp$CopayBin36[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]
    temp$FTcopay[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]<-temp$CopayBin37[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]
    temp$FTcopay[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]<-temp$CopayBin38[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]
    temp$FTcopay[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]<-temp$CopayBin39[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]
    temp$FTcopay[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]<-temp$CopayBin40[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]
    temp$FTcopay[temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max]<-temp$CopayBin41[temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max]
    temp$FTcopay[temp$income>temp$Bin41Max & temp$income<=temp$Bin42Max]<-temp$CopayBin42[temp$income>temp$Bin41Max & temp$income<=temp$Bin42Max]
    temp$FTcopay[temp$income>temp$Bin42Max & temp$income<=temp$Bin43Max]<-temp$CopayBin43[temp$income>temp$Bin42Max & temp$income<=temp$Bin43Max]
    temp$FTcopay[temp$income>temp$Bin43Max & temp$income<=temp$Bin44Max]<-temp$CopayBin44[temp$income>temp$Bin43Max & temp$income<=temp$Bin44Max]
    temp$FTcopay[temp$income>temp$Bin44Max & temp$income<=temp$Bin45Max]<-temp$CopayBin45[temp$income>temp$Bin44Max & temp$income<=temp$Bin45Max]
    temp$FTcopay[temp$income>temp$Bin45Max & temp$income<=temp$Bin46Max]<-temp$CopayBin46[temp$income>temp$Bin45Max & temp$income<=temp$Bin46Max]
    temp$FTcopay[temp$income>temp$Bin46Max & temp$income<=temp$Bin47Max]<-temp$CopayBin47[temp$income>temp$Bin46Max & temp$income<=temp$Bin47Max]
    temp$FTcopay[temp$income>temp$Bin47Max & temp$income<=temp$Bin48Max]<-temp$CopayBin48[temp$income>temp$Bin47Max & temp$income<=temp$Bin48Max]
    temp$FTcopay[temp$income>temp$Bin48Max & temp$income<=temp$Bin49Max]<-temp$CopayBin49[temp$income>temp$Bin48Max & temp$income<=temp$Bin49Max]
    temp$FTcopay[temp$income>temp$Bin49Max & temp$income<=temp$Bin50Max]<-temp$CopayBin50[temp$income>temp$Bin49Max & temp$income<=temp$Bin50Max]
    temp$FTcopay[temp$income>temp$Bin50Max & temp$income<=temp$Bin51Max]<-temp$CopayBin51[temp$income>temp$Bin50Max & temp$income<=temp$Bin51Max]
    temp$FTcopay[temp$income>temp$Bin51Max & temp$income<=temp$Bin52Max]<-temp$CopayBin52[temp$income>temp$Bin51Max & temp$income<=temp$Bin52Max]
    temp$FTcopay[temp$income>temp$Bin52Max & temp$income<=temp$Bin53Max]<-temp$CopayBin53[temp$income>temp$Bin52Max & temp$income<=temp$Bin53Max]
    temp$FTcopay[temp$income>temp$Bin53Max & temp$income<=temp$Bin54Max]<-temp$CopayBin54[temp$income>temp$Bin53Max & temp$income<=temp$Bin54Max]
    temp$FTcopay[temp$income>temp$Bin54Max & temp$income<=temp$Bin55Max]<-temp$CopayBin55[temp$income>temp$Bin54Max & temp$income<=temp$Bin55Max]
    temp$FTcopay[temp$income>temp$Bin55Max & temp$income<=temp$Bin56Max]<-temp$CopayBin56[temp$income>temp$Bin55Max & temp$income<=temp$Bin56Max]
    temp$FTcopay[temp$income>temp$Bin56Max & temp$income<=temp$Bin57Max]<-temp$CopayBin57[temp$income>temp$Bin56Max & temp$income<=temp$Bin57Max]
    temp$FTcopay[temp$income>temp$Bin57Max & temp$income<=temp$Bin58Max]<-temp$CopayBin58[temp$income>temp$Bin57Max & temp$income<=temp$Bin58Max]
    temp$FTcopay[temp$income>temp$Bin58Max & temp$income<=temp$Bin59Max]<-temp$CopayBin59[temp$income>temp$Bin58Max & temp$income<=temp$Bin59Max]
    temp$FTcopay[temp$income>temp$Bin59Max & temp$income<=temp$Bin60Max]<-temp$CopayBin60[temp$income>temp$Bin59Max & temp$income<=temp$Bin60Max]
    temp$FTcopay[temp$income>temp$Bin60Max & temp$income<=temp$Bin61Max]<-temp$CopayBin61[temp$income>temp$Bin60Max & temp$income<=temp$Bin61Max]
    temp$FTcopay[temp$income>temp$Bin61Max & temp$income<=temp$Bin62Max]<-temp$CopayBin62[temp$income>temp$Bin61Max & temp$income<=temp$Bin62Max]
    temp$FTcopay[temp$income>temp$Bin62Max & temp$income<=temp$Bin63Max]<-temp$CopayBin63[temp$income>temp$Bin62Max & temp$income<=temp$Bin63Max]
    temp$FTcopay[temp$income>temp$Bin63Max & temp$income<=temp$Bin64Max]<-temp$CopayBin64[temp$income>temp$Bin63Max & temp$income<=temp$Bin64Max]
    temp$FTcopay[temp$income>temp$Bin64Max & temp$income<=temp$Bin65Max]<-temp$CopayBin65[temp$income>temp$Bin64Max & temp$income<=temp$Bin65Max]
    temp$FTcopay[temp$income>temp$Bin65Max & temp$income<=temp$Bin66Max]<-temp$CopayBin66[temp$income>temp$Bin65Max & temp$income<=temp$Bin66Max]
    temp$FTcopay[temp$income>temp$Bin66Max & temp$income<=temp$Bin67Max]<-temp$CopayBin67[temp$income>temp$Bin66Max & temp$income<=temp$Bin67Max]
    temp$FTcopay[temp$income>temp$Bin67Max & temp$income<=temp$Bin68Max]<-temp$CopayBin68[temp$income>temp$Bin67Max & temp$income<=temp$Bin68Max]
    temp$FTcopay[temp$income>temp$Bin68Max & temp$income<=temp$Bin69Max]<-temp$CopayBin69[temp$income>temp$Bin68Max & temp$income<=temp$Bin69Max]
    temp$FTcopay[temp$income>temp$Bin69Max & temp$income<=temp$Bin70Max]<-temp$CopayBin70[temp$income>temp$Bin69Max & temp$income<=temp$Bin70Max]
    temp$FTcopay[temp$income>temp$Bin70Max & temp$income<=temp$Bin71Max]<-temp$CopayBin71[temp$income>temp$Bin70Max & temp$income<=temp$Bin71Max]
    temp$FTcopay[temp$income>temp$Bin71Max & temp$income<=temp$Bin72Max]<-temp$CopayBin72[temp$income>temp$Bin71Max & temp$income<=temp$Bin72Max]
    temp$FTcopay[temp$income>temp$Bin72Max & temp$income<=temp$Bin73Max]<-temp$CopayBin73[temp$income>temp$Bin72Max & temp$income<=temp$Bin73Max]
    temp$FTcopay[temp$income>temp$Bin73Max & temp$income<=temp$Bin74Max]<-temp$CopayBin74[temp$income>temp$Bin73Max & temp$income<=temp$Bin74Max]
    temp$FTcopay[temp$income>temp$Bin74Max & temp$income<=temp$Bin75Max]<-temp$CopayBin75[temp$income>temp$Bin74Max & temp$income<=temp$Bin75Max]
    temp$FTcopay[temp$income>temp$Bin75Max & temp$income<=temp$Bin76Max]<-temp$CopayBin76[temp$income>temp$Bin75Max & temp$income<=temp$Bin76Max]
    temp$FTcopay[temp$income>temp$Bin76Max & temp$income<=temp$Bin77Max]<-temp$CopayBin77[temp$income>temp$Bin76Max & temp$income<=temp$Bin77Max]
    temp$FTcopay[temp$income>temp$Bin77Max & temp$income<=temp$Bin78Max]<-temp$CopayBin78[temp$income>temp$Bin77Max & temp$income<=temp$Bin78Max]
    temp$FTcopay[temp$income>temp$Bin78Max & temp$income<=temp$Bin79Max]<-temp$CopayBin79[temp$income>temp$Bin78Max & temp$income<=temp$Bin79Max]
    temp$FTcopay[temp$income>temp$Bin79Max & temp$income<=temp$Bin80Max]<-temp$CopayBin80[temp$income>temp$Bin79Max & temp$income<=temp$Bin80Max]
    temp$FTcopay[temp$income>temp$Bin80Max & temp$income<=temp$Bin81Max]<-temp$CopayBin81[temp$income>temp$Bin80Max & temp$income<=temp$Bin81Max]
    temp$FTcopay[temp$income>temp$Bin81Max & temp$income<=temp$Bin82Max]<-temp$CopayBin82[temp$income>temp$Bin81Max & temp$income<=temp$Bin82Max]
    temp$FTcopay[temp$income>temp$Bin82Max & temp$income<=temp$Bin83Max]<-temp$CopayBin83[temp$income>temp$Bin82Max & temp$income<=temp$Bin83Max]
    temp$FTcopay[temp$income>temp$Bin83Max & temp$income<=temp$Bin84Max]<-temp$CopayBin84[temp$income>temp$Bin83Max & temp$income<=temp$Bin84Max]
    temp$FTcopay[temp$income>temp$Bin84Max & temp$income<=temp$Bin85Max]<-temp$CopayBin85[temp$income>temp$Bin84Max & temp$income<=temp$Bin85Max]
    temp$FTcopay[temp$income>temp$Bin85Max & temp$income<=temp$Bin86Max]<-temp$CopayBin86[temp$income>temp$Bin85Max & temp$income<=temp$Bin86Max]
    temp$FTcopay[temp$income>temp$Bin86Max & temp$income<=temp$Bin87Max]<-temp$CopayBin87[temp$income>temp$Bin86Max & temp$income<=temp$Bin87Max]
    temp$FTcopay[temp$income>temp$Bin87Max & temp$income<=temp$Bin88Max]<-temp$CopayBin88[temp$income>temp$Bin87Max & temp$income<=temp$Bin88Max]
    temp$FTcopay[temp$income>temp$Bin88Max & temp$income<=temp$Bin89Max]<-temp$CopayBin89[temp$income>temp$Bin88Max & temp$income<=temp$Bin89Max]
    temp$FTcopay[temp$income>temp$Bin89Max & temp$income<=temp$Bin90Max]<-temp$CopayBin90[temp$income>temp$Bin89Max & temp$income<=temp$Bin90Max]
    temp$FTcopay[temp$income>temp$Bin90Max & temp$income<=temp$Bin91Max]<-temp$CopayBin91[temp$income>temp$Bin90Max & temp$income<=temp$Bin91Max]
    temp$FTcopay[temp$income>temp$Bin91Max & temp$income<=temp$Bin92Max]<-temp$CopayBin92[temp$income>temp$Bin91Max & temp$income<=temp$Bin92Max]
    temp$FTcopay[temp$income>temp$Bin92Max & temp$income<=temp$Bin93Max]<-temp$CopayBin93[temp$income>temp$Bin92Max & temp$income<=temp$Bin93Max]
    temp$FTcopay[temp$income>temp$Bin93Max & temp$income<=temp$Bin94Max]<-temp$CopayBin94[temp$income>temp$Bin93Max & temp$income<=temp$Bin94Max]
    temp$FTcopay[temp$income>temp$Bin94Max & temp$income<=temp$Bin95Max]<-temp$CopayBin95[temp$income>temp$Bin94Max & temp$income<=temp$Bin95Max]
    temp$FTcopay[temp$income>temp$Bin95Max & temp$income<=temp$Bin96Max]<-temp$CopayBin96[temp$income>temp$Bin95Max & temp$income<=temp$Bin96Max]
    temp$FTcopay[temp$income>temp$Bin96Max & temp$income<=temp$Bin97Max]<-temp$CopayBin97[temp$income>temp$Bin96Max & temp$income<=temp$Bin97Max]
    temp$FTcopay[temp$income>temp$Bin97Max & temp$income<=temp$Bin98Max]<-temp$CopayBin98[temp$income>temp$Bin97Max & temp$income<=temp$Bin98Max]
    temp$FTcopay[temp$income>temp$Bin98Max & temp$income<=temp$Bin99Max]<-temp$CopayBin99[temp$income>temp$Bin98Max & temp$income<=temp$Bin99Max]
    temp$FTcopay[temp$income>temp$Bin99Max & temp$income<=temp$Bin100Max]<-temp$CopayBin100[temp$income>temp$Bin99Max & temp$income<=temp$Bin100Max]
    temp$FTcopay[temp$income>temp$Bin100Max & temp$income<=temp$Bin101Max]<-temp$CopayBin101[temp$income>temp$Bin100Max & temp$income<=temp$Bin101Max]
    temp$FTcopay[temp$income>temp$Bin101Max & temp$income<=temp$Bin102Max]<-temp$CopayBin102[temp$income>temp$Bin101Max & temp$income<=temp$Bin102Max]
    temp$FTcopay[temp$income>temp$Bin102Max & temp$income<=temp$Bin103Max]<-temp$CopayBin103[temp$income>temp$Bin102Max & temp$income<=temp$Bin103Max]
    temp$FTcopay[temp$income>temp$Bin103Max & temp$income<=temp$Bin104Max]<-temp$CopayBin104[temp$income>temp$Bin103Max & temp$income<=temp$Bin104Max]
    temp$FTcopay[temp$income>temp$Bin104Max & temp$income<=temp$Bin105Max]<-temp$CopayBin105[temp$income>temp$Bin104Max & temp$income<=temp$Bin105Max]
    temp$FTcopay[temp$income>temp$Bin105Max & temp$income<=temp$Bin106Max]<-temp$CopayBin106[temp$income>temp$Bin105Max & temp$income<=temp$Bin106Max]
    temp$FTcopay[temp$income>temp$Bin106Max & temp$income<=temp$Bin107Max]<-temp$CopayBin107[temp$income>temp$Bin106Max & temp$income<=temp$Bin107Max]
    temp$FTcopay[temp$income>temp$Bin107Max & temp$income<=temp$Bin108Max]<-temp$CopayBin108[temp$income>temp$Bin107Max & temp$income<=temp$Bin108Max]
    temp$FTcopay[temp$income>temp$Bin108Max & temp$income<=temp$Bin109Max]<-temp$CopayBin109[temp$income>temp$Bin108Max & temp$income<=temp$Bin109Max]
    temp$FTcopay[temp$income>temp$Bin109Max & temp$income<=temp$Bin110Max]<-temp$CopayBin110[temp$income>temp$Bin109Max & temp$income<=temp$Bin110Max]
    temp$FTcopay[temp$income>temp$Bin110Max & temp$income<=temp$Bin111Max]<-temp$CopayBin111[temp$income>temp$Bin110Max & temp$income<=temp$Bin111Max]
    temp$FTcopay[temp$income>temp$Bin111Max & temp$income<=temp$Bin112Max]<-temp$CopayBin112[temp$income>temp$Bin111Max & temp$income<=temp$Bin112Max]
    temp$FTcopay[temp$income>temp$Bin112Max & temp$income<=temp$Bin113Max]<-temp$CopayBin113[temp$income>temp$Bin112Max & temp$income<=temp$Bin113Max]
    temp$FTcopay[temp$income>temp$Bin113Max & temp$income<=temp$Bin114Max]<-temp$CopayBin114[temp$income>temp$Bin113Max & temp$income<=temp$Bin114Max]
    temp$FTcopay[temp$income>temp$Bin114Max & temp$income<=temp$Bin115Max]<-temp$CopayBin115[temp$income>temp$Bin114Max & temp$income<=temp$Bin115Max]
    temp$FTcopay[temp$income>temp$Bin115Max & temp$income<=temp$Bin116Max]<-temp$CopayBin116[temp$income>temp$Bin115Max & temp$income<=temp$Bin116Max]
    temp$FTcopay[temp$income>temp$Bin116Max & temp$income<=temp$Bin117Max]<-temp$CopayBin117[temp$income>temp$Bin116Max & temp$income<=temp$Bin117Max]
    temp$FTcopay[temp$income>temp$Bin117Max & temp$income<=temp$Bin118Max]<-temp$CopayBin118[temp$income>temp$Bin117Max & temp$income<=temp$Bin118Max]
    temp$FTcopay[temp$income>temp$Bin118Max & temp$income<=temp$Bin119Max]<-temp$CopayBin119[temp$income>temp$Bin118Max & temp$income<=temp$Bin119Max]
    temp$FTcopay[temp$income>temp$Bin119Max & temp$income<=temp$Bin120Max]<-temp$CopayBin120[temp$income>temp$Bin119Max & temp$income<=temp$Bin120Max]
    temp$FTcopay[temp$income>temp$Bin120Max & temp$income<=temp$Bin121Max]<-temp$CopayBin121[temp$income>temp$Bin120Max & temp$income<=temp$Bin121Max]
    temp$FTcopay[temp$income>temp$Bin121Max & temp$income<=temp$Bin122Max]<-temp$CopayBin122[temp$income>temp$Bin121Max & temp$income<=temp$Bin122Max]
    
    temp$FTcopay[temp$income>temp$Bin122Max & temp$income<=temp$Bin123Max]<-temp$CopayBin123[temp$income>temp$Bin122Max & temp$income<=temp$Bin123Max]
    temp$FTcopay[temp$income>temp$Bin123Max & temp$income<=temp$Bin124Max]<-temp$CopayBin124[temp$income>temp$Bin123Max & temp$income<=temp$Bin124Max]
    temp$FTcopay[temp$income>temp$Bin124Max & temp$income<=temp$Bin125Max]<-temp$CopayBin125[temp$income>temp$Bin124Max & temp$income<=temp$Bin125Max]
    temp$FTcopay[temp$income>temp$Bin125Max & temp$income<=temp$Bin126Max]<-temp$CopayBin126[temp$income>temp$Bin125Max & temp$income<=temp$Bin126Max]
    temp$FTcopay[temp$income>temp$Bin126Max & temp$income<=temp$Bin127Max]<-temp$CopayBin127[temp$income>temp$Bin126Max & temp$income<=temp$Bin127Max]
    temp$FTcopay[temp$income>temp$Bin127Max & temp$income<=temp$Bin128Max]<-temp$CopayBin128[temp$income>temp$Bin127Max & temp$income<=temp$Bin128Max]
    temp$FTcopay[temp$income>temp$Bin128Max & temp$income<=temp$Bin129Max]<-temp$CopayBin129[temp$income>temp$Bin128Max & temp$income<=temp$Bin129Max]
    temp$FTcopay[temp$income>temp$Bin129Max & temp$income<=temp$Bin130Max]<-temp$CopayBin130[temp$income>temp$Bin129Max & temp$income<=temp$Bin130Max]
    temp$FTcopay[temp$income>temp$Bin130Max & temp$income<=temp$Bin131Max]<-temp$CopayBin131[temp$income>temp$Bin130Max & temp$income<=temp$Bin131Max]
    temp$FTcopay[temp$income>temp$Bin131Max & temp$income<=temp$Bin132Max]<-temp$CopayBin132[temp$income>temp$Bin131Max & temp$income<=temp$Bin132Max]
    temp$FTcopay[temp$income>temp$Bin132Max & temp$income<=temp$Bin133Max]<-temp$CopayBin133[temp$income>temp$Bin132Max & temp$income<=temp$Bin133Max]
    temp$FTcopay[temp$income>temp$Bin133Max & temp$income<=temp$Bin134Max]<-temp$CopayBin134[temp$income>temp$Bin133Max & temp$income<=temp$Bin134Max]
    temp$FTcopay[temp$income>temp$Bin134Max & temp$income<=temp$Bin135Max]<-temp$CopayBin135[temp$income>temp$Bin134Max & temp$income<=temp$Bin135Max]
    temp$FTcopay[temp$income>temp$Bin135Max & temp$income<=temp$Bin136Max]<-temp$CopayBin136[temp$income>temp$Bin135Max & temp$income<=temp$Bin136Max]
    temp$FTcopay[temp$income>temp$Bin136Max & temp$income<=temp$Bin137Max]<-temp$CopayBin137[temp$income>temp$Bin136Max & temp$income<=temp$Bin137Max]
    temp$FTcopay[temp$income>temp$Bin137Max & temp$income<=temp$Bin138Max]<-temp$CopayBin138[temp$income>temp$Bin137Max & temp$income<=temp$Bin138Max]
    temp$FTcopay[temp$income>temp$Bin138Max & temp$income<=temp$Bin139Max]<-temp$CopayBin139[temp$income>temp$Bin138Max & temp$income<=temp$Bin139Max]
    temp$FTcopay[temp$income>temp$Bin139Max & temp$income<=temp$Bin140Max]<-temp$CopayBin140[temp$income>temp$Bin139Max & temp$income<=temp$Bin140Max]
    temp$FTcopay[temp$income>temp$Bin140Max & temp$income<=temp$Bin141Max]<-temp$CopayBin141[temp$income>temp$Bin140Max & temp$income<=temp$Bin141Max]
    temp$FTcopay[temp$income>temp$Bin141Max & temp$income<=temp$Bin142Max]<-temp$CopayBin142[temp$income>temp$Bin141Max & temp$income<=temp$Bin142Max]
    temp$FTcopay[temp$income>temp$Bin142Max & temp$income<=temp$Bin143Max]<-temp$CopayBin143[temp$income>temp$Bin142Max & temp$income<=temp$Bin143Max]
    temp$FTcopay[temp$income>temp$Bin143Max & temp$income<=temp$Bin144Max]<-temp$CopayBin144[temp$income>temp$Bin143Max & temp$income<=temp$Bin144Max]
    temp$FTcopay[temp$income>temp$Bin144Max & temp$income<=temp$Bin145Max]<-temp$CopayBin145[temp$income>temp$Bin144Max & temp$income<=temp$Bin145Max]
    temp$FTcopay[temp$income>temp$Bin145Max & temp$income<=temp$Bin146Max]<-temp$CopayBin146[temp$income>temp$Bin145Max & temp$income<=temp$Bin146Max]
    temp$FTcopay[temp$income>temp$Bin146Max & temp$income<=temp$Bin147Max]<-temp$CopayBin147[temp$income>temp$Bin146Max & temp$income<=temp$Bin147Max]
    temp$FTcopay[temp$income>temp$Bin147Max & temp$income<=temp$Bin148Max]<-temp$CopayBin148[temp$income>temp$Bin147Max & temp$income<=temp$Bin148Max]
    temp$FTcopay[temp$income>temp$Bin148Max & temp$income<=temp$Bin149Max]<-temp$CopayBin149[temp$income>temp$Bin148Max & temp$income<=temp$Bin149Max]
    temp$FTcopay[temp$income>temp$Bin149Max & temp$income<=temp$Bin150Max]<-temp$CopayBin150[temp$income>temp$Bin149Max & temp$income<=temp$Bin150Max]
    temp$FTcopay[temp$income>temp$Bin150Max & temp$income<=temp$Bin151Max]<-temp$CopayBin151[temp$income>temp$Bin150Max & temp$income<=temp$Bin151Max]
    temp$FTcopay[temp$income>temp$Bin151Max & temp$income<=temp$Bin152Max]<-temp$CopayBin152[temp$income>temp$Bin151Max & temp$income<=temp$Bin152Max]
    temp$FTcopay[temp$income>temp$Bin152Max & temp$income<=temp$Bin153Max]<-temp$CopayBin153[temp$income>temp$Bin152Max & temp$income<=temp$Bin153Max]
    temp$FTcopay[temp$income>temp$Bin153Max & temp$income<=temp$Bin154Max]<-temp$CopayBin154[temp$income>temp$Bin153Max & temp$income<=temp$Bin154Max]
    temp$FTcopay[temp$income>temp$Bin154Max & temp$income<=temp$Bin155Max]<-temp$CopayBin155[temp$income>temp$Bin154Max & temp$income<=temp$Bin155Max]
    temp$FTcopay[temp$income>temp$Bin155Max & temp$income<=temp$Bin156Max]<-temp$CopayBin156[temp$income>temp$Bin155Max & temp$income<=temp$Bin156Max]
    temp$FTcopay[temp$income>temp$Bin156Max & temp$income<=temp$Bin157Max]<-temp$CopayBin157[temp$income>temp$Bin156Max & temp$income<=temp$Bin157Max]
    temp$FTcopay[temp$income>temp$Bin157Max & temp$income<=temp$Bin158Max]<-temp$CopayBin158[temp$income>temp$Bin157Max & temp$income<=temp$Bin158Max]
    temp$FTcopay[temp$income>temp$Bin158Max & temp$income<=temp$Bin159Max]<-temp$CopayBin159[temp$income>temp$Bin158Max & temp$income<=temp$Bin159Max]
    temp$FTcopay[temp$income>temp$Bin159Max & temp$income<=temp$Bin160Max]<-temp$CopayBin160[temp$income>temp$Bin159Max & temp$income<=temp$Bin160Max]
    temp$FTcopay[temp$income>temp$Bin160Max & temp$income<=temp$Bin161Max]<-temp$CopayBin161[temp$income>temp$Bin160Max & temp$income<=temp$Bin161Max]
    temp$FTcopay[temp$income>temp$Bin161Max & temp$income<=temp$Bin162Max]<-temp$CopayBin162[temp$income>temp$Bin161Max & temp$income<=temp$Bin162Max]
    temp$FTcopay[temp$income>temp$Bin162Max & temp$income<=temp$Bin163Max]<-temp$CopayBin163[temp$income>temp$Bin162Max & temp$income<=temp$Bin163Max]
    temp$FTcopay[temp$income>temp$Bin163Max & temp$income<=temp$Bin164Max]<-temp$CopayBin164[temp$income>temp$Bin163Max & temp$income<=temp$Bin164Max]
    temp$FTcopay[temp$income>temp$Bin164Max & temp$income<=temp$Bin165Max]<-temp$CopayBin165[temp$income>temp$Bin164Max & temp$income<=temp$Bin165Max]
    temp$FTcopay[temp$income>temp$Bin165Max & temp$income<=temp$Bin166Max]<-temp$CopayBin166[temp$income>temp$Bin165Max & temp$income<=temp$Bin166Max]
    temp$FTcopay[temp$income>temp$Bin166Max & temp$income<=temp$Bin167Max]<-temp$CopayBin167[temp$income>temp$Bin166Max & temp$income<=temp$Bin167Max]
    temp$FTcopay[temp$income>temp$Bin167Max & temp$income<=temp$Bin168Max]<-temp$CopayBin168[temp$income>temp$Bin167Max & temp$income<=temp$Bin168Max]
    temp$FTcopay[temp$income>temp$Bin168Max & temp$income<=temp$Bin169Max]<-temp$CopayBin169[temp$income>temp$Bin168Max & temp$income<=temp$Bin169Max]
    temp$FTcopay[temp$income>temp$Bin169Max & temp$income<=temp$Bin170Max]<-temp$CopayBin170[temp$income>temp$Bin169Max & temp$income<=temp$Bin170Max]
    temp$FTcopay[temp$income>temp$Bin170Max & temp$income<=temp$Bin171Max]<-temp$CopayBin171[temp$income>temp$Bin170Max & temp$income<=temp$Bin171Max]
    temp$FTcopay[temp$income>temp$Bin171Max & temp$income<=temp$Bin172Max]<-temp$CopayBin172[temp$income>temp$Bin171Max & temp$income<=temp$Bin172Max]
    temp$FTcopay[temp$income>temp$Bin172Max & temp$income<=temp$Bin173Max]<-temp$CopayBin173[temp$income>temp$Bin172Max & temp$income<=temp$Bin173Max]
    temp$FTcopay[temp$income>temp$Bin173Max & temp$income<=temp$Bin174Max]<-temp$CopayBin174[temp$income>temp$Bin173Max & temp$income<=temp$Bin174Max]
    temp$FTcopay[temp$income>temp$Bin174Max & temp$income<=temp$Bin175Max]<-temp$CopayBin175[temp$income>temp$Bin174Max & temp$income<=temp$Bin175Max]
    temp$FTcopay[temp$income>temp$Bin175Max & temp$income<=temp$Bin176Max]<-temp$CopayBin176[temp$income>temp$Bin175Max & temp$income<=temp$Bin176Max]
    temp$FTcopay[temp$income>temp$Bin176Max & temp$income<=temp$Bin177Max]<-temp$CopayBin177[temp$income>temp$Bin176Max & temp$income<=temp$Bin177Max]
    temp$FTcopay[temp$income>temp$Bin177Max & temp$income<=temp$Bin178Max]<-temp$CopayBin178[temp$income>temp$Bin177Max & temp$income<=temp$Bin178Max]
    temp$FTcopay[temp$income>temp$Bin178Max & temp$income<=temp$Bin179Max]<-temp$CopayBin179[temp$income>temp$Bin178Max & temp$income<=temp$Bin179Max]
    temp$FTcopay[temp$income>temp$Bin179Max & temp$income<=temp$Bin180Max]<-temp$CopayBin180[temp$income>temp$Bin179Max & temp$income<=temp$Bin180Max]
    temp$FTcopay[temp$income>temp$Bin180Max & temp$income<=temp$Bin181Max]<-temp$CopayBin181[temp$income>temp$Bin180Max & temp$income<=temp$Bin181Max]
    temp$FTcopay[temp$income>temp$Bin181Max & temp$income<=temp$Bin182Max]<-temp$CopayBin182[temp$income>temp$Bin181Max & temp$income<=temp$Bin182Max]
    temp$FTcopay[temp$income>temp$Bin182Max & temp$income<=temp$Bin183Max]<-temp$CopayBin183[temp$income>temp$Bin182Max & temp$income<=temp$Bin183Max]
    temp$FTcopay[temp$income>temp$Bin183Max & temp$income<=temp$Bin184Max]<-temp$CopayBin184[temp$income>temp$Bin183Max & temp$income<=temp$Bin184Max]
    temp$FTcopay[temp$income>temp$Bin184Max & temp$income<=temp$Bin185Max]<-temp$CopayBin185[temp$income>temp$Bin184Max & temp$income<=temp$Bin185Max]
    temp$FTcopay[temp$income>temp$Bin185Max & temp$income<=temp$Bin186Max]<-temp$CopayBin186[temp$income>temp$Bin185Max & temp$income<=temp$Bin186Max]
    temp$FTcopay[temp$income>temp$Bin186Max & temp$income<=temp$Bin187Max]<-temp$CopayBin187[temp$income>temp$Bin186Max & temp$income<=temp$Bin187Max]
    temp$FTcopay[temp$income>temp$Bin187Max & temp$income<=temp$Bin188Max]<-temp$CopayBin188[temp$income>temp$Bin187Max & temp$income<=temp$Bin188Max]
    temp$FTcopay[temp$income>temp$Bin188Max & temp$income<=temp$Bin189Max]<-temp$CopayBin189[temp$income>temp$Bin188Max & temp$income<=temp$Bin189Max]
    temp$FTcopay[temp$income>temp$Bin189Max & temp$income<=temp$Bin190Max]<-temp$CopayBin190[temp$income>temp$Bin189Max & temp$income<=temp$Bin190Max]
    temp$FTcopay[temp$income>temp$Bin190Max & temp$income<=temp$Bin191Max]<-temp$CopayBin191[temp$income>temp$Bin190Max & temp$income<=temp$Bin191Max]
    temp$FTcopay[temp$income>temp$Bin191Max & temp$income<=temp$Bin192Max]<-temp$CopayBin192[temp$income>temp$Bin191Max & temp$income<=temp$Bin192Max]
    temp$FTcopay[temp$income>temp$Bin192Max & temp$income<=temp$Bin193Max]<-temp$CopayBin193[temp$income>temp$Bin192Max & temp$income<=temp$Bin193Max]
    temp$FTcopay[temp$income>temp$Bin193Max & temp$income<=temp$Bin194Max]<-temp$CopayBin194[temp$income>temp$Bin193Max & temp$income<=temp$Bin194Max]
    temp$FTcopay[temp$income>temp$Bin194Max & temp$income<=temp$Bin195Max]<-temp$CopayBin195[temp$income>temp$Bin194Max & temp$income<=temp$Bin195Max]
    temp$FTcopay[temp$income>temp$Bin195Max & temp$income<=temp$Bin196Max]<-temp$CopayBin196[temp$income>temp$Bin195Max & temp$income<=temp$Bin196Max]
    temp$FTcopay[temp$income>temp$Bin196Max & temp$income<=temp$Bin197Max]<-temp$CopayBin197[temp$income>temp$Bin196Max & temp$income<=temp$Bin197Max]
    temp$FTcopay[temp$income>temp$Bin197Max & temp$income<=temp$Bin198Max]<-temp$CopayBin198[temp$income>temp$Bin197Max & temp$income<=temp$Bin198Max]
    temp$FTcopay[temp$income>temp$Bin198Max & temp$income<=temp$Bin199Max]<-temp$CopayBin199[temp$income>temp$Bin198Max & temp$income<=temp$Bin199Max]
    temp$FTcopay[temp$income>temp$Bin199Max & temp$income<=temp$Bin200Max]<-temp$CopayBin200[temp$income>temp$Bin199Max & temp$income<=temp$Bin200Max]
    
    
    temp$FTcopay[temp$income>temp$Bin200Max & temp$income<=temp$Bin201Max]<-temp$CopayBin201[temp$income>temp$Bin200Max & temp$income<=temp$Bin201Max]
    temp$FTcopay[temp$income>temp$Bin201Max & temp$income<=temp$Bin202Max]<-temp$CopayBin202[temp$income>temp$Bin201Max & temp$income<=temp$Bin202Max]
    temp$FTcopay[temp$income>temp$Bin202Max & temp$income<=temp$Bin203Max]<-temp$CopayBin203[temp$income>temp$Bin202Max & temp$income<=temp$Bin203Max]
    temp$FTcopay[temp$income>temp$Bin203Max & temp$income<=temp$Bin204Max]<-temp$CopayBin204[temp$income>temp$Bin203Max & temp$income<=temp$Bin204Max]
    temp$FTcopay[temp$income>temp$Bin204Max & temp$income<=temp$Bin205Max]<-temp$CopayBin205[temp$income>temp$Bin204Max & temp$income<=temp$Bin205Max]
    temp$FTcopay[temp$income>temp$Bin205Max & temp$income<=temp$Bin206Max]<-temp$CopayBin206[temp$income>temp$Bin205Max & temp$income<=temp$Bin206Max]
    temp$FTcopay[temp$income>temp$Bin206Max & temp$income<=temp$Bin207Max]<-temp$CopayBin207[temp$income>temp$Bin206Max & temp$income<=temp$Bin207Max]
    temp$FTcopay[temp$income>temp$Bin207Max & temp$income<=temp$Bin208Max]<-temp$CopayBin208[temp$income>temp$Bin207Max & temp$income<=temp$Bin208Max]
    temp$FTcopay[temp$income>temp$Bin208Max & temp$income<=temp$Bin209Max]<-temp$CopayBin209[temp$income>temp$Bin208Max & temp$income<=temp$Bin209Max]
    temp$FTcopay[temp$income>temp$Bin209Max & temp$income<=temp$Bin210Max]<-temp$CopayBin210[temp$income>temp$Bin209Max & temp$income<=temp$Bin210Max]
    temp$FTcopay[temp$income>temp$Bin210Max & temp$income<=temp$Bin211Max]<-temp$CopayBin211[temp$income>temp$Bin210Max & temp$income<=temp$Bin211Max]
    temp$FTcopay[temp$income>temp$Bin211Max & temp$income<=temp$Bin212Max]<-temp$CopayBin212[temp$income>temp$Bin211Max & temp$income<=temp$Bin212Max]
    temp$FTcopay[temp$income>temp$Bin212Max & temp$income<=temp$Bin213Max]<-temp$CopayBin213[temp$income>temp$Bin212Max & temp$income<=temp$Bin213Max]
    temp$FTcopay[temp$income>temp$Bin213Max & temp$income<=temp$Bin214Max]<-temp$CopayBin214[temp$income>temp$Bin213Max & temp$income<=temp$Bin214Max]
    temp$FTcopay[temp$income>temp$Bin214Max & temp$income<=temp$Bin215Max]<-temp$CopayBin215[temp$income>temp$Bin214Max & temp$income<=temp$Bin215Max]
    temp$FTcopay[temp$income>temp$Bin215Max & temp$income<=temp$Bin216Max]<-temp$CopayBin216[temp$income>temp$Bin215Max & temp$income<=temp$Bin216Max]
    temp$FTcopay[temp$income>temp$Bin216Max & temp$income<=temp$Bin217Max]<-temp$CopayBin217[temp$income>temp$Bin216Max & temp$income<=temp$Bin217Max]
    temp$FTcopay[temp$income>temp$Bin217Max & temp$income<=temp$Bin218Max]<-temp$CopayBin218[temp$income>temp$Bin217Max & temp$income<=temp$Bin218Max]
    temp$FTcopay[temp$income>temp$Bin218Max & temp$income<=temp$Bin219Max]<-temp$CopayBin219[temp$income>temp$Bin218Max & temp$income<=temp$Bin219Max]
    temp$FTcopay[temp$income>temp$Bin219Max & temp$income<=temp$Bin220Max]<-temp$CopayBin220[temp$income>temp$Bin219Max & temp$income<=temp$Bin220Max]
    temp$FTcopay[temp$income>temp$Bin220Max & temp$income<=temp$Bin221Max]<-temp$CopayBin221[temp$income>temp$Bin220Max & temp$income<=temp$Bin221Max]
    temp$FTcopay[temp$income>temp$Bin221Max & temp$income<=temp$Bin222Max]<-temp$CopayBin222[temp$income>temp$Bin221Max & temp$income<=temp$Bin222Max]
    temp$FTcopay[temp$income>temp$Bin222Max & temp$income<=temp$Bin223Max]<-temp$CopayBin223[temp$income>temp$Bin222Max & temp$income<=temp$Bin223Max]
    temp$FTcopay[temp$income>temp$Bin223Max & temp$income<=temp$Bin224Max]<-temp$CopayBin224[temp$income>temp$Bin223Max & temp$income<=temp$Bin224Max]
    temp$FTcopay[temp$income>temp$Bin224Max & temp$income<=temp$Bin225Max]<-temp$CopayBin225[temp$income>temp$Bin224Max & temp$income<=temp$Bin225Max]
    temp$FTcopay[temp$income>temp$Bin225Max & temp$income<=temp$Bin226Max]<-temp$CopayBin226[temp$income>temp$Bin225Max & temp$income<=temp$Bin226Max]
    temp$FTcopay[temp$income>temp$Bin226Max & temp$income<=temp$Bin227Max]<-temp$CopayBin227[temp$income>temp$Bin226Max & temp$income<=temp$Bin227Max]
    temp$FTcopay[temp$income>temp$Bin227Max & temp$income<=temp$Bin228Max]<-temp$CopayBin228[temp$income>temp$Bin227Max & temp$income<=temp$Bin228Max]
    temp$FTcopay[temp$income>temp$Bin228Max & temp$income<=temp$Bin229Max]<-temp$CopayBin229[temp$income>temp$Bin228Max & temp$income<=temp$Bin229Max]
    temp$FTcopay[temp$income>temp$Bin229Max & temp$income<=temp$Bin230Max]<-temp$CopayBin230[temp$income>temp$Bin229Max & temp$income<=temp$Bin230Max]
    temp$FTcopay[temp$income>temp$Bin230Max & temp$income<=temp$Bin231Max]<-temp$CopayBin231[temp$income>temp$Bin230Max & temp$income<=temp$Bin231Max]
    temp$FTcopay[temp$income>temp$Bin231Max & temp$income<=temp$Bin232Max]<-temp$CopayBin232[temp$income>temp$Bin231Max & temp$income<=temp$Bin232Max]
    temp$FTcopay[temp$income>temp$Bin232Max & temp$income<=temp$Bin233Max]<-temp$CopayBin233[temp$income>temp$Bin232Max & temp$income<=temp$Bin233Max]
    temp$FTcopay[temp$income>temp$Bin233Max & temp$income<=temp$Bin234Max]<-temp$CopayBin234[temp$income>temp$Bin233Max & temp$income<=temp$Bin234Max]
    temp$FTcopay[temp$income>temp$Bin234Max & temp$income<=temp$Bin235Max]<-temp$CopayBin235[temp$income>temp$Bin234Max & temp$income<=temp$Bin235Max]
    temp$FTcopay[temp$income>temp$Bin235Max & temp$income<=temp$Bin236Max]<-temp$CopayBin236[temp$income>temp$Bin235Max & temp$income<=temp$Bin236Max]
    temp$FTcopay[temp$income>temp$Bin236Max & temp$income<=temp$Bin237Max]<-temp$CopayBin237[temp$income>temp$Bin236Max & temp$income<=temp$Bin237Max]
    temp$FTcopay[temp$income>temp$Bin237Max & temp$income<=temp$Bin238Max]<-temp$CopayBin238[temp$income>temp$Bin237Max & temp$income<=temp$Bin238Max]
    temp$FTcopay[temp$income>temp$Bin238Max & temp$income<=temp$Bin239Max]<-temp$CopayBin239[temp$income>temp$Bin238Max & temp$income<=temp$Bin239Max]
    temp$FTcopay[temp$income>temp$Bin239Max & temp$income<=temp$Bin240Max]<-temp$CopayBin240[temp$income>temp$Bin239Max & temp$income<=temp$Bin240Max]
    temp$FTcopay[temp$income>temp$Bin240Max & temp$income<=temp$Bin241Max]<-temp$CopayBin241[temp$income>temp$Bin240Max & temp$income<=temp$Bin241Max]
    temp$FTcopay[temp$income>temp$Bin241Max & temp$income<=temp$Bin242Max]<-temp$CopayBin242[temp$income>temp$Bin241Max & temp$income<=temp$Bin242Max]
    temp$FTcopay[temp$income>temp$Bin242Max & temp$income<=temp$Bin243Max]<-temp$CopayBin243[temp$income>temp$Bin242Max & temp$income<=temp$Bin243Max]
    temp$FTcopay[temp$income>temp$Bin243Max & temp$income<=temp$Bin244Max]<-temp$CopayBin244[temp$income>temp$Bin243Max & temp$income<=temp$Bin244Max]
    temp$FTcopay[temp$income>temp$Bin244Max & temp$income<=temp$Bin245Max]<-temp$CopayBin245[temp$income>temp$Bin244Max & temp$income<=temp$Bin245Max]
    temp$FTcopay[temp$income>temp$Bin245Max & temp$income<=temp$Bin246Max]<-temp$CopayBin246[temp$income>temp$Bin245Max & temp$income<=temp$Bin246Max]
    temp$FTcopay[temp$income>temp$Bin246Max & temp$income<=temp$Bin247Max]<-temp$CopayBin247[temp$income>temp$Bin246Max & temp$income<=temp$Bin247Max]
    temp$FTcopay[temp$income>temp$Bin247Max & temp$income<=temp$Bin248Max]<-temp$CopayBin248[temp$income>temp$Bin247Max & temp$income<=temp$Bin248Max]
    temp$FTcopay[temp$income>temp$Bin248Max & temp$income<=temp$Bin249Max]<-temp$CopayBin249[temp$income>temp$Bin248Max & temp$income<=temp$Bin249Max]
    temp$FTcopay[temp$income>temp$Bin249Max & temp$income<=temp$Bin250Max]<-temp$CopayBin250[temp$income>temp$Bin249Max & temp$income<=temp$Bin250Max]
    temp$FTcopay[temp$income>temp$Bin250Max & temp$income<=temp$Bin251Max]<-temp$CopayBin251[temp$income>temp$Bin250Max & temp$income<=temp$Bin251Max]
    temp$FTcopay[temp$income>temp$Bin251Max & temp$income<=temp$Bin252Max]<-temp$CopayBin252[temp$income>temp$Bin251Max & temp$income<=temp$Bin252Max]
    temp$FTcopay[temp$income>temp$Bin252Max & temp$income<=temp$Bin253Max]<-temp$CopayBin253[temp$income>temp$Bin252Max & temp$income<=temp$Bin253Max]
    temp$FTcopay[temp$income>temp$Bin253Max & temp$income<=temp$Bin254Max]<-temp$CopayBin254[temp$income>temp$Bin253Max & temp$income<=temp$Bin254Max]
    temp$FTcopay[temp$income>temp$Bin254Max & temp$income<=temp$Bin255Max]<-temp$CopayBin255[temp$income>temp$Bin254Max & temp$income<=temp$Bin255Max]
    temp$FTcopay[temp$income>temp$Bin255Max & temp$income<=temp$Bin256Max]<-temp$CopayBin256[temp$income>temp$Bin255Max & temp$income<=temp$Bin256Max]
    temp$FTcopay[temp$income>temp$Bin256Max & temp$income<=temp$Bin257Max]<-temp$CopayBin257[temp$income>temp$Bin256Max & temp$income<=temp$Bin257Max]
    temp$FTcopay[temp$income>temp$Bin257Max & temp$income<=temp$Bin258Max]<-temp$CopayBin258[temp$income>temp$Bin257Max & temp$income<=temp$Bin258Max]
    temp$FTcopay[temp$income>temp$Bin258Max & temp$income<=temp$Bin259Max]<-temp$CopayBin259[temp$income>temp$Bin258Max & temp$income<=temp$Bin259Max]
    temp$FTcopay[temp$income>temp$Bin259Max & temp$income<=temp$Bin260Max]<-temp$CopayBin260[temp$income>temp$Bin259Max & temp$income<=temp$Bin260Max]
    temp$FTcopay[temp$income>temp$Bin260Max & temp$income<=temp$Bin261Max]<-temp$CopayBin261[temp$income>temp$Bin260Max & temp$income<=temp$Bin261Max]
    temp$FTcopay[temp$income>temp$Bin261Max & temp$income<=temp$Bin262Max]<-temp$CopayBin262[temp$income>temp$Bin261Max & temp$income<=temp$Bin262Max]
    temp$FTcopay[temp$income>temp$Bin262Max & temp$income<=temp$Bin263Max]<-temp$CopayBin263[temp$income>temp$Bin262Max & temp$income<=temp$Bin263Max]
    temp$FTcopay[temp$income>temp$Bin263Max & temp$income<=temp$Bin264Max]<-temp$CopayBin264[temp$income>temp$Bin263Max & temp$income<=temp$Bin264Max]
    temp$FTcopay[temp$income>temp$Bin264Max & temp$income<=temp$Bin265Max]<-temp$CopayBin265[temp$income>temp$Bin264Max & temp$income<=temp$Bin265Max]
    temp$FTcopay[temp$income>temp$Bin265Max & temp$income<=temp$Bin266Max]<-temp$CopayBin266[temp$income>temp$Bin265Max & temp$income<=temp$Bin266Max]
    temp$FTcopay[temp$income>temp$Bin266Max & temp$income<=temp$Bin267Max]<-temp$CopayBin267[temp$income>temp$Bin266Max & temp$income<=temp$Bin267Max]
    temp$FTcopay[temp$income>temp$Bin267Max & temp$income<=temp$Bin268Max]<-temp$CopayBin268[temp$income>temp$Bin267Max & temp$income<=temp$Bin268Max]
    temp$FTcopay[temp$income>temp$Bin268Max & temp$income<=temp$Bin269Max]<-temp$CopayBin269[temp$income>temp$Bin268Max & temp$income<=temp$Bin269Max]
    temp$FTcopay[temp$income>temp$Bin269Max & temp$income<=temp$Bin270Max]<-temp$CopayBin270[temp$income>temp$Bin269Max & temp$income<=temp$Bin270Max]
    temp$FTcopay[temp$income>temp$Bin270Max & temp$income<=temp$Bin271Max]<-temp$CopayBin271[temp$income>temp$Bin270Max & temp$income<=temp$Bin271Max]
    temp$FTcopay[temp$income>temp$Bin271Max & temp$income<=temp$Bin272Max]<-temp$CopayBin272[temp$income>temp$Bin271Max & temp$income<=temp$Bin272Max]
    temp$FTcopay[temp$income>temp$Bin272Max & temp$income<=temp$Bin273Max]<-temp$CopayBin273[temp$income>temp$Bin272Max & temp$income<=temp$Bin273Max]
    temp$FTcopay[temp$income>temp$Bin273Max & temp$income<=temp$Bin274Max]<-temp$CopayBin274[temp$income>temp$Bin273Max & temp$income<=temp$Bin274Max]
    temp$FTcopay[temp$income>temp$Bin274Max & temp$income<=temp$Bin275Max]<-temp$CopayBin275[temp$income>temp$Bin274Max & temp$income<=temp$Bin275Max]
    temp$FTcopay[temp$income>temp$Bin275Max & temp$income<=temp$Bin276Max]<-temp$CopayBin276[temp$income>temp$Bin275Max & temp$income<=temp$Bin276Max]
    temp$FTcopay[temp$income>temp$Bin276Max & temp$income<=temp$Bin277Max]<-temp$CopayBin277[temp$income>temp$Bin276Max & temp$income<=temp$Bin277Max]
    temp$FTcopay[temp$income>temp$Bin277Max & temp$income<=temp$Bin278Max]<-temp$CopayBin278[temp$income>temp$Bin277Max & temp$income<=temp$Bin278Max]
    temp$FTcopay[temp$income>temp$Bin278Max & temp$income<=temp$Bin279Max]<-temp$CopayBin279[temp$income>temp$Bin278Max & temp$income<=temp$Bin279Max]
    temp$FTcopay[temp$income>temp$Bin279Max & temp$income<=temp$Bin280Max]<-temp$CopayBin280[temp$income>temp$Bin279Max & temp$income<=temp$Bin280Max]
    temp$FTcopay[temp$income>temp$Bin280Max & temp$income<=temp$Bin281Max]<-temp$CopayBin281[temp$income>temp$Bin280Max & temp$income<=temp$Bin281Max]
    temp$FTcopay[temp$income>temp$Bin281Max & temp$income<=temp$Bin282Max]<-temp$CopayBin282[temp$income>temp$Bin281Max & temp$income<=temp$Bin282Max]
    temp$FTcopay[temp$income>temp$Bin282Max & temp$income<=temp$Bin283Max]<-temp$CopayBin283[temp$income>temp$Bin282Max & temp$income<=temp$Bin283Max]
    temp$FTcopay[temp$income>temp$Bin283Max & temp$income<=temp$Bin284Max]<-temp$CopayBin284[temp$income>temp$Bin283Max & temp$income<=temp$Bin284Max]
    temp$FTcopay[temp$income>temp$Bin284Max & temp$income<=temp$Bin285Max]<-temp$CopayBin285[temp$income>temp$Bin284Max & temp$income<=temp$Bin285Max]
    temp$FTcopay[temp$income>temp$Bin285Max & temp$income<=temp$Bin286Max]<-temp$CopayBin286[temp$income>temp$Bin285Max & temp$income<=temp$Bin286Max]
    temp$FTcopay[temp$income>temp$Bin286Max & temp$income<=temp$Bin287Max]<-temp$CopayBin287[temp$income>temp$Bin286Max & temp$income<=temp$Bin287Max]
    temp$FTcopay[temp$income>temp$Bin287Max & temp$income<=temp$Bin288Max]<-temp$CopayBin288[temp$income>temp$Bin287Max & temp$income<=temp$Bin288Max]
    temp$FTcopay[temp$income>temp$Bin288Max & temp$income<=temp$Bin289Max]<-temp$CopayBin289[temp$income>temp$Bin288Max & temp$income<=temp$Bin289Max]
    
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    # Calculate total copay
    temp$totcopay<-temp$FTcopay*(temp$numkidsincare0to4+temp$numkidsincare5to12)*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==35]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==35]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==35]<-temp$InitialEligibility.y
  }
  
  
  
  # NEW HAMPSHIRE ----
  
  # Description:
  # Copay is a percentage of total income
  # Monthly frequency
  if(33 %in% unique(data$stateFIPS)){
    
    
    temp <- data[data$stateFIPS==33,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_NH$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_NH$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_NH$stateFIPS), AKorHI=unique(ccdfData_NH$AKorHI), famsize=unique(ccdfData_NH$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_NH[ccdfData_NH$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_NH$stateFIPS), AKorHI=unique(ccdfData_NH$AKorHI), famsize=unique(ccdfData_NH$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_NH, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_NH<-ccdfData_NH %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_NH<-ccdfData_NH %>% rbind(expandPastMiss2)}
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_NH, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$CopayBin7[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==33]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==33]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==33]<-temp$InitialEligibility.y
  }
  
  
  # NEW JERSEY ----
  
  if(34 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_NJ$stateFIPS <- 34
    
    temp<-data[data$stateFIPS==34,]
    
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_NJ$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_NJ$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_NJ$stateFIPS), AKorHI=unique(ccdfData_NJ$AKorHI), famsize=unique(ccdfData_NJ$famsize), numkidsInCare=unique(ccdfData_NJ$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_NJ[ccdfData_NJ$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_NJ$stateFIPS), AKorHI=unique(ccdfData_NJ$AKorHI), famsize=unique(ccdfData_NJ$famsize), numkidsInCare=unique(ccdfData_NJ$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_NJ, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_NJ<-ccdfData_NJ %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_NJ<-ccdfData_NJ %>% rbind(expandPastMiss2)}
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_NJ, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare", "ruleYear"))
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    temp$FTcopay[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]<-temp$CopayBin31[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    # Calculate total copay (12 months needed)
    temp$FTcopay[!is.na(temp$FTcopay)]<-as.numeric(temp$FTcopay[!is.na(temp$FTcopay)])
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Make sure the variables names are the same
    temp$totcopay <- temp$FTcopay*12 #totcopay is initilaized to NA in beginning
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==34]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==34]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==34]<-temp$InitialEligibility.y
  }
  
  
  
  # NORTH CAROLINA ----
  
  # Description:
  # Copay is a percentage of total income
  # Monthly frequency
  if(37 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_NC$stateFIPS <- 37
    #ccdfData_NC <- ccdfData_NC[ccdfData_NC$famsize < 8,]
    
    temp<-data[data$stateFIPS==37,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_NC$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_NC$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_NC$stateFIPS), AKorHI=unique(ccdfData_NC$AKorHI), famsize=unique(ccdfData_NC$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_NC[ccdfData_NC$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_NC$stateFIPS), AKorHI=unique(ccdfData_NC$AKorHI), famsize=unique(ccdfData_NC$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_NC, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_NC<-ccdfData_NC %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_NC<-ccdfData_NC %>% rbind(expandPastMiss2)}
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_NC, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1]<-temp$CopayBin1_MaxFractionOfIncome[temp$income>=0 & temp$income<=temp$Bin1]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==37]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==37]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==37]<-temp$InitialEligibility.y
  }
  
  
  
  # NORTH DAKOTA ----
  
  if(38 %in% unique(data$stateFIPS)){
    
    temp <- data[data$stateFIPS==38,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_ND$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_ND$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_ND$stateFIPS), AKorHI=unique(ccdfData_ND$AKorHI), famsize=unique(ccdfData_ND$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_ND[ccdfData_ND$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_ND$stateFIPS), AKorHI=unique(ccdfData_ND$AKorHI), famsize=unique(ccdfData_ND$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_ND, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_ND<-ccdfData_ND %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_ND<-ccdfData_ND %>% rbind(expandPastMiss2)}
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_ND, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$CopayBin7[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay#*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==38]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==38]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==38]<-temp$InitialEligibility.y
    
  }
  
  
  # NEW YORK ----
  
  # Description:
  # Copay is a percentage of total income in excess of SIS
  # Weekly frequency
  if(36 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_NY$stateFIPS <- 36
    temp<-data[data$stateFIPS==36,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_NY$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_NY$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_NY$stateFIPS), AKorHI=unique(ccdfData_NY$AKorHI), famsize=unique(ccdfData_NY$famsize), countyortownName=unique(ccdfData_NY$countyortownName), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_NY[ccdfData_NY$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_NY$stateFIPS), AKorHI=unique(ccdfData_NY$AKorHI), famsize=unique(ccdfData_NY$famsize), countyortownName=unique(ccdfData_NY$countyortownName), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_NY, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_NY<-ccdfData_NY %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_NY<-ccdfData_NY %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_NY, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "countyortownName"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    # No copay - just share of income which is already assigned
    # Calculate weekly copay
    # Can't pay more than the total remaining expenses
    #temp$weeklyCopay<-temp$PercentofIncome*(data$income-data$SIS)/52
    temp$weeklyCopay<-temp$PercentofIncome*(temp$income-temp$SIS)/52
    temp$weeklyCopay[temp$weeklyCopay<0]<-0 #replace negative values w/ 0 (income - sis can be neg if income is below FPL)
    temp$weeklyCopay[temp$income>temp$ContinuousEligibility]<-NA # Not eligible
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # TOTAL copays (number of weeks needed) - no weeks when family doesn't need childcare at all
    temp$totcopay<-temp$weeklyCopay*52
    
    # Can't pay more than the total remaining expenses
    temp$totcopay<-rowMins(cbind(temp$totcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$weeklyCopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==36]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==36]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==36]<-temp$InitialEligibility.y
  }
  
  
  
  # OHIO ----
  
  # Description:
  # Copay is a percentage of total income
  # Monthly frequency
  if(39 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_OH$stateFIPS <- 39
    
    temp<-data[data$stateFIPS==39,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_OH$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_OH$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_OH$stateFIPS), AKorHI=unique(ccdfData_OH$AKorHI), famsize=unique(ccdfData_OH$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_OH[ccdfData_OH$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_OH$stateFIPS), AKorHI=unique(ccdfData_OH$AKorHI), famsize=unique(ccdfData_OH$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_OH, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_OH<-ccdfData_OH %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_OH<-ccdfData_OH %>% rbind(expandPastMiss2)}
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_OH, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1_FractionOfGrossIncome[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2_FractionOfGrossIncome[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3_FractionOfGrossIncome[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4_FractionOfGrossIncome[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5_FractionOfGrossIncome[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6_FractionOfGrossIncome[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$CopayBin7_FractionOfGrossIncome[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    temp$FTcopay[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]<-temp$CopayBin8_FractionOfGrossIncome[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]
    temp$FTcopay[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]<-temp$CopayBin9_FractionOfGrossIncome[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]
    temp$FTcopay[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]<-temp$CopayBin10_FractionOfGrossIncome[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]
    temp$FTcopay[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]<-temp$CopayBin11_FractionOfGrossIncome[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]
    temp$FTcopay[temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max]<-temp$CopayBin12_FractionOfGrossIncome[temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max]
    temp$FTcopay[temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max]<-temp$CopayBin13_FractionOfGrossIncome[temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max]
    temp$FTcopay[temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max]<-temp$CopayBin14_FractionOfGrossIncome[temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max]
    temp$FTcopay[temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max]<-temp$CopayBin15_FractionOfGrossIncome[temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max]
    temp$FTcopay[temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max]<-temp$CopayBin16_FractionOfGrossIncome[temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max]
    temp$FTcopay[temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max]<-temp$CopayBin17_FractionOfGrossIncome[temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max]
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18_FractionOfGrossIncome[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19_FractionOfGrossIncome[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20_FractionOfGrossIncome[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21_FractionOfGrossIncome[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22_FractionOfGrossIncome[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23_FractionOfGrossIncome[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24_FractionOfGrossIncome[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25_FractionOfGrossIncome[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26_FractionOfGrossIncome[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27_FractionOfGrossIncome[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28_FractionOfGrossIncome[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29_FractionOfGrossIncome[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30_FractionOfGrossIncome[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    temp$FTcopay[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]<-temp$CopayBin31_FractionOfGrossIncome[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]
    temp$FTcopay[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]<-temp$CopayBin32_FractionOfGrossIncome[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]
    temp$FTcopay[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]<-temp$CopayBin33_FractionOfGrossIncome[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]
    temp$FTcopay[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]<-temp$CopayBin34_FractionOfGrossIncome[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]
    temp$FTcopay[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]<-temp$CopayBin35_FractionOfGrossIncome[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]
    temp$FTcopay[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]<-temp$CopayBin36_FractionOfGrossIncome[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]
    temp$FTcopay[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]<-temp$CopayBin37_FractionOfGrossIncome[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]
    temp$FTcopay[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]<-temp$CopayBin38_FractionOfGrossIncome[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]
    temp$FTcopay[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]<-temp$CopayBin39_FractionOfGrossIncome[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]
    temp$FTcopay[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]<-temp$CopayBin40_FractionOfGrossIncome[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]
    temp$FTcopay[temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max]<-temp$CopayBin41_FractionOfGrossIncome[temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Can't pay more than the total remaining expenses
    
    
    #temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    temp$totcopay[temp$ruleYear<=2022] <- temp$income[temp$ruleYear<=2022]*temp$FTcopay[temp$ruleYear<=2022]
    
    temp$totcopay[temp$ruleYear>=2023] <- temp$FTcopay[temp$ruleYear>=2023]*52 # 2023 onwards, Ohio charges copay as a weekly dollar amount, rather than as a monthly percentage of income
    
    temp$totcopay<-rowMins(cbind(temp$totcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==39]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==39]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==39]<-temp$InitialEligibility.y
  }
  
  
  
  # OKLAHOMA ----
  
  # Description:
  # Variation at the state level
  # Fixed copay per family
  # Monthly frequency
  # No additional charge after certain number of children
  if(40 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    #ccdfData_OK <- ccdfData_OK[ccdfData_OK$famsize <= 12,]
    
    temp<-data[data$stateFIPS==40,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_OK$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_OK$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_OK$stateFIPS), AKorHI=unique(ccdfData_OK$AKorHI), famsize=unique(ccdfData_OK$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_OK[ccdfData_OK$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_OK$stateFIPS), AKorHI=unique(ccdfData_OK$AKorHI), famsize=unique(ccdfData_OK$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_OK, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_OK<-ccdfData_OK %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_OK<-ccdfData_OK %>% rbind(expandPastMiss2)}
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_OK, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
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
    
    #View(temp[,c("countyortownName", "FTcopay", "numkidsincare0to4", "numkidsincare5to12", "SCHcopay", "SUMcopay", "netexp.childcare")])
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==40]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==40]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==40]<-temp$InitialEligibility.y
  }
  
  
  # OREGON ----
  
  if(41 %in% unique(data$stateFIPS)){
    
    temp <- data[data$stateFIPS==41,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_OR$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_OR$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_OR$stateFIPS), AKorHI=unique(ccdfData_OR$AKorHI), famsize=unique(ccdfData_OR$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_OR[ccdfData_OR$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_OR$stateFIPS), AKorHI=unique(ccdfData_OR$AKorHI), famsize=unique(ccdfData_OR$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_OR, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_OR<-ccdfData_OR %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_OR<-ccdfData_OR %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_OR, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==41]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==41]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==41]<-temp$InitialEligibility.y
    
  }
  
  # PENNSYLVANIA ----
  
  if(42 %in% unique(data$stateFIPS)){
    
    temp <- data[data$stateFIPS==42,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_PA$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_PA$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_PA$stateFIPS), AKorHI=unique(ccdfData_PA$AKorHI), famsize=unique(ccdfData_PA$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_PA[ccdfData_PA$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_PA$stateFIPS), AKorHI=unique(ccdfData_PA$AKorHI), famsize=unique(ccdfData_PA$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_PA, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_PA<-ccdfData_PA %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_PA<-ccdfData_PA %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    # #----------------------------------
    #ccdfData_PA$IncomeDisregard <- 0
    
    temp<-left_join(temp, ccdfData_PA, by=c("stateFIPS", "AKorHI", "famsize", "ruleYear"))
    
    temp[is.na(temp)] <- 0
    #temp$income<-temp$income-12*temp$IncomeDisregard
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin29[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    temp$FTcopay[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]<-temp$CopayBin30[temp$income>temp$Bin29Max & temp$income<=temp$Bin30Max]
    temp$FTcopay[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]<-temp$CopayBin31[temp$income>temp$Bin30Max & temp$income<=temp$Bin31Max]
    temp$FTcopay[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]<-temp$CopayBin32[temp$income>temp$Bin31Max & temp$income<=temp$Bin32Max]
    temp$FTcopay[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]<-temp$CopayBin33[temp$income>temp$Bin32Max & temp$income<=temp$Bin33Max]
    temp$FTcopay[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]<-temp$CopayBin34[temp$income>temp$Bin33Max & temp$income<=temp$Bin34Max]
    temp$FTcopay[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]<-temp$CopayBin35[temp$income>temp$Bin34Max & temp$income<=temp$Bin35Max]
    temp$FTcopay[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]<-temp$CopayBin36[temp$income>temp$Bin35Max & temp$income<=temp$Bin36Max]
    temp$FTcopay[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]<-temp$CopayBin37[temp$income>temp$Bin36Max & temp$income<=temp$Bin37Max]
    temp$FTcopay[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]<-temp$CopayBin38[temp$income>temp$Bin37Max & temp$income<=temp$Bin38Max]
    temp$FTcopay[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]<-temp$CopayBin39[temp$income>temp$Bin38Max & temp$income<=temp$Bin39Max]
    temp$FTcopay[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]<-temp$CopayBin40[temp$income>temp$Bin39Max & temp$income<=temp$Bin40Max]
    temp$FTcopay[temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max]<-temp$CopayBin41[temp$income>temp$Bin40Max & temp$income<=temp$Bin41Max]
    temp$FTcopay[temp$income>temp$Bin41Max & temp$income<=temp$Bin42Max]<-temp$CopayBin42[temp$income>temp$Bin41Max & temp$income<=temp$Bin42Max]
    temp$FTcopay[temp$income>temp$Bin42Max & temp$income<=temp$Bin43Max]<-temp$CopayBin43[temp$income>temp$Bin42Max & temp$income<=temp$Bin43Max]
    temp$FTcopay[temp$income>temp$Bin43Max & temp$income<=temp$Bin44Max]<-temp$CopayBin44[temp$income>temp$Bin43Max & temp$income<=temp$Bin44Max]
    temp$FTcopay[temp$income>temp$Bin44Max & temp$income<=temp$Bin45Max]<-temp$CopayBin45[temp$income>temp$Bin44Max & temp$income<=temp$Bin45Max]
    temp$FTcopay[temp$income>temp$Bin45Max & temp$income<=temp$Bin46Max]<-temp$CopayBin46[temp$income>temp$Bin45Max & temp$income<=temp$Bin46Max]
    temp$FTcopay[temp$income>temp$Bin46Max & temp$income<=temp$Bin47Max]<-temp$CopayBin47[temp$income>temp$Bin46Max & temp$income<=temp$Bin47Max]
    temp$FTcopay[temp$income>temp$Bin47Max & temp$income<=temp$Bin48Max]<-temp$CopayBin48[temp$income>temp$Bin47Max & temp$income<=temp$Bin48Max]
    temp$FTcopay[temp$income>temp$Bin48Max & temp$income<=temp$Bin49Max]<-temp$CopayBin49[temp$income>temp$Bin48Max & temp$income<=temp$Bin49Max]
    temp$FTcopay[temp$income>temp$Bin49Max & temp$income<=temp$Bin50Max]<-temp$CopayBin50[temp$income>temp$Bin49Max & temp$income<=temp$Bin50Max]
    temp$FTcopay[temp$income>temp$Bin50Max & temp$income<=temp$Bin51Max]<-temp$CopayBin51[temp$income>temp$Bin50Max & temp$income<=temp$Bin51Max]
    temp$FTcopay[temp$income>temp$Bin51Max & temp$income<=temp$Bin52Max]<-temp$CopayBin52[temp$income>temp$Bin51Max & temp$income<=temp$Bin52Max]
    temp$FTcopay[temp$income>temp$Bin52Max & temp$income<=temp$Bin53Max]<-temp$CopayBin53[temp$income>temp$Bin52Max & temp$income<=temp$Bin53Max]
    temp$FTcopay[temp$income>temp$Bin53Max & temp$income<=temp$Bin54Max]<-temp$CopayBin54[temp$income>temp$Bin53Max & temp$income<=temp$Bin54Max]
    temp$FTcopay[temp$income>temp$Bin54Max & temp$income<=temp$Bin55Max]<-temp$CopayBin55[temp$income>temp$Bin54Max & temp$income<=temp$Bin55Max]
    temp$FTcopay[temp$income>temp$Bin55Max & temp$income<=temp$Bin56Max]<-temp$CopayBin56[temp$income>temp$Bin55Max & temp$income<=temp$Bin56Max]
    temp$FTcopay[temp$income>temp$Bin56Max & temp$income<=temp$Bin57Max]<-temp$CopayBin57[temp$income>temp$Bin56Max & temp$income<=temp$Bin57Max]
    temp$FTcopay[temp$income>temp$Bin57Max & temp$income<=temp$Bin58Max]<-temp$CopayBin58[temp$income>temp$Bin57Max & temp$income<=temp$Bin58Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*52
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==42]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==42]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==42]<-temp$InitialEligibility.y
    
  }
  
  
  # RHODE ISLAND ----
  
  # Description:
  # Copay is a percentage of total income
  # Monthly frequency
  if(44 %in% unique(data$stateFIPS)){
    
    temp <- data[data$stateFIPS==44,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_RI$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_RI$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_RI$stateFIPS), AKorHI=unique(ccdfData_RI$AKorHI), famsize=unique(ccdfData_RI$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_RI[ccdfData_RI$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_RI$stateFIPS), AKorHI=unique(ccdfData_RI$AKorHI), famsize=unique(ccdfData_RI$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_RI, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_RI<-ccdfData_RI %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_RI<-ccdfData_RI %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_RI, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==44]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==44]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==44]<-temp$InitialEligibility.y
  }
  
  
  # SOUTH CAROLINA ----
  
  # Description:
  # Copay is a fixed dollar amount per child, same for each child
  # Weekly frequency
  if(45 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==45,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_SC$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_SC$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_SC$stateFIPS), AKorHI=unique(ccdfData_SC$AKorHI), famsize=unique(ccdfData_SC$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_SC[ccdfData_SC$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_SC$stateFIPS), AKorHI=unique(ccdfData_SC$AKorHI), famsize=unique(ccdfData_SC$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_SC, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_SC<-ccdfData_SC %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_SC<-ccdfData_SC %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_SC, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Calculate total copay
    temp$totcopay<-temp$FTcopay*(temp$numkidsincare0to4+temp$numkidsincare5to12)*52
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==45]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==45]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==45]<-temp$InitialEligibility.y
  }
  
  
  # SOUTH DAKOTA ----
  
  # Description:
  # Fixed copay dollar amount per family
  # Monthly frequency
  if(46 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==46,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_SD$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_SD$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_SD$stateFIPS), AKorHI=unique(ccdfData_SD$AKorHI), famsize=unique(ccdfData_SD$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_SD[ccdfData_SD$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_SD$stateFIPS), AKorHI=unique(ccdfData_SD$AKorHI), famsize=unique(ccdfData_SD$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_SD, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_SD<-ccdfData_SD %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_SD<-ccdfData_SD %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_SD, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income*(1-temp$IncomeDisregard)
    
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
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==46]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==46]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==46]<-temp$InitialEligibility.y
  }
  
  
  
  # TENNESSE ----
  # Description:
  # Copay is a dollar amount per child
  # Daily frequency
  # Discount for two or more children (under construction)
  # Fee is up to a maximum per family (under construction)
  if(47 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_TN$stateFIPS <- 47
    
    temp<-data[data$stateFIPS==47,]
    #  temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    #temp$numkidsInCare<-temp$numkidsincare0to4
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_TN$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_TN$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_TN$stateFIPS), AKorHI=unique(ccdfData_TN$AKorHI), famsize=unique(ccdfData_TN$famsize), numkidsInCare=unique(ccdfData_TN$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_TN[ccdfData_TN$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_TN$stateFIPS), AKorHI=unique(ccdfData_TN$AKorHI), famsize=unique(ccdfData_TN$famsize), numkidsInCare=unique(ccdfData_TN$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_TN, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_TN<-ccdfData_TN %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_TN<-ccdfData_TN %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_TN, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
    
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    
    temp$totcopay<-NA
    
    # temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # temp$totcopay<-rowMins(cbind(temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12),temp$netexp.childcare))
    temp$totcopay <- temp$FTcopay*12
    
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    
    # NEED TO REWORK THIS ONCE EDGAR & CO GET FAMILY MAX SIZE UP TO 12
    temp$totcopay[temp$agePerson7>5 & !is.na(temp$agePerson7>5)] <- temp$totcopay[temp$agePerson7>5 & !is.na(temp$agePerson7>5)] + temp$netexp.childcareperson7[temp$agePerson7>5 & !is.na(temp$agePerson7>5)]
    temp$totcopay[temp$agePerson8>5 & !is.na(temp$agePerson8>5)] <- temp$totcopay[temp$agePerson8>5 & !is.na(temp$agePerson8>5)] + temp$netexp.childcareperson8[temp$agePerson8>5 & !is.na(temp$agePerson8>5)]
    temp$totcopay[temp$agePerson9>5 & !is.na(temp$agePerson9>5)] <- temp$totcopay[temp$agePerson9>5 & !is.na(temp$agePerson9>5)] + temp$netexp.childcareperson9[temp$agePerson9>5 & !is.na(temp$agePerson9>5)]
    temp$totcopay[temp$agePerson10>5 & !is.na(temp$agePerson10>5)] <- temp$totcopay[temp$agePerson10>5 & !is.na(temp$agePerson10>5)] + temp$netexp.childcareperson10[temp$agePerson10>5 & !is.na(temp$agePerson10>5)]
    temp$totcopay[temp$agePerson11>5 & !is.na(temp$agePerson11>5)] <- temp$totcopay[temp$agePerson11>5 & !is.na(temp$agePerson11>5)] + temp$netexp.childcareperson10[temp$agePerson11>5 & !is.na(temp$agePerson11>5)]
    temp$totcopay[temp$agePerson12>5 & !is.na(temp$agePerson12>5)] <- temp$totcopay[temp$agePerson12>5 & !is.na(temp$agePerson12>5)] + temp$netexp.childcareperson10[temp$agePerson12>5 & !is.na(temp$agePerson12>5)]
    # Set copay to zero if no children
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==47]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==47]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==47]<-temp$InitialEligibility.y
    
  }
  
  
  
  # TEXAS ----
  # Description:
  # Variation at the Board level (approximated by Gulf Coast as the most populated area)
  # Fixed copay per family
  # Monthly frequency
  # 2nd+ kids in care have reduced price
  if(48 %in% unique(data$stateFIPS)){
    temp<-data[data$stateFIPS==48,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_TX$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_TX$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_TX$stateFIPS), AKorHI=unique(ccdfData_TX$AKorHI), famsize=unique(ccdfData_TX$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_TX[ccdfData_TX$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_TX$stateFIPS), AKorHI=unique(ccdfData_TX$AKorHI), famsize=unique(ccdfData_TX$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_TX, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_TX<-ccdfData_TX %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_TX<-ccdfData_TX %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_TX, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay.FirstChild<-NA
    temp$FTcopay.AdditionalChild<-NA
    
    temp$FTcopay.FirstChild[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1.FirstChild[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay.FirstChild[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2.FirstChild[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay.FirstChild[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3.FirstChild[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay.FirstChild[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4.FirstChild[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay.FirstChild[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5.FirstChild[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay.FirstChild[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6.FirstChild[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay.FirstChild[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$CopayBin7.FirstChild[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    temp$FTcopay.FirstChild[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]<-temp$CopayBin8.FirstChild[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]
    temp$FTcopay.FirstChild[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]<-temp$CopayBin9.FirstChild[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]
    
    temp$FTcopay.AdditionalChild[temp$income>0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1.AdditionalChild[temp$income>0 & temp$income<=temp$Bin1Max]
    temp$FTcopay.AdditionalChild[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2.AdditionalChild[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay.AdditionalChild[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3.AdditionalChild[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay.AdditionalChild[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4.AdditionalChild[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay.AdditionalChild[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5.AdditionalChild[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay.AdditionalChild[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6.AdditionalChild[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay.AdditionalChild[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$CopayBin7.AdditionalChild[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    temp$FTcopay.AdditionalChild[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]<-temp$CopayBin8.AdditionalChild[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]
    temp$FTcopay.AdditionalChild[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]<-temp$CopayBin9.AdditionalChild[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    temp$SCHcopay<-NA
    temp$SUMcopay<-NA
    temp$totcopay<-NA
    
    # Determine how much care child need
    # age < 5 requires full-time school time care and full-time summer care (Full-time school is)
    # age 5 to 12 requires part-time school time care and part-time summer care
    
    # TOTAL School time copay
    temp$SCHcopay<-6.5*(temp$FTcopay.FirstChild+(temp$numkidsincare0to4+temp$numkidsincare5to12-1)*temp$FTcopay.AdditionalChild)
    temp$SUMcopay<-2.3*(temp$FTcopay.FirstChild+(temp$numkidsincare0to4+temp$numkidsincare5to12-1)*temp$FTcopay.AdditionalChild)
    
    # Before and after-school care is 40% reduction
    # UNDER CONSTRUCTION
    
    temp$totcopay<-temp$SUMcopay+temp$SCHcopay
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay.FirstChild)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==48]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==48]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==48]<-temp$InitialEligibility.y
    
  }
  
  
  # UTAH ----
  # Description:
  # Copay is a fixed amount per child
  # Copay amount varies by number of children that family has in care
  # Monthly frequency
  if(49 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_UT$stateFIPS <- 49
    ccdfData_UT$AssetTest <- 1000000
    
    temp<-data[data$stateFIPS==49,]
    
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_UT$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_UT$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_UT$stateFIPS), AKorHI=unique(ccdfData_UT$AKorHI), famsize=unique(ccdfData_UT$famsize), numkidsInCare=unique(ccdfData_UT$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_UT[ccdfData_UT$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_UT$stateFIPS), AKorHI=unique(ccdfData_UT$AKorHI), famsize=unique(ccdfData_UT$famsize), numkidsInCare=unique(ccdfData_UT$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_UT, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_UT<-ccdfData_UT %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_UT<-ccdfData_UT %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_UT, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
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
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Calculate total copay (12 months needed)
    temp$totcopay<-temp$FTcopay*12
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==49]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==49]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==49]<-temp$InitialEligibility.y
  }
  
  
  # VERMONT ----
  if(50 %in% unique(data$stateFIPS)){
    
    temp <- data[data$stateFIPS==50,]
    
    # providercost_VT$stateFIPS <- 50
    #
    #if(temp$ruleYear < 2022){
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(providercost_VT$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(providercost_VT$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(providercost_VT$stateFIPS), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-providercost_VT[providercost_VT$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(providercost_VT$stateFIPS), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, providercost_VT, by=c("stateFIPS"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {providercost_VT<-providercost_VT %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {providercost_VT<-providercost_VT %>% rbind(expandPastMiss2)}
    
    
    temp<-left_join(temp, providercost_VT, by=c("stateFIPS", "ruleYear"))
    
    # Determine Expense based on Age of Children
    temp<-temp %>%
      mutate(sprPerson1=(case_when(agePerson1 %in% c(0)~ftdailyrate.infant,
                                   agePerson1 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson1 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson1 %in% c(6:12)~ftdailyrate.schoolage,
                                   agePerson1 > 12 ~ 0,
                                   TRUE~0)
      )
      )
    temp<-temp %>%
      mutate(sprPerson2=(case_when(agePerson2 %in% c(0)~ftdailyrate.infant,
                                   agePerson2 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson2 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson2 %in% c(6:12)~ftdailyrate.schoolage,
                                   agePerson2 > 12 ~ 0,
                                   TRUE~0)
      )
      )
    temp<-temp %>%
      mutate(sprPerson3=(case_when(agePerson3 %in% c(0)~ftdailyrate.infant,
                                   agePerson3 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson3 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson3 %in% c(6:12)~ftdailyrate.schoolage,
                                   agePerson3 > 12 ~ 0,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson4=(case_when(agePerson4 %in% c(0)~ftdailyrate.infant,
                                   agePerson4 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson4 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson4 %in% c(6:12)~ftdailyrate.schoolage,
                                   agePerson4 > 12 ~ 0,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson5=(case_when(agePerson5 %in% c(0)~ftdailyrate.infant,
                                   agePerson5 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson5 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson5 %in% c(6:12)~ftdailyrate.schoolage,
                                   agePerson5 > 12 ~ 0,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson6=(case_when(agePerson6 %in% c(0)~ftdailyrate.infant,
                                   agePerson6 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson6 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson6 %in% c(6:12)~ftdailyrate.schoolage,
                                   agePerson6 > 12 ~ 0,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson7=(case_when(agePerson7 %in% c(0)~ftdailyrate.infant,
                                   agePerson7 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson7 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson7 %in% c(6:12)~ftdailyrate.schoolage,
                                   agePerson7 > 12 ~ 0,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson8=(case_when(agePerson8 %in% c(0)~ftdailyrate.infant,
                                   agePerson8 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson8 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson8 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson9=(case_when(agePerson9 %in% c(0)~ftdailyrate.infant,
                                   agePerson9 %in% c(1:2)~ftdailyrate.toddler,
                                   agePerson9 %in% c(3:5)~ftdailyrate.preschool,
                                   agePerson9 %in% c(6:12)~ftdailyrate.schoolage,
                                   TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson10=(case_when(agePerson10 %in% c(0)~ftdailyrate.infant,
                                    agePerson10 %in% c(1:2)~ftdailyrate.toddler,
                                    agePerson10 %in% c(3:5)~ftdailyrate.preschool,
                                    agePerson10 %in% c(6:12)~ftdailyrate.schoolage,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson11=(case_when(agePerson11 %in% c(0)~ftdailyrate.infant,
                                    agePerson11 %in% c(1:2)~ftdailyrate.toddler,
                                    agePerson11 %in% c(3:5)~ftdailyrate.preschool,
                                    agePerson11 %in% c(6:12)~ftdailyrate.schoolage,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprPerson12=(case_when(agePerson12 %in% c(0)~ftdailyrate.infant,
                                    agePerson12 %in% c(1:2)~ftdailyrate.toddler,
                                    agePerson12 %in% c(3:5)~ftdailyrate.preschool,
                                    agePerson12 %in% c(6:12)~ftdailyrate.schoolage,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(sprTotal=sprPerson1+sprPerson2+sprPerson3+sprPerson4+sprPerson5+sprPerson6+sprPerson7+sprPerson8+sprPerson9+sprPerson10+sprPerson11+sprPerson12)
    
    temp<-temp %>%
      mutate(annualcost1=(case_when(agePerson1 < 5 ~ sprPerson1*daysofcareneeded0to4,
                                    agePerson1 > 4 & agePerson1 < 13 ~ sprPerson1*daysofcareneeded5to12,
                                    agePerson1 > 12 ~ 0,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost2=(case_when(agePerson2 < 5 ~ sprPerson2*daysofcareneeded0to4,
                                    agePerson2 > 4 & agePerson2 < 13 ~ sprPerson2*daysofcareneeded5to12,
                                    agePerson2 > 12 ~ 0,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost3=(case_when(agePerson3 < 5 ~ sprPerson3*daysofcareneeded0to4,
                                    agePerson3 > 4 & agePerson3 < 13 ~ sprPerson3*daysofcareneeded5to12,
                                    agePerson3 > 12 ~ 0,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost4=(case_when(agePerson4 < 5 ~ sprPerson4*daysofcareneeded0to4,
                                    agePerson4 > 4 & agePerson4 < 13 ~ sprPerson4*daysofcareneeded5to12,
                                    agePerson4 > 12 ~ 0,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost5=(case_when(agePerson5 < 5 ~ sprPerson5*daysofcareneeded0to4,
                                    agePerson5 > 4 & agePerson5 < 13 ~ sprPerson5*daysofcareneeded5to12,
                                    agePerson5 > 12 ~ 0,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost6=(case_when(agePerson6 < 5 ~ sprPerson6*daysofcareneeded0to4,
                                    agePerson6 > 4 & agePerson6 < 13 ~ sprPerson6*daysofcareneeded5to12,
                                    agePerson6 > 12 ~ 0,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost7=(case_when(agePerson7 < 5 ~ sprPerson7*daysofcareneeded0to4,
                                    agePerson7 > 4 & agePerson7 < 13 ~ sprPerson7*daysofcareneeded5to12,
                                    agePerson7 > 12 ~ 0,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost8=(case_when(agePerson8 < 5 ~ sprPerson8*daysofcareneeded0to4,
                                    agePerson8 > 4 & agePerson8 < 13 ~ sprPerson8*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost9=(case_when(agePerson9 < 5 ~ sprPerson9*daysofcareneeded0to4,
                                    agePerson9 > 4 & agePerson9 < 13 ~ sprPerson9*daysofcareneeded5to12,
                                    TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost10=(case_when(agePerson10 < 5 ~ sprPerson10*daysofcareneeded0to4,
                                     agePerson10 > 4 & agePerson10 < 13 ~ sprPerson10*daysofcareneeded5to12,
                                     TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost11=(case_when(agePerson11 < 5 ~ sprPerson11*daysofcareneeded0to4,
                                     agePerson11 > 4 & agePerson11 < 13 ~ sprPerson11*daysofcareneeded5to12,
                                     TRUE~0)
      )
      )
    
    temp<-temp %>%
      mutate(annualcost12=(case_when(agePerson12 < 5 ~ sprPerson12*daysofcareneeded0to4,
                                     agePerson12 > 4 & agePerson12 < 13 ~ sprPerson12*daysofcareneeded5to12,
                                     TRUE~0)
      )
      )
    
    temp$annualCost<-temp$annualcost1+temp$annualcost2+temp$annualcost3+temp$annualcost4+temp$annualcost5+temp$annualcost6+temp$annualcost7+temp$annualcost8+temp$annualcost9+temp$annualcost10+temp$annualcost11+temp$annualcost12
    # }
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    # Uses most current rules in PRD and imputes them for years in the data set
    
    #  ccdfData_VT <- ccdfData_VT[ccdfData_VT$ruleYear == 2022,]
    #  ccdfData_VT$ruleYear <- 2021
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_VT$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_VT$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_VT$stateFIPS), AKorHI=unique(ccdfData_VT$AKorHI), famsize=unique(ccdfData_VT$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_VT[ccdfData_VT$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_VT$stateFIPS), AKorHI=unique(ccdfData_VT$AKorHI), famsize=unique(ccdfData_VT$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_VT, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_VT<-ccdfData_VT %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_VT<-ccdfData_VT %>% rbind(expandPastMiss2)}
    
    temp<-left_join(temp, ccdfData_VT, by=c("stateFIPS", "famsize", "ruleYear"))
    
    # temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$ShareofCost1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$ShareofCost2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$ShareofCost3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$ShareofCost4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$ShareofCost5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$ShareofCost6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    temp$FTcopay[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]<-temp$ShareofCost7[temp$income>temp$Bin6Max & temp$income<=temp$Bin7Max]
    temp$FTcopay[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]<-temp$ShareofCost8[temp$income>temp$Bin7Max & temp$income<=temp$Bin8Max]
    temp$FTcopay[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]<-temp$ShareofCost9[temp$income>temp$Bin8Max & temp$income<=temp$Bin9Max]
    
    temp$FTcopay[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]<-temp$ShareofCost10[temp$income>temp$Bin9Max & temp$income<=temp$Bin10Max]
    temp$FTcopay[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]<-temp$ShareofCost11[temp$income>temp$Bin10Max & temp$income<=temp$Bin11Max]
    temp$FTcopay[temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max]<-temp$ShareofCost12[temp$income>temp$Bin11Max & temp$income<=temp$Bin12Max]
    temp$FTcopay[temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max]<-temp$ShareofCost13[temp$income>temp$Bin12Max & temp$income<=temp$Bin13Max]
    temp$FTcopay[temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max]<-temp$ShareofCost14[temp$income>temp$Bin13Max & temp$income<=temp$Bin14Max]
    temp$FTcopay[temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max]<-temp$ShareofCost15[temp$income>temp$Bin14Max & temp$income<=temp$Bin15Max]
    temp$FTcopay[temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max]<-temp$ShareofCost16[temp$income>temp$Bin15Max & temp$income<=temp$Bin16Max]
    temp$FTcopay[temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max]<-temp$ShareofCost17[temp$income>temp$Bin16Max & temp$income<=temp$Bin17Max]
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$ShareofCost18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$ShareofCost19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$ShareofCost20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$ShareofCost21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$ShareofCost22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$ShareofCost23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    
    
    #
    # temp$totcopay[temp$ruleYear!=2022]<-rowMins(cbind(temp$annualCost[temp$ruleYear!=2022]*(1-temp$FTcopay[temp$ruleYear!=2022]),temp$netexp.childcare[temp$ruleYear!=2022]))
    
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    temp$totcopay[temp$ruleYear<=2022] <- temp$annualCost[temp$ruleYear<=2022]*(1-temp$FTcopay[temp$ruleYear<=2022])
    
    temp$totcopay[temp$ruleYear>=2023] <- temp$FTcopay[temp$ruleYear>=2023]*52
    
    temp$totcopay<-rowMins(cbind(temp$totcopay,temp$netexp.childcare))
    
    
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==50]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==50]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==50]<-temp$InitialEligibility.y
  }
  
  
  
  # VIRGINIA ----
  # Description:
  # Copay is a percentage of total income
  # Weekly frequency
  if(51 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_VA$stateFIPS <- 51
    
    temp<-data[data$stateFIPS==51,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_VA$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_VA$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_VA$stateFIPS), AKorHI=unique(ccdfData_VA$AKorHI), famsize=unique(ccdfData_VA$famsize), countyortownName=unique(ccdfData_VA$countyortownName), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_VA[ccdfData_VA$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_VA$stateFIPS), AKorHI=unique(ccdfData_VA$AKorHI), famsize=unique(ccdfData_VA$famsize), countyortownName=unique(ccdfData_VA$countyortownName), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_VA, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_VA<-ccdfData_VA %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_VA<-ccdfData_VA %>% rbind(expandPastMiss2)}
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    
    temp<-left_join(temp, ccdfData_VA, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "countyortownName"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Can't pay more than the total remaining expenses
    
    temp$totcopay[temp$ruleYear<2023]<-temp$income[temp$ruleYear<2023]*temp$FTcopay[temp$ruleYear<2023]
    
    temp$totcopay[temp$ruleYear>=2023]<-temp$income[temp$ruleYear>=2023]*12
    
    temp$totcopay<-rowMins(cbind(temp$totcopay,temp$netexp.childcare))
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==51]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==51]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==51]<-temp$InitialEligibility.y
  }
  
  
  
  # WASHINGTON  ----
  # Description:
  # Variation at the state level
  # Fixed copay per family
  # Monthly frequency
  if(53 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==53,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_WA$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_WA$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_WA$stateFIPS), AKorHI=unique(ccdfData_WA$AKorHI), famsize=unique(ccdfData_WA$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_WA[ccdfData_WA$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_WA$stateFIPS), AKorHI=unique(ccdfData_WA$AKorHI), famsize=unique(ccdfData_WA$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_WA, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_WA<-ccdfData_WA %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_WA<-ccdfData_WA %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_WA, by=c("stateFIPS", "AKorHI", "ruleYear", "famsize"))
    
    #######################################
    
    if(2010 %in% unique(temp$ruleYear)){
      
      temp_2011<-temp[temp$ruleYear==2010,]
      
      # Adjust for the income disregard
      temp_2011$income<-temp_2011$income-12*temp_2011$IncomeDisregard
      
      temp_2011$FTcopay<-NA
      
      temp_2011$FTcopay[temp_2011$income>=0 & temp_2011$income<=temp_2011$Bin1Max]<-temp_2011$CopayBin1[temp_2011$income>=0 & temp_2011$income<=temp_2011$Bin1Max]
      temp_2011$FTcopay[temp_2011$income>temp_2011$Bin1Max & temp_2011$income<=temp_2011$Bin2Max]<-temp_2011$CopayBin2[temp_2011$income>temp_2011$Bin1Max & temp_2011$income<=temp_2011$Bin2Max]
      temp_2011$FTcopay[temp_2011$income>temp_2011$Bin2Max & temp_2011$income<=temp_2011$Bin4Max]<-temp_2011$CopayBin2[temp_2011$income>temp_2011$Bin2Max & temp_2011$income<=temp_2011$Bin4Max]+0.5*(temp_2011$income[temp_2011$income>temp_2011$Bin2Max & temp_2011$income<=temp_2011$Bin4Max]-temp_2011$Bin2Max[temp_2011$income>temp_2011$Bin2Max & temp_2011$income<=temp_2011$Bin4Max])/12 # Apply sliding fee scale formula
      
      
      # Apply asset test
      subset<-temp_2011$totalassets > temp_2011$AssetTest
      temp_2011$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2011$totcopay<-temp_2011$FTcopay*12
      
      # Set copay to zero if no children
      temp_2011$totcopay[temp_2011$numkidsincare0to4+temp_2011$numkidsincare5to12==0]<-0
      
      temp_2011$totcopay[is.na(temp_2011$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2011$childcare.overage[temp_2011$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2010]<-temp_2011$childcare.overage
      temp$totcopay[temp$ruleYear==2010]<-temp_2011$totcopay
      
    }
    
    
    # APPLY 2011 RULES
    if(2011 %in% unique(temp$ruleYear)){
      
      temp_2011<-temp[temp$ruleYear==2011,]
      
      # Adjust for the income disregard
      temp_2011$income<-temp_2011$income-12*temp_2011$IncomeDisregard
      
      temp_2011$FTcopay<-NA
      
      temp_2011$FTcopay[temp_2011$income>=0 & temp_2011$income<=temp_2011$Bin1Max]<-temp_2011$CopayBin1[temp_2011$income>=0 & temp_2011$income<=temp_2011$Bin1Max]
      temp_2011$FTcopay[temp_2011$income>temp_2011$Bin1Max & temp_2011$income<=temp_2011$Bin2Max]<-temp_2011$CopayBin2[temp_2011$income>temp_2011$Bin1Max & temp_2011$income<=temp_2011$Bin2Max]
      temp_2011$FTcopay[temp_2011$income>temp_2011$Bin2Max & temp_2011$income<=temp_2011$Bin4Max]<-temp_2011$CopayBin2[temp_2011$income>temp_2011$Bin2Max & temp_2011$income<=temp_2011$Bin4Max]+0.5*(temp_2011$income[temp_2011$income>temp_2011$Bin2Max & temp_2011$income<=temp_2011$Bin4Max]-temp_2011$Bin2Max[temp_2011$income>temp_2011$Bin2Max & temp_2011$income<=temp_2011$Bin4Max])/12 # Apply sliding fee scale formula
      
      
      # Apply asset test
      subset<-temp_2011$totalassets > temp_2011$AssetTest
      temp_2011$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2011$totcopay<-temp_2011$FTcopay*12
      
      # Set copay to zero if no children
      temp_2011$totcopay[temp_2011$numkidsincare0to4+temp_2011$numkidsincare5to12==0]<-0
      
      temp_2011$totcopay[is.na(temp_2011$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2011$childcare.overage[temp_2011$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2011]<-temp_2011$childcare.overage
      temp$totcopay[temp$ruleYear==2011]<-temp_2011$totcopay
      
    }
    
    #######################################
    # APPLY 2012 RULES
    if(2012 %in% unique(temp$ruleYear)){
      
      temp_2012<-temp[temp$ruleYear==2012,]
      
      # Adjust for the income disregard
      temp_2012$income<-temp_2012$income-12*temp_2012$IncomeDisregard
      
      temp_2012$FTcopay<-NA
      
      temp_2012$FTcopay[temp_2012$income>=0 & temp_2012$income<=temp_2012$Bin1Max]<-temp_2012$CopayBin1[temp_2012$income>=0 & temp_2012$income<=temp_2012$Bin1Max]
      temp_2012$FTcopay[temp_2012$income>temp_2012$Bin1Max & temp_2012$income<=temp_2012$Bin2Max]<-temp_2012$CopayBin2[temp_2012$income>temp_2012$Bin1Max & temp_2012$income<=temp_2012$Bin2Max]
      temp_2012$FTcopay[temp_2012$income>temp_2012$Bin2Max & temp_2012$income<=temp_2012$Bin4Max]<-temp_2012$CopayBin2[temp_2012$income>temp_2012$Bin2Max & temp_2012$income<=temp_2012$Bin4Max]+0.5*(temp_2012$income[temp_2012$income>temp_2012$Bin2Max & temp_2012$income<=temp_2012$Bin4Max]-temp_2012$Bin2Max[temp_2012$income>temp_2012$Bin2Max & temp_2012$income<=temp_2012$Bin4Max])/12 # Apply sliding fee scale formula
      
      
      # Apply asset test
      subset<-temp_2012$totalassets > temp_2012$AssetTest
      temp_2012$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2012$totcopay<-temp_2012$FTcopay*12
      
      # Set copay to zero if no children
      temp_2012$totcopay[temp_2012$numkidsincare0to4+temp_2012$numkidsincare5to12==0]<-0
      
      temp_2012$totcopay[is.na(temp_2012$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2012$childcare.overage[temp_2012$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2012]<-temp_2012$childcare.overage
      temp$totcopay[temp$ruleYear==2012]<-temp_2012$totcopay
      
    }
    
    #######################################
    # APPLY 2013 RULES
    if(2013 %in% unique(temp$ruleYear)){
      
      temp_2013<-temp[temp$ruleYear==2013,]
      
      # Adjust for the income disregard
      temp_2013$income<-temp_2013$income-12*temp_2013$IncomeDisregard
      
      temp_2013$FTcopay<-NA
      
      temp_2013$FTcopay[temp_2013$income>=0 & temp_2013$income<=temp_2013$Bin1Max]<-temp_2013$CopayBin1[temp_2013$income>=0 & temp_2013$income<=temp_2013$Bin1Max]
      temp_2013$FTcopay[temp_2013$income>temp_2013$Bin1Max & temp_2013$income<=temp_2013$Bin2Max]<-temp_2013$CopayBin2[temp_2013$income>temp_2013$Bin1Max & temp_2013$income<=temp_2013$Bin2Max]
      temp_2013$FTcopay[temp_2013$income>temp_2013$Bin2Max & temp_2013$income<=temp_2013$Bin4Max]<-temp_2013$CopayBin2[temp_2013$income>temp_2013$Bin2Max & temp_2013$income<=temp_2013$Bin4Max]+0.5*(temp_2013$income[temp_2013$income>temp_2013$Bin2Max & temp_2013$income<=temp_2013$Bin4Max]-temp_2013$Bin2Max[temp_2013$income>temp_2013$Bin2Max & temp_2013$income<=temp_2013$Bin4Max])/12 # Apply sliding fee scale formula
      
      # Apply asset test
      subset<-temp_2013$totalassets > temp_2013$AssetTest
      temp_2013$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2013$totcopay<-temp_2013$FTcopay*12
      
      # Set copay to zero if no children
      temp_2013$totcopay[temp_2013$numkidsincare0to4+temp_2013$numkidsincare5to12==0]<-0
      
      temp_2013$totcopay[is.na(temp_2013$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2013$childcare.overage[temp_2013$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2013]<-temp_2013$childcare.overage
      temp$totcopay[temp$ruleYear==2013]<-temp_2013$totcopay
      
    }
    
    #######################################
    # APPLY 2014 RULES
    if(2014 %in% unique(temp$ruleYear)){
      
      temp_2014<-temp[temp$ruleYear==2014,]
      
      # Adjust for the income disregard
      temp_2014$income<-temp_2014$income-12*temp_2014$IncomeDisregard
      
      temp_2014$FTcopay<-NA
      
      temp_2014$FTcopay[temp_2014$income>=0 & temp_2014$income<=temp_2014$Bin1Max]<-temp_2014$CopayBin1[temp_2014$income>=0 & temp_2014$income<=temp_2014$Bin1Max]
      temp_2014$FTcopay[temp_2014$income>temp_2014$Bin1Max & temp_2014$income<=temp_2014$Bin2Max]<-temp_2014$CopayBin2[temp_2014$income>temp_2014$Bin1Max & temp_2014$income<=temp_2014$Bin2Max]
      temp_2014$FTcopay[temp_2014$income>temp_2014$Bin2Max & temp_2014$income<=temp_2014$Bin4Max]<-temp_2014$CopayBin2[temp_2014$income>temp_2014$Bin2Max & temp_2014$income<=temp_2014$Bin4Max]+0.5*(temp_2014$income[temp_2014$income>temp_2014$Bin2Max & temp_2014$income<=temp_2014$Bin4Max]-temp_2014$Bin2Max[temp_2014$income>temp_2014$Bin2Max & temp_2014$income<=temp_2014$Bin4Max])/12 # Apply sliding fee scale formula
      
      
      # Apply asset test
      subset<-temp_2014$totalassets > temp_2014$AssetTest
      temp_2014$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2014$totcopay<-temp_2014$FTcopay*12
      
      # Set copay to zero if no children
      temp_2014$totcopay[temp_2014$numkidsincare0to4+temp_2014$numkidsincare5to12==0]<-0
      
      temp_2014$totcopay[is.na(temp_2014$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2014$childcare.overage[temp_2014$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2014]<-temp_2014$childcare.overage
      temp$totcopay[temp$ruleYear==2014]<-temp_2014$totcopay
      
    }
    
    #######################################
    # APPLY 2015 RULES
    if(2015 %in% unique(temp$ruleYear)){
      
      temp_2015<-temp[temp$ruleYear==2015,]
      
      # Adjust for the income disregard
      temp_2015$income<-temp_2015$income-12*temp_2015$IncomeDisregard
      
      temp_2015$FTcopay<-NA
      
      temp_2015$FTcopay[temp_2015$income>=0 & temp_2015$income<=temp_2015$Bin1Max]<-temp_2015$CopayBin1[temp_2015$income>=0 & temp_2015$income<=temp_2015$Bin1Max]
      temp_2015$FTcopay[temp_2015$income>temp_2015$Bin1Max & temp_2015$income<=temp_2015$Bin2Max]<-temp_2015$CopayBin2[temp_2015$income>temp_2015$Bin1Max & temp_2015$income<=temp_2015$Bin2Max]
      temp_2015$FTcopay[temp_2015$income>temp_2015$Bin2Max & temp_2015$income<=temp_2015$Bin4Max]<-temp_2015$CopayBin2[temp_2015$income>temp_2015$Bin2Max & temp_2015$income<=temp_2015$Bin4Max]+0.5*(temp_2015$income[temp_2015$income>temp_2015$Bin2Max & temp_2015$income<=temp_2015$Bin4Max]-temp_2015$Bin2Max[temp_2015$income>temp_2015$Bin2Max & temp_2015$income<=temp_2015$Bin4Max])/12 # Apply sliding fee scale formula
      
      # Apply asset test
      subset<-temp_2015$totalassets > temp_2015$AssetTest
      temp_2015$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2015$totcopay<-temp_2015$FTcopay*12
      
      # Set copay to zero if no children
      temp_2015$totcopay[temp_2015$numkidsincare0to4+temp_2015$numkidsincare5to12==0]<-0
      
      temp_2015$totcopay[is.na(temp_2015$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2015$childcare.overage[temp_2015$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2015]<-temp_2015$childcare.overage
      temp$totcopay[temp$ruleYear==2015]<-temp_2015$totcopay
      
    }
    
    #######################################
    # APPLY 2016 RULES
    if(2016 %in% unique(temp$ruleYear)){
      
      temp_2016<-temp[temp$ruleYear==2016,]
      
      # Adjust for the income disregard
      temp_2016$income<-temp_2016$income-12*temp_2016$IncomeDisregard
      
      temp_2016$FTcopay<-NA
      
      temp_2016$FTcopay[temp_2016$income>=0 & temp_2016$income<=temp_2016$Bin1Max]<-temp_2016$CopayBin1[temp_2016$income>=0 & temp_2016$income<=temp_2016$Bin1Max]
      temp_2016$FTcopay[temp_2016$income>temp_2016$Bin1Max & temp_2016$income<=temp_2016$Bin2Max]<-temp_2016$CopayBin2[temp_2016$income>temp_2016$Bin1Max & temp_2016$income<=temp_2016$Bin2Max]
      temp_2016$FTcopay[temp_2016$income>temp_2016$Bin2Max & temp_2016$income<=temp_2016$Bin4Max]<-temp_2016$CopayBin2[temp_2016$income>temp_2016$Bin2Max & temp_2016$income<=temp_2016$Bin4Max]+0.5*(temp_2016$income[temp_2016$income>temp_2016$Bin2Max & temp_2016$income<=temp_2016$Bin4Max]-temp_2016$Bin2Max[temp_2016$income>temp_2016$Bin2Max & temp_2016$income<=temp_2016$Bin4Max])/12 # Apply sliding fee scale formula
      
      
      # Apply asset test
      subset<-temp_2016$totalassets > temp_2016$AssetTest
      temp_2016$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2016$totcopay<-temp_2016$FTcopay*12
      
      # Set copay to zero if no children
      temp_2016$totcopay[temp_2016$numkidsincare0to4+temp_2016$numkidsincare5to12==0]<-0
      
      temp_2016$totcopay[is.na(temp_2016$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2016$childcare.overage[temp_2016$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2016]<-temp_2016$childcare.overage
      temp$totcopay[temp$ruleYear==2016]<-temp_2016$totcopay
      
    }
    
    #######################################
    # APPLY 2017 RULES
    if(2017 %in% unique(temp$ruleYear)){
      
      temp_2017<-temp[temp$ruleYear==2017,]
      
      # Adjust for the income disregard
      temp_2017$income<-temp_2017$income-12*temp_2017$IncomeDisregard
      
      temp_2017$FTcopay<-NA
      
      temp_2017$FTcopay[temp_2017$income>=0 & temp_2017$income<=temp_2017$Bin1Max]<-temp_2017$CopayBin1[temp_2017$income>=0 & temp_2017$income<=temp_2017$Bin1Max]
      temp_2017$FTcopay[temp_2017$income>temp_2017$Bin1Max & temp_2017$income<=temp_2017$Bin2Max]<-temp_2017$CopayBin2[temp_2017$income>temp_2017$Bin1Max & temp_2017$income<=temp_2017$Bin2Max]
      temp_2017$FTcopay[temp_2017$income>temp_2017$Bin2Max & temp_2017$income<=temp_2017$Bin4Max]<-temp_2017$CopayBin2[temp_2017$income>temp_2017$Bin2Max & temp_2017$income<=temp_2017$Bin4Max]+0.5*(temp_2017$income[temp_2017$income>temp_2017$Bin2Max & temp_2017$income<=temp_2017$Bin4Max]-temp_2017$Bin2Max[temp_2017$income>temp_2017$Bin2Max & temp_2017$income<=temp_2017$Bin4Max])/12 # Apply sliding fee scale formula
      
      
      # Apply asset test
      subset<-temp_2017$totalassets > temp_2017$AssetTest
      temp_2017$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2017$totcopay<-temp_2017$FTcopay*12
      
      # Set copay to zero if no children
      temp_2017$totcopay[temp_2017$numkidsincare0to4+temp_2017$numkidsincare5to12==0]<-0
      
      temp_2017$totcopay[is.na(temp_2017$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2017$childcare.overage[temp_2017$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2017]<-temp_2017$childcare.overage
      temp$totcopay[temp$ruleYear==2017]<-temp_2017$totcopay
      
    }
    
    #######################################
    # APPLY 2018 RULES
    if(2018 %in% unique(temp$ruleYear)){
      
      temp_2018<-temp[temp$ruleYear==2018,]
      
      # Adjust for the income disregard
      temp_2018$income<-temp_2018$income-12*temp_2018$IncomeDisregard
      
      temp_2018$FTcopay<-NA
      
      temp_2018$FTcopay[temp_2018$income>=0 & temp_2018$income<=temp_2018$Bin1Max]<-temp_2018$CopayBin1[temp_2018$income>=0 & temp_2018$income<=temp_2018$Bin1Max]
      temp_2018$FTcopay[temp_2018$income>temp_2018$Bin1Max & temp_2018$income<=temp_2018$Bin2Max]<-temp_2018$CopayBin2[temp_2018$income>temp_2018$Bin1Max & temp_2018$income<=temp_2018$Bin2Max]
      temp_2018$FTcopay[temp_2018$income>temp_2018$Bin2Max & temp_2018$income<=temp_2018$Bin4Max]<-temp_2018$CopayBin2[temp_2018$income>temp_2018$Bin2Max & temp_2018$income<=temp_2018$Bin4Max]+0.5*(temp_2018$income[temp_2018$income>temp_2018$Bin2Max & temp_2018$income<=temp_2018$Bin4Max]-temp_2018$Bin2Max[temp_2018$income>temp_2018$Bin2Max & temp_2018$income<=temp_2018$Bin4Max])/12 # Apply sliding fee scale formula
      
      
      # Apply asset test
      subset<-temp_2018$totalassets > temp_2018$AssetTest
      temp_2018$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2018$totcopay<-temp_2018$FTcopay*12
      
      # Set copay to zero if no children
      temp_2018$totcopay[temp_2018$numkidsincare0to4+temp_2018$numkidsincare5to12==0]<-0
      
      temp_2018$totcopay[is.na(temp_2018$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2018$childcare.overage[temp_2018$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2018]<-temp_2018$childcare.overage
      temp$totcopay[temp$ruleYear==2018]<-temp_2018$totcopay
      
    }
    
    #######################################
    # APPLY 2019 RULES
    if(2019 %in% unique(temp$ruleYear)){
      
      temp_2019<-temp[temp$ruleYear==2019,]
      
      # Adjust for the income disregard
      temp_2019$income<-temp_2019$income-12*temp_2019$IncomeDisregard
      
      temp_2019$FTcopay<-NA
      
      temp_2019$FTcopay[temp_2019$income>=0 & temp_2019$income<=temp_2019$Bin1Max]<-temp_2019$CopayBin1[temp_2019$income>=0 & temp_2019$income<=temp_2019$Bin1Max]
      temp_2019$FTcopay[temp_2019$income>temp_2019$Bin1Max & temp_2019$income<=temp_2019$Bin2Max]<-temp_2019$CopayBin2[temp_2019$income>temp_2019$Bin1Max & temp_2019$income<=temp_2019$Bin2Max]
      temp_2019$FTcopay[temp_2019$income>temp_2019$Bin2Max & temp_2019$income<=temp_2019$Bin4Max]<-temp_2019$CopayBin2[temp_2019$income>temp_2019$Bin2Max & temp_2019$income<=temp_2019$Bin4Max]+0.5*(temp_2019$income[temp_2019$income>temp_2019$Bin2Max & temp_2019$income<=temp_2019$Bin4Max]-temp_2019$Bin2Max[temp_2019$income>temp_2019$Bin2Max & temp_2019$income<=temp_2019$Bin4Max])/12 # Apply sliding fee scale formula
      
      # Apply asset test
      subset<-temp_2019$totalassets > temp_2019$AssetTest
      temp_2019$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2019$totcopay<-temp_2019$FTcopay*12
      
      # Set copay to zero if no children
      temp_2019$totcopay[temp_2019$numkidsincare0to4+temp_2019$numkidsincare5to12==0]<-0
      
      temp_2019$totcopay[is.na(temp_2019$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2019$childcare.overage[temp_2019$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2019]<-temp_2019$childcare.overage
      temp$totcopay[temp$ruleYear==2019]<-temp_2019$totcopay
      
    }
    
    #######################################
    # APPLY 2020 RULES
    if(2020 %in% unique(temp$ruleYear)){
      
      temp_2020<-temp[temp$ruleYear==2020,]
      
      # Adjust for the income disregard
      temp_2020$income<-temp_2020$income-12*temp_2020$IncomeDisregard
      
      temp_2020$FTcopay<-NA
      
      temp_2020$FTcopay[temp_2020$income>=0 & temp_2020$income<=temp_2020$Bin1Max]<-temp_2020$CopayBin1[temp_2020$income>=0 & temp_2020$income<=temp_2020$Bin1Max]
      temp_2020$FTcopay[temp_2020$income>temp_2020$Bin1Max & temp_2020$income<=temp_2020$Bin2Max]<-temp_2020$CopayBin2[temp_2020$income>temp_2020$Bin1Max & temp_2020$income<=temp_2020$Bin2Max]
      temp_2020$FTcopay[temp_2020$income>temp_2020$Bin2Max & temp_2020$income<=temp_2020$Bin4Max]<-temp_2020$CopayBin2[temp_2020$income>temp_2020$Bin2Max & temp_2020$income<=temp_2020$Bin4Max]+0.5*(temp_2020$income[temp_2020$income>temp_2020$Bin2Max & temp_2020$income<=temp_2020$Bin4Max]-temp_2020$Bin2Max[temp_2020$income>temp_2020$Bin2Max & temp_2020$income<=temp_2020$Bin4Max])/12 # Apply sliding fee scale formula
      
      
      # Apply asset test
      subset<-temp_2020$totalassets > temp_2020$AssetTest
      temp_2020$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2020$totcopay<-temp_2020$FTcopay*12
      
      # Set copay to zero if no children
      temp_2020$totcopay[temp_2020$numkidsincare0to4+temp_2020$numkidsincare5to12==0]<-0
      
      temp_2020$totcopay[is.na(temp_2020$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2020$childcare.overage[temp_2020$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2020]<-temp_2020$childcare.overage
      temp$totcopay[temp$ruleYear==2020]<-temp_2020$totcopay
      
    }
    
    #######################################
    # APPLY 2021 RULES
    if(2021 %in% unique(temp$ruleYear)){
      
      temp_2021<-temp[temp$ruleYear==2021,]
      
      # Adjust for the income disregard
      #temp_2021$income<-temp_2021$income-12*temp_2021$IncomeDisregard
      
      temp_2021$FTcopay<-NA
      
      temp_2021$CopayBin1 <- 15
      temp_2021$CopayBin2 <- 65
      temp_2021$CopayBin3 <- 90
      temp_2021$CopayBin4 <- 115
      temp_2021$CopayBin5 <- 215
      
      temp_2021$FTcopay[temp_2021$income>=0 & temp_2021$income<=temp_2021$Bin1Max]<-temp_2021$CopayBin1[temp_2021$income>=0 & temp_2021$income<=temp_2021$Bin1Max]
      temp_2021$FTcopay[temp_2021$income>temp_2021$Bin1Max & temp_2021$income<=temp_2021$Bin2Max]<-temp_2021$CopayBin2[temp_2021$income>temp_2021$Bin1Max & temp_2021$income<=temp_2021$Bin2Max]
      temp_2021$FTcopay[temp_2021$income>temp_2021$Bin2Max & temp_2021$income<=temp_2021$Bin3Max]<-temp_2021$CopayBin3[temp_2021$income>temp_2021$Bin2Max & temp_2021$income<=temp_2021$Bin3Max]
      temp_2021$FTcopay[temp_2021$income>temp_2021$Bin3Max & temp_2021$income<=temp_2021$Bin4Max]<-temp_2021$CopayBin4[temp_2021$income>temp_2021$Bin3Max & temp_2021$income<=temp_2021$Bin4Max]
      temp_2021$FTcopay[temp_2021$income>temp_2021$Bin4Max & temp_2021$income<=temp_2021$Bin5Max]<-temp_2021$CopayBin5[temp_2021$income>temp_2021$Bin4Max & temp_2021$income<=temp_2021$Bin5Max]
      
      temp_2021$FTcopay<-as.numeric(temp_2021$FTcopay)
      
      # Apply asset test
      subset<-temp_2021$totalassets > temp_2021$AssetTest
      temp_2021$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2021$totcopay<-temp_2021$FTcopay*12
      
      # Set copay to zero if no children
      temp_2021$totcopay[temp_2021$numkidsincare0to4+temp_2021$numkidsincare5to12==0]<-0
      
      temp_2021$totcopay[is.na(temp_2021$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2021$childcare.overage[temp_2021$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2021]<-temp_2021$childcare.overage
      temp$totcopay[temp$ruleYear==2021]<-temp_2021$totcopay
      
    }
    
    #######################################
    # APPLY 2022 RULES
    if(2022 %in% unique(temp$ruleYear)){
      
      temp_2022<-temp[temp$ruleYear==2022,]
      
      # Adjust for the income disregard
      temp_2022$income<-temp_2022$income-12*temp_2022$IncomeDisregard
      
      temp_2022$FTcopay<-NA
      
      temp_2022$FTcopay[temp_2022$income>=0 & temp_2022$income<=temp_2022$Bin1Max]<-temp_2022$CopayBin1[temp_2022$income>=0 & temp_2022$income<=temp_2022$Bin1Max]
      temp_2022$FTcopay[temp_2022$income>temp_2022$Bin1Max & temp_2022$income<=temp_2022$Bin2Max]<-temp_2022$CopayBin2[temp_2022$income>temp_2022$Bin1Max & temp_2022$income<=temp_2022$Bin2Max]
      temp_2022$FTcopay[temp_2022$income>temp_2022$Bin2Max & temp_2022$income<=temp_2022$Bin3Max]<-temp_2022$CopayBin3[temp_2022$income>temp_2022$Bin2Max & temp_2022$income<=temp_2022$Bin3Max]
      temp_2022$FTcopay[temp_2022$income>temp_2022$Bin3Max & temp_2022$income<=temp_2022$Bin4Max]<-temp_2022$CopayBin4[temp_2022$income>temp_2022$Bin3Max & temp_2022$income<=temp_2022$Bin4Max]
      temp_2022$FTcopay[temp_2022$income>temp_2022$Bin4Max & temp_2022$income<=temp_2022$Bin5Max]<-temp_2022$CopayBin5[temp_2022$income>temp_2022$Bin4Max & temp_2022$income<=temp_2022$Bin5Max]
      
      temp_2022$FTcopay<-as.numeric(temp_2022$FTcopay)
      
      # Apply asset test
      subset<-temp_2022$totalassets > temp_2022$AssetTest
      temp_2022$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2022$totcopay<-temp_2022$FTcopay*12
      
      # Set copay to zero if no children
      temp_2022$totcopay[temp_2022$numkidsincare0to4+temp_2022$numkidsincare5to12==0]<-0
      
      temp_2022$totcopay[is.na(temp_2022$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2022$childcare.overage[temp_2022$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2022]<-temp_2022$childcare.overage
      temp$totcopay[temp$ruleYear==2022]<-temp_2022$totcopay
      
    }
    
    
    
    if(2023 %in% unique(temp$ruleYear)){
      
      
      temp_2023<-temp[temp$ruleYear==2023,]
      
      # Adjust for the income disregard
      temp_2023$income<-temp_2023$income-12*temp_2023$IncomeDisregard
      
      temp_2023$FTcopay<-NA
      
      temp_2023$FTcopay[temp_2023$income>=0 & temp_2023$income<=temp_2023$Bin1Max]<-temp_2023$CopayBin1[temp_2023$income>=0 & temp_2023$income<=temp_2023$Bin1Max]
      temp_2023$FTcopay[temp_2023$income>temp_2023$Bin1Max & temp_2023$income<=temp_2023$Bin2Max]<-temp_2023$CopayBin2[temp_2023$income>temp_2023$Bin1Max & temp_2023$income<=temp_2023$Bin2Max]
      temp_2023$FTcopay[temp_2023$income>temp_2023$Bin2Max & temp_2023$income<=temp_2023$Bin3Max]<-temp_2023$CopayBin3[temp_2023$income>temp_2023$Bin2Max & temp_2023$income<=temp_2023$Bin3Max]
      temp_2023$FTcopay[temp_2023$income>temp_2023$Bin3Max & temp_2023$income<=temp_2023$Bin4Max]<-temp_2023$CopayBin4[temp_2023$income>temp_2023$Bin3Max & temp_2023$income<=temp_2023$Bin4Max]
      temp_2023$FTcopay[temp_2023$income>temp_2023$Bin4Max & temp_2023$income<=temp_2023$Bin5Max]<-temp_2023$CopayBin5[temp_2023$income>temp_2023$Bin4Max & temp_2023$income<=temp_2023$Bin5Max]
      
      temp_2023$FTcopay<-as.numeric(temp_2023$FTcopay)
      
      # Apply asset test
      subset<-temp_2023$totalassets > temp_2023$AssetTest
      temp_2023$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2023$totcopay<-temp_2023$FTcopay*12
      
      # Set copay to zero if no children
      temp_2023$totcopay[temp_2023$numkidsincare0to4+temp_2023$numkidsincare5to12==0]<-0
      
      temp_2023$totcopay[is.na(temp_2023$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2023$childcare.overage[temp_2023$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2023]<-temp_2023$childcare.overage
      temp$totcopay[temp$ruleYear==2023]<-temp_2023$totcopay
      
      
    }
    
    if(2024 %in% unique(temp$ruleYear)){
      
      temp_2024<-temp[temp$ruleYear==2024,]
      
      # Adjust for the income disregard
      temp_2024$income<-temp_2024$income-12*temp_2024$IncomeDisregard
      
      temp_2024$FTcopay<-NA
      
      temp_2024$FTcopay[temp_2024$income>=0 & temp_2024$income<=temp_2024$Bin1Max]<-temp_2024$CopayBin1[temp_2024$income>=0 & temp_2024$income<=temp_2024$Bin1Max]
      temp_2024$FTcopay[temp_2024$income>temp_2024$Bin1Max & temp_2024$income<=temp_2024$Bin2Max]<-temp_2024$CopayBin2[temp_2024$income>temp_2024$Bin1Max & temp_2024$income<=temp_2024$Bin2Max]
      temp_2024$FTcopay[temp_2024$income>temp_2024$Bin2Max & temp_2024$income<=temp_2024$Bin3Max]<-temp_2024$CopayBin3[temp_2024$income>temp_2024$Bin2Max & temp_2024$income<=temp_2024$Bin3Max]
      temp_2024$FTcopay[temp_2024$income>temp_2024$Bin3Max & temp_2024$income<=temp_2024$Bin4Max]<-temp_2024$CopayBin4[temp_2024$income>temp_2024$Bin3Max & temp_2024$income<=temp_2024$Bin4Max]
      temp_2024$FTcopay[temp_2024$income>temp_2024$Bin4Max & temp_2024$income<=temp_2024$Bin5Max]<-temp_2024$CopayBin5[temp_2024$income>temp_2024$Bin4Max & temp_2024$income<=temp_2024$Bin5Max]
      
      temp_2024$FTcopay<-as.numeric(temp_2024$FTcopay)
      
      # Apply asset test
      subset<-temp_2024$totalassets > temp_2024$AssetTest
      temp_2024$totcopay[subset]<-NA_real_
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      
      # Calculate total copay (12 months needed)
      temp_2024$totcopay<-temp_2024$FTcopay*12
      
      # Set copay to zero if no children
      temp_2024$totcopay[temp_2024$numkidsincare0to4+temp_2024$numkidsincare5to12==0]<-0
      
      temp_2024$totcopay[is.na(temp_2024$FTcopay)]<-NA
      # Note: code produces NAs for a good reason, because family is ineligible for CCDF
      # Copay is NOT zero for ineligible, but there are people who pay 0 copay
      
      # Adjust overage depending on whether states allow to charge it
      temp_2024$childcare.overage[temp_2024$OverageOption=="No"]<-0
      
      temp$childcare.overage[temp$ruleYear==2024]<-temp_2024$childcare.overage
      temp$totcopay[temp$ruleYear==2024]<-temp_2024$totcopay
      
    }
    # Merge back
    data$childcare.overage[data$stateFIPS==53]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==53]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==53]<-temp$InitialEligibility.y
  }
  
  
  # WEST VIRIGINIA ----
  # Description:
  # Copay is a dollar amount per child
  # Daily frequency
  if(54 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_WV$stateFIPS <- 54
    ccdfData_WV$AssetTest <- 1000000
    
    temp<-data[data$stateFIPS==54,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_WV$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_WV$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_WV$stateFIPS), AKorHI=unique(ccdfData_WV$AKorHI), famsize=unique(ccdfData_WV$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_WV[ccdfData_WV$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_WV$stateFIPS), AKorHI=unique(ccdfData_WV$AKorHI), famsize=unique(ccdfData_WV$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_WV, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_WV<-ccdfData_WV %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_WV<-ccdfData_WV %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_WV, by=c("stateFIPS", "AKorHI", "famsize", "ruleYear"))
    
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
    
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    temp$totcopay<-NA
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # Set copay to zero if no children
    temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==54]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==54]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==54]<-temp$InitialEligibility.y
  }
  
  # WYOMING ----
  
  # Description:
  # Copay is a fixed dollar amount per child
  # Daily frequency
  if(56 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    ccdfData_WY$stateFIPS <- 56
    
    temp<-data[data$stateFIPS==56,]
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_WY$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_WY$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_WY$stateFIPS), AKorHI=unique(ccdfData_WY$AKorHI), famsize=unique(ccdfData_WY$famsize), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_WY[ccdfData_WY$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_WY$stateFIPS), AKorHI=unique(ccdfData_WY$AKorHI), famsize=unique(ccdfData_WY$famsize), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_WY, by=c("stateFIPS", "AKorHI", "famsize"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_WY<-ccdfData_WY %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_WY<-ccdfData_WY %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_WY, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize"))
    
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
    temp$FTcopay[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]<-temp$CopayBin6[temp$income>temp$Bin5Max & temp$income<=temp$Bin6Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Determine how much care child need
    # age < 5 requires full-time school time care and full-time summer care
    # age 5 to 12 requires part-time school time care and full-time summer care
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # Set copay to zero if no children
    
    temp$totcopay[is.na(temp$FTcopay)]<-NA
    # Note: code produces NAs for a good reason, because family is ineligible for CCDF
    # Copay is NOT zero for ineligible, but there are people who pay 0 copay
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==56]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==56]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==56]<-temp$InitialEligibility.y
  }
  
  
  
  # WISCONSIN ----
  
  # Description:
  # Copay is a fixed amount per child
  # Copay amount varies by number of children that family has in care
  # Hourly frequency
  # ! Need to adjust definition of countable income
  if(55 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==55,]
    
    temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
    
    # Add most recent benefit rules we have to the current year if we do not have most up-to-date rules
    years<-unique(data$ruleYear) # years in data set
    yearsinexpdata<- unique(ccdfData_WI$ruleYear) # rule years in benefit data
    yearstouse<-match(years, yearsinexpdata) # compares list of years in data set to years in benefit data
    yearstouse<-years[is.na(yearstouse)] # keeps years from data set that are not in benefit data set
    # Create data for the future
    maxyearofdata<-max(ccdfData_WI$ruleYear) # collect latest year of benefit data
    futureYrs<-yearstouse[yearstouse>maxyearofdata] # Keep years from data set that are larger than latest benefit rule year
    if(length(futureYrs)>0){
      # Create data frame with future years
      expand<-expand.grid(stateFIPS=unique(ccdfData_WI$stateFIPS), AKorHI=unique(ccdfData_WI$AKorHI), famsize=unique(ccdfData_WI$famsize), numkidsInCare=unique(ccdfData_WI$numkidsInCare), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-ccdfData_WI[ccdfData_WI$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }
    # Create data for past and gap years (missing data) - not the future
    nonFutureYrs<-yearstouse[yearstouse<maxyearofdata]
    if(length(nonFutureYrs)>0){
      #Create data frame with past years and year for which we are missing benefit data
      expandPastMiss<-expand.grid(stateFIPS=unique(ccdfData_WI$stateFIPS), AKorHI=unique(ccdfData_WI$AKorHI), famsize=unique(ccdfData_WI$famsize), numkidsInCare=unique(ccdfData_WI$numkidsInCare), Year=nonFutureYrs)
      # Merge on benefit data and for each past/missing year assign benefit data that is closest to that year
      expandPastMiss2<-left_join(expandPastMiss, ccdfData_WI, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      expandPastMiss2$yeardiff<-expandPastMiss2$ruleYear-expandPastMiss2$Year
      expandPastMiss2<-expandPastMiss2%>%
        group_by(Year)%>%
        mutate(minyeardiff = min(yeardiff))
      expandPastMiss2<-expandPastMiss2 %>%
        filter(yeardiff==minyeardiff) %>% select(-c(yeardiff, minyeardiff, ruleYear)) %>% rename("ruleYear"=Year)
    }  # Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {ccdfData_WI<-ccdfData_WI %>% rbind(expand)}
    if(length(nonFutureYrs)>0) {ccdfData_WI<-ccdfData_WI %>% rbind(expandPastMiss2)}
    
    #----------------------------------
    # Step 1: Assign copays
    #----------------------------------
    temp<-left_join(temp, ccdfData_WI, by=c("ruleYear", "stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
    
    # Adjust for the income disregard
    temp$income<-temp$income-12*temp$IncomeDisregard
    
    temp$FTcopay<-NA
    
    temp$FTcopay[temp$income>=0 & temp$income<=temp$Bin1Max]<-temp$CopayBin1[temp$income>=0 & temp$income<=temp$Bin1Max]
    temp$FTcopay[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]<-temp$CopayBin2[temp$income>temp$Bin1Max & temp$income<=temp$Bin2Max]
    temp$FTcopay[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]<-temp$CopayBin3[temp$income>temp$Bin2Max & temp$income<=temp$Bin3Max]
    temp$FTcopay[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]<-temp$CopayBin4[temp$income>temp$Bin3Max & temp$income<=temp$Bin4Max]
    temp$FTcopay[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]<-temp$CopayBin5[temp$income>temp$Bin4Max & temp$income<=temp$Bin5Max]
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
    temp$FTcopay[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
    temp$FTcopay[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
    temp$FTcopay[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
    temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
    temp$FTcopay[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]<-temp$CopayBin22[temp$income>temp$Bin21Max & temp$income<=temp$Bin22Max]
    temp$FTcopay[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]<-temp$CopayBin23[temp$income>temp$Bin22Max & temp$income<=temp$Bin23Max]
    temp$FTcopay[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]<-temp$CopayBin24[temp$income>temp$Bin23Max & temp$income<=temp$Bin24Max]
    temp$FTcopay[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]<-temp$CopayBin25[temp$income>temp$Bin24Max & temp$income<=temp$Bin25Max]
    temp$FTcopay[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]<-temp$CopayBin26[temp$income>temp$Bin25Max & temp$income<=temp$Bin26Max]
    temp$FTcopay[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]<-temp$CopayBin27[temp$income>temp$Bin26Max & temp$income<=temp$Bin27Max]
    temp$FTcopay[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]<-temp$CopayBin28[temp$income>temp$Bin27Max & temp$income<=temp$Bin28Max]
    temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin28[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    
    
    # On purpose assign copay from the 28th bin. Adjust later
    #temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin28[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
    
    # Apply asset test
    subset<-temp$totalassets > temp$AssetTest
    temp$totcopay[subset]<-NA_real_
    
    #----------------------------------
    # Step 2: Calculate total copays
    #----------------------------------
    
    # Initialize
    
    temp$totcopay<-NA
    
    #turn hourly copay into daily copay (assume 8 hours)
    temp$FTcopay=8*temp$FTcopay
    
    temp$totcopay<-temp$FTcopay*(temp$daysofcareneeded0to4+temp$daysofcareneeded5to12)
    
    # For the last income bin the AG (total monthly) copay is increasing by $1 for every $3 that the income exceeds IncomeBin 27
    subset<-temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max
    temp$totcopay[subset]<-temp$totcopay[subset]+((1/3)*(temp$income[subset]-temp$Bin28Max[subset])#*12
    )
    
    # Adjust overage depending on whether states allow to charge it
    temp$childcare.overage[temp$OverageOption=="No"]<-0
    
    # Merge back
    data$childcare.overage[data$stateFIPS==55]<-temp$childcare.overage
    data$totcopay[data$stateFIPS==55]<-temp$totcopay
    data$InitialEligibility[data$stateFIPS==55]<-temp$InitialEligibility.y
  }
  
  data$totcopay<-as.numeric(data$totcopay)
  
  data$childcare.overage <- 0
  
  # Calculate value of CCDF as a portion of remaining net expenses covered by the subsidy
  data$value.CCDF<-rowMaxs(cbind(data$netexp.childcare-data$totcopay-data$childcare.overage,0))
  
  data$value.CCDF[is.na(data$value.CCDF)]<-0
  
  data$numkidsunder13=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=12,na.rm=TRUE)
  
  data$value.CCDF[data$numkidsunder13 == 0 | is.na(data$numkidsunder13)] <- 0
  
  #determine initial elig  & use this elig if contelig=FALSE & its the first yr receivng the program
  #NOTE! need to do this by unique id if using a larger dataset w/ multiple ppl
  data<- data %>%
    mutate(incomeeligible_initial_ccdf=case_when(income<InitialEligibility ~1, TRUE~0),
           firstyearofCCDF=min(Year[value.CCDF>0 & incomeeligible_initial_ccdf==1], 9999, na.rm = TRUE), #min function cant handle missing values, so set to 9999 if person is never eligible for head start
           incomeeligible_CCDF=case_when(`contelig.ccdf`==TRUE ~1 , Year>firstyearofCCDF ~1, `contelig.ccdf`==FALSE & firstyearofCCDF!=9999 & Year<=firstyearofCCDF ~ incomeeligible_initial_ccdf, TRUE~0),
           value.CCDF=value.CCDF*incomeeligible_CCDF)
  
  }  
  return(data$value.CCDF)
 
} #end function.CCDF


