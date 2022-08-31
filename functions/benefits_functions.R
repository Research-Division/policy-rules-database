 #-------------------------------------------------------------
# FUNCTIONS TO CALCULATE VALUE OF BENEFITS PROGRAMS----
#-------------------------------------------------------------


# PUBLIC BENEFITS----

## Temporary Assistance for Needy Families (TANF)----

function.tanfBenefit<-function(data){
  
  data$tanfValue<-0

    # Alabama----
    
    if(1 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==1,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$income=temp$income+temp$income.gift # add gift income 
      temp$net.income<-rowMaxs(cbind(temp$income-12*temp$EarnedIncomeDisregard,0)) # earned income deduction
      #temp$net.income<-rowMaxs(cbind(temp$net.income-0.2*temp$netexp.childcare,0)) # childcare deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      data$tanfValue[data$stateFIPS==1]<-temp$tanfValue
      
      
    }
    
    # California----
   
    if(6 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==6,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      
      temp$income=temp$income+temp$income.gift # add gift income
      
      temp$numworkingadults <- 1
      
      #  for(i in 1:length(temp$income)){
      #    if(temp$income[i] > 0 & temp$income.otherfamily[i] == 0){
      #      temp$numworkingadults[i] <- 1
      #    }else if(temp$income[i] > 0 & temp$income.otherfamily[i] > 0){
      #      temp$numworkingadults[i] <- 2
      #    }
      #  }
      
      
      temp$net.income<-rowMaxs(cbind(temp$income-((90*temp$numworkingadults*12)+(12*550)+temp$EarnedIncomeDisregard*(temp$income-(90*temp$numworkingadults*12)-(12*550))),0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income-(90*12)>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      
      # Merge back
      data$tanfValue[data$stateFIPS==6]<-temp$tanfValue
      
      
    }
    
   
    # Colorado----
    
    if(8 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==8,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      
      temp$income=temp$income+temp$income.gift # add gift income
      
      temp$net.income<-rowMaxs(cbind(temp$income-temp$EarnedIncomeDisregard*temp$income,0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      # Merge back
      data$tanfValue[data$stateFIPS==8]<-temp$tanfValue
      
    }
    
    
    # Connecticut----
    
    if(9 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==9,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$net.income<-rowMaxs(cbind(temp$income-12*temp$EarnedIncomeDisregard,0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      # 
      # Merge back
      data$tanfValue[data$stateFIPS==9]<-temp$tanfValue
      
    }
    
    
    # District of Columbia ----
    
    if(11 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==11,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      
      temp$income=temp$income+temp$income.gift # add gift income
      
      # Apply TANF child care income deduction (175 for kids in childcare + $25 for <2)
      temp$numkidsunder13=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=12,na.rm=TRUE)
      temp$numkidsunder2=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=1,na.rm=TRUE)
      
      temp$childcareDeduction<-temp$numkidsunder13*175+temp$numkidsunder2*25
      
      temp$net.income<-rowMaxs(cbind(temp$income-(12*160+temp$EarnedIncomeDisregard*(temp$income-12*160)),0)) # earned income deduction
      temp$net.income<-rowMaxs(cbind(temp$net.income-12*temp$childcareDeduction,0))
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      # Merge back
      data$tanfValue[data$stateFIPS==11]<-temp$tanfValue
      
    }
    
    
    # Florida ----
    
    if(12 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==12,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      
      temp$income=temp$income+temp$income.gift # add gift income
      
      temp$net.income<-rowMaxs(cbind(temp$income-((90*12)+(12*200)+temp$EarnedIncomeDisregard*(temp$income-(90*12)-(12*200))+(10*12)),0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[(temp$income - (90*12)) > temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      
      # Merge back
      data$tanfValue[data$stateFIPS==12]<-temp$tanfValue
      
      
    }
    
    
    # Kentucky ----
    
    if(21 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==21,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      temp$income=temp$income+temp$income.gift # add gift income
      
      
      # Step I: Calculate net income
      temp$net.income<-rowMaxs(cbind(temp$income-12*temp$EarnedIncomeDisregard,0)) # earned income deduction
      #temp$net.income<-rowMaxs(cbind(temp$net.income-0.2*temp$netexp.childcare,0)) # childcare deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      data$tanfValue[data$stateFIPS==21]<-temp$tanfValue
      
      
    }
    
    
    # Louisiana ----
    
    if(22 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==22,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$net.income<-rowMaxs(cbind(temp$income-12*temp$EarnedIncomeDisregard,0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      # Merge back
      data$tanfValue[data$stateFIPS==22]<-temp$tanfValue
      
    }
    
    # Maine ----
    
    if(23 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==23,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$income=temp$income+temp$income.gift
      temp$net.income<-rowMaxs(cbind(temp$income-(12*108+temp$EarnedIncomeDisregard*(temp$income-12*108)),0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      # Merge back
      data$tanfValue[data$stateFIPS==23]<-temp$tanfValue
      
    }
    

    # Maryland----
    
    if(24 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==24,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$income=temp$income+temp$income.gift
      
      # temp$monthly_hours <- 1
      # temp$monthly_hours[temp$hours > 25] <- 2
      
      temp$net.income<-rowMaxs(cbind(temp$income-(temp$EarnedIncomeDisregard*temp$income)-200*temp$numkids,0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[(0.8*temp$income)>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          abc <- 1
          temp$count_tanf <- 0
          temp$row_tanf <- 0
          temp$count_tanf[temp$tanfValue > 0] <- 1
          for(i in 1:length(temp$tanfValue)){
            temp$row_tanf[i] <- sum(temp$count_tanf[1:i])
          }
          temp$tanfValue[temp$row_tanf > (temp$timeLimit/12)] <- 0
        }else{
          abc <- 0
        }
      }
      
      # Merge back
      data$tanfValue[data$stateFIPS==24]<-temp$tanfValue
      
    }
    
    #
    # Massachusetts----
    
    if(25 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==25,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$income=temp$income+temp$income.gift
      temp$net.income<-rowMaxs(cbind(temp$income-(12*200+temp$EarnedIncomeDisregard*(temp$income-12*200)),0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      #temp$tanfValue[subset]<-12*temp$Value[subset]
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      # Merge back
      data$tanfValue[data$stateFIPS==25]<-temp$tanfValue
      
    }
    
    
    # New York----
    
    if(36 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==36,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$income=temp$income+temp$income.gift
      temp$net.income<-rowMaxs(cbind(temp$income-(12*90+temp$EarnedIncomeDisregard*(temp$income-12*90)),0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      # Merge back
      data$tanfValue[data$stateFIPS==36]<-temp$tanfValue
      
    }
    
    
    # Tennessee ----
    
    if(47 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==47,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$income=temp$income+temp$income.gift
      temp$net.income<-rowMaxs(cbind(temp$income-(temp$EarnedIncomeDisregard*temp$income),0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      # 
      # Merge back
      data$tanfValue[data$stateFIPS==47]<-temp$tanfValue
      
    }
    
    
    # Texas ----
    
    if(48 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==48,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$income=temp$income+temp$income.gift
      
      temp$net.income<-rowMaxs(cbind(
        temp$income-(temp$EarnedIncomeDisregard*temp$income)-(120*12),0)
      ) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[((temp$EarnedIncomeDisregard*temp$income)-(120*12))>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          abc <- 1
          temp$count_tanf <- 0
          temp$row_tanf <- 0
          temp$count_tanf[temp$tanfValue > 0] <- 1
          for(i in 1:length(temp$tanfValue)){
            temp$row_tanf[i] <- sum(temp$count_tanf[1:i])
          }
          temp$tanfValue[temp$row_tanf > (temp$timeLimit/12)] <- 0
        }else{
          abc <- 0
        }
      }
      
      # Merge back
      data$tanfValue[data$stateFIPS==48]<-temp$tanfValue
      
    }
    
    
    # Washington ----
    
    if(53 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==53,]
      
      temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
      
      # Step I: Calculate net income
      temp$income=temp$income+temp$income.gift
      temp$net.income<-rowMaxs(cbind(temp$income-(temp$EarnedIncomeDisregard*temp$income),0)) # earned income deduction
      
      # Step II: Calculate value of the benefit
      temp$tanfValue<-0
      subset<-temp$totalassets<temp$AssetTest
      temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Value[subset] - temp$net.income[subset],0))
      
      # Apply gross income test (if applicable)
      temp$tanfValue[temp$income>temp$grossIncomeTest]<-0
      
      # TANF is not available for two married adults
      temp$tanfValue[temp$FilingStatus==2 & temp$twoAdults_tanf_policy=="No"]<-0
      
      # In some states TANF is not available for childless adults
      temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
      
      if(nrow(data)>1){
        if(data$Year[1] != data$Year[2]){
          
          abc <- 1
          
          temp_1 <- temp[temp$careerpathID == 2,]
          temp_2 <- temp[temp$careerpathID == 1,]
          
          temp_1$count_tanf <- 0
          temp_1$row_tanf <- 0
          temp_1$count_tanf[temp_1$tanfValue > 0] <- 1
          for(i in 1:length(temp_1$tanfValue)){
            temp_1$row_tanf[i] <- sum(temp_1$count_tanf[1:i])
          }
          temp_1$tanfValue[temp_1$row_tanf > ceiling(temp_1$timeLimit/12)] <- 0
          temp_1$tanfValue[2] <- 0.75*temp_1$tanfValue[2]
          
          temp_2$count_tanf <- 0
          temp_2$row_tanf <- 0
          temp_2$count_tanf[temp_2$tanfValue > 0] <- 1
          for(i in 1:length(temp_2$tanfValue)){
            temp_2$row_tanf[i] <- sum(temp_2$count_tanf[1:i])
          }
          temp_2$tanfValue[temp_2$row_tanf > ceiling(temp_2$timeLimit/12)] <- 0
          temp_2$tanfValue[2] <- 0.75*temp_2$tanfValue[2]
          
          temp <- rbind(temp_1, temp_2)
          
        }else{
          abc <- 0
        }
      }
      
      # Merge back
      data$tanfValue[data$stateFIPS==53]<-temp$tanfValue
      
    }
    
    
    return(data$tanfValue)
    
  
}



# Social Security Disability Insurance (SSDI)----

function.ssdiBenefit<-function(data){
  
  # generate a separate "married" variable for SSI/SSDI
  data$married<-case_when(data$FilingStatus==2 ~ 1
                          ,TRUE ~ 0)
  
  data<-left_join(data, ssdiData, by=c("married","ruleYear"))
  data$value.ssdi<-0
  data$value.ssdi.mnth<-0
  data$ssdiTotalRecdMnth<-0
  data$ssdi_sga<-data$ssdi_sga*12 # make into a yearly value
  data$ssdi_blind_sga<-data$ssdi_blind_sga*12  # make into a yearly value
  data$disabledkids<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<18 & cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==1, na.rm=TRUE)
  
  #Step 1(pg.23): Determine if the earnings of each household member are high enough to disqualify them from receiving SSDI (after the 
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
  data$rank1<-case_when(data$married==1~rowMaxs(cbind(data$ssdiRecdMnth1,data$ssdiRecdMnth2),na.rm=TRUE)
                        ,data$married==0~data$ssdiRecdMnth1
                        ,TRUE~data$ssdiRecdMnth1) # when both values are the same amount
  
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
                               ,data$married==1 & data$countSSDI==2 & rowSums(cbind(data$withheldAmount1,data$withheldAmount2),na.rm=TRUE)==0 ~ rowSums(cbind(data$numkids),na.rm=TRUE) # withheld == 0, then spouse (IF THERE IS ONE) receives auxiliary benefit
                               ,data$married==1 & data$countSSDI==2 & rowSums(cbind(data$withheldAmount1,data$withheldAmount2),na.rm=TRUE)>0 ~ rowSums(cbind(data$numkids,1),na.rm=TRUE) # witheld>0, then there is a spouse that receives auxiliary benefit
                               ,data$married==1 & data$countSSDI==1 ~ rowSums(cbind(data$numkids,1),na.rm=TRUE)) # Spouse and kids receive aux benefit
  
  # PIA + potential Auxiliary benefits for Person1 (does not check if child was disabled before age of 22)
  data$actualAuxandPIASpouse1<-case_when((data$married==1 & data$ssdiRecdMnth1==0 & data$ssdiRecdMnth2!=0) & (data$agePerson1>=62 | data$checkUnder16==1 | data$disabledkids!=0)~rowMaxs(cbind(data$receivesAux^-1*((0.5*data$ssdiRecdMnth2)+data$ssdiRecdMnth1),0),na.rm=TRUE) # No PIA for person1 only for person2
                                         ,(data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2==0) & (data$agePerson1>=62 | data$checkUnder16==1 | data$disabledkids!=0)~ data$ssdiRecdMnth1 # PIA for person1 no PIA for person2
                                         ,(data$married==1 & data$numkids==0 & (data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2==0 | data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0) ~ data$ssdiRecdMnth1) # No children No Auxiliary; only PIA for person1
                                         ,(data$married==0 & data$ssdiRecdMnth1!=0 ~ data$ssdiRecdMnth1) # Not married No Auxiliary; only PIA
                                         ,(data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0 & data$withheldAmount1!=0) & (data$agePerson2>=62 | data$checkUnder16==1 | data$disabledkids!=0)~rowMaxs(cbind((data$receivesAux^-1*(data$famMaxAmount-data$ssdiRecdMnth2))-data$ssdiRecdMnth1+data$ssdiRecdMnth1,0),na.rm=TRUE) # Person1 has lower PIA than Person2
                                         ,(data$married==1 & data$ssdiRecdMnth1!=0 & data$ssdiRecdMnth2!=0 & data$withheldAmount1==0) & (data$agePerson2>=62 | data$checkUnder16==1 | data$disabledkids!=0)~rowSums(cbind(data$potentialBenSpouse1,data$ssdiRecdMnth1),na.rm=TRUE)# Person1 has higher PIA than Person2
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
  
  #### TEST SSDI OUTPUT ####
  # outputTest<-function(data){
  #   returnData<-data %>%
  #     select(countyortownName, stateAbbrev, income1, income2, agePerson1,agePerson2,agePerson3, agePerson4, agePerson5, agePerson6, agePerson7,
  #            disability1, disability2,disability3, disability4,disability5, disability6, disability7, married,
  #            ssdiRecdMnth1,ssdiRecdMnth2,ssdiRecdMnth3,ssdiRecdMnth4,ssdiRecdMnth5,ssdiRecdMnth6,ssdiRecdMnth7,
  #            rank1,famSSDIbenefit,value.ssdi.mnth,value.ssdi
  #            )
  # 
  #   write.csv(returnData,paste0(getwd(),"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/Output/SSDI_Output.csv"),row.names=FALSE)
  # }
  # 
  # outputTest(data)
  ################
  
  return(data.ssdi)
  
}  



# Supplemental Security Income Program (SSI)----

function.ssiBenefit<-function(data){ 

  # generate a separate "married" variable for SSI/SSDI
  data$married<-case_when(data$FilingStatus==2 ~ 1
                          ,TRUE ~ 0)
  
  data<-left_join(data, ssiData, by=c("married","ruleYear"))
  data<-left_join(data, sspData, by=c("stateName","ruleYear"))
  data$value.ssi<-0
  data$value.ssi.mnth<-0
  
  # Step 1: Calculate number of disabled & non-disabled children
  data$disabledkids<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<18 & cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==1, na.rm=TRUE)
  data$non_disabledkids<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<18 & cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==0, na.rm=TRUE)
  data$disabledAdlts<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=18 & cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==1, na.rm=TRUE)
  
  # Distribute household assets & monthly disability work expense across all adults
  data$gift.distr<-data$income.gift/data$numadults
  data$child_support.distr<-data$income.child_support/data$numadults
  data$investment.distr<-data$income.investment/data$numadults
  data$disab.work.exp.distr<-data$disab.work.exp/data$disabledAdlts
   
  # Step 2a: Calculate annual and monthly total countable earned income for each person in the home.
  data$ann.earned.income1<-data$income1
  data$ann.earned.income2<-ifelse(data$agePerson2>=18, data$income2, 0)
  data$ann.earned.income3<-ifelse(data$agePerson3>=18, data$income3, 0)
  data$ann.earned.income4<-ifelse(data$agePerson4>=18, data$income4, 0)
  data$ann.earned.income5<-ifelse(data$agePerson5>=18, data$income5, 0)
  data$ann.earned.income6<-ifelse(data$agePerson6>=18, data$income6, 0)
  data$ann.earned.income7<-ifelse(data$agePerson7>=18, data$income7, 0)
  data$ann.earned.income8<-ifelse(data$agePerson8>=18, data$income8, 0)
  data$ann.earned.income9<-ifelse(data$agePerson9>=18, data$income9, 0)
  data$ann.earned.income10<-ifelse(data$agePerson10>=18, data$income10, 0)
  data$ann.earned.income11<-ifelse(data$agePerson11>=18, data$income11, 0)
  data$ann.earned.income12<-ifelse(data$agePerson12>=18, data$income12, 0)
  
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
    # annual countable unearned income - SSI counts child support as income for the child so it is not included here, instead in Step 11
  data$ann.unearned.income1<-rowSums(cbind(data$gift.distr, data$investment.distr, data$portionedAuxAdlt1),na.rm=TRUE)
  data$ann.unearned.income2<-rowSums(cbind(data$gift.distr, data$investment.distr, data$portionedAuxAdlt2),na.rm=TRUE)
  data$ann.unearned.income3<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income4<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income5<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income6<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income7<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income8<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income9<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income10<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income11<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  data$ann.unearned.income12<-rowSums(cbind(data$gift.distr, data$investment.distr),na.rm=TRUE)
  
  # children don't earn any income
  if(is.na(data$agePerson2)||data$agePerson2<18){
    data$ann.unearned.income2<-0
  }
  if(is.na(data$agePerson3)||data$agePerson3<18){
    data$ann.unearned.income3<-0
  }
  if(is.na(data$agePerson4)||data$agePerson4<18){
    data$ann.unearned.income4<-0
  }
  if(is.na(data$agePerson5)||data$agePerson5<18){
    data$ann.unearned.income5<-0
  }
  if(is.na(data$agePerson6)||data$agePerson6<18){
    data$ann.unearned.income6<-0
  }
  if(is.na(data$agePerson7)||data$agePerson7<18){
    data$ann.unearned.income7<-0
  }
  if(is.na(data$agePerson8)||data$agePerson8<18){
    data$ann.unearned.income8<-0
  }
  if(is.na(data$agePerson9)||data$agePerson9<18){
    data$ann.unearned.income9<-0
  }
  if(is.na(data$agePerson10)||data$agePerson10<18){
    data$ann.unearned.income10<-0
  }
  if(is.na(data$agePerson11)||data$agePerson11<18){
    data$ann.unearned.income11<-0
  }
  if(is.na(data$agePerson12)||data$agePerson12<18){
    data$ann.unearned.income12<-0
  }
  
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
  data$income.disregard.remain<-0 # difference between eligible.adlt.unearned.income & income disregard ($20)
  
  subset3<-data$married==1 & (data$disability1==1 & !is.na(data$disability1)) & (data$disability2==0 & !is.na(data$disability2)) # Currently don't have situation where disability1==0 & disability2==1
  
  data$deemed.nondisabled.child.allocation<-rowMaxs(cbind(data$non_disabledkids*data$nondisabled_child_allocation,0),na.rm=TRUE)
  
  data$eligible.adlt.earnings<-data$month.earned.income1
  data$ineligible.adlt.earnings<-data$month.earned.income2
  data$gross.ineligible.adlt.unearned.income<-data$month.unearned.income2
  data$eligible.adlt.unearned.income<-rowMaxs(cbind(data$month.unearned.income1,0),na.rm=TRUE) # Monthly unearned income - monthly interest (we don't have monthly interest)
  data$income.disregard.remain<-rowMaxs(cbind(data$eligible.adlt.unearned.income-data$income_disregard,0),na.rm=TRUE)
  
  data$ineligible.adlt.unearned.income[subset3]<-rowMaxs(cbind(data$gross.ineligible.adlt.unearned.income[subset3]-data$deemed.nondisabled.child.allocation[subset3],0),na.rm=TRUE)
  data$remaining.deemable.income[subset3]<-rowMaxs(cbind(data$deemed.nondisabled.child.allocation[subset3]-data$gross.ineligible.adlt.unearned.income[subset3],0),na.rm=TRUE)
  data$ineligible.adlt.earned.income[subset3]<-rowMaxs(cbind(data$ineligible.adlt.earnings[subset3]-data$remaining.deemable.income[subset3],0),na.rm=TRUE)
  
  subset4<-data$ineligible.adlt.earned.income+data$ineligible.adlt.unearned.income<=data$fbr_difference & data$married==1 & (data$disability1==1 & !is.na(data$disability1)) & (data$disability2==0 & !is.na(data$disability2))
  data$ssi.income[subset4]<-rowMaxs(cbind(rowMaxs(cbind(data$eligible.adlt.unearned.income[subset4]-data$income_disregard[subset4],0),na.rm=TRUE)+rowMaxs(cbind(data$earnings_disregard_pct[subset4],0),na.rm=TRUE)*rowMaxs(cbind(data$eligible.adlt.earnings[subset4]-data$income.disregard.remain[subset4]-data$earnings_disregard_amt[subset4]-data$disab.work.exp[subset4],0),na.rm=TRUE),0),na.rm=TRUE)
  data$value.ssi.mnth[subset4]<-rowMaxs(cbind(data$fbr_individual[subset4]+data$ssp_spouse_as_fbr_individual[subset4]-data$ssi.income[subset4],0),na.rm=TRUE)
  
  subset5<-data$ineligible.adlt.earned.income+data$ineligible.adlt.unearned.income>data$fbr_difference & data$married==1 & (data$disability1==1 & !is.na(data$disability1)) & (data$disability2==0 & !is.na(data$disability2))
  data$included.unearned.income[subset5]<-rowMaxs(cbind(data$ineligible.adlt.unearned.income[subset5]+data$eligible.adlt.unearned.income[subset5]-data$income_disregard[subset5],0),na.rm=TRUE) # all unearned income is currently 0 (no investments,gifts,child support)
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
  data$eligible.adlt.unearned.income<-rowMaxs(cbind(data$month.unearned.income2,0),na.rm=TRUE) # Monthly unearned income - monthly interest (we don't have monthly interest)
  data$income.disregard.remain<-rowMaxs(cbind(data$eligible.adlt.unearned.income-data$income_disregard,0),na.rm=TRUE)
  
  data$ineligible.adlt.unearned.income[subset3.1]<-rowMaxs(cbind(data$gross.ineligible.adlt.unearned.income[subset3.1]-data$deemed.nondisabled.child.allocation[subset3.1],0),na.rm=TRUE)
  data$remaining.deemable.income[subset3.1]<-rowMaxs(cbind(data$deemed.nondisabled.child.allocation[subset3.1]-data$gross.ineligible.adlt.unearned.income[subset3.1],0),na.rm=TRUE)
  data$ineligible.adlt.earned.income[subset3.1]<-rowMaxs(cbind(data$ineligible.adlt.earnings[subset3.1]-data$remaining.deemable.income[subset3.1],0),na.rm=TRUE)
  
  subset4<-data$ineligible.adlt.earned.income+data$ineligible.adlt.unearned.income<=data$fbr_difference & data$married==1 & (data$disability1==0 & !is.na(data$disability1)) & (data$disability2==1 & !is.na(data$disability2))
  data$ssi.income[subset4]<-rowMaxs(cbind(rowMaxs(cbind(data$eligible.adlt.unearned.income[subset4]-data$income_disregard[subset4],0),na.rm=TRUE)+rowMaxs(cbind(data$earnings_disregard_pct[subset4],0),na.rm=TRUE)*rowMaxs(cbind(data$eligible.adlt.earnings[subset4]-data$income.disregard.remain[subset4]-data$earnings_disregard_amt[subset4]-data$disab.work.exp[subset4],0),na.rm=TRUE),0),na.rm=TRUE)
  data$value.ssi.mnth[subset4]<-rowMaxs(cbind(data$fbr_individual[subset4]+data$ssp_spouse_as_fbr_individual[subset4]-data$ssi.income[subset4],0),na.rm=TRUE)
  
  subset5<-data$ineligible.adlt.earned.income+data$ineligible.adlt.unearned.income>data$fbr_difference & data$married==1 & (data$disability1==0 & !is.na(data$disability1)) & (data$disability2==1 & !is.na(data$disability2))
  data$included.unearned.income[subset5]<-rowMaxs(cbind(data$ineligible.adlt.unearned.income[subset5]+data$eligible.adlt.unearned.income[subset5]-data$income_disregard[subset5],0),na.rm=TRUE) # all unearned income is currently 0 (no investments,gifts,child support)
  data$remain.disregard[subset5]<-rowMaxs(cbind(data$income_disregard[subset5]-data$total.unearned.income[subset5],0),na.rm=TRUE)
  data$included.earned.income[subset5]<-(1-data$earnings_disregard_pct[subset5])*rowMaxs(cbind(data$ineligible.adlt.earned.income[subset5]+data$eligible.adlt.earnings[subset5]-data$earnings_disregard_amt[subset5]-data$remain.disregard[subset5],0),na.rm=TRUE)
  data$ssi.income[subset5]<-rowMaxs(cbind(data$included.unearned.income[subset5]+data$included.earned.income[subset5]-data$disab.work.exp[subset5],0),na.rm=TRUE)
  data$value.ssi.mnth[subset5]<-rowMaxs(cbind(data$fbr[subset5]+data$ssp_spouse_in_fbr_couple[subset5]-data$ssi.income[subset5],0),na.rm=TRUE) # SSP couple amount goes here or one line below?
  data$ssi.deemed.remain[subset5]<-rowMaxs(cbind(data$ssi.income[subset5]-data$fbr[subset5]+data$ssp_spouse_in_fbr_couple[subset5],0),na.rm=TRUE)
  
  # Step 7.5: Check for other adults with disability 
    # Calculations when all adults in the household have a disability and no one is married - extension of Step 6A
  subset2.1A<-(data$disability2==1 & !is.na(data$disability2)) & (data$agePerson2>18 & !is.na(data$agePerson2)) & subset1==TRUE
  data$included.unearned.income[subset2.1A]<-rowMaxs(cbind((data$month.unearned.income1[subset2.1A]+data$month.unearned.income2[subset2.1A]-data$income_disregard[subset2.1A]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.1A]<-rowMaxs(cbind((data$income_disregard[subset2.1A]-data$month.unearned.income1[subset2.1A]-data$month.unearned.income2[subset2.1A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.1A]<-(1-data$earnings_disregard_pct[subset2.1A])*rowMaxs(cbind((data$month.earned.income1[subset2.1A]+data$month.earned.income2[subset2.1A]-data$earnings_disregard_amt[subset2.1A]-data$remain.disregard[subset2.1A]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.1A]<-rowMaxs(cbind((data$included.unearned.income[subset2.1A]+data$included.earned.income[subset2.1A]-data$disab.work.exp[subset2.1A]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.1A]<-rowMaxs(cbind((data$fbr[subset2.1A]+data$ssp_individual[subset2.1A]-data$ssi.income_1[subset2.1A]),0),na.rm=TRUE)
    # Add ssi.income and value.ssi.mnth from Step 6A to get total household SSI values
  data$ssi.income[subset2.1A]<-rowSums(cbind(data$ssi.income_1[subset2.1A], data$ssi.income[subset2.1A]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.1A]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.1A], data$value.ssi.mnth[subset2.1A]),na.rm=TRUE)
  
  subset2.2A<-(data$disability3==1 & !is.na(data$disability3)) & (data$agePerson3>18 & !is.na(data$agePerson3)) & subset2.1A==TRUE
  data$included.unearned.income[subset2.2A]<-rowMaxs(cbind((data$month.unearned.income1[subset2.2A]+data$month.unearned.income2[subset2.2A]+data$month.unearned.income3[subset2.2A]-data$income_disregard[subset2.2A]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.2A]<-rowMaxs(cbind((data$income_disregard[subset2.2A]-data$month.unearned.income1[subset2.2A]-data$month.unearned.income2[subset2.2A]-data$month.unearned.income3[subset2.2A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.2A]<-(1-data$earnings_disregard_pct[subset2.2A])*rowMaxs(cbind((data$month.earned.income1[subset2.2A]+data$month.earned.income2[subset2.2A]+data$month.earned.income3[subset2.2A]-data$earnings_disregard_amt[subset2.2A]-data$remain.disregard[subset2.2A]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.2A]<-rowMaxs(cbind((data$included.unearned.income[subset2.2A]+data$included.earned.income[subset2.2A]-data$disab.work.exp[subset2.2A]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.2A]<-rowMaxs(cbind((data$fbr[subset2.2A]+data$ssp_individual[subset2.2A]-data$ssi.income_1[subset2.2A]),0),na.rm=TRUE)
    # Add ssi.income and value.ssi.mnth from Step 6A to get total household SSI values
  data$ssi.income[subset2.2A]<-rowSums(cbind(data$ssi.income_1[subset2.2A], data$ssi.income[subset2.2A]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.2A]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.2A], data$value.ssi.mnth[subset2.2A]),na.rm=TRUE)
  
  subset2.3A<-(data$disability4==1 & !is.na(data$disability4)) & (data$agePerson4>18 & !is.na(data$agePerson4)) & subset2.2A==TRUE 
  data$included.unearned.income[subset2.3A]<-rowMaxs(cbind((data$month.unearned.income1[subset2.3A]+data$month.unearned.income2[subset2.3A]+data$month.unearned.income3[subset2.3A]+data$month.unearned.income4[subset2.3A]-data$income_disregard[subset2.3A]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.3A]<-rowMaxs(cbind((data$income_disregard[subset2.3A]-data$month.unearned.income1[subset2.3A]-data$month.unearned.income2[subset2.3A]-data$month.unearned.income3[subset2.3A]-data$month.unearned.income4[subset2.3A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.3A]<-(1-data$earnings_disregard_pct[subset2.3A])*rowMaxs(cbind((data$month.earned.income1[subset2.3A]+data$month.earned.income2[subset2.3A]+data$month.earned.income3[subset2.3A]+data$month.earned.income4[subset2.3A]-data$earnings_disregard_amt[subset2.3A]-data$remain.disregard[subset2.3A]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.3A]<-rowMaxs(cbind((data$included.unearned.income[subset2.3A]+data$included.earned.income[subset2.3A]-data$disab.work.exp[subset2.3A]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.3A]<-rowMaxs(cbind((data$fbr[subset2.3A]+data$ssp_individual[subset2.3A]-data$ssi.income_1[subset2.3A]),0),na.rm=TRUE)
    # Add ssi.income and value.ssi.mnth from Step 6A to get total household SSI values
  data$ssi.income[subset2.3A]<-rowSums(cbind(data$ssi.income_1[subset2.3A], data$ssi.income[subset2.3A]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.3A]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.3A], data$value.ssi.mnth[subset2.3A]),na.rm=TRUE)
  
  subset2.4A<-(data$disability5==1 & !is.na(data$disability5)) & (data$agePerson5>18 & !is.na(data$agePerson5)) & subset2.3A==TRUE 
  data$included.unearned.income[subset2.4A]<-rowMaxs(cbind((data$month.unearned.income1[subset2.4A]+data$month.unearned.income2[subset2.4A]+data$month.unearned.income3[subset2.4A]+data$month.unearned.income4[subset2.4A]+data$month.unearned.income5[subset2.4A]-data$income_disregard[subset2.4A]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.4A]<-rowMaxs(cbind((data$income_disregard[subset2.4A]-data$month.unearned.income1[subset2.4A]-data$month.unearned.income2[subset2.4A]-data$month.unearned.income3[subset2.4A]-data$month.unearned.income4[subset2.4A]-data$month.unearned.income5[subset2.4A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.4A]<-(1-data$earnings_disregard_pct[subset2.4A])*rowMaxs(cbind((data$month.earned.income1[subset2.4A]+data$month.earned.income2[subset2.4A]+data$month.earned.income3[subset2.4A]+data$month.earned.income4[subset2.4A]+data$month.earned.income5[subset2.4A]-data$earnings_disregard_amt[subset2.4A]-data$remain.disregard[subset2.4A]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.4A]<-rowMaxs(cbind((data$included.unearned.income[subset2.4A]+data$included.earned.income[subset2.4A]-data$disab.work.exp[subset2.4A]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.4A]<-rowMaxs(cbind((data$fbr[subset2.4A]+data$ssp_individual[subset2.4A]-data$ssi.income_1[subset2.4A]),0),na.rm=TRUE)
  # Add ssi.income and value.ssi.mnth from Step 6A to get total household SSI values
  data$ssi.income[subset2.4A]<-rowSums(cbind(data$ssi.income_1[subset2.4A], data$ssi.income[subset2.4A]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.4A]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.4A], data$value.ssi.mnth[subset2.4A]),na.rm=TRUE)
  
  subset2.5A<-(data$disability6==1 & !is.na(data$disability6)) & (data$agePerson6>18 & !is.na(data$agePerson6)) & subset2.4A==TRUE 
  data$included.unearned.income[subset2.5A]<-rowMaxs(cbind((data$month.unearned.income1[subset2.5A]+data$month.unearned.income2[subset2.5A]+data$month.unearned.income3[subset2.5A]+data$month.unearned.income4[subset2.5A]+data$month.unearned.income5[subset2.5A]+data$month.unearned.income6[subset2.5A]-data$income_disregard[subset2.5A]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.5A]<-rowMaxs(cbind((data$income_disregard[subset2.5A]-data$month.unearned.income1[subset2.5A]-data$month.unearned.income2[subset2.5A]-data$month.unearned.income3[subset2.5A]-data$month.unearned.income4[subset2.5A]-data$month.unearned.income5[subset2.5A]-data$month.unearned.income6[subset2.5A]),0),na.rm=TRUE)
  data$included.earned.income[subset2.5A]<-(1-data$earnings_disregard_pct[subset2.5A])*rowMaxs(cbind((data$month.earned.income1[subset2.5A]+data$month.earned.income2[subset2.5A]+data$month.earned.income3[subset2.5A]+data$month.earned.income4[subset2.5A]+data$month.earned.income5[subset2.5A]+data$month.earned.income6[subset2.5A]-data$earnings_disregard_amt[subset2.5A]-data$remain.disregard[subset2.5A]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.5A]<-rowMaxs(cbind((data$included.unearned.income[subset2.5A]+data$included.earned.income[subset2.5A]-data$disab.work.exp[subset2.5A]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.5A]<-rowMaxs(cbind((data$fbr[subset2.5A]+data$ssp_individual[subset2.5A]-data$ssi.income_1[subset2.5A]),0),na.rm=TRUE)
  # Add ssi.income and value.ssi.mnth from Step 6A to get total household SSI values
  data$ssi.income[subset2.5A]<-rowSums(cbind(data$ssi.income_1[subset2.5A], data$ssi.income[subset2.5A]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.5A]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.5A], data$value.ssi.mnth[subset2.5A]),na.rm=TRUE)
  
  
    # Calculations when all adults in the household have a disability and married is true - extension of Step 6B
  subset2.1B<-(data$disability3==1 & !is.na(data$disability3)) & (data$agePerson3>18 & !is.na(data$agePerson3)) & subset2==TRUE
  data$included.unearned.income[subset2.1B]<-rowMaxs(cbind((data$month.unearned.income1[subset2.1B]+data$month.unearned.income2[subset2.1B]+data$month.unearned.income3[subset2.1B]-data$income_disregard[subset2.1B]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.1B]<-rowMaxs(cbind((data$income_disregard[subset2.1B]-data$month.unearned.income1[subset2.1B]-data$month.unearned.income2[subset2.1B]-data$month.unearned.income3[subset2.1B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.1B]<-(1-data$earnings_disregard_pct[subset2.1B])*rowMaxs(cbind((data$month.earned.income1[subset2.1B]+data$month.earned.income2[subset2.1B]+data$month.earned.income3[subset2.1B]-data$earnings_disregard_amt[subset2.1B]-data$remain.disregard[subset2.1B]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.1B]<-rowMaxs(cbind((data$included.unearned.income[subset2.1B]+data$included.earned.income[subset2.1B]-data$disab.work.exp[subset2.1B]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.1B]<-rowMaxs(cbind((data$fbr_individual[subset2.1B]+data$ssp_spouse_as_fbr_individual[subset2.1B]-data$ssi.income_1[subset2.1B]),0),na.rm=TRUE) 
    # Add ssi.income and value.ssi.mnth from Step 6B to get total household SSI values
  data$ssi.income[subset2.1B]<-rowSums(cbind(data$ssi.income_1[subset2.1B], data$ssi.income[subset2.1B]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.1B]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.1B], data$value.ssi.mnth[subset2.1B]),na.rm=TRUE)
  
  subset2.2B<-(data$disability4==1 & !is.na(data$disability4)) & (data$agePerson4>18 & !is.na(data$agePerson4)) & subset2.1B==TRUE
  data$included.unearned.income[subset2.2B]<-rowMaxs(cbind((data$month.unearned.income1[subset2.2B]+data$month.unearned.income2[subset2.2B]+data$month.unearned.income3[subset2.2B]+data$month.unearned.income4[subset2.2B]-data$income_disregard[subset2.2B]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.2B]<-rowMaxs(cbind((data$income_disregard[subset2.2B]-data$month.unearned.income1[subset2.2B]-data$month.unearned.income2[subset2.2B]-data$month.unearned.income3[subset2.2B]-data$month.unearned.income4[subset2.2B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.2B]<-(1-data$earnings_disregard_pct[subset2.2B])*rowMaxs(cbind((data$month.earned.income1[subset2.2B]+data$month.earned.income2[subset2.2B]+data$month.earned.income3[subset2.2B]+data$month.earned.income4[subset2.2B]-data$earnings_disregard_amt[subset2.2B]-data$remain.disregard[subset2.2B]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.2B]<-rowMaxs(cbind((data$included.unearned.income[subset2.2B]+data$included.earned.income[subset2.2B]-data$disab.work.exp[subset2.2B]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.2B]<-rowMaxs(cbind((data$fbr_individual[subset2.2B]+data$ssp_spouse_as_fbr_individual[subset2.2B]-data$ssi.income_1[subset2.2B]),0),na.rm=TRUE) 
    # Add ssi.income and value.ssi.mnth from Step 6B to get total household SSI values
  data$ssi.income[subset2.2B]<-rowSums(cbind(data$ssi.income_1[subset2.2B], data$ssi.income[subset2.2B]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.2B]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.2B], data$value.ssi.mnth[subset2.2B]),na.rm=TRUE)
  
  subset2.3B<-(data$disability5==1 & !is.na(data$disability5)) & (data$agePerson5>18 & !is.na(data$agePerson5)) & subset2.2B==TRUE
  data$included.unearned.income[subset2.3B]<-rowMaxs(cbind((data$month.unearned.income1[subset2.3B]+data$month.unearned.income2[subset2.3B]+data$month.unearned.income3[subset2.3B]+data$month.unearned.income4[subset2.3B]+data$month.unearned.income5[subset2.3B]-data$income_disregard[subset2.3B]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.3B]<-rowMaxs(cbind((data$income_disregard[subset2.3B]-data$month.unearned.income1[subset2.3B]-data$month.unearned.income2[subset2.3B]-data$month.unearned.income3[subset2.3B]-data$month.unearned.income4[subset2.3B]-data$month.unearned.income5[subset2.3B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.3B]<-(1-data$earnings_disregard_pct[subset2.3B])*rowMaxs(cbind((data$month.earned.income1[subset2.3B]+data$month.earned.income2[subset2.3B]+data$month.earned.income3[subset2.3B]+data$month.earned.income4[subset2.3B]+data$month.earned.income5[subset2.3B]-data$earnings_disregard_amt[subset2.3B]-data$remain.disregard[subset2.3B]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.3B]<-rowMaxs(cbind((data$included.unearned.income[subset2.3B]+data$included.earned.income[subset2.3B]-data$disab.work.exp[subset2.3B]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.3B]<-rowMaxs(cbind((data$fbr_individual[subset2.3B]+data$ssp_spouse_as_fbr_individual[subset2.3B]-data$ssi.income_1[subset2.3B]),0),na.rm=TRUE) 
  # Add ssi.income and value.ssi.mnth from Step 6B to get total household SSI values
  data$ssi.income[subset2.3B]<-rowSums(cbind(data$ssi.income_1[subset2.3B], data$ssi.income[subset2.3B]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.3B]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.3B], data$value.ssi.mnth[subset2.3B]),na.rm=TRUE)
  
  subset2.4B<-(data$disability6==1 & !is.na(data$disability6)) & (data$agePerson6>18 & !is.na(data$agePerson6)) & subset2.3B==TRUE
  data$included.unearned.income[subset2.4B]<-rowMaxs(cbind((data$month.unearned.income1[subset2.4B]+data$month.unearned.income2[subset2.4B]+data$month.unearned.income3[subset2.4B]+data$month.unearned.income4[subset2.4B]+data$month.unearned.income5[subset2.4B]+data$month.unearned.income6[subset2.4B]-data$income_disregard[subset2.4B]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.4B]<-rowMaxs(cbind((data$income_disregard[subset2.4B]-data$month.unearned.income1[subset2.4B]-data$month.unearned.income2[subset2.4B]-data$month.unearned.income3[subset2.4B]-data$month.unearned.income4[subset2.4B]-data$month.unearned.income5[subset2.4B]-data$month.unearned.income6[subset2.4B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.4B]<-(1-data$earnings_disregard_pct[subset2.4B])*rowMaxs(cbind((data$month.earned.income1[subset2.4B]+data$month.earned.income2[subset2.4B]+data$month.earned.income3[subset2.4B]+data$month.earned.income4[subset2.4B]+data$month.earned.income5[subset2.4B]+data$month.earned.income6[subset2.4B]-data$earnings_disregard_amt[subset2.4B]-data$remain.disregard[subset2.4B]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.4B]<-rowMaxs(cbind((data$included.unearned.income[subset2.4B]+data$included.earned.income[subset2.4B]-data$disab.work.exp[subset2.4B]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth_1[subset2.4B]<-rowMaxs(cbind((data$fbr_individual[subset2.4B]+data$ssp_spouse_as_fbr_individual[subset2.4B]-data$ssi.income_1[subset2.4B]),0),na.rm=TRUE) 
  # Add ssi.income and value.ssi.mnth from Step 6B to get total household SSI values
  data$ssi.income[subset2.4B]<-rowSums(cbind(data$ssi.income_1[subset2.4B], data$ssi.income[subset2.4B]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.4B]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.4B], data$value.ssi.mnth[subset2.4B]),na.rm=TRUE)
  
    # Calculations for adults 3-5 with disability if there is a married couple where one spouse has a disability and the other doesn't 
    # (different rules per Step 7 compared to where both spouses have a disability)
  subset2.1C<-(data$disability3==1 & !is.na(data$disability3)) & (data$agePerson3>18 & !is.na(data$agePerson3)) & (subset3==TRUE | subset3.1==TRUE)
  data$included.unearned.income[subset2.1C]<-rowMaxs(cbind((data$month.unearned.income3[subset2.1C]-data$income_disregard[subset2.1C]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.1C]<-rowMaxs(cbind((data$income_disregard[subset2.1C]-data$month.unearned.income3[subset2.1C]),0),na.rm=TRUE)
  data$included.earned.income[subset2.1C]<-(1-data$earnings_disregard_pct[subset2.1C])*rowMaxs(cbind((data$month.earned.income3[subset2.1C]-data$earnings_disregard_amt[subset2.1C]-data$remain.disregard[subset2.1C]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.1C]<-rowMaxs(cbind((data$included.unearned.income[subset2.1C]+data$included.earned.income[subset2.1C]-data$disab.work.exp.distr[subset2.1C]),0),na.rm=TRUE) # Distributed Work expense x 1 other adult in home w/disab
  data$value.ssi.mnth_1[subset2.1C]<-rowMaxs(cbind((data$fbr_individual[subset2.1C]+data$ssp_spouse_as_fbr_individual[subset2.1C]-data$ssi.income_1[subset2.1C]),0),na.rm=TRUE)
    # Add ssi.income and value.ssi.mnth from Step 7 to get total household SSI values
  data$ssi.income[subset2.1C]<-rowSums(cbind(data$ssi.income_1[subset2.1C], data$ssi.income[subset2.1C]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.1C]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.1C], data$value.ssi.mnth[subset2.1C]),na.rm=TRUE)
  
  subset2.2C<-(data$disability4==1 & !is.na(data$disability4)) & (data$agePerson4>18 & !is.na(data$agePerson4)) & subset2.1C==TRUE
  data$included.unearned.income[subset2.2C]<-rowMaxs(cbind((data$month.unearned.income3[subset2.2C]+data$month.unearned.income4[subset2.2C]-data$income_disregard[subset2.2C]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.2C]<-rowMaxs(cbind((data$income_disregard[subset2.2C]-data$month.unearned.income3[subset2.2B]-data$month.unearned.income4[subset2.2B]),0),na.rm=TRUE)
  data$included.earned.income[subset2.2C]<-(1-data$earnings_disregard_pct[subset2.2C])*rowMaxs(cbind((data$month.earned.income3[subset2.2C]+data$month.earned.income4[subset2.2C]-data$earnings_disregard_amt[subset2.2C]-data$remain.disregard[subset2.2C]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.2C]<-rowMaxs(cbind((data$included.unearned.income[subset2.2C]+data$included.earned.income[subset2.2C]-(2*data$disab.work.exp.distr[subset2.2C])),0),na.rm=TRUE) # Distributed Work expense x 2 other adults in home w/disab
  data$value.ssi.mnth_1[subset2.2C]<-rowMaxs(cbind((data$fbr_individual[subset2.2C]+data$ssp_spouse_as_fbr_individual[subset2.2C]-data$ssi.income_1[subset2.2C]),0),na.rm=TRUE)
    # Add ssi.income and value.ssi.mnth from Step 7 to get total household SSI values
  data$ssi.income[subset2.2C]<-rowSums(cbind(data$ssi.income_1[subset2.2C], data$ssi.income[subset2.2C]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.2C]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.2C], data$value.ssi.mnth[subset2.2C]),na.rm=TRUE)
  
  subset2.3C<-(data$disability5==1 & !is.na(data$disability5)) & (data$agePerson5>18 & !is.na(data$agePerson5)) & subset2.2C==TRUE 
  data$included.unearned.income[subset2.3C]<-rowMaxs(cbind((data$month.unearned.income3[subset2.3C]+data$month.unearned.income4[subset2.3C]+data$month.unearned.income5[subset2.3C]-data$income_disregard[subset2.3C]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.3C]<-rowMaxs(cbind((data$income_disregard[subset2.3C]-data$month.unearned.income3[subset2.3C]-data$month.unearned.income4[subset2.2B]-data$month.unearned.income5[subset2.3C]),0),na.rm=TRUE)
  data$included.earned.income[subset2.3C]<-(1-data$earnings_disregard_pct[subset2.3C])*rowMaxs(cbind((data$month.earned.income3[subset2.3C]+data$month.earned.income4[subset2.3C]+data$month.earned.income5[subset2.3C]-data$earnings_disregard_amt[subset2.3C]-data$remain.disregard[subset2.3C]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.3C]<-rowMaxs(cbind((data$included.unearned.income[subset2.3C]+data$included.earned.income[subset2.3C]-(3*data$disab.work.exp.distr[subset2.3C])),0),na.rm=TRUE) # Distributed Work expense x 3 other adults in home w/disab
  data$value.ssi.mnth_1[subset2.3C]<-rowMaxs(cbind((data$fbr_individual[subset2.3C]+data$ssp_spouse_as_fbr_individual[subset2.3C]-data$ssi.income_1[subset2.3C]),0),na.rm=TRUE)
    # Add ssi.income and value.ssi.mnth from Step 7 to get total household SSI values
  data$ssi.income[subset2.3C]<-rowSums(cbind(data$ssi.income_1[subset2.3C], data$ssi.income[subset2.3C]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.3C]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.3C], data$value.ssi.mnth[subset2.3C]),na.rm=TRUE)
  
  subset2.4C<-(data$disability6==1 & !is.na(data$disability6)) & (data$agePerson6>18 & !is.na(data$agePerson6)) & subset2.3C==TRUE 
  data$included.unearned.income[subset2.4C]<-rowMaxs(cbind((data$month.unearned.income3[subset2.4C]+data$month.unearned.income4[subset2.4C]+data$month.unearned.income5[subset2.4C]+data$month.unearned.income6[subset2.4C]-data$income_disregard[subset2.4C]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.4C]<-rowMaxs(cbind((data$income_disregard[subset2.4C]-data$month.unearned.income3[subset2.4C]-data$month.unearned.income4[subset2.2B]-data$month.unearned.income5[subset2.4C]-data$month.unearned.income6[subset2.4C]),0),na.rm=TRUE)
  data$included.earned.income[subset2.4C]<-(1-data$earnings_disregard_pct[subset2.4C])*rowMaxs(cbind((data$month.earned.income3[subset2.4C]+data$month.earned.income4[subset2.4C]+data$month.earned.income5[subset2.4C]+data$month.earned.income6[subset2.4C]-data$earnings_disregard_amt[subset2.4C]-data$remain.disregard[subset2.4C]),0),na.rm=TRUE)
  data$ssi.income_1[subset2.4C]<-rowMaxs(cbind((data$included.unearned.income[subset2.4C]+data$included.earned.income[subset2.4C]-(4*data$disab.work.exp.distr[subset2.4C])),0),na.rm=TRUE) # Distributed Work expense x 3 other adults in home w/disab
  data$value.ssi.mnth_1[subset2.4C]<-rowMaxs(cbind((data$fbr_individual[subset2.4C]+data$ssp_spouse_as_fbr_individual[subset2.4C]-data$ssi.income_1[subset2.4C]),0),na.rm=TRUE)
  # Add ssi.income and value.ssi.mnth from Step 7 to get total household SSI values
  data$ssi.income[subset2.4C]<-rowSums(cbind(data$ssi.income_1[subset2.4C], data$ssi.income[subset2.4C]),na.rm=TRUE)
  data$value.ssi.mnth[subset2.4C]<-rowSums(cbind(data$value.ssi.mnth_1[subset2.4C], data$value.ssi.mnth[subset2.4C]),na.rm=TRUE)
  
  # Calculations for adults 3-5 when Persons 1-2 are married but neither have a disability
  subset2.4<-data$married==1 & (data$disability1==0 & !is.na(data$disability1)) & (data$disability2==0 & !is.na(data$disability2)) & (data$disability3==1 & !is.na(data$disability3)) & (data$agePerson3>18 & !is.na(data$agePerson3))
  data$included.unearned.income[subset2.4]<-rowMaxs(cbind((data$month.unearned.income3[subset2.4]-data$income_disregard[subset2.4]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.4]<-rowMaxs(cbind((data$income_disregard[subset2.4]-data$month.unearned.income3[subset2.4]),0),na.rm=TRUE)
  data$included.earned.income[subset2.4]<-(1-data$earnings_disregard_pct[subset2.4])*rowMaxs(cbind((data$month.earned.income3[subset2.4]-data$earnings_disregard_amt[subset2.4]-data$remain.disregard[subset2.4]),0),na.rm=TRUE)
  data$ssi.income[subset2.4]<-rowMaxs(cbind((data$included.unearned.income[subset2.4]+data$included.earned.income[subset2.4]-data$disab.work.exp[subset2.4]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth[subset2.4]<-rowMaxs(cbind((data$fbr_individual[subset2.4]+data$ssp_spouse_as_fbr_individual[subset2.4]-data$ssi.income[subset2.4]),0),na.rm=TRUE)
  
  subset2.5<-(data$disability4==1 & !is.na(data$disability4)) & (data$agePerson4>18 & !is.na(data$agePerson4)) & subset2.4==TRUE
  data$included.unearned.income[subset2.5]<-rowMaxs(cbind((data$month.unearned.income3[subset2.5]+data$month.unearned.income4[subset2.5]-data$income_disregard[subset2.5]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.5]<-rowMaxs(cbind((data$income_disregard[subset2.5]-data$month.unearned.income3[subset2.5]-data$month.unearned.income4[subset2.5]),0),na.rm=TRUE)
  data$included.earned.income[subset2.5]<-(1-data$earnings_disregard_pct[subset2.5])*rowMaxs(cbind((data$month.earned.income3[subset2.5]+data$month.earned.income4[subset2.5]-data$earnings_disregard_amt[subset2.5]-data$remain.disregard[subset2.5]),0),na.rm=TRUE)
  data$ssi.income[subset2.5]<-rowMaxs(cbind((data$included.unearned.income[subset2.5]+data$included.earned.income[subset2.5]-data$disab.work.exp[subset2.5]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth[subset2.5]<-rowMaxs(cbind((data$fbr_individual[subset2.5]+data$ssp_spouse_as_fbr_individual[subset2.5]-data$ssi.income[subset2.5]),0),na.rm=TRUE)
  
  subset2.6<-(data$disability5=1 & !is.na(data$disability5)) & (data$agePerson5>18 & !is.na(data$agePerson5)) & subset2.5==TRUE
  data$included.unearned.income[subset2.6]<-rowMaxs(cbind((data$month.unearned.income3[subset2.6]+data$month.unearned.income4[subset2.6]+data$month.unearned.income4[subset2.6]+data$month.unearned.income5[subset2.6]-data$income_disregard[subset2.6]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.6]<-rowMaxs(cbind((data$income_disregard[subset2.6]-data$month.unearned.income3[subset2.6]-data$month.unearned.income4[subset2.6]-data$month.unearned.income5[subset2.6]),0),na.rm=TRUE)
  data$included.earned.income[subset2.6]<-(1-data$earnings_disregard_pct[subset2.6])*rowMaxs(cbind((data$month.earned.income3[subset2.6]+data$month.earned.income4[subset2.6]+data$month.earned.income5[subset2.6]-data$earnings_disregard_amt[subset2.6]-data$remain.disregard[subset2.6]),0),na.rm=TRUE)
  data$ssi.income[subset2.6]<-rowMaxs(cbind((data$included.unearned.income[subset2.6]+data$included.earned.income[subset2.6]-data$disab.work.exp[subset2.6]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth[subset2.6]<-rowMaxs(cbind((data$fbr_individual[subset2.6]+data$ssp_spouse_as_fbr_individual[subset2.6]-data$ssi.income[subset2.6]),0),na.rm=TRUE)
  
  subset2.7<-(data$disability6=1 & !is.na(data$disability6)) & (data$agePerson6>18 & !is.na(data$agePerson6)) & subset2.6==TRUE
  data$included.unearned.income[subset2.7]<-rowMaxs(cbind((data$month.unearned.income3[subset2.7]+data$month.unearned.income4[subset2.7]+data$month.unearned.income4[subset2.7]+data$month.unearned.income5[subset2.7]+data$month.unearned.income6[subset2.7]-data$income_disregard[subset2.7]),0),na.rm=TRUE) 
  data$remain.disregard[subset2.7]<-rowMaxs(cbind((data$income_disregard[subset2.7]-data$month.unearned.income3[subset2.7]-data$month.unearned.income4[subset2.7]-data$month.unearned.income5[subset2.7]-data$month.unearned.income6[subset2.7]),0),na.rm=TRUE)
  data$included.earned.income[subset2.7]<-(1-data$earnings_disregard_pct[subset2.7])*rowMaxs(cbind((data$month.earned.income3[subset2.7]+data$month.earned.income4[subset2.7]+data$month.earned.income5[subset2.7]+data$month.earned.income6[subset2.7]-data$earnings_disregard_amt[subset2.7]-data$remain.disregard[subset2.7]),0),na.rm=TRUE)
  data$ssi.income[subset2.7]<-rowMaxs(cbind((data$included.unearned.income[subset2.7]+data$included.earned.income[subset2.7]-data$disab.work.exp[subset2.7]),0),na.rm=TRUE) # Entire mnthly disab work exp
  data$value.ssi.mnth[subset2.7]<-rowMaxs(cbind((data$fbr_individual[subset2.7]+data$ssp_spouse_as_fbr_individual[subset2.7]-data$ssi.income[subset2.7]),0),na.rm=TRUE)


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
  
  data$asset.test<-ifelse(data$disabledkids>0 & (data$total.countable.assets-data$asset_limit)/data$disabledkids<data$asset_limit_child, data$asset.test<-1, data$asset.test<-0)
  
  # Step 10: Count number of SSI-eligible parents of eligible child and factor in allocations
  data$num.parents<-ifelse(data$married==1,2,1)
  data$parent1.tanf<-ifelse(data$value.tanf>0, 1, 0) 
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
  
  data$deemable.unearned.income<-rowMaxs(cbind(data$mnthly.unearned.income-(data$nondisabled_child_allocation*data$non_disabledkids),0),na.rm=TRUE)
  data$allocation.remainder<-rowMaxs(cbind((data$nondisabled_child_allocation*data$non_disabledkids)-data$mnthly.unearned.income,0),na.rm = TRUE)
  data$deemable.earned.income<-rowMaxs(cbind(data$mnthly.earned.income-data$allocation.remainder,0),na.rm=TRUE)
  data$deemed.unearned.income<-rowMaxs(cbind(data$deemable.unearned.income-data$income_disregard,0),na.rm=TRUE)
  data$deemed.disregard.remainder<-rowMaxs(cbind(data$income_disregard-data$deemable.unearned.income,0),na.rm=TRUE)
  data$deemed.earned.income<-rowMaxs(cbind(1-data$earnings_disregard_pct,0),na.rm=TRUE)*rowMaxs(cbind(data$deemable.earned.income-data$deemed.disregard.remainder-data$earnings_disregard_amt,0),na.rm=TRUE)
  data$deemed.income<-rowMaxs(cbind(data$deemed.unearned.income+data$deemed.earned.income-data$parental_allocation,0),na.rm=TRUE) # ER 8/15/22: Add in 2/3 child support
  
  # Step 11: deem income to disabled children
  data$deemed.income.perchild<-rowMaxs(cbind((data$deemed.income + ((data$child_support/data$numkids)*.66))/data$disabledkids,0),na.rm=TRUE)
  
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
  # 
  # outputTest(data)
  
  
  return(data.ssi)
  
}



# Supplemental Nutrition Assistance Program (SNAP)----

function.snapBenefit<-function(data){

    data<-left_join(data, snapData, by=c("ruleYear","stateFIPS", "famsize"))
    
    # First, calculate total countable income
    data$income.gross<-rowSums(cbind(data$income,data$income.gift,data$income.child_support,data$income.investment,data$value.tanf,data$value.ssi,data$value.ssdi),na.rm=TRUE) # ER 7/4/22: I added rowSums, I was getting error without it
    #data$income.gross<-data$income+data$income.gift+data$income.child_support+data$income.investment+data$value.tanf+data$value.ssi+data$value.ssdi
    
    # Determine if anyone in the household is elderly (above 60) or has a disability
    data$disabled_count<-rowSums(cbind(data$disability1, data$disability2, data$disability3, data$disability4, data$disability5, data$disability6, data$disability7, data$disability8, data$disability9, data$disability10, data$disability11, data$disability12)==1, na.rm=TRUE)
    data$elderly_count<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>60, na.rm=TRUE)
    data$kid_count<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<19, na.rm=TRUE)
    
    # Step I: Calculate Earned Income Deduction
    data$EarnedIncomeDeduction<-0.2*data$income.gross
    
    # Step II: Calculate adjusted income
    data$adjustedincome<-rowMaxs(cbind(data$income.gross-data$EarnedIncomeDeduction-12*data$StandardDeduction-data$netexp.childcare,0),na.rm=TRUE) #ER 7/4/22: standard deduction and childcare are NULL; I added rowMaxs, I was getting error just without it
    #data$adjustedincome<-data$income.gross-data$EarnedIncomeDeduction-12*data$StandardDeduction-data$netexp.childcare
    
    # Step III: Calculate Utility Deductions
    data$UtiilityDeduction<-0
    subset<-(data$netexp.utilities>0 | data$liheap>0 | data$HeatandEatState=="Yes") & data$HCSUA=="Mandatory"
    data$UtiilityDeduction[subset]<-12*data$HCSUAValue[subset]
    
    subset<-(data$netexp.utilities>0 | data$liheap>0 | data$HeatandEatState=="Yes") & data$HCSUA=="Optional"
    data$UtiilityDeduction[subset]<-rowMaxs(cbind(12*data$HCSUAValue[subset],data$netexp.utilities[subset]))
    
    
    # # Step V: Calculate Medical Expense Deduction (those on SSI,SSDI, and elderly can deduct their medical expenses)
    data$MedicalDeduction.person1<-case_when( (data$value.ssiAdlt1>0 | data$ssdiPIA1>0 | data$agePerson1 >60) ~ data$exp.special.disability.person1
                                             , TRUE ~ 0)
    data$MedicalDeduction.person2<-case_when( (data$value.ssiAdlt2>0 | data$ssdiPIA2>0 | data$agePerson2 >60) ~ data$exp.special.disability.person2
                                              , TRUE ~ 0)
    data$MedicalDeduction.person3<-case_when( (data$value.ssiAdlt3>0 | data$ssdiPIA3>0 | data$agePerson3 >60) ~ data$exp.special.disability.person3
                                              , TRUE ~ 0)
    data$MedicalDeduction.person4<-case_when( (data$value.ssiAdlt4>0 | data$ssdiPIA4>0 | data$agePerson4 >60) ~ data$exp.special.disability.person4
                                              , TRUE ~ 0)
    data$MedicalDeduction.person5<-case_when( (data$value.ssiAdlt5>0 | data$ssdiPIA5>0 | data$agePerson5 >60) ~ data$exp.special.disability.person5
                                              , TRUE ~ 0)
    data$MedicalDeduction.person6<-case_when( (data$value.ssiAdlt6>0 | data$ssdiPIA6>0 | data$agePerson6 >60) ~ data$exp.special.disability.person6
                                              , TRUE ~ 0)
    
    data$MedicalDeduction.person7<-case_when( (data$value.ssiChild1>0 | data$agePerson7 >60) ~ data$exp.special.disability.person7 # child age > 60 is impossible but it doesn't crash
                                              , TRUE ~ 0)
    data$MedicalDeduction.person8<-case_when( (data$value.ssiChild2>0 | data$agePerson8 >60) ~ data$exp.special.disability.person8
                                              , TRUE ~ 0)
    data$MedicalDeduction.person9<-case_when( (data$value.ssiChild3>0 | data$agePerson9 >60) ~ data$exp.special.disability.person9
                                              , TRUE ~ 0)
    data$MedicalDeduction.person10<-case_when( (data$value.ssiChild4>0 | data$agePerson10 >60) ~ data$exp.special.disability.person10
                                              , TRUE ~ 0)
    data$MedicalDeduction.person11<-case_when( (data$value.ssiChild5>0 | data$agePerson11 >60) ~ data$exp.special.disability.person11
                                              , TRUE ~ 0)
    data$MedicalDeduction.person12<-case_when( (data$value.ssiChild6>0 | data$agePerson12 >60) ~ data$exp.special.disability.person12
                                              , TRUE ~ 0)

    data<-data %>%
      mutate(MedicalDeduction = MedicalDeduction.person1+MedicalDeduction.person2+MedicalDeduction.person3+MedicalDeduction.person4+MedicalDeduction.person5+MedicalDeduction.person6+MedicalDeduction.person7+MedicalDeduction.person8+MedicalDeduction.person9+MedicalDeduction.person10+MedicalDeduction.person11+MedicalDeduction.person12)

    data$MedicalDeduction<-rowMaxs(cbind(data$MedicalDeduction-data$MedicalExpenseDeductionFloor*12,0)) # Floor

    #data$MedicalDeduction<-0
    
    # Step IV: Calculate Net Income
    data$netincome<-rowMaxs(cbind(0,data$adjustedincome-rowMins(cbind(data$netexp.rentormortgage + data$UtiilityDeduction + data$MedicalDeduction - 0.5*data$adjustedincome,data$MaxShelterDeduction*12))))
    
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
    not_categ_elig_ssi<-data$ssi_recipients_count<data$famsize # if not everyone in the family receives SSI
    
    not_categ_elig_tanf<-data$value.tanf==0 # if family doesn't receive TANF
    
    # Determine if the family FAILS income tests
    fail_grossIncomeTest<-data$income.gross>data$GrossIncomeEligibility & (not_categ_elig_tanf==TRUE & not_categ_elig_ssi==TRUE)
    #some states waive net income tests
    fail_netIncomeTest_nonelddis<-(data$disabled_count==0 & data$elderly_count==0) & data$netincome>data$NetIncomeEligibility_nonelddis  
    fail_netIncomeTest_Elder_Dis<-(data$disabled_count>0 | data$elderly_count>0) & data$netincome>data$NetIncomeEligibility_Elder_Dis
    
    # Determine if the family FAILS asset tests
    fail_assetTest_regular<-(data$disabled_count==0 & data$elderly_count==0) & data$totalassets>data$AssetTest_nonelddis & (not_categ_elig_tanf==TRUE & not_categ_elig_ssi==TRUE) # regular asset test for families WITHOUT elderly/disabled
    fail_assetTest_Elderly_Disabled_under200FPL<-(data$disabled_count>0 | data$elderly_count>0) & data$income.gross <= 2*data$FPL & data$totalassets>data$AssetTest_Elder_Dis_under200FPL & (not_categ_elig_tanf==TRUE & not_categ_elig_ssi==TRUE) # regular asset test for families WITH elderly/disabled
    fail_assetTest_Elderly_Disabled_over200FPL<-(data$disabled_count>0 | data$elderly_count>0) & data$income.gross > 2*data$FPL & data$totalassets>data$AssetTest_Elder_Dis_over200FPL & (not_categ_elig_tanf==TRUE & not_categ_elig_ssi==TRUE) # special asset test for families WITH elderly/disabled and income > 200% FPL
    
    # Calculate the benefit if all income and asset tests are satisfied
    subset<-fail_grossIncomeTest==FALSE & fail_netIncomeTest_Elder_Dis==FALSE & fail_netIncomeTest_nonelddis==FALSE & fail_assetTest_regular==FALSE & fail_assetTest_Elderly_Disabled_under200FPL==FALSE & fail_assetTest_Elderly_Disabled_over200FPL==FALSE
    
    data$snapValue<-0
    data$snapValue[subset]<-rowMins(cbind(rowMaxs(cbind(12*data$MaxBenefit[subset]-0.3*data$netincome[subset],12*data$MinBenefit[subset])),12*data$MaxBenefit[subset]))
    
    data$snapValue<-round(data$snapValue,0)
    
    return(data$snapValue)
    
  }



# Special Supplemental Nutrition Program for Women, Infanta and Children (WIC)----

function.wicBenefit<-function(data){
  
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
  data$categorically.eligible[data$value.medicaid.child>0]<-1
  
  # Double check with Seth
  #data$categorically.eligible[data$value.ssi>0]<-1
  #data$categorically.eligible[data$value.ssdi>0]<-1
  
  # Step 3: Determine income eligibility
  data$income.eligible<-FALSE
  data$income.eligible[data$categorically.eligible == 1] <-TRUE
  data$income.eligible[data$income.countable < data$IncomeEligibility]<-TRUE
  
  # Step 5: Calculate WIC value
  data$value.WIC<- data$numinfants * data$value.infant + data$numkidsage1to4 * data$value.kidsage1to4 + data$mom*data$value.women 
  data$value.WIC[data$income.eligible==FALSE]<-0
  
  # Step 6: Inflate Costs, transition to annual & round
  data$value.WIC<-12*round(data$value.WIC*1.02^(data$ruleYear-data$yearofData),0)
  
  return(data$value.WIC)
  
}



# Section 8 Housing Choice Voucher----

function.section8Benefit<-function(data){
    
    data<-left_join(data, section8Data, by=c("ruleYear","stateFIPS", "countyortownName", "numadults", "numkids"))
    
    data$income.countable= data$income + data$income.gift + data$value.tanf + data$value.ssi + data$value.ssdi + data$income.child_support
    
    # $400 Flat Deduction for a disabled family  ER: SHOULD WE INCLUDE CHILDREN?
    data$disabilityDeduction<-ifelse(data$disability1==1 | data$disability2==1| data$disability3==1 | data$disability4==1 | data$disability5==1 | data$disability6==1 |
                                       data$ssdiPIA1>0 | data$ssdiPIA2>0 | data$ssdiPIA3>0 | data$ssdiPIA4>0 | data$ssdiPIA5>0 | data$ssdiPIA6>0
                                     ,400, 0)
    # Calculate Medical Expense Deduction (those on SSI,SSDI, and elderly can deduct their medical expenses)
    data$MedicalDeduction.person1<-case_when( (data$value.ssiAdlt1>0 | data$ssdiPIA1>0 | data$agePerson1 >60) ~ data$exp.special.disability.person1
                                              , TRUE ~ 0)
    data$MedicalDeduction.person2<-case_when( (data$value.ssiAdlt2>0 | data$ssdiPIA2>0 | data$agePerson2 >60) ~ data$exp.special.disability.person2
                                              , TRUE ~ 0)
    data$MedicalDeduction.person3<-case_when( (data$value.ssiAdlt3>0 | data$ssdiPIA3>0 | data$agePerson3 >60) ~ data$exp.special.disability.person3
                                              , TRUE ~ 0)
    data$MedicalDeduction.person4<-case_when( (data$value.ssiAdlt4>0 | data$ssdiPIA4>0 | data$agePerson4 >60) ~ data$exp.special.disability.person4
                                              , TRUE ~ 0)
    data$MedicalDeduction.person5<-case_when( (data$value.ssiAdlt5>0 | data$ssdiPIA5>0 | data$agePerson5 >60) ~ data$exp.special.disability.person5
                                              , TRUE ~ 0)
    data$MedicalDeduction.person6<-case_when( (data$value.ssiAdlt6>0 | data$ssdiPIA6>0 | data$agePerson6 >60) ~ data$exp.special.disability.person6
                                              , TRUE ~ 0)
    data$MedicalDeduction.person7<-case_when( (data$value.ssiChild1>0 | data$agePerson7 >60) ~ data$exp.special.disability.person7 # child age > 60 is impossible but it doesn't crash
                                              , TRUE ~ 0)
    data$MedicalDeduction.person8<-case_when( (data$value.ssiChild2>0 | data$agePerson8 >60) ~ data$exp.special.disability.person8
                                              , TRUE ~ 0)
    data$MedicalDeduction.person9<-case_when( (data$value.ssiChild3>0 | data$agePerson9 >60) ~ data$exp.special.disability.person9
                                              , TRUE ~ 0)
    data$MedicalDeduction.person10<-case_when( (data$value.ssiChild4>0 | data$agePerson10 >60) ~ data$exp.special.disability.person10
                                               , TRUE ~ 0)
    data$MedicalDeduction.person11<-case_when( (data$value.ssiChild5>0 | data$agePerson11 >60) ~ data$exp.special.disability.person11
                                               , TRUE ~ 0)
    data$MedicalDeduction.person12<-case_when( (data$value.ssiChild6>0 | data$agePerson12 >60) ~ data$exp.special.disability.person12
                                               , TRUE ~ 0)
    data<-data %>%
      mutate(MedicalDeduction = MedicalDeduction.person1+MedicalDeduction.person2+MedicalDeduction.person3+MedicalDeduction.person4+MedicalDeduction.person5+MedicalDeduction.person6+MedicalDeduction.person7+MedicalDeduction.person8+MedicalDeduction.person9+MedicalDeduction.person10+MedicalDeduction.person11+MedicalDeduction.person12)
    
    # Medical deductions above 3% of gross income
    data$MedicalExp<-rowMaxs(cbind((data$income.countable * .03) - (12*(data$disab.work.exp + data$MedicalDeduction)),0))
    
    # Step I: Calculate Adjusted Income
    data$adjustedincome<-rowMaxs(cbind(data$income.countable - data$numkids*data$DependentDeduction - data$netexp.childcare - data$disabilityDeduction - data$MedicalExp,0)) # HERE - subtract $400/m and subtract medical expense like SNAP (everything above 3% of gross income)
    
    # Step II: Determine Total Tenant Payment
    data$ttp<-rowMaxs(cbind(0.1*data$income.countable,0.3*data$adjustedincome))
    
    # Step III: Determine benefit value (voucher also covers GROSS rent, hence add utilities to rent)
    data$section8value<-0
    data$section8value<-rowMaxs(cbind(rowMins(cbind(data$exp.rentormortgage+data$exp.utilities,data$MaxGrossRent))-data$ttp,0))
    
    data$section8value[data$ownorrent!="rent"]<-0
    
    data$section8value<-round(data$section8value,0)
    
    return(data$section8value)
    
  }



# Connecticut Rental Assistance Program (RAP) ----

function.RAPBenefit<-function(data){
    
    data<-left_join(data, section8Data, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
    
    data$income.countable = data$income
    
    # Step I: Calculate Adjusted Income
    data$adjustedincome<-rowMaxs(cbind(data$income.countable - data$numkids*data$DependentDeduction - data$netexp.childcare,0))
    
    # Step II: Determine Total Tenant Payment
    data$ttp<-rowMaxs(cbind(0.1*data$income.countable,0.4*data$adjustedincome))
    
    # Step III: Determine benefit value
    data$section8value<-0
    data$section8value<-rowMaxs(cbind(rowMins(cbind(data$exp.rentormortgage,data$MaxGrossRent))-data$ttp,0))
    
    data$section8value[data$ownorrent!="rent"]<-0
    
    data$section8value<-round(data$section8value,0)
    
    return(data$section8value)
    
  }
  


# DC Specific Housing Program - Family Rehousing and Stabilization Program (FRSP) ----

function.FRSPBenefit<-function(data
                              , shareOfRent = 0.4 # User input - varies from 40 to 60%
){
  
  data<-left_join(data, section8Data, by=c("stateFIPS", "countyortownName", "numadults", "numkids"))
  
  data$income.countable = data$income
  
  # Step I: Calculate Adjusted Income
  data$adjustedincome<-rowMaxs(cbind(data$income.countable - data$numkids*data$DependentDeduction,0))
  
  # Step II: Determine Total Tenant Payment
  data$ttp<-shareOfRent*data$adjustedincome
  
  # Step III: Determine benefit value
  data$section8value<-0
  data$section8value<-rowMaxs(cbind(rowMins(cbind(data$exp.rentormortgage,data$MaxGrossRent))-data$ttp,0))
  
  data$section8value[data$ownorrent!="rent"]<-0
  
  data$section8value<-round(data$section8value,0)
  
  return(data$section8value)
  
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
  
  return(data$value.liheap)
  
}



# CCDF function (new) ----

function.CCDFcopay<-function(data
                               , contelig.ccdf = TRUE
                               ){
    
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
    
    
    # Alabama ----
    
    # Description:
    # Copay is a dollar amount per child
    # Daily frequency 
    if(1 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==1,]
      temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
      
      temp$ruleYear<-2022 # always use the most recent year
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_AL, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare", "ruleYear"))
    
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_AK, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_AZ, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      temp$ruleYear<-2022 # always use the most recent year
      
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
    
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_CA, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_CO, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName", "numkidsInCare"))
      
      temp$income<-temp$income-12*temp$IncomeDisregard
      
      temp$FTcopay<-NA
      temp$COPAY <- 0
      temp$x <- 0
      
      for(i in 1:length(temp$income)){
        temp$x[i] <- max(0, temp$income[i] - temp$Bin1Max[i])
      }
      
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
    
    # CONNECTITUC ----
    
    # Description:
    # Copay is a percentage of total income
    # Weekly frequency 
    # Discount for a second child (UNDER CONSTRUCTION)
    if(9 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      ############ TEMP FIX FOR CT
      
      ccdfData_CT$stateFIPS <- 9
    
      temp<-data[data$stateFIPS==9,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_CT, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_DE, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      temp$ruleYear<-2022 # always use the most recent year
      
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
    
    # Description:
    # Variation at the county level
    # Fixed copay per child
    # Daily frequency 
    # Discount for a second child
    if(12 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==12,]
      
      temp$ruleYear<-2022 # always use the most recent year
      
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
      #IF Martin or St Lucie counties, also assign Bins 18-20:
      # temp$FTcopay[temp$countyortownName %in% c("St. Lucie County","Martin County") & temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]<-temp$CopayBin18[temp$income>temp$Bin17Max & temp$income<=temp$Bin18Max]
      # temp$FTcopay[temp$countyortownName %in% c("St. Lucie County","Martin County") & temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]<-temp$CopayBin19[temp$income>temp$Bin18Max & temp$income<=temp$Bin19Max]
      # temp$FTcopay[temp$countyortownName %in% c("St. Lucie County","Martin County") & temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]<-temp$CopayBin20[temp$income>temp$Bin19Max & temp$income<=temp$Bin20Max]
      # 
      
      
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
    
    
    # GEORGIA ----
    
    # Description:
    # Copay is a percentage of total income
    # Weekly frequency 
    # Discount for a second child (UNDER CONSTRUCTION)
    if(13 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
   #   ccdfData_GA$stateFIPS <- 13
    #  ccdfData_GA$IncomeDisregard <- 0
    #  ccdfData_GA$InitialEligibility <- c(19814, 25910, 32007, 38103, 44199, 50296, 56392, 62489, 68585, 74682, 80778, 86875)
    #  ccdfData_GA$ContinuousEligibility <- c(33683, 44047, 54411, 64775, 75139, 85503, 95867, 106231, 116595, 126959, 137323, 147687)
    #  ccdfData_GA$Bin4Max <- ccdfData_GA$ContinuousEligibility
      
      temp<-data[data$stateFIPS==13,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_GA, by=c("stateFIPS", "AKorHI", "famsize"))
      
      # Adjust for the income disregard
      temp$income<-temp$income-12*temp$IncomeDisregard
      
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
      
     
      ccdfData_HI$stateFIPS <- 15
      ccdfData_HI$AssetTest <- 1000000
      
       temp<-data[data$stateFIPS==15,]
      
       temp$ruleYear<-2022 # always use the most recent year
       
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
      
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_IA, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_ID, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      
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
      
      temp$ruleYear<-2022 # always use the most recent year
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_IN, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_KS, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------``
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_KY, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      
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
    
    
    
    # LOUSIANA ----
    
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_LA, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_ME, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      temp$ruleYear<-2022 # always use the most recent year
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_MD, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare", "ruleYear"))
      
      # Adjust for the income disregard
      temp$income<-temp$income-12*temp$IncomeDisregard
      
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
      
    #  ccdfData_MA$stateFIPS <- 25
    #  ccdfData_MA$AssetTest <- 1000000
      
      temp<-data[data$stateFIPS==25,]
      
      temp$ruleYear<-2022 # always use the most recent year
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_MA, by=c("stateFIPS", "AKorHI", "famsize", "ruleYear"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_MI, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_MN, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_MS, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_MO, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_MT, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      ccdfData_NE <- ccdfData_NE[ccdfData_NE$famsize < 12,]
      
      temp <- data[data$stateFIPS==31,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_NE, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      ccdfData_NV$stateFIPS <- 32
      ccdfData_NV$AssetTest <- 1000000
      ccdfData_NV$IncomeDisregard <- 0
      providercost_NV$stateFIPS <- 32
      
      temp <- data[data$stateFIPS==32,]
      
      temp$ruleYear<-2022 # always use the most recent year
      
      
      temp<-left_join(temp, providercost_NV, by=c("stateFIPS", "countyortownName"))
      
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
      temp<-left_join(temp, ccdfData_NV, by=c("stateFIPS", "famsize", "countyortownName", "ruleYear"))
      
      temp$income<-temp$income-12*temp$IncomeDisregard
      
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
      temp$totcopay <- temp$annualCost*(1-temp$FTcopay)
      
      temp$totcopay<-rowMins(cbind(temp$annualCost*(1-temp$FTcopay),temp$netexp.childcare))
      
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
      
      ccdfData_NV$stateFIPS <- 35
      
      temp<-data[data$stateFIPS==35,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_NM, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_NH, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      temp$ruleYear<-2022 # always use the most recent year
      
      temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_NC, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_ND, by=c("stateFIPS", "AKorHI", "famsize"))
      
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

      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_NY, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName"))

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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_OH, by=c("stateFIPS", "AKorHI", "famsize"))
      
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

      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_OK, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_OR, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_PA, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_RI, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_SC, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
    
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_SD, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_TN, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      
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

      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_TX, by=c("stateFIPS", "AKorHI", "famsize"))
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_UT, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      
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
      
      providercost_VT$stateFIPS <- 50
      
      
      temp<-left_join(temp, providercost_VT, by=c("stateFIPS"))
      
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
      
      # sprTotal=sprPerson1+sprPerson2+sprPerson3+sprPerson4+sprPerson5+sprPerson6+sprPerson7
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_VT, by=c("stateFIPS", "famsize"))
      
      temp$income<-temp$income-12*temp$IncomeDisregard
      
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
      
      #----------------------------------
      # Step 2: Calculate total copays
      #----------------------------------
      temp$totcopay <- temp$annualCost*(1-temp$FTcopay)
      
      temp$totcopay<-rowMins(cbind(temp$annualCost*(1-temp$FTcopay),temp$netexp.childcare))
      
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

      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      
      temp<-left_join(temp, ccdfData_VA, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName"))
      
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
      temp$totcopay<-rowMins(cbind(temp$income*temp$FTcopay,temp$netexp.childcare))
      
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
    
    
    
    # WAHINGTON  ----
    # Description:
    # Variation at the state level
    # Fixed copay per family
    # Monthly frequency 
    if(53 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      ccdfData_WA$stateFIPS <- 53
      
      temp<-data[data$stateFIPS==53,]
      
      temp$ruleYear<-2022 # always use the most recent year
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_WA, by=c("stateFIPS", "AKorHI", "ruleYear", "famsize"))
      
      #######################################
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
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
      subset<-temp$totalassets > temp$AssetTest
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
        temp_2021$income<-temp_2021$income-12*temp_2021$IncomeDisregard
        
        temp_2021$FTcopay<-NA
        
        temp_2021$FTcopay[temp_2021$income>=0 & temp_2021$income<=temp_2021$Bin1Max]<-temp_2021$CopayBin1[temp_2021$income>=0 & temp_2021$income<=temp_2021$Bin1Max]
        temp_2021$FTcopay[temp_2021$income>temp_2021$Bin1Max & temp_2021$income<=temp_2021$Bin2Max]<-temp_2021$CopayBin2[temp_2021$income>temp_2021$Bin1Max & temp_2021$income<=temp_2021$Bin2Max]
        temp_2021$FTcopay[temp_2021$income>temp_2021$Bin2Max & temp_2021$income<=temp_2021$Bin3Max]<-temp_2021$CopayBin3[temp_2021$income>temp_2021$Bin2Max & temp_2021$income<=temp_2021$Bin3Max]
        temp_2021$FTcopay[temp_2021$income>temp_2021$Bin3Max & temp_2021$income<=temp_2021$Bin4Max]<-temp_2021$CopayBin4[temp_2021$income>temp_2021$Bin3Max & temp_2021$income<=temp_2021$Bin4Max]
        temp_2021$FTcopay[temp_2021$income>temp_2021$Bin4Max & temp_2021$income<=temp_2021$Bin5Max]<-temp_2021$CopayBin5[temp_2021$income>temp_2021$Bin4Max & temp_2021$income<=temp_2021$Bin5Max]
        
        temp_2021$FTcopay<-as.numeric(temp_2021$FTcopay)
        
        # Apply asset test
        subset<-temp$totalassets > temp$AssetTest
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
        subset<-temp$totalassets > temp$AssetTest
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
      
      temp$ruleYear<-2022 # always use the most recent year
      
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
      
      temp$numkidsInCare<-temp$numkidsincare0to4+temp$numkidsincare5to12
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_WY, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      
      
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
      temp$totcopay[temp$numkidsincare0to4+temp$numkidsincare5to12==0]<-0
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, ccdfData_WI, by=c("stateFIPS", "AKorHI", "famsize", "numkidsInCare"))
      
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
      
      # On purpose assign copay from the 28th bin. Adjust later
      temp$FTcopay[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]<-temp$CopayBin28[temp$income>temp$Bin28Max & temp$income<=temp$Bin29Max]
      
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
      temp$totcopay[subset]<-temp$totcopay[subset]+1*12*(temp$income[subset]-temp$Bin28Max[subset])/3
      
      # Adjust overage depending on whether states allow to charge it
      temp$childcare.overage[temp$OverageOption=="No"]<-0
      
      # Merge back
      data$childcare.overage[data$stateFIPS==55]<-temp$childcare.overage
      data$totcopay[data$stateFIPS==55]<-temp$totcopay
      data$InitialEligibility[data$stateFIPS==55]<-temp$InitialEligibility.y
    }
    
    
    
    
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
    
   
  return(data$value.CCDF)
    
  } #end function.CCDF
  
  
 
# FATES CCDF ----

function.CCDFcopayFATES<-function(data
                                   , contelig.ccdf = TRUE
 ){
   
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
     #IF Martin or St Lucie counties, also assign Bins 18-20:
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
         mutate(incomeeligible_initial_earlyheadstart=case_when(countableincome<IncomeEligibilityLimit|value.ssi>0|value.tanf>0 ~1, TRUE~0),
                   firstyearofearlyheadstart=min(Year[incomeeligible_initial_earlyheadstart==1 & ageeligible_earlyheadstart==1], 9999, na.rm = TRUE), #min function cant handle missing values, so set to 9999 if person is never eligible for earlh head start
                   incomeeligible_earlyheadstart=case_when(`contelig.earlyheadstart`==TRUE ~1 , `contelig.earlyheadstart`==FALSE & Year>firstyearofearlyheadstart ~1, `contelig.earlyheadstart`==FALSE & firstyearofearlyheadstart!=9999 & Year<=firstyearofearlyheadstart ~ incomeeligible_initial_earlyheadstart, TRUE~0),
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
       #determine initial elig  & use this elig if contelig=FALSE & its the first yr receivng the program
       #NOTE! need to do this by unique id if using a larger dataset w/ multiple records per person   
       mutate(firstyearearlyheadstart_receive=min(Year[value.earlyheadstart>0], 9999, na.rm = TRUE), # Determine whether kid received Early Head Start at any point in the past
         incomeeligible_initial_headstart=case_when(countableincome<IncomeEligibilityLimit|value.ssi>0|value.tanf>0 ~1, TRUE~0),
              firstyearofheadstart=min(Year[incomeeligible_initial_headstart==1 & ageeligible_headstart==1], 9999, na.rm = TRUE), #min function cant handle missing values, so set to 9999 if person is never eligible for head starta
         #apply initial eliibility if flag set to TRUE or if need to redetermine kid's eligibility from Early HS to HS     
         incomeeligible_headstart=case_when((`contelig.headstart`==TRUE & firstyearearlyheadstart_receive == 9999) ~1 , (`contelig.headstart`==FALSE | firstyearearlyheadstart_receive != 9999) & Year>firstyearofheadstart ~1, (`contelig.headstart`==FALSE | firstyearearlyheadstart_receive != 9999) & firstyearofheadstart!=9999 & Year<=firstyearofheadstart ~ incomeeligible_initial_headstart, TRUE~0),
              headstart=headstart*incomeeligible_headstart,
              value.headstart=headstart*exp.child) 
     
          
      returnData<-data %>% 
              select(value.earlyheadstart, value.headstart)
      
      return(returnData)
  }
    



#Pre K program -----

function.prek<-function(data
                        , agePersonvar
                        , headstartPersonvar
                        , headstart_ftorpt = "PT"
                        , childcare.expvar
                        , preK_ftorpt = "PT"
                        , schoolagesummercare="PT"
                        , contelig.headstart = TRUE
                        , contelig.earlyheadstart = TRUE)
{
  
  data<-data %>% 
    rename("agePerson" = agePersonvar
           ,"value.headstart"=headstartPersonvar
           ,"childcare.exp.person"=childcare.expvar
    )
  
  
  #in dashboards need to add discalimer "PreK, Head Start, and Early Head Start are not available in all locations or may have wait lists."
  
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

function.EmployerHealthcare<-function(data
                                        , famsizevar
                                        , currentyr = 2021){
    
    # Rename variables if necessary
    colnames(data)[colnames(data)==famsizevar]<-"famsize.enrolled"
    colnames(employerHealthcareData)[colnames(employerHealthcareData)=="famsize"]<-"famsize.enrolled"
    
    data<-left_join(data, employerHealthcareData, by=c("stateFIPS", "famsize.enrolled"))
    
    data$premium.healthcare.byfamsize<-data$premium.healthcare.byfamsize*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
    
    #annualize and round
    data$premium.healthcare.byfamsize<-round(data$premium.healthcare.byfamsize,0)*12
    
    return(data$premium.healthcare.byfamsize)
  }
  
  

#  Affordable Care Act ACA ----

function.aca<-function(data
                       , currentyr = 2014){
    
    data<-left_join(data, acaData, by=c("ruleYear", "famsize", "AKorHI"))
    data<-left_join(data,employerHealthcareData, by=c("stateFIPS", "famsize")) # Merge empl healthcare to obtain costs of the individual plan
    
    # Inflate/deflate
    data$premium.healthcare.individual<-12*data$premium.healthcare.individual*(1+parameters.defaults$inflationrate[1])^(currentyr-data$yearofdata)
    
    data$premium.aca<-NA_real_
    
    data_preACA<-data[data$ruleYear<2014,]
    data_postACA<-data[data$ruleYear>=2014,]
    
    subset<-data_postACA$income>=data_postACA$IncomeLowerBound & data_postACA$income<=data_postACA$IncomeBin1Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin1.Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeLowerBound[subset])*(data_postACA$ShareOfIncomesBin1.Final[subset]-data_postACA$ShareOfIncomesBin1.Initial[subset])/(data_postACA$IncomeBin1Max[subset]-data_postACA$IncomeLowerBound[subset]))
    
    subset<-data_postACA$income>data_postACA$IncomeBin1Max & data_postACA$income<=data_postACA$IncomeBin2Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin2.Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin1Max[subset])*(data_postACA$ShareOfIncomesBin2.Final[subset]-data_postACA$ShareOfIncomesBin2.Initial[subset])/(data_postACA$IncomeBin2Max[subset]-data_postACA$IncomeBin1Max[subset]))
    
    subset<-data_postACA$income>data_postACA$IncomeBin2Max & data_postACA$income<=data_postACA$IncomeBin3Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin3.Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin2Max[subset])*(data_postACA$ShareOfIncomesBin3.Final[subset]-data_postACA$ShareOfIncomesBin3.Initial[subset])/(data_postACA$IncomeBin3Max[subset]-data_postACA$IncomeBin2Max[subset]))
    
    subset<-data_postACA$income>data_postACA$IncomeBin3Max & data_postACA$income<=data_postACA$IncomeBin4Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin4.Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin3Max[subset])*(data_postACA$ShareOfIncomesBin4.Final[subset]-data_postACA$ShareOfIncomesBin4.Initial[subset])/(data_postACA$IncomeBin4Max[subset]-data_postACA$IncomeBin3Max[subset]))
    
    subset<-data_postACA$income>data_postACA$IncomeBin4Max & data_postACA$income<=data_postACA$IncomeBin5Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin5.Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin4Max[subset])*(data_postACA$ShareOfIncomesBin5.Final[subset]-data_postACA$ShareOfIncomesBin5.Initial[subset])/(data_postACA$IncomeBin5Max[subset]-data_postACA$IncomeBin4Max[subset]))
    
    subset<-data_postACA$income>data_postACA$IncomeBin5Max & data_postACA$income<=data_postACA$IncomeBin6Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin6.Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin5Max[subset])*(data_postACA$ShareOfIncomesBin6.Final[subset]-data_postACA$ShareOfIncomesBin6.Initial[subset])/(data_postACA$IncomeBin6Max[subset]-data_postACA$IncomeBin5Max[subset]))
    
    subset<-data_postACA$income>data_postACA$IncomeBin6Max & data_postACA$income<=data_postACA$IncomeBin7Max
    data_postACA$premium.aca[subset]<-data_postACA$income[subset]*(data_postACA$ShareOfIncomesBin7.Initial[subset]+(data_postACA$income[subset]-data_postACA$IncomeBin6Max[subset])*(data_postACA$ShareOfIncomesBin7.Final[subset]-data_postACA$ShareOfIncomesBin7.Initial[subset])/(data_postACA$IncomeBin7Max[subset]-data_postACA$IncomeBin6Max[subset]))
    
    
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
  
  

# Value of School Meals ----

function.schoolmeals<-function(data){
    
    data<-left_join(data, schoolmealData, by=c("ruleYear", "famsize", "AKorHI"))

    data$value.schoolmeals<-0
    
    data$copay<-NA
    
    #value of school lunch depends on income
    data$income.countable=data$income+data$income.gift + data$income.child_support + data$value.ssdi + data$value.ssi + data$value.tanf
    data$copay[data$income.countable<=data$IncomeBin1Max]<-data$CopayBin1[data$income.countable<=data$IncomeBin1Max]
    data$copay[data$income.countable>data$IncomeBin1Max & data$income.countable<=data$IncomeBin2Max]<-data$CopayBin2[data$income.countable>data$IncomeBin1Max & data$income.countable<=data$IncomeBin2Max]
    data$copay<-data$copay*parameters.defaults$numberofSchoolDays[1] #Annualize
    
    #categorical elibiblity: note need to add TANF, child ssi too 
    data$copay[data$value.snap>0| data$value.headstart>0 | data$value.ssi>0 |data$value.tanf>0]<-data$CopayBin1[data$value.snap>0| data$value.headstart>0 | data$value.ssi>0 |data$value.tanf>0]
    
    data$value.schoolmeals<-data$exp.schoolMeals-data$copay
      
    data$value.schoolmeals[is.na(data$value.schoolmeals)]<-0
    
    data$value.schoolmeals<-data$value.schoolmeals*data$numkidsinschool
      
    data$value.schoolmeals<-round(data$value.schoolmeals,2)
    
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

calculate_taxableamtofSSDI=function(value.ssdi
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
    } 
    else{
      TaxableamtofSSDI = 0
    }
    return(TaxableamtofSSDI) 
  } 
  
  
  
  
function.fedinctax<-function(data
                               , incomevar){
    
    colnames(data)[colnames(data)==incomevar]<-"income.base"
    
    data$income.base<-data$income.base+data$income.investment # For now, tax investment income at a income tax rate
    
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

    
    
    data$TaxableamtofSSDI<- apply(data[,c("value.ssdi"
                                          , "income.base"
                                          , "baseAmount_SSDI"
                                          , "SSDI_taxable_test")], 1, function(x) calculate_taxableamtofSSDI(x[1],x[2],x[3],x[4]))
    
    
    
    
    data$income.base<-data$income.base+data$TaxableamtofSSDI-data$Standard # adjust countable income
    
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
    #return(data)
    
  }
  


# State Income Tax----

function.stateinctax<-function(data
                                 , incomevar
                                 , fedincometaxvar
                                 , fedtaxcreditsvar){
    
    data<-data %>% 
      rename("income.base" = incomevar
             ,"fedincometax" = fedincometaxvar
             ,"fedtaxcredits" = fedtaxcreditsvar)

    data<-left_join(data, stateinctaxData, by=c("ruleYear", "stateFIPS", "FilingStatus"))
    
    # Net Federal Income Tax
    data$fedincometax<-rowMaxs(cbind(data$fedincometax-data$fedtaxcredits,0))
    
    # Ten different tax brackets
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
             ,value.tax10=0)
   
    # Indicator variable for whether or not deduct federal income tax
    data<-data %>% 
      mutate(deductfedtax=case_when(FederalIncomeTaxDeductible=="Yes"~1
                                    ,FederalIncomeTaxDeductible=="No"~0))
    
    data$income.base<-data$income.base+data$income.investment-data$Standard-data$fedincometax*data$deductfedtax # adjust countable income
    
    # Calculate income tax for each bracket separately
    data$taxableincome.bin1<-rowMaxs(cbind((data$income.base-0)-rowMaxs(cbind(data$income.base-data$IncomeBin1Max,0)),0))
    data$taxableincome.bin2<-rowMaxs(cbind((data$income.base-data$IncomeBin1Max)-rowMaxs(cbind(data$income.base-data$IncomeBin2Max,0)),0))
    data$taxableincome.bin3<-rowMaxs(cbind((data$income.base-data$IncomeBin2Max)-rowMaxs(cbind(data$income.base-data$IncomeBin3Max,0)),0))
    data$taxableincome.bin4<-rowMaxs(cbind((data$income.base-data$IncomeBin3Max)-rowMaxs(cbind(data$income.base-data$IncomeBin4Max,0)),0))
    data$taxableincome.bin5<-rowMaxs(cbind((data$income.base-data$IncomeBin4Max)-rowMaxs(cbind(data$income.base-data$IncomeBin5Max,0)),0))
    data$taxableincome.bin6<-rowMaxs(cbind((data$income.base-data$IncomeBin5Max)-rowMaxs(cbind(data$income.base-data$IncomeBin6Max,0)),0))
    data$taxableincome.bin7<-rowMaxs(cbind((data$income.base-data$IncomeBin6Max)-rowMaxs(cbind(data$income.base-data$IncomeBin7Max,0)),0))
    data$taxableincome.bin8<-rowMaxs(cbind((data$income.base-data$IncomeBin7Max)-rowMaxs(cbind(data$income.base-data$IncomeBin8Max,0)),0))
    data$taxableincome.bin9<-rowMaxs(cbind((data$income.base-data$IncomeBin8Max)-rowMaxs(cbind(data$income.base-data$IncomeBin9Max,0)),0))
    data$taxableincome.bin10<-rowMaxs(cbind((data$income.base-data$IncomeBin5Max),0))
    
    # Calculate income tax for each bracket separately
    data$value.tax1<-data$TaxRate1*data$taxableincome.bin1
    data$value.tax2<-data$TaxRate2*data$taxableincome.bin2
    data$value.tax3<-data$TaxRate3*data$taxableincome.bin3
    data$value.tax4<-data$TaxRate4*data$taxableincome.bin4
    data$value.tax5<-data$TaxRate5*data$taxableincome.bin5
    data$value.tax6<-data$TaxRate6*data$taxableincome.bin6
    data$value.tax7<-data$TaxRate7*data$taxableincome.bin7
    data$value.tax8<-data$TaxRate7*data$taxableincome.bin8
    data$value.tax9<-data$TaxRate7*data$taxableincome.bin9
    data$value.tax10<-data$TaxRate7*data$taxableincome.bin10
    
    data$value.stateinctax<-(data$value.tax1+data$value.tax2+data$value.tax3
                           +data$value.tax4+data$value.tax5+data$value.tax6
                           +data$value.tax7+data$value.tax8+data$value.tax9
                           +data$value.tax10)
    
    data$value.stateinctax[data$stateFIPS %in% c(33,47)]<-0 # New Hampshire and Tennessee do not tax wages, but tax investment income (add later)
    
    data$value.stateinctax<-round(data$value.stateinctax,0)
    
    return(data$value.stateinctax)
  }
  
  

# Federal Child Tax Credit (CTC) ----

function.fedctc<-function(data
                            , incomevar
                            , totalfederaltaxvar){
    
    data<-data %>% 
      rename("income.base" = incomevar
             ,"totalfederaltax" = totalfederaltaxvar)
    
    data$value.fedctc<-0
    
    
    if(2022 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2022,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
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
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2020,]<-temp
    }
    
    if(2019 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2019,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2019,]<-temp
    }
    
    if(2018 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2018,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2018,]<-temp
    }
    
    if(2017 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2017,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2017,]<-temp
    }
    
    if(2016 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2016,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2016,]<-temp
    }
    
    if(2015 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2015,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2015,]<-temp
    }
    
    if(2014 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2014,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2014,]<-temp
    }
    
    if(2013 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2013,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2013,]<-temp
    }
    
    if(2012 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2012,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2012,]<-temp
    }
    
    if(2011 %in% unique(data$ruleYear)){ # make sure that year is in the list
      
      temp<-data[data$ruleYear==2011,]
      
      #----------------------------------
      # Step 1: Assign copays
      #----------------------------------
      temp<-left_join(temp, fedctcData, by=c("ruleYear", "FilingStatus"))
      
      # Calculate number of eligible dependents
      temp$numkidsunder17=rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=temp$AgeofDependentMax & cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)>=temp$AgeofDependentMin, na.rm=TRUE)
      
      subset1<-temp$income.base<=temp$IncomeBin1Max
      temp$value.fedctc[subset1]<-0
      
      subset2<-temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.fedctc.refundable[subset2]<-rowMins(cbind(0.15*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]),temp$RefundableCredit[subset2]))
      temp$value.fedctc[subset2]<-rowMins(cbind(rowMaxs(cbind(temp$totalfederaltax[subset2],temp$value.fedctc.refundable[subset2])),temp$CreditBin1[subset2]))
      
      subset3<-temp$income.base>temp$IncomeBin2Max
      temp$value.fedctc[subset3]<-rowMaxs(cbind(temp$CreditBin1[subset3]-(temp$income.base[subset3]-temp$IncomeBin2Max[subset3])*temp$PhaseOutSlope1[subset3],0))
      
      temp$value.fedctc<-temp$value.fedctc*temp$numkidsunder17
      
      # Make sure the variables names are the same
      temp<-temp %>% 
        select(colnames(data),"value.fedctc")
      
      # Merge back
      data[data$ruleYear==2011,]<-temp
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
    
    subset2<- data$income.base>data$IncomeBin1Max & data$income.base<data$IncomeBin2Max
    data$value.statectc[subset2]<-rowMaxs(cbind(data$ValueBin2[subset2], data$federalctc[subset2]*data$PercentOfFederalBin2[subset2]))
    
    subset3<- data$income.base>data$IncomeBin2Max & data$income.base<data$IncomeBin3Max
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
      rename(  "income.base" = incomevar
               ,"investmentincome" = investmentincomevar 
               ,"agePerson1" = ageofRespondentvar 
               ,"agePerson2" = ageofSpousevar) 
    
    # Create two variables: AGI and earned income (later that will go to the InitialTransformations)
    data$income.base.AGI<-data$income.base
    data$income.base.earned<-data$income.base
    
    # Step I: merge parameters by filing status and number of children
    data<-left_join(data, fedeitcData, by=c("FilingStatus", "numkids", "ruleYear"))
    
    
    # Compute value of the based on Earned Income
    data$value.fedeitc<-0
    
    subset1<-data$income.base.earned<data$IncomeBin1Max
    data$value.fedeitc[subset1]<-0
    
    subset2<-data$income.base.earned>=data$IncomeBin1Max & data$income.base.earned<data$IncomeBin2Max
    data$value.fedeitc[subset2]<-0+(data$income.base.earned[subset2]-data$IncomeBin1Max[subset2])*data$PhaseInRate[subset2]

    subset3<-data$income.base.earned>=data$IncomeBin2Max & data$income.base.earned<data$IncomeBin3Max
    data$value.fedeitc[subset3]<-data$MaxCredit[subset3]
    
    subset4<-data$income.base.earned>=data$IncomeBin3Max & data$income.base.earned<data$IncomeBin4Max
    data$value.fedeitc[subset4]<-data$MaxCredit[subset4]-(data$income.base.earned[subset4]-data$IncomeBin3Max[subset4])*data$PhaseOutRate[subset4]
    
    subset5<-data$income.base.earned>=data$IncomeBin4Max
    data$value.fedeitc[subset5]<-0
    
    
    # Recalculate EITC based on AGI if required
    subset4a<-data$income.base.AGI!=data$income.base.earned & data$income.base.AGI>=data$IncomeBin3Max & data$income.base.AGI<data$IncomeBin4Max
    data$value.fedeitc[subset4a]<-data$MaxCredit[subset4a]-(data$income.base.AGI[subset4a]-data$IncomeBin3Max[subset4a])*data$PhaseOutRate[subset4a]
    
    subset5a<-data$income.base.AGI!=data$income.base.earned & data$income.base.AGI>=data$IncomeBin4Max
    data$value.fedeitc[subset5a]<-0
    
    
    # Apply special rule for those who do not claim a child
    subset1<-data$numkids==0 & (data$FilingStatus==1 | data$FilingStatus==3) & (data$agePerson1<data$ageLimitMin | data$agePerson1>data$ageLimitMax)
    data$value.fedeitc[subset1]<-0
    
    subset2<-data$numkids==0 & (data$FilingStatus==2) & ((data$agePerson1<data$ageLimitMin & data$agePerson2<data$ageLimitMin) | (data$agePerson1>data$ageLimitMax & data$agePerson2>data$ageLimitMax))
    data$value.fedeitc[subset2]<-0
    
    
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
  
  # Step I: merge parameters by filing status and number of children
  data<-left_join(data, fedeitcData, by=c("FilingStatus", "numkids", "ruleYear"))
  
  
  # Compute value of the based on Earned Income
  data$value.fedeitc<-0
  
  subset1<-data$income.base.earned<data$IncomeBin1Max
  data$value.fedeitc[subset1]<-0
  
  subset2<-data$income.base.earned>=data$IncomeBin1Max & data$income.base.earned<data$IncomeBin2Max
  data$value.fedeitc[subset2]<-0+(data$income.base.earned[subset2]-data$IncomeBin1Max[subset2])*data$PhaseInRate[subset2]
  
  subset3<-data$income.base.earned>=data$IncomeBin2Max & data$income.base.earned<data$IncomeBin3Max
  data$value.fedeitc[subset3]<-data$MaxCredit[subset3]
  
  subset4<-data$income.base.earned>=data$IncomeBin3Max & data$income.base.earned<data$IncomeBin4Max
  data$value.fedeitc[subset4]<-data$MaxCredit[subset4]-(data$income.base.earned[subset4]-data$IncomeBin3Max[subset4])*data$PhaseOutRate[subset4]
  
  subset5<-data$income.base.earned>=data$IncomeBin4Max
  data$value.fedeitc[subset5]<-0
  
  
  # Recalculate EITC based on AGI if required
  subset4a<-data$income.base.AGI!=data$income.base.earned & data$income.base.AGI>=data$IncomeBin3Max & data$income.base.AGI<data$IncomeBin4Max
  data$value.fedeitc[subset4a]<-data$MaxCredit[subset4a]-(data$income.base.AGI[subset4a]-data$IncomeBin3Max[subset4a])*data$PhaseOutRate[subset4a]
  
  subset5a<-data$income.base.AGI!=data$income.base.earned & data$income.base.AGI>=data$IncomeBin4Max
  data$value.fedeitc[subset5a]<-0
  
  
  # Apply special rule for those who do not claim a child
  subset1<-data$numkids==0 & (data$FilingStatus==1 | data$FilingStatus==3) & (data$agePerson1<data$ageLimitMin | data$agePerson1>data$ageLimitMax)
  data$value.fedeitc[subset1]<-0
  
  subset2<-data$numkids==0 & (data$FilingStatus==2) & ((data$agePerson1<data$ageLimitMin & data$agePerson2<data$ageLimitMin) | (data$agePerson1>data$ageLimitMax & data$agePerson2>data$ageLimitMax))
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
    
    
    # Step I: merge parameters by filing status and number of children    
    data_main<-left_join(data, stateeitcData[,colnames(stateeitcData)!="ruleYear"], by=c("stateFIPS", "numkids"))
    
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
    subset<-data$Refundable=="No"
    data$value.stateeitc[subset]<-rowMins(cbind(data$value.stateeitc[subset],data$stateincometax[subset]))
    
    # State-specific rules
    
    #-------------------------------------
    #1. California (stateFIPS==6)
    #-------------------------------------
    if(6 %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data[data$stateFIPS==6,]
    
    temp$value.stateeitc<-0
    temp$value.stateeitc<-temp$PercentOfFederal*temp$federaleitc
    
    # Adjust for refundability
    subset<-temp$Refundable=="No"
    temp$value.stateeitc[subset]<-rowMins(cbind(temp$value.stateeitc[subset],temp$stateincometax[subset]))
    
    #Invoke CA Income eligibility
    subset<-temp$income.base.earned>30000
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
      
      subset<-temp$income.countable<19489
      temp$value.stateeitc.additional[subset]<-temp$value.eitc.base[subset]
      
      subset<-temp$income.countable>=19489
      temp$value.stateeitc.additional[subset]<-rowMaxs(cbind(0,temp$value.eitc.base[subset]-0.0848*(temp$income.countable[subset]-19489)))
      
      # Make sure that age requirements for Federal EITC are met
      subset1<-temp$numkids==0 & (temp$FilingStatus==1 | temp$FilingStatus==3) & (temp$agePerson1<19 | temp$agePerson1>999)
      temp$value.stateeitc.additional[subset1]<-0
      
      subset2<-temp$numkids==0 & (temp$FilingStatus==2) & ((temp$agePerson1<19 & temp$agePerson2<19) | (temp$agePerson1>999 & temp$agePerson2>999))
      temp$value.stateeitc.additional[subset2]<-0
      
      # Check if family satisfies investment income requirements
      subset3<-temp$investmentincome<3650
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
    subset<-temp$Refundable=="No"
    temp$value.stateeitc[subset]<-rowMins(cbind(temp$value.stateeitc[subset],temp$stateincometax[subset]))
    
    #OH has different rules for refundability
    subset<-temp$income.base>20000
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
    subset<-temp$ageChild1<3
    temp$PercentOfFederal[subset]<-0.12
    
    # Calculate tax credit amount in a standard way
    temp$value.stateeitc<-0
    temp$value.stateeitc<-temp$PercentOfFederal*temp$federaleitc
    
    # Adjust for refundability
    subset<-temp$Refundable=="No"
    temp$value.stateeitc[subset]<-rowMins(cbind(temp$value.stateeitc[subset],temp$stateincometax[subset]))
    
    # Replace back
    data$value.stateeitc[data$stateFIPS==41]<-temp$value.stateeitc
    }
    
    
    #-------------------------------------
    #4. Maryland (stateFIPS==24)
    #-------------------------------------
    if(24 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==24,]
      
      # Step 1: Need to recalulate federal EITC, because Maryland has minimum age requirement disregard
      # Recalculate Federal tax credit
      temp<-left_join(temp, fedeitcData, by=c("FilingStatus", "numkids", "ruleYear"))
      
      # Compute value of the based on Earned Income
      temp$federaleitc.rep<-0
      
      subset1<-temp$income.base.earned<temp$IncomeBin1Max
      temp$federaleitc.rep[subset1]<-0
      
      subset2<-temp$income.base.earned>=temp$IncomeBin1Max & temp$income.base.earned<temp$IncomeBin2Max
      temp$federaleitc.rep[subset2]<-0+(temp$income.base.earned[subset2]-temp$IncomeBin1Max[subset2])*temp$PhaseInRate[subset2]
      
      subset3<-temp$income.base.earned>=temp$IncomeBin2Max & temp$income.base.earned<temp$IncomeBin3Max
      temp$federaleitc.rep[subset3]<-temp$MaxCredit[subset3]
      
      subset4<-temp$income.base.earned>=temp$IncomeBin3Max & temp$income.base.earned<temp$IncomeBin4Max
      temp$federaleitc.rep[subset4]<-temp$MaxCredit[subset4]-(temp$income.base.earned[subset4]-temp$IncomeBin3Max[subset4])*temp$PhaseOutRate[subset4]
      
      subset5<-temp$income.base.earned>=temp$IncomeBin4Max
      temp$federaleitc.rep[subset5]<-0
      
      
      # Recalculate EITC based on AGI if required
      subset4a<-temp$income.base.AGI!=temp$income.base.earned & temp$income.base.AGI>=temp$IncomeBin3Max & temp$income.base.AGI<temp$IncomeBin4Max
      temp$federaleitc.rep[subset4a]<-temp$MaxCredit[subset4a]-(temp$income.base.AGI[subset4a]-temp$IncomeBin3Max[subset4a])*temp$PhaseOutRate[subset4a]
      
      subset5a<-temp$income.base.AGI!=temp$income.base.earned & temp$income.base.AGI>=temp$IncomeBin4Max
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
    subset1<-data$numkids==0 & (data$FilingStatus==1 | data$FilingStatus==3) & data$agePerson1<25 & data$stateFIPS != 24
    data$value.stateeitc[subset1]<-0
    
    subset2<-data$numkids==0 & (data$FilingStatus==2) & (data$agePerson1<25 & data$agePerson2<25) & data$stateFIPS != 24
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
    
    # Calculate number of dependents under the age of 13
    data$NumberOfKidsUnder13<-rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=12 & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=0, na.rm=TRUE)
    
    data<-left_join(data, fedcdctcData, by=c("NumberOfKidsUnder13", "ruleYear"))
    
    # Separate between earned income and AGI
    data$income.base.earned<-data$income.base
    data$income.base.AGI<-data$income.base
    
    # LIMITATION
    # Right now the function doesn't check for spouses income to be >1
    
    data$value.fedcdctc<-0
    
    subset1<-data$income.base.AGI<=data$IncomeBin1Max
    data$value.fedcdctc[subset1]<-data$CreditRateBin1[subset1]*rowMins(cbind(data$qualifyingExpenses[subset1],data$MaxExpense[subset1]))
    
    subset2<-data$income.base.AGI>data$IncomeBin1Max & data$income.base.AGI<=data$IncomeBin2Max
    data$value.fedcdctc[subset2]<-(data$CreditRateBin1[subset2]-data$PhaseOutRate1[subset2]*(data$income.base.AGI[subset2]-data$IncomeBin1Max[subset2]))*rowMins(cbind(data$qualifyingExpenses[subset2],data$MaxExpense[subset2]))
    
    subset3<-data$income.base.AGI>data$IncomeBin2Max
    data$value.fedcdctc[subset3]<-rowMaxs(cbind((data$CreditRateBin2[subset3]-data$PhaseOutRate2[subset3]*(data$income.base.AGI[subset3]-data$IncomeBin1Max[subset3]))*rowMins(cbind(data$qualifyingExpenses[subset3],data$MaxExpense[subset3])),0))
    
    # CDCTC is non-refundable
    subset<-data$Refundable=="No"
    data$value.fedcdctc[subset]<-rowMins(cbind(data$value.fedcdctc[subset],data$totalfederaltax[subset]))
    
    # Earned Income Test
    subset<-data$income.base.earned==0
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
    
    subset1<- data$income.base<=data$IncomeBin1Max
    data$value.statecdctc[subset1]<-rowMaxs(cbind(data$PercentOfFederalBin1[subset1]*data$federalcdctc[subset1], data$PercentOfExpensesBin1[subset1]*data$qualifyingExpenses[subset1]))
    
    subset2<- data$income.base>data$IncomeBin1Max & data$income.base<=data$IncomeBin2Max
    data$value.statecdctc[subset2]<-rowMaxs(cbind(data$PercentOfFederalBin2[subset2]*data$federalcdctc[subset2], data$PercentOfExpensesBin2[subset2]*data$qualifyingExpenses[subset2]))
    
    subset3<- data$income.base>data$IncomeBin2Max & data$income.base<=data$IncomeBin3Max
    data$value.statecdctc[subset3]<-rowMaxs(cbind(data$PercentOfFederalBin3[subset3]*data$federalcdctc[subset3], data$PercentOfExpensesBin3[subset3]*data$qualifyingExpenses[subset3]))
    
    subset4<- data$income.base>data$IncomeBin3Max & data$income.base<=data$IncomeBin4Max
    data$value.statecdctc[subset4]<-rowMaxs(cbind(data$PercentOfFederalBin4[subset4]*data$federalcdctc[subset4], data$PercentOfExpensesBin4[subset4]*data$qualifyingExpenses[subset4]))
    
    subset5<- data$income.base>data$IncomeBin4Max & data$income.base<=data$IncomeBin5Max
    data$value.statecdctc[subset5]<-rowMaxs(cbind(data$PercentOfFederalBin5[subset5]*data$federalcdctc[subset5], data$PercentOfExpensesBin5[subset5]*data$qualifyingExpenses[subset5]))
    
    subset6<- data$income.base>data$IncomeBin5Max & data$income.base<=data$IncomeBin6Max
    data$value.statecdctc[subset6]<-rowMaxs(cbind(data$PercentOfFederalBin6[subset6]*data$federalcdctc[subset6], data$PercentOfExpensesBin6[subset6]*data$qualifyingExpenses[subset6]))
    
    subset7<- data$income.base>data$IncomeBin6Max & data$income.base<=data$IncomeBin7Max
    data$value.statecdctc[subset7]<-rowMaxs(cbind(data$PercentOfFederalBin7[subset7]*data$federalcdctc[subset7], data$PercentOfExpensesBin6[subset7]*data$qualifyingExpenses[subset7]))
    
    # Adjust for refundability
    subset<-data$Refundable=="No"
    data$value.statecdctc[subset]<-rowMins(cbind(data$value.statecdctc[subset],data$stateincometax[subset]))
    
    
    # State-specific rules
    
    #-------------------------------------
    #1. Colorado (stateFIPS==8)
    #-------------------------------------
    if(8 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==8,]
      
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
      
      # Adjust for refundability
      subset<-temp$Refundable=="No"
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      
      # Adjust for the Low-Income Child Care Expenses Credit
      subset<-temp$value.statecdctc==0 & temp$qualifyingExpenses>0 & temp$NumberOfKidsUnder13<=2
      temp$value.statecdctc[subset]<-rowMaxs(cbind(0.25*temp$qualifyingExpenses[subset],500*temp$NumberOfKidsUnder13[subset]))
      
      subset<-temp$value.statecdctc==0 & temp$qualifyingExpenses>0 & temp$NumberOfKidsUnder13>3
      temp$value.statecdctc[subset]<-rowMaxs(cbind(0.25*temp$qualifyingExpenses[subset],1000))
      
      
      # Replace back
      data[data$stateFIPS==8,]<-temp
    }
    
    
    
    #-------------------------------------
    # District of Columbia (stateFIPS==11)
    #-------------------------------------
    if(11 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      
      temp<-data[data$stateFIPS==11,]
      
      # DC has an additional "Keep Child Care Affordable Tax Credit" (also known as Early Learning Credit - ELC)
      
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
      subset.kid1<-temp$income.base<=151900 & (!is.na(temp$agePerson1) & temp$agePerson1<=3)
      temp$value.statecdctc.elc.kid1[subset.kid1]<-rowMins(cbind(temp$netexp.childcareperson1[subset.kid1],1010)) # Tax credit is a min of a qualifying expense or a maximum credit per child
      
      # Child 2
      subset.kid2<-temp$income.base<=151900 & (!is.na(temp$agePerson2) & temp$agePerson2<=3)
      temp$value.statecdctc.elc.kid2[subset.kid2]<-rowMins(cbind(temp$netexp.childcareperson2[subset.kid2],1010)) 
      
      # Child 3
      subset.kid3<-temp$income.base<=151900 & (!is.na(temp$agePerson3) & temp$agePerson3<=3)
      temp$value.statecdctc.elc.kid3[subset.kid3]<-rowMins(cbind(temp$netexp.childcareperson3[subset.kid3],1010)) 
      
      # Child 4
      subset.kid4<-temp$income.base<=151900 & (!is.na(temp$agePerson4) & temp$agePerson4<=3)
      temp$value.statecdctc.elc.kid4[subset.kid4]<-rowMins(cbind(temp$netexp.childcareperson4[subset.kid4],1010)) 
      
      # Child 5
      subset.kid5<-temp$income.base<=151900 & (!is.na(temp$agePerson5) & temp$agePerson5<=3)
      temp$value.statecdctc.elc.kid5[subset.kid5]<-rowMins(cbind(temp$netexp.childcareperson5[subset.kid5],1010)) 
      
      # Child 6
      subset.kid6<-temp$income.base<=151900 & (!is.na(temp$agePerson6) & temp$agePerson6<=3)
      temp$value.statecdctc.elc.kid6[subset.kid6]<-rowMins(cbind(temp$netexp.childcareperson6[subset.kid6],1010)) 
      
      # Child 7
      subset.kid7<-temp$income.base<=151900 & (!is.na(temp$agePerson7) & temp$agePerson7<=3)
      temp$value.statecdctc.elc.kid7[subset.kid7]<-rowMins(cbind(temp$netexp.childcareperson7[subset.kid7],1010)) 
      
      # Child 8
      subset.kid8<-temp$income.base<=151900 & (!is.na(temp$agePerson8) & temp$agePerson8<=3)
      temp$value.statecdctc.elc.kid8[subset.kid8]<-rowMins(cbind(temp$netexp.childcareperson8[subset.kid8],1010)) 
      
      # Child 9
      subset.kid9<-temp$income.base<=151900 & (!is.na(temp$agePerson9) & temp$agePerson9<=3)
      temp$value.statecdctc.elc.kid9[subset.kid9]<-rowMins(cbind(temp$netexp.childcareperson9[subset.kid9],1010)) 
      
      # Child 10
      subset.kid10<-temp$income.base<=151900 & (!is.na(temp$agePerson10) & temp$agePerson10<=3)
      temp$value.statecdctc.elc.kid10[subset.kid10]<-rowMins(cbind(temp$netexp.childcareperson10[subset.kid10],1010)) 
      
      # Child 11
      subset.kid11<-temp$income.base<=151900 & (!is.na(temp$agePerson11) & temp$agePerson11<=3)
      temp$value.statecdctc.elc.kid11[subset.kid11]<-rowMins(cbind(temp$netexp.childcareperson11[subset.kid11],1010)) 
      
      # Child 12
      subset.kid12<-temp$income.base<=151900 & (!is.na(temp$agePerson12) & temp$agePerson12<=3)
      temp$value.statecdctc.elc.kid12[subset.kid12]<-rowMins(cbind(temp$netexp.childcareperson12[subset.kid12],1010)) 
      
      # Total value of the credit
      temp$value.statecdctc.elc<-temp$value.statecdctc.elc.kid1+temp$value.statecdctc.elc.kid2+temp$value.statecdctc.elc.kid3+temp$value.statecdctc.elc.kid4+temp$value.statecdctc.elc.kid5+temp$value.statecdctc.elc.kid6+temp$value.statecdctc.elc.kid7+temp$value.statecdctc.elc.kid8+temp$value.statecdctc.elc.kid9+temp$value.statecdctc.elc.kid10+temp$value.statecdctc.elc.kid11+temp$value.statecdctc.elc.kid12
      
      # Add it back to the total state CDCTC
      temp$value.statecdctc<-temp$value.statecdctc+temp$value.statecdctc.elc
      
      # Replace back
      data$value.statecdctc[data$stateFIPS==11]<-temp$value.statecdctc
    }
    
    
    #-------------------------------------
    #2. Hawaii (stateFIPS==15)
    #-------------------------------------
    if(15 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==15,]
      
      # Adjust for maximum expenses
      subset<-temp$NumberOfKidsUnder13==1
      temp$qualifyingExpenses[subset]<-rowMins(cbind(temp$qualifyingExpenses[subset],2400))
      
      subset<-temp$NumberOfKidsUnder13>1
      temp$qualifyingExpenses[subset]<-rowMins(cbind(temp$qualifyingExpenses[subset],4800))
      
      subset1<- temp$income.base<=temp$IncomeBin1Max
      temp$value.statecdctc[subset1]<-rowMaxs(cbind(temp$PercentOfFederalBin1[subset1]*temp$federalcdctc[subset1], temp$PercentOfExpensesBin1[subset1]*temp$qualifyingExpenses[subset1]))
      
      subset2<- temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.statecdctc[subset2]<-rowMaxs(cbind(temp$PercentOfFederalBin2[subset2]*temp$federalcdctc[subset2], (temp$PercentOfExpensesBin1[subset2]-temp$PhaseOutRate[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]))*temp$qualifyingExpenses[subset2]))
      
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
      
      # Adjust for refundability
      subset<-temp$Refundable=="No"
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      # Replace back
      data[data$stateFIPS==15,]<-temp
    }
    
    
    
    #-------------------------------------
    #3. Louisiana (stateFIPS==22)
    #-------------------------------------
    if(22 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==22,]
      
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
      
      # Adjust for refundability
      subset<-temp$income.base>25000
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      # Replace back
      data[data$stateFIPS==22,]<-temp
    }
    
    
    
    
    #-------------------------------------
    #4. Maine (stateFIPS==23)
    #-------------------------------------
    if(23 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==23,]
      
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
      
      # Adjust for refundability
      subset<-temp$value.statecdctc>500
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      # Replace back
      data[data$stateFIPS==23,]<-temp
    }
    
    
    #-------------------------------------
    #5. Maryland (stateFIPS==24)
    #-------------------------------------
    if(24 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==24,]
      
      subset<-temp$FilingStatus==2
      temp$IncomeBin1Max[subset]<-75000
      temp$IncomeBin2Max[subset]<-110000
      temp$IncomeBin3Max[subset]<-125000
      temp$IncomeBin4Max[subset]<-141000
      
      subset1<- temp$income.base<=temp$IncomeBin1Max
      temp$value.statecdctc[subset1]<-rowMaxs(cbind(temp$PercentOfFederalBin1[subset1]*temp$federalcdctc[subset1], temp$PercentOfExpensesBin1[subset1]*temp$qualifyingExpenses[subset1]))
      
      subset2<- temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.statecdctc[subset2]<-rowMaxs(cbind((temp$PercentOfFederalBin2[subset2]-temp$PhaseOutRate[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]))*temp$federalcdctc[subset2], temp$PercentOfExpensesBin2[subset2]*temp$qualifyingExpenses[subset2]))
      
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
      
      # Adjust for refundability
      subset<-temp$income.base>temp$IncomeBin1Max
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      # Replace back
      data[data$stateFIPS==24,]<-temp
    }
    
    
    #-------------------------------------
    #6. Minnesota (stateFIPS==27)
    #-------------------------------------
    if(27 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==27,]
      
      subset1<- temp$income.base<=temp$IncomeBin1Max
      temp$value.statecdctc[subset1]<-rowMaxs(cbind(temp$PercentOfFederalBin1[subset1]*temp$federalcdctc[subset1], temp$PercentOfExpensesBin1[subset1]*temp$qualifyingExpenses[subset1]))
      
      subset2<- temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.statecdctc[subset2]<-rowMaxs(cbind((temp$PercentOfFederalBin2[subset2]-temp$PhaseOutRate[subset2]*(temp$income.base[subset2]-temp$IncomeBin1Max[subset2]))*temp$federalcdctc[subset2], temp$PercentOfExpensesBin2[subset2]*temp$qualifyingExpenses[subset2]))
      
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
      
      # Only state with maximum credit
      temp$value.statecdctc<-rowMaxs(cbind(temp$value.statecdctc,1400))
      
      # Adjust for refundability
      subset<-temp$Refundable=="No"
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      # Replace back
      data[data$stateFIPS==27,]<-temp
    }
    
    
    #-------------------------------------
    #7. Nebraska (stateFIPS==31)
    #-------------------------------------
    if(31 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==31,]
      
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
      
      # Adjust for refundability
      subset<-temp$income.base>temp$IncomeBin2Max
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      # Replace back
      data[data$stateFIPS==31,]<-temp
    }
    
    
    
    #-------------------------------------
    #8. New Mexico (stateFIPS==35)
    #-------------------------------------
    if(35 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==35,]
      
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
      
      # Adjust for refundability
      subset<-temp$Refundable=="No"
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      
      # Adjust for maximum values
      subset<-temp$NumberOfKidsUnder13<=2
      temp$value.statecdctc[subset]<-rowMaxs(cbind(temp$value.statecdctc[subset],480*temp$NumberOfKidsUnder13[subset]))
      
      subset<-temp$NumberOfKidsUnder13>3
      temp$value.statecdctc[subset]<-rowMaxs(cbind(temp$value.statecdctc[subset],1200))
      
      
      # Replace back
      data[data$stateFIPS==35,]<-temp
    }
    
    
    
    #-------------------------------------
    #9. New York (stateFIPS==36)
    #-------------------------------------
    if(36 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==36,]
      
      subset1<- temp$income.base<=temp$IncomeBin1Max
      temp$value.statecdctc[subset1]<-rowMaxs(cbind(temp$PercentOfFederalBin1[subset1]*temp$federalcdctc[subset1], temp$PercentOfExpensesBin1[subset1]*temp$qualifyingExpenses[subset1]))
      
      subset2<- temp$income.base>temp$IncomeBin1Max & temp$income.base<=temp$IncomeBin2Max
      temp$value.statecdctc[subset2]<-rowMaxs(cbind(temp$PercentOfFederalBin2[subset2]*temp$federalcdctc[subset2], temp$PercentOfExpensesBin2[subset2]*temp$qualifyingExpenses[subset2]))
      
      subset3<- temp$income.base>temp$IncomeBin2Max & temp$income.base<=temp$IncomeBin3Max
      temp$value.statecdctc[subset3]<-rowMaxs(cbind((temp$PercentOfFederalBin2[subset3]-temp$PhaseOutRate[subset3]*(temp$income.base[subset3]-temp$IncomeBin2Max[subset3]))*temp$federalcdctc[subset3], temp$PercentOfExpensesBin3[subset3]*temp$qualifyingExpenses[subset3]))
      
      subset4<- temp$income.base>temp$IncomeBin3Max & temp$income.base<=temp$IncomeBin4Max
      temp$value.statecdctc[subset4]<-rowMaxs(cbind(temp$PercentOfFederalBin4[subset4]*temp$federalcdctc[subset4], temp$PercentOfExpensesBin4[subset4]*temp$qualifyingExpenses[subset4]))
      
      subset5<- temp$income.base>temp$IncomeBin4Max & temp$income.base<=temp$IncomeBin5Max
      temp$value.statecdctc[subset5]<-rowMaxs(cbind(temp$PercentOfFederalBin5[subset5]*temp$federalcdctc[subset5], temp$PercentOfExpensesBin5[subset5]*temp$qualifyingExpenses[subset5]))
      
      subset6<- temp$income.base>temp$IncomeBin5Max & temp$income.base<=temp$IncomeBin6Max
      temp$value.statecdctc[subset6]<-rowMaxs(cbind(temp$PercentOfFederalBin6[subset6]*temp$federalcdctc[subset6], temp$PercentOfExpensesBin6[subset6]*temp$qualifyingExpenses[subset6]))
      
      subset7<- temp$income.base>temp$IncomeBin6Max & temp$income.base<=temp$IncomeBin7Max
      temp$value.statecdctc[subset7]<-rowMaxs(cbind(temp$PercentOfFederalBin7[subset7]*temp$federalcdctc[subset7], temp$PercentOfExpensesBin6[subset7]*temp$qualifyingExpenses[subset7]))
      
      # Adjust for refundability
      subset<-temp$Refundable=="No"
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      # Replace back
      data[data$stateFIPS==36,]<-temp
    }
    
    
    
    #-------------------------------------
    #10. South Carolina (stateFIPS==45)
    #-------------------------------------
    if(45 %in% unique(data$stateFIPS)){ # make sure that state is in the list
      temp<-data[data$stateFIPS==45,]
      
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
      
      # Adjust for refundability
      subset<-temp$Refundable=="No"
      temp$value.statecdctc[subset]<-rowMins(cbind(temp$value.statecdctc[subset],temp$stateincometax[subset]))
      
      
      # Adjust for maximum values
      subset<-temp$NumberOfKidsUnder13<=2
      temp$value.statecdctc[subset]<-rowMaxs(cbind(temp$value.statecdctc[subset],210*temp$NumberOfKidsUnder13[subset]))
      
      subset<-temp$NumberOfKidsUnder13>3
      temp$value.statecdctc[subset]<-rowMaxs(cbind(temp$value.statecdctc[subset],420))
      
      # Spouses filing separately are ineligible for CDCTC
      subset<-temp$FilingStatus==4
      temp$value.statecdctc[subset]<-0
      
      # Replace back
      data[data$stateFIPS==45,]<-temp
    }
    
    
    
    data$value.statecdctc<-round(data$value.statecdctc,0)
    
    # Plug states with CTC back
    data_main[data_main$stateFIPS %in% states_with_cdctc,]<-data
    
    return(data_main$value.statecdctc)
  }
  
  
  
  
# Federal Insurance Constribution Act (FICA) Tax----

function.ficatax<-function(data
                               , employmentincomevar){
    
    colnames(data)[colnames(data)==employmentincomevar]<-"income.base"
    
    data<-left_join(data, ficataxData, by=c("FilingStatus"))
    
    data$tax.ss<-rowMins(cbind(data$income.base,data$SocialSecurityTaxBase))*data$SocialSecurityTaxRate
  
    
    # Calculate income tax for each bracket separately
    data$taxableincome.bin1<-rowMaxs(cbind((data$income.base-0)-rowMaxs(cbind(data$income.base-data$IncomeBin1Max,0)),0))
    data$taxableincome.bin2<-rowMaxs(cbind((data$income.base-data$IncomeBin1Max),0))
    
    data$value.medicaretax<-data$taxableincome.bin1*data$MedicareTaxRateBin1+data$taxableincome.bin2*data$MedicareTaxRateBin2
    
    data$value.ficatax<-data$tax.ss+data$value.medicaretax
    
    data$value.ficatax<-round(data$value.ficatax,0)
    
    return(data$value.ficatax)
  }
  
  
  
  
  
  
  