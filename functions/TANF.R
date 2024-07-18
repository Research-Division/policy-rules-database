
## Temporary Assistance for Needy Families (TANF)----

# childcare deduction states:
# ARIZONA
# GEORGIA
# NEW HAMPSHIRE
# maybe IOWA
# 

# incapacitated deduction states:
# HAWAII


# USUAL TANF PROCESS

# 1. SEGMENT DATA FOR SPECIFIC STATE
# 2. ADD GIFT INCOME TO INCOME
# 3. NEED TO DETERMINE APPLICANT VERSUS RECIPIENT
# 4. CALCULATE NET INCOME - LOOK AT "LIST OF DEDUCTIONS FOR QUALIFICATION" AND "IncomeDisregardDescription" FOR REFERENCE TO HOW TO CALCULATE
# 5. ALSO LOOK AT "INCOME DISREGARD DESCRIPTION" FOR DETAILS ON DEDUCTIONS MADE FOR NET INCOME CALCULATION VS. BENEFIT CALCULATION
# 6. SEE IF NET INCOME IS LESS THAN THE STANDARD OF NEED (StandardOfNeed). Also add SSDI to unearned income so that total net income = earned income (after deductions) + unearned income, needed for the gross income test  
# 7. IF NET INCOME IS LESS THAN THE STANDARD OF NEED, CALCULATE THE BENEFIT AMOUNT. USUALLY, THIS IS THE MAXIMUM BENEFIT MINUS THE NET INCOME, THOUGH IT VARIES BY STATE
# 8. LOOK AT "LIST OF DEDUCTIONS FOR QUALIFICATIONS" AND "LIST OF DEDUCTIONS FOR BENEFIT ALLOTMENT" TO SEE THE DIFFERENCE BETWEEN CALCULATING THE NET INCOME AND CALCULATING BENEFIT ALLOTMENT
# 9. IF THE AMOUNT CALCULATED FOR THE BENEFIT ALLOTMENT IS GREATER THAN THE MAXIMUM BENEFIT, THEN BENEFIT = 0.

# NOTE: MOST STATES REQUIRES A $10 MINIMUM BENEFIT. THE EXCEPTION IS NORTH CAROLINA, WHICH REQUIRES A $25 MINIMUM BENEFIT.

# 10. ONCE YOU'VE PASSED THE STANDARD OF NEED TEST (if applicable) AND THE BENEFIT ALLOTMENT > 0, CHECK THE CHILDLESS_TANF_POLICY AND THE TWO PARENTS POLICY AND THE GROSS INCOME TEST
# 11. IF CHILDLESS = "NO' AND THE FAMILY OBSERVED HAS NO CHILDREN, THEN THEY'RE DISQUALIFIED FROM TANF
# 12. IF TWO PARENTS = "NO" AND THE FAMILY HAS TWO PARENTS IN THE HOUSEHOLD, THEN THEY'RE DISQUALIFIED
# 13. IF THE GROSS EARNED INCOME IS GREATER THAN THE GROSSINCOMETEST AMOUNT FOR FAMILY SIZE X, THEN BENEFIT = 0
# 14. IF THE SERIES IS CROSS SECTION (I.E. INCORPORATES MULTIPLE HOUSEHOLD / INDIVIDUAL OBSERVATIONS), THEN WE GROUP TOGETHER THE IDS AND SEE IF THERE IS PANEL / TIME SERIES INFORMATION FOR EACH HOUSEHOLDID. RELEVANT FOR STEP 15
# 15. IF THE SERIES IS TIME SERIES / PANEL, THEN WE CUT OFF THE TANF VALUES AT THE TIME LIMIT, DESIGNATED BY THE STATE. I.E. FOR ALABAMA OBSERVATIONS, ALL TANF VALUES ARE CUT OFF (= 0) AFTER THE 60 MONTH TIME LIMIT 
# 16. WE THEN MERGE THE TANF VALUES BACK INTO THE ORIGINAL DATA SET 


# NOTE: WE KNOW THAT SOME NEW ENGLAND STATES SUCH AS MASSACHUSETTS, CONNECTICUT AND RHODE ISLAND (AND SOME OTHER STATES LIKE ALASKA) REDUCE THE AMOUNT OF THE STANDARD OF NEED AND/OR THE MAXIMUM BENEFIT ALLOTMENT
# IF THE RECEIPIENT(S) RECEIVES PUBLIC OR SUBSIDIZED HOUSING, OR PAYS BELOW A CERTAIN THRESHOLD FOR RENT. WE DO NOT CURRENTLY INCLUDE THIS INTO OUR CALCULATIONS FOR THOSE STATES 


function.tanfBenefit<-function(data){
  
  data$tanfValue<-0
  data$value.tanf <-0  

  if(min(data$ruleYear)<=2024){
   
    # Alabama----
  state_code=1 
  if(state_code %in% unique(data$stateFIPS)){
    
    
    # step 1 
    temp<-data[data$stateFIPS==state_code,]
    
    householdids <- unique(temp$householdid)
    
    # step 2
    temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
    
    # Step 3: add gift income
    temp$income=temp$income+temp$income.gift 
    
    # Calculate net income - For Alabama, a 30% dedication in work expense and childcare are subtracted from gross income 
    temp$net.income<-rowMaxs(cbind((temp$income - temp$EarnedIncomeDisregard*temp$income)/12,0)) 
    temp$net.income<-temp$net.income+(temp$value.ssdi/12)
    # Step II: Calculate value of the benefit
    temp$tanfValue<-0
    subset<-temp$totalassets<temp$AssetTest
    temp$tanfValue[subset]<-rowMaxs(cbind(temp$Maxbenefit[subset] - temp$net.income[subset],0))
    
    # Apply gross income test (if applicable)
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    temp$tanfValue[temp$net.income>temp$StandardOfNeed]<-0
    
    # Two parent TANF policy: if 'Yes' then two parent homes are eligible
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Calculate the value of tanf over however many years are in the original dataset. Alabama has a 60 months time limit 
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    
    # Merge back into dataset 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
  }
  
  # Alaska----
  state_code=2
  if(state_code %in% unique(data$stateFIPS)){ 
    
    # Filter by state_code (AK = 2) and join with tanfData
    temp<-data %>% 
      filter(stateFIPS==state_code) %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      #arrange(Year) %>% 
      
      # Step I: Calculate net income
      mutate(income=income+income.gift)
    
    # create unique householdid for cross section data
    householdids <- unique(temp$householdid)
    
    # Alaska's net income is calculate by subtracting a $150 lump sum from income, and then subtracting a percentage of remaining earned income. This percentage decreases for every year tanf is earned:
    # Year 1: 33%, Year 2: 25%, Year 3: 20%, Year 4: 15%, Year 5: 10%
    temp$net.income <- temp$income
    temp<-temp %>%
      group_by(householdid) %>%
      mutate(min_Year=min(Year)) %>%
      mutate(net.income=ifelse(Year == min_Year,  income-(150*12)-0.33*(income-(150*12)),net.income)) %>%
      mutate(net.income=ifelse(Year == min_Year+1,income-(150*12)-0.25*(income-(150*12)),net.income)) %>%
      mutate(net.income=ifelse(Year == min_Year+2,income-(150*12)-0.20*(income-(150*12)),net.income)) %>%
      mutate(net.income=ifelse(Year == min_Year+3,income-(150*12)-0.15*(income-(150*12)),net.income)) %>%
      mutate(net.income=ifelse(Year == min_Year+4,income-(150*12)-0.10*(income-(150*12)),net.income)) %>%
      select(-c(min_Year))
    
    # Any net income that is less than 0 gets set to 0
    temp$net.income[temp$net.income < 0] <- 0
    
    # add SSDI value as unearned income to the total net income, and set the net.income to monthly values
    temp<- temp %>% 
      mutate(net.income=net.income/12) %>%
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit-net.income),0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      
      # Two parent TANF policy: if 'Yes' then two parent homes are eligible  
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes",0,tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Gross Income Test
    
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # Standard of Need Test
    
    temp$tanfValue[(temp$net.income) > temp$StandardOfNeed] <- 0
    
    # Any benefit amount that is less than $10 is not allowed
    
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # Set to annual values
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Alaska has a time limit of 60 months for receiving benefits
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # Merge back to original data
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
  }
  
  # Arizona ----
  
  state_code=4
  if(state_code %in% unique(data$stateFIPS)){ 
    
    # Filter data based on state code (AZ = 4)
    temp<-data %>% 
      filter(stateFIPS==state_code)
    
    # Create unique householdid list for cross section data
    
    householdids <- unique(temp$householdid)
    
    
    # Join dataset with 
    temp <- temp  %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # THIS CODE IS NOT CURRENTLY INCLUDED IN THE CALCULATIONS
      
      #  mutate(numkidsunder13 = rowSums(cbind(agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12)<13,na.rm=TRUE)) %>% 
      # mutate(numkidsunder2 = rowSums(cbind(agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12)<2,na.rm=TRUE)) %>% 
      
      # Step I: Calculate net income - Arizona deducts $90 and then 30% of the remaining income
      # Arizona does deduct money spent on childcare per month: Maximum $200 per child under the age of 2, Up to $175 for each child 2 and older OR an adult with a disability. We do not currently incorporate that. 
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12)-90-EarnedIncomeDisregard*((income/12) - 90),0))) %>% 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # Two parent TANF policy: if 'Yes' then two parent homes are eligible
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Gross Income Test
    
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # $10 Minimum Benefit 
    
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # Income made annually 
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Arizona has a lifetime limit of 12 months
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # Incorporate back into original dataset 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  # Arkansas----
  
  state_code=5
  if(state_code %in% unique(data$stateFIPS)){ 
    
    # filter data by state code (AR = 5)
    temp<-data %>% 
      filter(stateFIPS==state_code) #%>%
    
    # create unique household ids list for cross section data
    
    householdids <- unique(temp$householdid)
    
    # Join data
    temp <- temp  %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      #Add gift income
      mutate(income=income+income.gift) %>% 
      
      # Arkansas deducts 20% off earn income for applicants; for recipients, there is a 20% work related deduction followed by a 60% work incentive deduction
      mutate(net.income.first = rowMaxs(cbind((income/12) - (income*EarnedIncomeDisregard/12),0))) %>% 
      mutate(net.income = rowMaxs(cbind(net.income.first - (3*EarnedIncomeDisregard*net.income.first),0))) %>% 
      
      # Add ssdi value as unearned income to total net income
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      # In Arkansas, for recipients, the monthly amounts are not reduced by monthly countable income until a specified income trigger is achieved. 
      #When the family’s monthly gross earned income plus unearned income equals $446 or more, while the net income remains $223 or less, then the maximum payment for the family size is reduced by 50%. 
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest & net.income<=StandardOfNeed,(tanfValue/2),tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))
    
    # Two parent TANF policy: if 'Yes' then two parent homes are eligible
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Make value yearly
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Arkansas has a 24 month liftime limit 
    
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+1), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back to data
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  # California----
  state_code=6
  
  # filter based on state code 
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    
    # California's standard of need & maximum benefit amounts are based on county. Region A includes the counties listed below, Region B is all other counties 
    temp_countys<-data %>% 
      filter(stateFIPS==state_code) %>%
      filter(    
        countyortownName == "Alameda County" 
        | countyortownName ==  "Contra Costa County" 
        | countyortownName == "Los Angeles County" 
        | countyortownName == "Marin County" 
        | countyortownName ==  "Monterey County" 
        | countyortownName ==  "Napa County" 
        | countyortownName ==  "Orange County" 
        | countyortownName ==  "San Diego County" 
        | countyortownName ==  "San Francisco County" 
        | countyortownName ==  "San Luis Obispo County" 
        | countyortownName ==  "San Mateo County"
        | countyortownName ==  "Santa Barbara County"
        | countyortownName ==  "Santa Clara County" 
        | countyortownName ==  "Santa Cruz County" 
        | countyortownName ==  "Sonoma County" 
        | countyortownName ==  "Ventura County" 
      )  %>%
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - this is NOT including DBI, need to look into that. 
      # Subtract $90 from earned income, then subtract $600, then subtract 50% of the remaining income 
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12) - 90,0))) %>%
      mutate(net.income = rowMaxs(cbind(net.income - 600,0))) %>%
      mutate(net.income = rowMaxs(cbind(net.income*EarnedIncomeDisregard,0))) %>% 
      
      # Add 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      #    mutate(tanfValue=ifelse())
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # Two parent TANF policy: if 'Yes' then two parent homes are eligible 
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) %>% 
      
      # In some states TANF is not available for childless adults
      
      mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No",0,tanfValue ))
    
    # Other counties
    
    temp_othercountys<-data %>% 
      filter(stateFIPS==state_code)  %>% 
      filter(    
        countyortownName != "Alameda County" 
        & countyortownName !=  "Contra Costa County" 
        & countyortownName != "Los Angeles County" 
        & countyortownName != "Marin County" 
        & countyortownName !=  "Monterey County" 
        & countyortownName !=  "Napa County" 
        & countyortownName !=  "Orange County" 
        & countyortownName !=  "San Diego County" 
        & countyortownName !=  "San Francisco County" 
        & countyortownName !=  "San Luis Obispo County" 
        & countyortownName !=  "San Mateo County"
        & countyortownName !=  "Santa Barbara County"
        & countyortownName !=  "Santa Clara County" 
        & countyortownName !=  "Santa Cruz County" 
        & countyortownName !=  "Sonoma County" 
        & countyortownName !=  "Ventura County" 
      )  %>%
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - this is NOT including DBI, need to look into that 
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12) - 90,0))) %>%
      mutate(net.income = rowMaxs(cbind(net.income - 600,0))) %>%
      mutate(net.income = rowMaxs(cbind(net.income*EarnedIncomeDisregard,0))) %>% 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit2-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed2,0,tanfValue)) %>% 
      
      # Two parent TANF policy: if 'Yes' then two parent homes are eligible
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) %>% 
      
      # In some states TANF is not available for childless adults
      
      mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No",0,tanfValue ))
    
    # row bind the two datasets with the different counties 
    
    temp<-rbind(temp_countys, temp_othercountys)
    
    # $10 minimum benefit amount
    
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make yearly
    
    temp$tanfValue <- temp$tanfValue*12
    
    # unique household ids for cross section data 
    
    householdids <- unique(temp$householdid)
    
    # see if there is yearly information in the data, and if there is more than 1 row. This is important for the child-only scenarios in California
    
    if(nrow(temp)>1 & "Year" %in% colnames(temp) ){
      
      # Establish minimum Year for each individual householdid 
      
      temp$min_Year <- temp$Year
      
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year))
      
      
      
      # For children, California tanf continues until they reach the age of adulthood. For Adults, Californias lifetime limit is 48 months 
      temp_adults <- temp[temp$Year <= temp$min_Year + 3,]
      
      # If there is data that exceeds the 48 time limit for adults, depending on the household, then tanf needs to be recalculated for families such that only their children receive tanf after the 48 month time limit. 
      if(nrow(temp_adults) != nrow(temp)){
        
        temp<-data %>% 
          filter(stateFIPS==state_code)
        
        temp$min_Year <- temp$Year
        
        temp<-temp %>% 
          group_by(householdid) %>% 
          mutate(min_Year=min(Year))
        
        ############## REDO NOW WITH ADULTS EXCLUDED 
        
        tanfData_CA <- tanfData
        
        tanfData_CA$numkids <- tanfData_CA$famsize
        
        
        temp_countys<-temp[temp$Year > temp$min_Year + 3,] %>% 
          filter(stateFIPS==state_code) %>% 
          filter(    
            countyortownName == "Alameda County" 
            | countyortownName ==  "Contra Costa County" 
            | countyortownName == "Los Angeles County" 
            | countyortownName == "Marin County" 
            | countyortownName ==  "Monterey County" 
            | countyortownName ==  "Napa County" 
            | countyortownName ==  "Orange County" 
            | countyortownName ==  "San Diego County" 
            | countyortownName ==  "San Francisco County" 
            | countyortownName ==  "San Luis Obispo County" 
            | countyortownName ==  "San Mateo County"
            | countyortownName ==  "Santa Barbara County"
            | countyortownName ==  "Santa Clara County" 
            | countyortownName ==  "Santa Cruz County" 
            | countyortownName ==  "Sonoma County" 
            | countyortownName ==  "Ventura County" 
          ) %>%
          left_join(tanfData_CA, by=c("stateFIPS", "numkids")) %>% 
          
          # Step I: Calculate net income - this is NOT including DBI, need to look into that 
          mutate(income=income+income.gift) %>% 
          mutate(net.income = rowMaxs(cbind((income/12) - 90,0))) %>%
          mutate(net.income = rowMaxs(cbind(net.income - 600,0))) %>%
          mutate(net.income = rowMaxs(cbind(net.income*EarnedIncomeDisregard,0))) %>% 
          
          # Step II: Calculate value of the benefit
          mutate(tanfValue=0,
                 subset=totalassets<AssetTest) %>% 
          mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
          
          # Apply gross income test (if applicable) & net income / standard test (if applicable)
          
          mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
          
          mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
          
          # Two parent TANF policy: if 'Yes' then two parent homes are eligible
          mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes",0,tanfValue)) %>% 
          
          # In some states TANF is not available for childless adults
          
          mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No",0,tanfValue ))
        
        
        temp_othercountys<-temp[temp$Year > temp$min_Year + 3,] %>% 
          filter(stateFIPS==state_code)%>% 
          filter(    
            countyortownName != "Alameda County" 
            & countyortownName !=  "Contra Costa County" 
            & countyortownName != "Los Angeles County" 
            & countyortownName != "Marin County" 
            & countyortownName !=  "Monterey County" 
            & countyortownName !=  "Napa County" 
            & countyortownName !=  "Orange County" 
            & countyortownName !=  "San Diego County" 
            & countyortownName !=  "San Francisco County" 
            & countyortownName !=  "San Luis Obispo County" 
            & countyortownName !=  "San Mateo County"
            & countyortownName !=  "Santa Barbara County"
            & countyortownName !=  "Santa Clara County" 
            & countyortownName !=  "Santa Cruz County" 
            & countyortownName !=  "Sonoma County" 
            & countyortownName !=  "Ventura County" 
          )  %>% 
          left_join(tanfData_CA, by=c("stateFIPS", "numkids")) %>% 
          
          # Step I: Calculate net income - this is NOT including DBI, need to look into that 
          mutate(income=income+income.gift) %>% 
          mutate(net.income = rowMaxs(cbind((income/12) - 90,0))) %>%
          mutate(net.income = rowMaxs(cbind(net.income - 600,0))) %>%
          mutate(net.income = rowMaxs(cbind(net.income*EarnedIncomeDisregard,0))) %>% 
          
          # Step II: Calculate value of the benefit
          mutate(tanfValue=0,
                 subset=totalassets<AssetTest) %>% 
          mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit2-net.income,0)),tanfValue)) %>% 
          
          # Apply gross income test (if applicable) & net income / standard test (if applicable)
          
          mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
          
          mutate(tanfValue=ifelse(net.income>StandardOfNeed2,0,tanfValue)) %>% 
          
          # Two parent TANF policy: if 'Yes' then two parent homes are eligible
          mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) %>% 
          
          # In some states TANF is not available for childless adults
          
          mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No",0,tanfValue ))
        
        temp<-rbind(temp_countys, temp_othercountys)
        
        temp$tanfValue[temp$tanfValue < 10] <- 0
        
        temp$tanfValue <- temp$tanfValue*12
        
        temp <- rbind(temp_adults, temp)
        
        rm(temp_adults)
        rm(tanfData_CA) # ER: Austin, let me know if this will be alright to include.
      }else{
        rm(temp_adults)
      }
      
    }else{
      abc <- 0
      
    }
    
    
    householdids <- unique(temp$householdid)
    # 
    # Merge back
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
    
  }
  
  
  # Colorado----
  
  state_code=8
  if(state_code %in% unique(data$stateFIPS)){ 
    
    
    # step 1 - filter based on state_code 
    temp<-data[data$stateFIPS==8,]
    
    # step 2 - join w/ tanf data
    temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
    
    # Step 3: add gift income
    temp$income=temp$income+temp$income.gift 
    
    # add gift income 
    householdids <- unique(temp$householdid)
    
    # For applicants, deduct $90 and compare to Standard of Need. For recipients and benefits computation, deduct 67% of earned income (NOT including the $90 deduction)
    # we should ignore family members that earn ssi, but we don't currently do that 
    temp$net.income<-rowMaxs(cbind((temp$income - temp$EarnedIncomeDisregard*temp$income)/12,0)) # earned income deduction
    
    # add ssdi as unearned income
    temp$net.income<-temp$net.income+(temp$value.ssdi/12)
    
    # subset so we exclude those who exceed the asset limit 
    temp$tanfValue<-0
    subset<-temp$totalassets<temp$AssetTest
    
    # Colorado maximum benefits amounts depend on the number of parents in the household 
    temp<-temp %>% 
      mutate(tanfValue=ifelse(subset & numadults<2 & !is.na(numadults), rowMaxs(cbind(Maxbenefit - net.income,0)),tanfValue), 
             tanfValue=ifelse(net.income>StandardOfNeed & numadults<2 & !is.na(numadults), 0, tanfValue),
             tanfValue=ifelse(subset & numadults >= 2 & !is.na(numadults), rowMaxs(cbind(Maxbenefit2 - net.income,0)),tanfValue),
             tanfValue=ifelse(net.income>StandardOfNeed2 & numadults >= 2 & !is.na(numadults), 0, tanfValue),
             tanfValue=ifelse(is.na(numadults),0, tanfValue),
             net.income=ifelse(is.na(numadults),0, net.income))
    
    
    # Two parent TANF policy: if 'Yes' then two parent homes are eligible
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    # CO and TX allow for childless household to receive TANF
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Gross Income test 
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # Make yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Colorado has a 60 month time limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
    
    
  }
  
  
  # Connecticut----
  
  state_code=9
  if(state_code %in% unique(data$stateFIPS)){# make sure that state is in the list
    
    # filter by state code
    temp<-data[data$stateFIPS==9,]
    # join w/ tanfdata
    temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
    
    #create list of unique of householdids (for cross section data)
    householdids <- unique(temp$householdid)
    
    # Step I: Calculate net income (only used for applicants) (we have this calculated, but don't actually check for it since we assume that we are dealing with only recipients)
    temp$net.income<-rowMaxs(cbind((temp$income/12) - temp$EarnedIncomeDisregard,0)) # earned income deduction
    
    # value.ssdi is used for unearned income 
    temp$net.income<-temp$net.income+(temp$value.ssdi/12)
    # Step II: Calculate value of the benefit - Connecticut provides full amount of benefit to those who have total net income (earned + unearned) less than 100% FPL
    temp$tanfValue<-0
    subset<-temp$totalassets<temp$AssetTest
    temp$tanfValue[subset]<-rowMaxs(cbind(temp$Maxbenefit[subset],0))
    
    # Apply gross income test (if applicable)
    
    #Gross Income Test
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # Two parent TANF policy: if 'Yes' then two parent homes are eligible
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # If income or tanfValue are NA, make them 0
    temp$income[is.na(temp$income)] <- 0
    temp$tanfValue[is.na(temp$tanfValue)] <- 0
    
    # make tanfValue yearly 
    
    temp$tanfValue <- temp$tanfValue*12
    
    # carry tanfValue to a different column, needed given how Connecticut does it's time limit 
    temp$tanfValue2 <- temp$tanfValue
    
    # see if there are multiple observations in the data, and see if "Year" data is in the data
    if(nrow(temp)>1 & "Year" %in% colnames(temp) ){
      years <- unique(temp$Year)
      
      # see if there is more than 1 unique year in the dataset, necessary for the time series information 
      if(length(years)>1){
        
        # group together the household ids and see what each household's starting point is (i.e. minimum year value)
        
        # Connecticut has a 21 month time limit for tanf, with two six-month extensions if the household observed earns an income value less than the maximum tanf benefit value (based on their family size). 
        # Once the first extension is used (if applicable), this requirement is re-analyzed. If the household is still earning less than the maximum tanf benefit value, they qualify for the second extension.
        # After the second extension (if applicable), the household no longer qualifies for tanf benefits 
        # We understand there are exceptions to this and more extensions may be applied, but we only include the income-relevant extensions for now 
        temp <- temp %>%
          group_by(householdid) %>%
          mutate(min_Year=min(Year)) %>% 
          mutate(tanfValue2=ifelse(Year>(min_Year+2), 0, tanfValue2))
        
        # see if the data observed is time series data 
        temp_adults <- temp[temp$Year <= temp$min_Year,]
        
        if(nrow(temp_adults) != nrow(temp)){
          
          # read in the time limit variable 
          timeLimit <- unique(temp$timeLimit)
          if(timeLimit == 21){
            abc <- 1
            
            # establish dummy flags that are set to 1 depending on the income of the family, as well as what Year is being observed relative to the household's initial year 
            temp$flag_1 <- 0
            temp$flag_2 <- 0
            temp$flag_3 <- 0
            temp$flag_4 <- 0
            
            
            temp <- temp %>%
              # group_by(householdid) %>%
              
              mutate(flag_1=ifelse(income/12 >= Maxbenefit, 1, 0)) %>%
              mutate(flag_2=ifelse(income/12 <  Maxbenefit, 1, 0)) %>%
              mutate(flag_3=ifelse((Year==min_Year+1), 1, 0)) %>%
              mutate(flag_4=ifelse((Year==min_Year+2), 1, 0)) 
            
            
            temp$tanfValue2[temp$flag_1 == 1 & temp$flag_3 == 1] <- 0.75*temp$tanfValue2[temp$flag_1 == 1 & temp$flag_3 == 1]
            temp$tanfValue2[temp$flag_1 == 1 & temp$flag_4 == 1] <- 0*temp$tanfValue2[temp$flag_1 == 1 & temp$flag_4 == 1]
            
            temp$tanfValue2[temp$flag_2 == 1 & temp$flag_3 == 1] <- 1*temp$tanfValue2[temp$flag_2 == 1 & temp$flag_3 == 1]
            temp$tanfValue2[temp$flag_1 == 1 & temp$flag_4 == 1] <- 0.25*temp$tanfValue2[temp$flag_1 == 1 & temp$flag_4 == 1]
            
            temp$tanfValue2[temp$flag_2 == 1 & temp$flag_3 == 1] <- 1*temp$tanfValue2[temp$flag_2 == 1 & temp$flag_3 == 1]
            temp$tanfValue2[temp$flag_2 == 1 & temp$flag_4 == 1] <- 0.75*temp$tanfValue2[temp$flag_2 == 1 & temp$flag_4 == 1]
            
            
            
          }else{
            abc <- 0
          }
        }
        abc <- 1
      }else{
        abc <- 0
      }
    }
    
    # carry over the time series calculated tanf value to the data 
    
    temp$tanfValue <- temp$tanfValue2
    temp$tanfValue2 <- NULL
    
    # if data is cross section, carry it back to the original dataset so that it matches the original household id observed 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    # 
    
    
    
  }
  
  
  # Delaware ----
  
  state_code=10
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    # filter data by state code 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create list for unique household ids 
    
    householdids <- unique(temp$householdid)
    
    # join data w/ tanfData
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income
      mutate(income=income+income.gift) %>% 
      
      # subtract $90 for work related expense
      mutate(net.income = rowMaxs(cbind((income/12)-90,0))) #%>%
    
    # Currently we do not subtract up to $200 for child care, or the first $50 of child support received to determine the net income. Nor do we account for the fact that
    # Non-parent caretakers can elect to be payee for related children in their care. The income of these caretakers does not count but income of the children may count.
    
    # We do subtract an additional $30 for the first year and the 33% of the remaining income for the first four months.
    
    temp <- temp %>%
      group_by(householdid) %>%
      mutate(min_Year=min(Year)) %>% 
      mutate(net.income=ifelse(Year==(min_Year), net.income - 30 - 0.25*EarnedIncomeDisregard*(net.income - 30), net.income))
    
    
    
    # We add ssdi as unearned income to be added to earned income, for the gross income test
    temp$net.income<-temp$net.income+(temp$value.ssdi/12)
    
    # If I'm not mistaken, Delaware, to calculate the benefit amount, takes the calculated net income, subtracts it from the Standard of Need and divide by 2. 
    # The benefit amount is then the minimum of the new "net.income" or the Maximum benefit amount. If income is 0, then the maximum benefit value is the amount received. 
    # Need to check if this is true, but last time I checked the most recent Delaware rules, this was the case. If this is incorrect, then will fix. 
    temp$net.income<-temp$StandardOfNeed-temp$net.income
    temp$net.income[temp$net.income<0] <- 0
    temp$net.income <- temp$net.income*0.5
    temp <- temp %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMins(cbind(Maxbenefit,net.income)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) #%>% 
    
    #mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) 
    
    # Two parent TANF policy: if 'Yes' then two parent homes are eligible
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Minimum Benefit amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # Make tanf Value yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Delaware has a 36 month time limit 
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+2), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  
  # District of Columbia ----
  
  state_code=11
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    # filter based on the state code & join the data w/ the tanf information 
    
    temp<-data[data$stateFIPS==11,]
    
    temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
    
    householdids <- unique(temp$householdid)
    
    
    # Step I: Calculate net income
    
    temp$income <- temp$income+temp$income.gift # add gift income
    
    # Apply TANF child care income deduction (175 for kids in childcare + $25 for <2). We do not currently include this in the benefit / net income calculation 
    # temp$numkidsunder13 <- rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=12,na.rm=TRUE)
    #temp$numkidsunder2 <- rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=1,na.rm=TRUE)
    
    #   temp$childcareDeduction<-temp$numkidsunder13*175+temp$numkidsunder2*25 *CHECK THIS LINE *UNDER 2 SHOULD BE 200 NOT 25
    
    # DC Net income is calculated as income minus $160, and then subtract 67% of the remaining income 
    
    temp$net.income<-rowMaxs(cbind(temp$income-(12*160+temp$EarnedIncomeDisregard*(temp$income-12*160)),0)) # earned income deduction
    
    # value.ssdi is added to income as "unearned income" 
    temp$net.income<-temp$net.income+(temp$value.ssdi)
    # Step II: Calculate value of the benefit
    temp$tanfValue<-0
    subset<-temp$totalassets<temp$AssetTest
    temp$tanfValue[subset]<-rowMaxs(cbind(12*temp$Maxbenefit[subset] - temp$net.income[subset],0))
    
    # Apply gross income test (if applicable) 
    temp$tanfValue[((temp$income+temp$value.ssdi)/12)>temp$grossIncomeTest]<-0
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Merge back
    
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
    
  }
  
  
  # Florida ----
  
  state_code=12
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    # filter based on state code 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # unique household ids list (for cross section data)
    householdids <- unique(temp$householdid)
    
    # join data w/ tanfData
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - Florida subtracts $90 (for both applicants & recipients), 
      # then additional $200 , then half of remaining income
      mutate(income=income+income.gift) %>% 
      mutate(net.income=income/12-(90+200))%>% 
      mutate(net.income = rowMaxs(cbind(net.income*EarnedIncomeDisregard,0))) %>% 
      
      # add ssdi value as unearned income
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Florida has a 48 month lifetime limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+3), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge tanf results to original data
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  
  # Georgia ----
  
  state_code=13
  if(state_code %in% unique(data$stateFIPS)){
    
    # filter data based on state code 
    temp<-data %>% 
      filter(stateFIPS==state_code)
    
    # unique household ids list for cross section data
    householdids <- unique(temp$householdid)
    
    # join data w/ tanfData 
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Childcare deductions. We do not currently use this in our calculations 
      
      #   mutate(numkidsunder13 = rowSums(cbind(agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12)<13,na.rm=TRUE)) %>% 
      #  mutate(numkidsunder2 = rowSums(cbind(agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12)<2,na.rm=TRUE)) %>% 
      #   mutate(childcareDeduction = numkidsunder13*175 + numkidsunder2*25) %>%
      
      # Step I: Calculate net income - Georgia disregards $250 from earned income 
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12) - EarnedIncomeDisregard,0))) %>% 
      
      # count ssdi value as unearned income and add to earned income to get total net income 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) %>% 
      
      # In some states TANF is not available for childless adults
      mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No",0,tanfValue ))
    
    # Minimum Benefit Amount 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # Make tanf value yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Georgia has a 48 month lifetime limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+3), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge generated tanf results back to original data
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
    
  }
  
  # Hawaii ----
  
  state_code=15
  
  # filter based on state code & join w/ tanf Data
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) #%>% 
    
    
    
    # unique household ids list for cross section data
    householdids <- unique(temp$householdid)
    
    
    # Step I: Calculate net income
    
    # Hawaii first deducts the current earned income by 20%, then subtracts $200.
    # Then, depending on the year, the remaining income is deducted by a percentage of income 
    # For the first two years, the income is deducted by 55%. The remaining years see income deducted by 36% instead 
    
    temp <- temp %>%   
      mutate(income=income+income.gift) %>% 
      mutate(net.income=.80*(income/12)) %>% 
      mutate(net.income=net.income-200)#%>%
    
    temp<-temp %>%
      group_by(householdid) %>%
      mutate(min_Year=min(Year)) %>%
      mutate(net.income=ifelse(Year == min_Year,net.income-0.55*net.income,net.income)) %>%
      mutate(net.income=ifelse(Year == min_Year+1,net.income-0.55*net.income,net.income)) %>%
      mutate(net.income=ifelse(Year == min_Year+2,net.income-0.36*net.income,net.income)) %>%
      mutate(net.income=ifelse(Year == min_Year+3,net.income-0.36*net.income,net.income)) %>%
      mutate(net.income=ifelse(Year == min_Year+4,net.income-0.36*net.income,net.income)) %>%
      select(-c(min_Year))
    
    temp$net.income[temp$net.income < 0] <- 0
    
    
    # add ssdi to the remaining earned income - as "unearned income" - to get total net income
    temp <- temp %>%
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      # should deduct $175 a month for each incapacitated adult if said adult is employed full-time, or $165 if applicant is employed part-time
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit-net.income),0)),tanfValue)) #%>% 
    
    # The first two months have a higher benefit maximum than the remaining time (for applicants and those recipients just starting tanf for the first time)
    temp<-temp %>%
      group_by(householdid) %>%
      mutate(min_Year=min(Year)) %>%
      mutate(tanfValue = ifelse(Year==min_Year,(1/6)*(Maxbenefit2-net.income)+(5/6)*(Maxbenefit - net.income),tanfValue))
    
    # Apply gross income test (if applicable) & net income / standard test (if applicable)
    
    temp$tanfValue[temp$tanfValue < 0] <- 0
    
    # Standard of need test for net income
    temp <- temp %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))
    
    # Some states don't allow married households to receive tanf
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Gross Income Test
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # Minimum Benefit amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # Make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Hawaii has a 60 month lifetime limit for receiving tanf 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # Merge tanf results back into original data
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  # Idaho ----
  
  state_code=16
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    
    # filtr data based on state code
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # unique household ids list for cross section data
    householdids <- unique(temp$householdid)
    
    # join w/ tanf data 
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - Idaho deducts 60% of gross income
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - (income*EarnedIncomeDisregard/12),0))) %>% 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Minimum benefit amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make tanf yearly instead of monthly 
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Idaho has a 24 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+1), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # carry back tanf result to original data
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  # Illinois----
  
  # NOTE: Illinois has applicants pass a test where family income must not exceed the Assistance Payment Level (MaxBenefit).
  # A disregard is applied to family income, the amount being the difference being the Assistance Payment Level for the family size and 50% of FPL for the family size 
  # Based on what we've read, recipients don't have to worry about a net income test, but will correct if we misinterpreted the rules. 
  
  state_code=17
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # unique household ids list for cross section
    householdids <- unique(temp$householdid)
    
    # join w/ tanf Data
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Add Gift Income
      mutate(income=income+income.gift) %>% 
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      # Benefit allotment determined by Max Benefit Amount minus 75% of gross income 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-(EarnedIncomeDisregard*(income/12)),0)),tanfValue)) #%>% 
    
    # Apply gross income test (if applicable) & net income / standard test (if applicable). Neither apply to Illinois to our knowledge
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Minimum Benefit Amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # convert tanf to yearly values
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Illinois has a 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back w/ original data
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  # Indiana ----
  
  state_code=18
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    
    # NOTE: IT APPEARS THAT INDIANA DOES A SIMILAR THING TO CALIFORNIA, REMOVING ADULTS FROM THE EQUATION AFTER THE STATE LIFETME LIMIT (in THIS CASE, 24 MONTHS), WHILE CHILDREN CAN STILL RECEIVE UP TO THE AGE LIMIT
    # WE DO NOT CURRENTLY INCLUDE THIS IN OUR CALCULATIONS FOR INDIANA, WHILE WE DO FOR CALIFORNIA 
    
    # make household ids list for cross section data
    householdids <- unique(temp$householdid)
    
    # join w/ tanf Data
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - 
      # Indiana subtracts $90 for work expense, $30 for the first year, and 33% of the remaining income for the first four months
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12)-90,0))) #%>% 
    
    temp$net.income[temp$net.income < 0] <- 0
    
    temp <- temp %>%
      group_by(householdid) %>%
      mutate(min_Year=min(Year)) %>% 
      mutate(net.income=ifelse(Year==(min_Year), net.income - 30 - 0.25*0.33*(net.income - 30), net.income)) %>%
      # add ssdi as "unearned income" to net earned income to get total net income
      mutate(net.income=net.income+(value.ssdi/12))
    
    
    # net income that is less than 0 should be set to 0
    temp$net.income[temp$net.income < 0] <- 0
    
    
    temp <- temp %>%
      # Step II: Calculate value of the benefit
      # Indiana calculates tanf benefits as the Maximum benefit minus 75% of gross income 
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-(income*(EarnedIncomeDisregard)/12),0)),tanfValue)) #%>% 
    
    # Apply gross income test (if applicable) & net income / standard test (if applicable)
    
    temp <- temp %>%
      
      mutate(tanfValue=ifelse((income)/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) 
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Minimum Benefit amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # maake tanf values yearly
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Indiana has a 24 month lifetime limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+1), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back tanf results to original data
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  # Iowa ----
  
  state_code=19
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create household ids list for cross section data
    householdids <- unique(temp$householdid)
    
    # join w/ tanf Dataset
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - deduct 20% of gross income 
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - (income*EarnedIncomeDisregard/12),0))) %>% 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit - 
      # if person passes the net income test, then deduct 50% from the remaining net income, and then compare against Max Benefit Amount 
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-.50*net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) 
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Minimum Benefit Amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Iowa has a 60 month lifetime limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  
  # Kansas----
  
  state_code=20
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create householdid list for cross section data
    householdids <- unique(temp$householdid)
    
    # join w/ tanf Data
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - $90 deduction, and then 60% of the remaining income
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12) - 90 - EarnedIncomeDisregard*((income/12)-90),0))) %>% 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      
      # NOTE:
      # The amount of direct cash assistance paid to a family is based on the difference between the need standard and the countable income post deductions 
      
      # In Kansas (last we checked), Benefit amount is based on county (rural or not), cost in county, and population of county. 
      # For simplicity, we just go w/ the high population, high cost counties. 
      # The chart containing all values is in the state plan in our Rules.TANF folder. 
      
      # Kansas also bases their standard of need & benefit amount on a "Shared Living Arrangement", 
      # where a situation in which the household contains an individual who is at least 18 years of age and not a member of the assistance unit; 
      # or a situation in which the assistance unit receives a subsidy for shelter expenses, resides in public housing, receives assistance for the payment of shelter costs from someone not residing in the home, is not solely responsible for the assistance unit's shelter costs, or has an SSI recipient residing in the home. 
      # We do not currently account for this.
      
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(StandardOfNeed-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))  %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0 , tanfValue))
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Minimum Benefit Amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Kansas has a 24 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+1), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  # Kentucky ----
  
  state_code=21
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==21,]
    
    # create household ids list for cross section data
    householdids <- unique(temp$householdid)
    
    # join data w/ tanf Data
    temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
    
    temp$income=temp$income+temp$income.gift # add gift income
    
    
    # Step I: Calculate net income
    # $90 work expense standard, $50 for child support exclusion (only if child support is included as unearned income), and $175 per child (2 yrs or older) in care 
    # We do not currently account for the childcare or child support deductions
    # Also subtract additional $30 for first year and 33% of remaining income for first four months. Once these disregards are applied, compare to Standard of Need amount
    
    # The family’s income is subtracted from the Standard of Need to  arrive at the deficit amount 
    # The deficit amount is multiplied by 55 percent (45 percent ratable reduction)
    # the benefit amount is the lesser of this rate reduction or the maximum benefit. If rate reduction is = $0, then benefit amount is the Maximum Benefit
    
    # NOTE: To our knowledge Kentucky updated their amounts in Mar 2023. We have not accounted for these yet. 
    
    temp$net.income <- rowMaxs(cbind((temp$income/12)-90,0)) # earned income deduction: work expense
    temp$rate.reduction<-rowMaxs(cbind(temp$EarnedIncomeDisregard*(temp$StandardOfNeed - temp$net.income),0)) # earned income deduction
    temp$rate.reduction[temp$income <= 0] <- temp$Maxbenefit[temp$income <= 0]
    # Step II: Calculate value of the benefit
    temp$tanfValue<-0
    subset<-temp$totalassets<temp$AssetTest
    temp$tanfValue[subset]<-rowMins(cbind(temp$Maxbenefit[subset],temp$rate.reduction[subset]))
    
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Gross Income Test
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # Standard of Need Test
    temp <- temp %>%
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))
    
    # Minimum Benefit Amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # Important: One-time exclusion of the first two months earned income per adult, at individual's option. Quote from the ESP doc:
    # "If the recipient becomes newly employed or reports increased wages acquired after approval they are offered a one-time 2-month disregard of all earnings. 
    # The two-month earned income exclusion begins the first month cash assistance benefits would be affected had the earnings from employment not been excluded."
    # We do not currently include this 
    
    # Make tanf yearly values
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Kentucky has a 60 month lifetime limit 
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back to original data 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  
  # Louisiana ----
  
  # NOTE: There is a one-time, six-month deduction of $900 that can be applied. However, it need not apply consecutively
  
  state_code=22
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code)
    
    # unique household ids for cross section data
    householdids <- unique(temp$householdid)
    
    # join data w/ tanf DATA
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize"))# %>% 
    
    # LA has three types of deductions
    #  1. Standard earned income deduction of $120 for each employed member.  
    #  2. Dependent care deductions may be allowable for each employed member.  Certain requirements must be met to qualify for this deduction.
    #  3. Time-limited deduction of $900 for six months for each employed member.
    
    # We currently only incorporate the first and third deductions, and we assume the third deduction is applied in the first six months of receiving benefits
    
    #$120 per WORKING adult. We measure income for up to 6 adults, so each flag is created, = 1 if an adult's income > 0, and 0 otherwise. 
    temp$income1_flag <- 0
    temp$income2_flag <- 0
    temp$income3_flag <- 0
    temp$income4_flag <- 0
    temp$income5_flag <- 0
    temp$income6_flag <- 0
    
    temp$income1_flag[temp$income1 > 0] <- 1
    temp$income2_flag[temp$income2 > 0] <- 1
    temp$income3_flag[temp$income3 > 0] <- 1
    temp$income4_flag[temp$income4 > 0] <- 1
    temp$income5_flag[temp$income5 > 0] <- 1
    temp$income6_flag[temp$income6 > 0] <- 1
    
    temp <- temp %>%
      # Step I: Calculate net income
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12) - EarnedIncomeDisregard*(temp$income1_flag + temp$income2_flag + temp$income3_flag + temp$income4_flag + temp$income5_flag + temp$income6_flag),0))) %>%
      mutate(net.income = ifelse(net.income < 0, 0, net.income)) %>%
      group_by(householdid) %>% 
      mutate(min_Year=min(Year)) %>% 
      mutate(net.income=ifelse(Year==(min_Year), 
                               net.income-rowMaxs(cbind((income/12) - 900*(temp$income1_flag + temp$income2_flag + temp$income3_flag + temp$income4_flag + temp$income5_flag + temp$income6_flag),0)), net.income)) %>%
      mutate(net.income = ifelse(net.income < 0, 0, net.income)) %>%
      # Add ssdi as unearned income to net income to get total net income 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income > StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy=="No", 0, tanfValue)) %>% 
      
      # In some states TANF is not available for childless adults
      
      mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No", 0, tanfValue ))
    
    # Minimum benefit amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # Make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES
    
    # LA has a 60 month lifetime limit, but tanf can only be received 24 months consecutively in a 60 month span. 
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(year_index=Year-min(Year)) %>%
        mutate(year_index=year_index %% 5) %>%
        mutate(tanfValue=ifelse(year_index > 1, 0, tanfValue)) %>%
        mutate(tanfValue=ifelse(Year>(min_Year+10), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # Merge back
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  # Maine ----
  
  state_code=23
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create household ids list for cross section data
    householdids <- unique(temp$householdid)
    
    # join data w/ tanf data
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - Maine subtracts $108 and then halfs the remaining income to get net earned income
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12) - 108 - (EarnedIncomeDisregard*((income/12)) - 108),0))) %>% 
      mutate(net.income=net.income+(value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) %>% 
      
      # In some states TANF is not available for childless adults
      mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No",0,tanfValue ))
    
    # Minimum Benefit Value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    # make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Maine has a 60 month time limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  
  
  # Maryland----
  
  state_code=24
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    temp<-data[data$stateFIPS==24,]
    
    # create household id list for cross section data
    householdids <- unique(temp$householdid)
    
    # join w/ tanf data
    temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
    
    # Add gift income
    temp$income=temp$income+temp$income.gift
    
    # Step I: Calculate net income - First take 20% of gross earned income and compare to SON; 
    # if passed, then do 40% + deductions for child care to determine actual amount. The 20% becomes 50% if person is self-employed
    # Not 100% sure if the deduction for childcare requires some level of childcare expenses to be had, or if it's just that amount per child. Assuming the latter for now.
    # Childcare deduction is $200 per child if working more than 100 hours per month OR + 100 per child if working less than 100 hours per month. Assuming the former for now. 
    temp$net.income<-rowMaxs(cbind((temp$income/12)
                                   -(temp$EarnedIncomeDisregard*2*temp$income/12)-200*temp$numkids,0)) # earned income deduction
    temp$net.income[temp$net.income < 0] <- 0
    temp$net.income <- temp$net.income + (temp$value.ssdi/12)
    # Step II: Calculate value of the benefit
    temp$tanfValue<-0
    subset<-temp$totalassets<temp$AssetTest
    temp$tanfValue[subset]<-rowMaxs(cbind(temp$Maxbenefit[subset] - temp$net.income[subset],0))
    
    # Apply gross income test (if applicable)
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    temp$tanfValue[(temp$net.income/12)>temp$StandardofNeed]<-0
    
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Minimum Benefit Value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Maryland has a 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # Merge back
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  #
  # Massachusetts----
  
  state_code=25
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create household id list for cross section data
    householdids <- unique(temp$householdid)
    
    # join data w/ tanf Data
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - $200 if working + 50% of remaining earnings if having gotten TAFDC in the past 4 months; then compare to the gross income limit / standard of need
      # We assume at least one adult is working, and that the household does not have a reduced standard of need amount for receiving public / subsidized housing 
      
      mutate(income=income+income.gift) %>% # add gift income
      mutate(net.income = rowMaxs(cbind((income/12) - 200 - EarnedIncomeDisregard*((income/12)-200),0))) %>% # calculate net income
      # add ssdi as unearned income to net income to get total net income
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) 
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0 
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # minimum benefit amount 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Massachusetts doesn't have a lifetime limit, but does have an intermittent limit of 24/60 months for non exempt units
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(year_index=Year-min(Year)) %>%
        mutate(year_index=year_index %% 5) %>%
        mutate(tanfValue=ifelse(year_index > 1, 0, tanfValue))# %>%
      
    }else{
      abc <- 0
    }
    
    
    # merge back
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  
  # Michigan ----
  
  state_code=26
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create household id list for cross section data
    householdids <- unique(temp$householdid)
    
    # join w/ tanf data
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - $200 + a % of the remainder of each employed person's monthly earned income. For applicant's, this is 20%; for recipient's, this is 50%
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - 200 - EarnedIncomeDisregard*((income/12)-200),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes",0,tanfValue))
    
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0 
    
    # minimum tanf value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - 60 month lifetime limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  
  # Minnesota----
  
  state_code=27
  if(state_code %in% unique(data$stateFIPS)){ 
    #
    # MINNESOTA IS WEIRD IN THAT THEY HAVE A VERY SPECIFIC FOOD & CASH PORTION OF THEIR TANF PROGRAM, MFIP / DWP
    # IF I RECALL CORRECTLY, THEY CALCULATE THE FOOD PORTION FIRST, THEN THE CASH PORTION, AND THEN IF THERE IS NO FOOD PORTION, A RECEPIENT CAN QUALIFY FOR TANF
    # HOWEVER, IF THEY EARN ANY OF THE FOOD PORTION, A PERSON IS NOT QUALIFIED FOR TANF
    # OUR CURRENT SETUP DOES NOT ALLOW FOR THAT TO BE CALCULATED AND DETERMINED
    # FOR NOW, WE JUST CALCULATE THE CASH PORTION OF TANF 
    
    
    # filter based on state code 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # unique household ids list for cross section data
    householdids <- unique(temp$householdid)
    
    # join data w/ tanf Data
    temp <- temp %>%   
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income
      
      # NOTE: the $65 deduction should be per working adult that earns income. we do not currently account for that
      
      
      
      mutate(income=income+income.gift) %>% # add gift income
      mutate(net.income=(income/12)-65-(70*famsize)-35) %>%  # disregard $ 65 for earned income deduction. 
      #The rules do mention a $70 deduction per family member, but I cannot locate it in the rules. Could've sworn it was there . I think it's like a family electronics / phone deduction
      
      mutate(net.income=net.income*EarnedIncomeDisregard) %>% # divide remaining income by half
      mutate(net.income = net.income + (value.ssdi/12))
    
    temp$net.income[temp$net.income < 0] <- 0
    
    # Step II: Calculate value of the benefit
    temp <- temp %>%
      
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) #%>%
    
    temp$tanfValue[temp$subset==TRUE]<-rowMaxs(cbind(temp$Maxbenefit[temp$subset==TRUE] - temp$net.income[temp$subset==TRUE], 0)) # tanf can't be negative 
    
    temp$tanfValue[temp$tanfValue < 0] <- 0
    max_benefit <- as.numeric(unique(temp$Maxbenefit))
    temp$tanfValue[temp$tanfValue > temp$Maxbenefit] <- max_benefit # tanf value can't be greater than max value 
    
    # Apply gross income test (if applicable) & net income / standard test (if applicable)
    temp <- temp %>% 
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # minimum tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  # Mississippi----
  
  state_code=28
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create household id list for cross section data
    householdids <- unique(temp$householdid)
    
    # join data w/ tanf data
    temp <- temp %>%  
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income
      # Recipients are eligible for a one-time 100 percent disregard for six months if they find employment of 35 hours a week within 30 days of either their initial approval for TANF or the beginning of job-readiness training. 
      # If work is not found, the recipient will never be eligible to receive the disregard again. 
      #An additional 100 percent disregard is available to units for three months when the unit's case is subject to closure because of increased earnings and the individual is employed for at least 25 hours a week at the federal minimum wage or higher. 
      #The recipient may not have already received the six-month disregard, unless there has been at least a 12-month break in receipt of TANF benefits. 
      #The three-month disregard may be received more than once during the 60-month TANF benefit period, provided there is a period of at least 12 consecutive months in which a family does not receive TANF benefits before the family reapplies for assistance. 
      #If a recipient marries for the first time, his or her new spouse may receive a one-time 100 percent disregard for six consecutive months.
      
      # For now we just include th $90 disregard and the six-month income disregard
      mutate(income=income+income.gift) %>% 
      mutate(net.income=(income/12)-EarnedIncomeDisregard) %>%
      mutate(net.income = net.income + (value.ssdi/12))
    
    
    temp <- temp %>%
      group_by(householdid) %>%
      mutate(min_Year=min(Year)) %>% 
      mutate(net.income=ifelse(Year==(min_Year), 0.5*net.income, net.income))
    
    
    # Step II: Calculate value of the benefit
    temp$net.income[temp$net.income < 0] <- 0
    
    temp <- temp %>%
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # minimum benefit value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Mississippi has a 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  # Missouri----
  
  state_code=29
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    # If someone IS in Kansas City, then it's a $90 disregard (the one done for application), a $30 disregard, and then deduct 33% of the remaining income. 
    #The latter two are done for four years. 
    
    #If someone is NOT in Kansas City and they become employed DURING receiving assistance (or were employed before I think), it's $90 disregard for the whole time and 66.7% of the remaining income for the first year. 
    #For those NOT in Kansas City and has NOT become employed during receiving assistance, it's $90 disregard, $30 for the first year, and then 33.3% of the remaining income for the first four months 
    
    # we condition on whether or not a recipient lives in Jackson County (main county for Kansas City) or not
    
    temp_jc<-data %>% 
      filter(stateFIPS==state_code & countyortownName== "Jackson County") 
    
    # create household id list for cross section data
    householdids <- unique(temp_jc$householdid)
    
    temp_jc <- temp_jc %>%  
      left_join(tanfData, by=c("stateFIPS", "famsize"))# %>% 
    #arrange(Year)
    
    if(nrow(temp_jc)>0){
      
      # Step I: Calculate net income
      temp_jc <- temp_jc %>%
        mutate(income=income+income.gift) %>% 
        mutate(net.income=(income/12)-90) #%>% 
      
      temp_jc <- temp_jc %>%
        group_by(householdid) %>%
        mutate(min_Year=min(Year)) %>% 
        mutate(net.income=ifelse(Year==min_Year,net.income-30-EarnedIncomeDisregard*(net.income-30),net.income)) %>%
        mutate(net.income=ifelse(Year==min_Year+1,net.income-30-EarnedIncomeDisregard*(net.income-30),net.income)) %>%
        mutate(net.income=ifelse(Year==min_Year+2,net.income-30-EarnedIncomeDisregard*(net.income-30),net.income)) %>%
        mutate(net.income=ifelse(Year==min_Year+3,0.75*(net.income-30-EarnedIncomeDisregard*(net.income-30)),net.income))
      
      
      
      temp_jc <- temp_jc %>%
        mutate(net.income = net.income + (value.ssdi/12)) %>%
        
        # Step II: Calculate value of the benefit
        mutate(tanfValue=0,
               subset=totalassets<AssetTest) %>% 
        mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit - net.income),0)),tanfValue)) %>% 
        
        # Apply gross income test (if applicable) & net income / standard test (if applicable)
        
        mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))  %>% 
        
        mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
        
        # TANF is not available for two married adults
        
        mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
      
    }
    
    # recipints in any other county BUT Jackson 
    
    temp_njc<-data %>% 
      filter(stateFIPS==state_code & countyortownName!= "Jackson County" ) 
    
    householdids <- unique(temp_njc$householdid)
    
    temp_njc <- temp_njc %>%  
      left_join(tanfData, by=c("stateFIPS", "famsize"))# %>% 
    
    
    
    if(nrow(temp_njc)>0){
      
      temp_njc <- temp_njc %>%
        
        # Step I: Calculate net income
        mutate(income=income+income.gift) %>% 
        mutate(net.income=(income/12)-90)
      
      temp_njc <- temp_njc %>%
        group_by(householdid) %>%
        mutate(min_Year=min(Year)) %>% 
        mutate(net.income=ifelse(Year==min_Year,net.income*EarnedIncomeDisregard,net.income))
      
      
      temp_njc  <-  temp_njc  %>% 
        mutate(net.income = net.income + (value.ssdi/12)) %>%
        
        # Step II: Calculate value of the benefit
        mutate(tanfValue=0,
               subset=totalassets<AssetTest) %>% 
        mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit - net.income),0)),tanfValue)) %>% 
        
        # Apply gross income test (if applicable) & net income / standard test (if applicable)
        
        mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))  %>% 
        
        mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
        
        # TANF is not available for two married adults
        
        mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
      
    }
    
    temp<-temp_njc
    
    
    # In some states TANF is not available for childless adults
    
    temp<-rbind(temp, temp_jc)
    
    householdids <- unique(temp$householdid)
    
    # states that don't allow tanf allotment if there are no children in the household
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # minimum tanf benefit amount
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - based on what we've read, Missouri has a 48 month lifetime limit, unless you're in Kansas City which is 45 months
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+3), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  # Montana----
  
  state_code=30
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    #create household id list for cross section data
    householdids <- unique(temp$householdid)
    
    temp <- temp %>%  
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - $200 work expense + 25% of the remaining earned income
      mutate(income=income+income.gift) %>% 
      mutate(net.income=income/12-(200))%>% 
      mutate(net.income = rowMaxs(cbind(net.income - EarnedIncomeDisregard*net.income,0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) %>% 
      
      # In some states TANF is not available for childless adults
      mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No",0,tanfValue ))
    
    # Min Tanf Value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - Montana has a 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # Merge back
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  # Nebraska----
  
  state_code=31
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create household id list for cross section data
    householdids <- unique(temp$householdid)
    
    # join w/ tanf Data 
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - Subtract 50% of total gross earned income, and compare to Payment Standard; do this for both ongoing eligibility & determing benefit amount
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - ((income/12)*EarnedIncomeDisregard),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Min Tanf Value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Nebraska has a 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  # Nevada----
  
  state_code=32
  if(state_code %in% unique(data$stateFIPS)){ 
    
    # filter based on state code 
    temp<-data %>% 
      filter(stateFIPS==state_code)
    
    # create unique household id list for cross section data 
    householdids <- unique(temp$householdid)
    
    # join w/ tanf Data 
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize"))
    
    # Step I: Calculate net income
    
    # First year & applicants: 100% deduction first three months, 85% deduction second three months, 75% deduction third three months, and 65% deduction last three months. 
    # Second year onwards: $90 deducted or 20% of gross income, whichever is greater. Compare to the Standard of Need
    
    temp<-temp %>% 
      group_by(householdid) %>% 
      mutate(min_Year=min(Year))
    
    temp <- temp %>%
      mutate(income=income+income.gift) %>% 
      mutate(income_month=income/12) %>% 
      mutate(first.year = ifelse(Year==min_Year,1,0)) %>% 
      mutate(net.income=ifelse(first.year==1, (income-(3*(1*income_month))-(3*(0.85*income_month))-(3*(0.75*income_month))-(3*(0.65*income_month)))/12 ,
                               ifelse((income/12)*EarnedIncomeDisregard>90, (income-income*EarnedIncomeDisregard)/12, (income/12)-90))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit - net.income),0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))  %>% 
      
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Gross Income Test
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # Minimum Tanf Value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make tanf value yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # Nevada has a 60 month limit. After each 24 months of benefit, there are 12 months of ineligibility
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(year_index=Year-min(Year)) %>%
        mutate(year_index=year_index %% 3) %>%
        mutate(tanfValue=ifelse(year_index > 1, 0, tanfValue)) %>%
        mutate(tanfValue=ifelse(Year>(min_Year+6), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  } 
  
  
  
  # New Hampshire----
  
  state_code=33
  if(state_code %in% unique(data$stateFIPS)){ 
    temp<-data %>% 
      filter(stateFIPS==state_code) # filter based on state code
    
    # create list for household ids for cross section data 
    householdids <- unique(temp$householdid)
    
    # join data to tanf Data
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize"))# %>% 
    
    # We do not currently include childcare deductions in our calculations 
    
    #   mutate(numkidsunder13 = rowSums(cbind(agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12)<=12,na.rm=TRUE)) %>% 
    #   mutate(numkidsunder6 = rowSums(cbind(agePerson1, agePerson2, agePerson3, agePerson4, agePerson5, agePerson6, agePerson7, agePerson8, agePerson9, agePerson10, agePerson11, agePerson12)<=5,na.rm=TRUE)) %>% 
    #   mutate(childcareDeduction = numkidsunder13*175 + numkidsunder6*25)
    
    
    # Step I: Calculate net income
    # For applicants, disregard 20% of gross earned income; for recipients and benefits computation, disregard 50% of gross income. 
    # Should include child care deduction but don't right now (for a parent earning more than $377 an hour, deduct $200 per month for each child less than 6 & $175 for each incapacitated parent and each child 6 and older; divide these deduction amounts by 2 if parent is working part-time instead). 
    
    temp <- temp %>%
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income - EarnedIncomeDisregard*income)/12,0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit - For applicants, disregard 20% of gross earned income; for recipients and benefits computation, disregard 50% of gross income. If, after deduction, the amount is less than the StandardofNeed, then subtract net income amount from MaxBenefit to get benefit amount
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Note: we don't differentiate between those who pay shelter and those who don't. We know that the standard of need is lower for those who have to pay shelter
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy=="No", 0, tanfValue ))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # minimum tanf value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - New Hampshire has a 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  # New Jersey----
  
  state_code=34
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    # create household id list for cross section data
    householdids <- unique(temp$householdid)
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) #%>%  join w/ tanf data 
    
    
    temp<-temp %>% 
      group_by(householdid) %>% 
      mutate(min_Year=min(Year)) 
    
    # Step I: Calculate net income - 
    
    # In computing the monthly cash assistance benefit, WFNJ/TANF allows for the application of certain disregards for earned income. 
    # If a recipient is employed for 20 hours or more per week, 100 percent of the gross earned income is disregarded for the first full month of employment, 
    # 75 percent is disregarded for six consecutive months and 50 percent is disregarded for each additional month of employment thereafter. 
    # If a recipient is employed for less than 20 hours per week, 100 percent of the gross earned income is disregarded for the first full month of employment and 
    # 50 percent is disregarded for each additional month of employment thereafter
    
    # We assume a recipient is employed for at least 20 hours per week
    temp <- temp %>%
      mutate(income=income+income.gift) %>% 
      mutate(income_month=income/12) %>% 
      mutate(first.year = ifelse(Year==min_Year,1,0)) %>% 
      mutate(net.income=ifelse(first.year==1, (income-(1*income_month)-(6*(0.75*income_month))-(5*(0.5*income_month)))/12,
                               (income-(12*(0.5*income_month)))/12
      )
      ) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit - net.income),0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))  %>% 
      
      
      # TANF is not available for two married adults
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # gross income test
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # min tanf value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES
    
    # This one works a bit differently - it seems that New Jersey allows for one to have up to 60 months lifetime, however the recipient MUST work after 24 months of receiving tanf benefits. 
    # The cumulative lifetime amount is 60 months. We do not currently account for whether or not a recipient has gained work after 24 months of receiving benefits. 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  # New Mexico----
  
  state_code=35
  if(state_code %in% unique(data$stateFIPS)){ # filter by state code 
    temp<-data %>% 
      filter(stateFIPS==state_code)
    
    # create househeold id list for cross section data 
    householdids <- unique(temp$householdid)
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      #arrange(Year) %>% 
      
      # Step I: Calculate net income
      
      # Subtract $125 from gross income if single parent, and $225 if it's a two-parent household. Then subtract 50% of the remaining income. $200 for children under 2,  $175 for children 2 and older
      # We do not currently account for childcare costs 
      # We are also assuming, in this case, that the parents are married, but if this is not actually required in the rules, we will change it 
      
      mutate(income=income+income.gift) %>% 
      mutate(net.income=ifelse(FilingStatus==1, (income/12)-125, ifelse(FilingStatus==2, (income/12)-225, (income/12)))) %>%
      mutate(net.income=net.income*EarnedIncomeDisregard) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit - net.income),0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))  %>% 
      
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Gross Income Test
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # Min Tanf Benefit
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # Make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - New Mexico has a 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # Merge Back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  # New York----
  
  state_code=36
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code)
    
    householdids <- unique(temp$householdid) # unique household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data
      
      # Step I: Calculate net income - deduct $90 work expense + 52% of remaining income
      mutate(income=income+income.gift) %>% 
      mutate(net.income=income/12-(90))%>% 
      mutate(net.income = rowMaxs(cbind(net.income - EarnedIncomeDisregard*net.income,0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) 
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    # make tanf yearly
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - New York has a 60 month lifetime limit 
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  # North Carolina ----
  
  state_code=37
  if(state_code %in% unique(data$stateFIPS)){ # filter based on state code 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # unique household id list for cross section data 
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% 
      
      # Step I: Calculate net income - Subtract 27.5% of gross earned income and compare to the Need Standard; 
      # the amount is calculated as 50% of the difference between the total countable income (after the 27.5% deduction) and the Need Standard
      
      
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - (income*EarnedIncomeDisregard/12),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(.50*(StandardOfNeed-net.income),0)),tanfValue)) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMins(cbind(tanfValue, Maxbenefit)), tanfvalue)) %>% 
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Min tanf value. North Carolina's min is $25
    
    temp$tanfValue[temp$tanfValue < 25] <- 0
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES: North Carolina has a 60 month limit. After each 24 months of benefit, there are 36 months of ineligibility
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(year_index=Year-min(Year)) %>%
        mutate(year_index=year_index %% 5) %>%
        mutate(tanfValue=ifelse(year_index > 1, 0, tanfValue)) %>%
        mutate(tanfValue=ifelse(Year>(min_Year+10), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # Merge back 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  # North Dakota----
  
  state_code=38
  if(state_code %in% unique(data$stateFIPS)){ # filter by state code 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data  
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize"))  # join data w/ tanf Data
    
    
    temp<-temp %>% 
      group_by(householdid) %>% 
      mutate(min_Year=min(Year)) 
    
    # Step I: Calculate net income - Work Disregard = either 27% or $180 per month, whichever is greater (applies to each working adult). 
    # Time Limit Disregard = 50% of net earned income (income after the work disregard) is disregard for months 1 through 6; 
    # 35% of net earned income disregarded from months 7 through 9; 25% of net earned income disregarded from months 10 through 13. 
    temp <- temp %>%
      mutate(income=income+income.gift) %>% 
      group_by(householdid) %>% 
      mutate(first.year = ifelse(Year==min_Year,1,ifelse(Year==min_Year+1,2,0))) %>% 
      mutate(work_disregard=ifelse((income/12)*EarnedIncomeDisregard>180,income*EarnedIncomeDisregard/12, 180)) %>% 
      mutate(income_month=(income/12) - work_disregard) %>% 
      mutate(net.income=(income/12) - work_disregard) 
    
    temp$income_month[temp$income_month < 0] <- 0
    temp$net.income[temp$net.income < 0] <- 0
    
    temp <- temp %>%
      
      mutate(net.income=ifelse(first.year==1, net.income-((6*(0.5*income_month))-(3*(0.35*income_month)) - (3*(0.25*income_month)))/12,
                               net.income-((6*(0.5*income_month))-(3*(0.35*income_month)) - (3*(0.25*income_month)))/12
      )
      ) %>% 
      mutate(net.income=ifelse(first.year==2, net.income-(0.25*income_month/12), net.income-(0.25*income_month/12))) %>%
      # Step II: Calculate value of the benefit
      mutate(net.income=ifelse(net.income < 0, 0, net.income)) %>%
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit - net.income),0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))  %>% 
      
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes",0,tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # gross income test
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # min tanf value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  
  # Ohio----
  
  state_code=39
  if(state_code %in% unique(data$stateFIPS)){ 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize"))%>%   # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - $250 + 50% of remaining income
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - 250 - EarnedIncomeDisregard*((income/12) - 250),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  
  # Oklahoma ----
  
  state_code=40
  if(state_code %in% unique(data$stateFIPS)){ 
    temp<-data %>% 
      filter(stateFIPS==state_code) %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize"))  # join data w/ tanf Data
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      
      # Step I: Calculate net income - $240 disregard for indivduals working 30 hours or more; 
      # $120 if not. If passed the net income test, then apply the 50% disregard on remaining income to determine the benefit amount. 
      # We assume individuals working for 30 hours or more for the time being, and for one working adult at the moment 
      
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12) - 240 - EarnedIncomeDisregard*((income/12) - 240),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse((income/12)>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse((income/12)-240>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - 60 month lifetime limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  
  # Oregon ----
  
  state_code=41
  if(state_code %in% unique(data$stateFIPS)){ 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - 50% income deduction to overall family earned income; must be less than the Standard of Need
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind((income/12) - EarnedIncomeDisregard*(income/12),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed2,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - Oregon has a 60 month time limit, but theoretically offers reduced benefits....need to ask Urban Institute about this
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  # Pennsylvania----
  
  state_code=42
  if(state_code %in% unique(data$stateFIPS)){ # filter by state code 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - $200 deduction for each working individual in the family, then a 50% income reduction to determine benefit amount
      mutate(income=income+income.gift) %>% 
      mutate(net.income=(income/12)-200) %>% 
      mutate(net.income = rowMaxs(cbind(net.income - EarnedIncomeDisregard*(net.income),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>%
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue))  %>% 
      
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes",0,tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  # Rhode Island----
  
  state_code=44
  if(state_code %in% unique(data$stateFIPS)){ # filter by state code 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize"))%>%  # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income -     $170 + 50% of remaining income
      # NOTE: there is a reduced amount for SON & tanf benefit for those who receive subsidizd housing. We do not currently include this 
      mutate(income=income+income.gift) %>% 
      mutate(net.income=income/12-(170))%>% 
      mutate(net.income = rowMaxs(cbind(net.income - EarnedIncomeDisregard*net.income,0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) 
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - 48 month lifetime limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+3), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  
  # Soth Carolina----
  
  state_code=45
  if(state_code %in% unique(data$stateFIPS)){ # filter by state code 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - 50 percent of the monthly gross countable earned income of each individual whose needs are included in the budget group for the first four months in which earned income is countable. 
      # This is a onetime only disregard. Then a $100 per month from gross countable income of each individual whose needs are included in the budget group, for the remaining months of eligibility after the four months in (1) above have been exhausted
      mutate(income=income+income.gift)
    
    
    temp$net.income <- temp$income/12
    
    #  temp$net.income <- temp$net.income - 100*temp$famsize 
    temp<-temp %>% 
      group_by(householdid) %>% 
      mutate(min_Year=min(Year)) %>% 
      mutate(net.income=ifelse(Year==min_Year,net.income-0.33*0.5*net.income,net.income))%>% 
      mutate(net.income=net.income+(value.ssdi/12))%>% 
      mutate(net.income=ifelse(Year==min_Year,net.income-0.67*100*famsize,net.income))
    
    
    temp$net.income[temp$net.income < 0] <- 0
    # Step II: Calculate value of the benefit
    temp<-temp %>% 
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) 
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - 24 month lifetime limit 
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+1), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # mergee back 
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  # South Dakota ----
  
  state_code=46
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income -- $90 + 20% of the remaining income
      mutate(income=income+income.gift) %>% 
      mutate(net.income=(income/12)-90) %>% 
      mutate(net.income = rowMaxs(cbind(net.income - EarnedIncomeDisregard*net.income,0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }  
  # Tennessee ----
  
  state_code=47
  if(state_code %in% unique(data$stateFIPS)){ 
    
    temp<-data[data$stateFIPS==47,]
    
    temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))  # join data w/ tanf Data
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    
    #   temp$numkidsunder13 <- rowSums(cbind(temp$agePerson1, temp$agePerson2, temp$agePerson3, temp$agePerson4, temp$agePerson5, temp$agePerson6, temp$agePerson7, temp$agePerson8, temp$agePerson9, temp$agePerson10, temp$agePerson11, temp$agePerson12)<=12,na.rm=TRUE)
    
    #  temp$childcareDeduction<-temp$numkidsunder13*175
    
    # Step I: Calculate net income
    # $250 work related expenses. Also they typical "minus cost of dependent care for each child (up to $200 for each child under 2 years of age, $175 for each child 2 years or older)". We don't include that yet. 
    temp$income=temp$income+temp$income.gift
    temp$net.income<-rowMaxs(cbind((temp$income/12)-temp$EarnedIncomeDisregard,0)) # earned income deduction
    #  temp$net.income<-rowMaxs(cbind(temp$net.income-temp$childcareDeduction,0))
    temp$net.income <- temp$net.income + (temp$value.ssdi/12)   
    # Step II: Calculate value of the benefit
    temp$deficit <- rowMaxs(cbind(temp$StandardOfNeed - temp$net.income,0))
    
    temp$tanfValue<-0
    subset<-temp$totalassets<temp$AssetTest
    temp$tanfValue[subset]<-rowMins(cbind(temp$deficit[subset],temp$Maxbenefit[subset]))
    
    # Apply gross income test (if applicable) & net income / standard test (if applicable)
    
    temp <- temp %>%
      
      mutate(tanfValue=ifelse(net.income>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) 
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    # In some states TANF is not available for childless adults
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0  
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }  
  
  
  # Texas ----
  
  state_code=48
  if(state_code %in% unique(data$stateFIPS)){ # filter by state code 
    
    temp<-data[data$stateFIPS==48,]
    
    temp<-left_join(temp, tanfData, by=c("stateFIPS", "famsize"))
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    # Step I: Calculate net income - For benefit computation and recipient qualification, do the $120 work expense deduction + either 90% of remaining income or $1400, whichever is smaller
    # This second part (90% or 1400) lasts for the first four months
    temp$income=temp$income+temp$income.gift
    
    temp$net.income<-rowMaxs(cbind((temp$income/12)-120,0)) # earned income deduction
    temp$min_deduction <- rowMins(cbind(1400,0.9*temp$net.income))
    
    temp<-temp %>% 
      group_by(householdid) %>% 
      mutate(min_Year=min(Year)) %>%
      mutate(net.income=ifelse(Year==min_Year,net.income-0.25*min_deduction,net.income))
    
    temp$net.income <- temp$net.income + (temp$value.ssdi/12)
    temp$net.income[temp$net.income < 0] <- 0
    subset<-temp$totalassets<temp$AssetTest
    temp$tanfValue<-0
    temp$subset<-subset
    temp<-temp %>% 
      mutate(cases=ifelse(subset & numadults<2 & !is.na(numadults),"case_1",NA),
             cases=ifelse(net.income>StandardOfNeed & numadults<2 & !is.na(numadults),"case_2",cases),
             cases=ifelse(subset & numadults >= 2 & !is.na(numadults),"case_3",cases),
             cases=ifelse(net.income>StandardOfNeed2 & numadults >= 2 & !is.na(numadults),"case_4",cases),
             cases=ifelse(is.na(numadults),"case_5",cases)) %>% 
      
      mutate(tanfValue=ifelse(subset & numadults<2 & !is.na(numadults), rowMaxs(cbind(Maxbenefit - net.income,0)),tanfValue), 
             tanfValue=ifelse(net.income>StandardOfNeed & numadults<2 & !is.na(numadults), 0, tanfValue),
             tanfValue=ifelse(subset & numadults >= 2 & !is.na(numadults), rowMaxs(cbind(Maxbenefit2 - net.income,0)),tanfValue),
             tanfValue=ifelse(net.income>StandardOfNeed2 & numadults >= 2 & !is.na(numadults), 0, tanfValue),
             tanfValue=ifelse(is.na(numadults),0, tanfValue),
             net.income=ifelse(is.na(numadults),0, net.income))
    
    # TANF is not available for two married adults
    temp$tanfValue[temp$FilingStatus==2 & temp$twoParents_tanf_policy=="No"]<-0
    
    # CO and TX allow households with no children to qualify for TANF
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    temp$tanfValue <- temp$tanfValue*12
    
    # for some reason we don't have the gross income test or SON test for texas. Including soon. 
    
    
    # TIME SERIES - 60 month lifetime limit 
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
    
  }
  
  # Utah ----
  
  state_code=49
  if(state_code %in% unique(data$stateFIPS)){ 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - For applicants, deduct $100 per month; if less than the standard of need, applicant qualifies. 
      # For recipients and those who pass the standard of need, it's $100 per month + 50% of the remaining income, subtract that from maximum tanf benefit 
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - 100 - EarnedIncomeDisregard*(income/12 - 100),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - 36 lifetime limit
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+2), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  # Vermont----
  
  state_code=50
  if(state_code %in% unique(data$stateFIPS)){ 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - If the person has a "subsidized" job, only disregard $90. 
      # If it is an "unsubsidized" job, deduct $250 plus 25% of the remaining income. Compare this to the StandardofNeed. 
      # If it passes, multiple by 0.496 and compare to MaxBenefit. If passed, the amount is the difference between the calculated amount and MaxBenefit
      
      # we just assume the person has an unsubsidized job 
      
      mutate(income=income+income.gift) %>% 
      mutate(net.income=(income/12)-250) %>% 
      mutate(net.income=net.income*(1-EarnedIncomeDisregard)) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit - (0.496*net.income),0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue))  %>% 
      
      mutate(tanfValue=ifelse(0.496*net.income>Maxbenefit,0,tanfValue))  %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    # Gross Income Test
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  
  # Virginia----
  
  state_code=51
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    
    # Virginia has different Standard of need & gross income tests & benefits values depending on the county. 
    
    temp_countys<-data %>% 
      filter(stateFIPS==state_code) %>% 
      filter(     
        countyortownName == "Albemarle County" 
        | countyortownName ==  "Arlington County" 
        | countyortownName == "Augusta County" 
        | countyortownName == "Caroline County" 
        | countyortownName ==  "Fairfax County" 
        | countyortownName ==  "Fauquier County" 
        | countyortownName ==  "James City County" 
        | countyortownName ==  "King George County" 
        | countyortownName ==  "Montgomery County" 
        | countyortownName ==  "Prince William County" 
        | countyortownName ==  "Spotsylvania County"
        | countyortownName ==  "Stafford County"
        | countyortownName ==  "York County" 
        | countyortownName ==  "Alexandria city" 
        | countyortownName ==  "Charlottesville city"
        | countyortownName ==  "Colonial Heights city" 
        | countyortownName ==  "Fairfax city" 
        | countyortownName ==  "Falls Church city" 
        | countyortownName ==  "Fredericksburg city" 
        | countyortownName ==  "Hampton city" 
        | countyortownName ==  "Manassas city" 
        | countyortownName ==  "Manassas Park city"
        | countyortownName ==  "Newport News city"
        | countyortownName ==  "Poquoson city" 
        | countyortownName ==  "Staunton city" 
        | countyortownName ==  "Waynesboro city")  %>% 
      
      
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - deduct a certain about of money depending on family size, then deduct 20% of remaining gross earned income
      mutate(income=income+income.gift) %>% 
      mutate(net.income=(income/12)-177) %>% 
      mutate(net.income=ifelse(famsize>=4,net.income-9,net.income)) %>%
      mutate(net.income=ifelse(famsize>=5,net.income-31,net.income)) %>%
      mutate(net.income=ifelse(famsize>=6,net.income-31,net.income)) %>%
      mutate(net.income=net.income*EarnedIncomeDisregard) %>% 
      mutate(net.income=ifelse(net.income<0,0,net.income)) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit2 - net.income),0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed2,0,tanfValue))  %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) %>% 
      
      # Gross Income Test
      mutate(tanfValue=ifelse(income/12>grossIncomeTest2,0,tanfValue))
    
    
    # In some states TANF is not available for childless adults
    
    temp_countys$tanfValue[temp_countys$numkids==0 & temp_countys$childless_tanf_policy=="No"]<-0
    
    
    
    temp_othercountys<-data %>% 
      filter(stateFIPS==state_code) %>% 
      filter(  countyortownName != "Albemarle County" 
               & countyortownName !=  "Arlington County" 
               & countyortownName != "Augusta County" 
               & countyortownName != "Caroline County" 
               & countyortownName !=  "Fairfax County" 
               & countyortownName !=  "Fauquier County" 
               & countyortownName !=  "James City County" 
               & countyortownName !=  "King George County" 
               & countyortownName !=  "Montgomery County" 
               & countyortownName !=  "Prince William County" 
               & countyortownName !=  "Spotsylvania County"
               & countyortownName !=  "Stafford County"
               & countyortownName !=  "York County" 
               & countyortownName !=  "Alexandria city" 
               & countyortownName !=  "Charlottesville city"
               & countyortownName !=  "Colonial Heights city" 
               & countyortownName !=  "Fairfax city" 
               & countyortownName !=  "Falls Church city" 
               & countyortownName !=  "Fredericksburg city" 
               & countyortownName !=  "Hampton city" 
               & countyortownName !=  "Manassas city" 
               & countyortownName !=  "Manassas Park city"
               & countyortownName !=  "Newport News city"
               & countyortownName !=  "Poquoson city" 
               & countyortownName !=  "Staunton city" 
               & countyortownName !=  "Waynesboro city") %>% 
      
      left_join(tanfData, by=c("stateFIPS", "famsize"))%>%  # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income
      mutate(income=income+income.gift) %>% 
      mutate(net.income=(income/12)-177) %>% 
      mutate(net.income=ifelse(famsize>=4,net.income-9,net.income)) %>%
      mutate(net.income=ifelse(famsize>=5,net.income-31,net.income)) %>%
      mutate(net.income=ifelse(famsize>=6,net.income-31,net.income)) %>%
      mutate(net.income=net.income*EarnedIncomeDisregard) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit - net.income),0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(net.income/12>StandardOfNeed,0,tanfValue))  %>% 
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0 , tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp_othercountys$tanfValue[temp_othercountys$numkids==0 & temp_othercountys$childless_tanf_policy=="No"]<-0
    
    # merge the dataset so it's all virginia counties 
    temp<-rbind(temp_countys, temp_othercountys)
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
  }
  # Washington ----
  
  state_code=53
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - Deduct 50% of all earned income and unearned income
      mutate(income=income+income.gift) %>% 
      mutate(income = income + value.ssdi) %>%
      mutate(net.income = rowMaxs(cbind(income/12 - (income*EarnedIncomeDisregard/12),0))) %>% 
      
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) %>% 
      
      # In some states TANF is not available for childless adults
      mutate(tanfValue=ifelse(numkids==0 & childless_tanf_policy=="No",0,tanfValue ))
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  
  # West Virginia ----
  
  state_code=54
  if(state_code %in% unique(data$stateFIPS)){ # make sure that state is in the list
    temp<-data %>% 
      filter(stateFIPS==state_code)
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - Deduct 40% from earned income
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - (income*EarnedIncomeDisregard/12),0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue)) 
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    
    # min tanf value 
    temp$tanfValue[temp$tanfValue < 10] <- 0
    
    
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - 60 months lifetme limit 
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  # Wisconsin ----
  
  state_code=55
  if(state_code %in% unique(data$stateFIPS)){ 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data%>% 
      
      # Step I: Calculate net income - we've looked but it seems like Wisconsin doesn't have deductions for a standard of need test. 
      # Wisconsin's a weird one that we're trying to do more research on 
      mutate(income=income+income.gift) %>%
      mutate(net.income=income/12) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit - Wisconsin changes benefit amount dependng on if someone has a job already or is looking for one 
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind((Maxbenefit - net.income),0)),tanfValue)) %>% 
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      
      
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    
    temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
    
    temp$tanfValue <- temp$tanfValue*12
    
    # TIME SERIES - If person is employed through the W-2 program, they get up to 24 months, instead of 48 months.
    # We just assume 48 months for now 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+3), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    # merge back 
    
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  
  # Wyoming----
  
  state_code=56
  if(state_code %in% unique(data$stateFIPS)){ 
    temp<-data %>% 
      filter(stateFIPS==state_code) 
    
    householdids <- unique(temp$householdid) # create  household id list for cross section data
    
    temp <- temp %>% 
      left_join(tanfData, by=c("stateFIPS", "famsize")) %>% # join data w/ tanf Data
      
      # Step I: Calculate net income - Deduct $1200 if it's a married couple with at least one child, otherwise deduct $600
      mutate(income=income+income.gift) %>% 
      mutate(net.income = rowMaxs(cbind(income/12 - (EarnedIncomeDisregard),0))) #%>% 
    
    temp$net.income[temp$FilingStatus == 2] <- temp$net.income[temp$FilingStatus == 2] - 600
    
    
    # The amounts change dependng on a number of factors:
    # for a family that pays when the assistance unit pays any portion of their own housing and/or utility costs except the assistance unit(s) receiving a government housing subsidy, 
    # has a member of the assistance unit member who receives SSI and is a relative or under the minor parent requirements. 
    
    # A different standard of need & Benefit allotment exist for families when all housing and utility costs are provided to the assistance unit without cost 
    # and for the assistance unit(s) receiving a government rental subsidy, 
    # has a member of the assistance unit member who receives SSI and is a relative or under the minor parent requirements. 
    # For married couples, add $600 to StandardofNeed and StandardofNeed2
    
    temp <- temp %>% 
      mutate(net.income = rowMaxs(cbind(net.income,0))) %>% 
      mutate(net.income = net.income + (value.ssdi/12)) %>%
      
      # Step II: Calculate value of the benefit
      mutate(tanfValue=0,
             subset=totalassets<AssetTest) %>% 
      mutate(tanfValue=ifelse(subset==TRUE,rowMaxs(cbind(Maxbenefit-net.income,0)),tanfValue)) %>% 
      
      # Apply gross income test (if applicable) & net income / standard test (if applicable)
      
      mutate(tanfValue=ifelse(income/12>grossIncomeTest,0,tanfValue)) %>% 
      
      mutate(tanfValue=ifelse(net.income>StandardOfNeed,0,tanfValue)) %>% 
      
      # TANF is not available for two married adults
      
      mutate(tanfValue=ifelse(FilingStatus==2 & twoParents_tanf_policy!="Yes", 0, tanfValue))
    
    # In some states TANF is not available for childless adults
    
    temp$tanfValue[temp$numkids==0 & temp$childless_tanf_policy=="No"]<-0
    
    temp$tanfValue <- temp$tanfValue*12
    
    
    # TIME SERIES - 60 month lifetime limit 
    
    years <- unique(temp$Year)
    if(nrow(temp)>1 & length(years)>1 ){
      temp<-temp %>% 
        group_by(householdid) %>% 
        mutate(min_Year=min(Year)) %>% 
        mutate(tanfValue=ifelse(Year>(min_Year+4), 0, tanfValue))
      
    }else{
      abc <- 0
    }
    
    
    # merge back 
    
    if(length(householdids)>1){
      
      data <- data[order(data$householdid, data$Year),]
      temp <- temp[order(temp$householdid, temp$Year),]
      data$tanfValue[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue  
      data$value.tanf[data$stateFIPS==state_code & data$householdid %in% householdids]<-temp$tanfValue
      
    }else{
      data$tanfValue[data$stateFIPS==state_code]<-temp$tanfValue  
      
      data$value.tanf[data$stateFIPS==state_code]<-temp$tanfValue
    }
    
  }
  ### result ####
  }
 
  return(data$value.tanf) 

  }




