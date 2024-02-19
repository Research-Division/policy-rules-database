 #-------------------------------------------------------------
# description: calculate the value of each public benefit in the PRD
# inputs: each benefit's 'parameters'.rdata file (e.g. snapData.rdata)
# output: values of each benefit & other output used in other programs
#-------------------------------------------------------------

 
# PUBLIC BENEFITS----
 
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
     temp$tanfValue[(temp$income/12)>temp$grossIncomeTest]<-0
     
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
       
       # Step I: Calculate net income - Flordia subtracts $90 (for both applicants & recipients), 
       # then additional $200 for some reason, then half of remaining income
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
   return(data$value.tanf) 
   
 }



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
                               ,data$married==1 & data$countSSDI==2 & rowSums(cbind(data$withheldAmount1,data$withheldAmount2),na.rm=TRUE)==0 ~ rowSums(cbind(data$numkids),na.rm=TRUE) # withheld == 0, then spouse (IF THERE IS ONE) receives auxiliary benefit
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
  
  # create lag of the value of SSDI. Once value of SSDI reaches zero, disabled people may still qualify for Medicare part A
  data$hadssdi1<-case_when(data$disability1==1 & !is.na(data$disability1) & data$value.ssdi>0 ~ 0
                          ,data$disability1==1 & !is.na(data$disability1) & data$value.ssdi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssdi2<-case_when(data$disability2==1 & !is.na(data$disability2) & data$value.ssdi>0 ~ 0,
                          data$disability2==1 & !is.na(data$disability2) & data$value.ssdi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssdi3<-case_when(data$disability3==1 & !is.na(data$disability3) & data$value.ssdi>0 ~ 0,
                          data$disability3==1 & !is.na(data$disability3) & data$value.ssdi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssdi4<-case_when(data$disability4==1 & !is.na(data$disability4) & data$value.ssdi>0 ~ 0,
                          data$disability4==1 & !is.na(data$disability4) & data$value.ssdi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssdi5<-case_when(data$disability5==1 & !is.na(data$disability5) & data$value.ssdi>0 ~ 0,
                          data$disability5==1 & !is.na(data$disability5) & data$value.ssdi<=0 ~ 1
                          ,TRUE ~ 0)
  data$hadssdi6<-case_when(data$disability6==1 & !is.na(data$disability6) & data$value.ssdi>0 ~ 0,
                          data$disability6==1 & !is.na(data$disability6) & data$value.ssdi<=0 ~ 1
                          ,TRUE ~ 0)
  
  data.ssdi<-data %>%
    select(value.ssdi, portionedAuxAdlt1, portionedAuxAdlt2,hadssdi1,hadssdi2,hadssdi3
           ,hadssdi4,hadssdi5,hadssdi6) # may need data$recdssdi1-6 here
  
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
                              , shareOfRent = 0.3){ # User input - varies from 40 to 60%
  
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
  data$adjustedincome<-rowMaxs(cbind(data$income.countable - data$numkids*data$DependentDeduction,0))
  
  # Step II: Determine Total Tenant Payment
  data$ttp<-shareOfRent*data$adjustedincome
  
  # Step III: Determine benefit value
  data$section8value<-0
  data$section8value<-rowMaxs(cbind(rowMins(cbind(data$exp.rentormortgage,data$MaxBenefit))-data$ttp,0))
  
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


# CCDF function (new) ----

function.CCDFcopay<-function(data
                               , contelig.ccdf = TRUE
                               ){
    
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
        expandPastMiss2<-left_join(expandPastMiss, ccdfData_CO, by=c("stateFIPS", "AKorHI", "famsize", "countyortownName", "numkidsInCare"))# %>% drop_na()
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
    
    # CONNECTITUC ----
    
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
      temp$FTcopay[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]<-temp$CopayBin21[temp$income>temp$Bin20Max & temp$income<=temp$Bin21Max]
      
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
        
        
        temp_2022<-temp[temp$ruleYear==2023,]
        
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
        
        temp$childcare.overage[temp$ruleYear==2023]<-temp_2022$childcare.overage
        temp$totcopay[temp$ruleYear==2023]<-temp_2022$totcopay
        
        
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
    
   
  return(data$value.CCDF)
    
  } #end function.CCDF
  
  
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
    data$premium.healthcare.individual<-12*data$premium.healthcare.individual*(1+parameters.defaults$inflationrate[1])^(data$Year-data$ruleYear)
    
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
             ,"fedtaxcredits" = fedtaxcreditsvar)
    
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
      expand<-expand.grid(stateFIPS=unique(stateinctaxData$stateFIPS), FilingStatus=unique(stateinctaxData$FilingStatus), Year=futureYrs)
      # Collect latest benefit data there is and merge w/data frame
      expand2<-stateinctaxData[stateinctaxData$ruleYear==maxyearofdata, ]
      expand<-expand%>%left_join(expand2, by=c("stateFIPS","FilingStatus")) %>% select(-c(ruleYear)) %>% rename("ruleYear"=Year)
    }# Attach copied future, historical, and missing benefit data
    if(length(futureYrs)>0) {stateinctaxData<-stateinctaxData %>% rbind(expand)}
    
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
    data$taxableincome.bin10<-rowMaxs(cbind((data$income.base-data$IncomeBin9Max)-rowMaxs(cbind(data$income.base-data$IncomeBin10Max,0)),0))
    
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
  
  
# Federal Insurance Constribution Act (FICA) Tax----

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
  
