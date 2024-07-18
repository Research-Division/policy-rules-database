
# Benefits Calculator ----
# This program provides calculations for:
# - Minimum Household Budget using United Way ALICE calculated expenses
# - Public Assistance
# - Taxes and Tax Credits

if (!exists("FATES")) FATES=FALSE
if (!exists("APPLY_FRSP")) APPLY_FRSP=FALSE
#if (!exists("APPLY_RAP")) APPLY_RAP=FALSE

#CREATE DATA AND PERFORM INITIAL TRANSFORMATIONS ----

# Function to create data from inputs
function.createData<-function(inputs){

  # Process "locations" input
  if(inputs$locations=="all"){ #include all counties
    inputs$locations<-paste(table.countypop$countyortownName,table.countypop$stateAbbrev,sep=", ")
  } else{
    inputs$locations<-inputs$locations
  }

  # Income sequence
  inputs$income<-seq(inputs$income_start,inputs$income_end,by=inputs$income_increase_by)

  # variables for which you want to create different combinations
  data<-expand.grid(income = as.numeric(inputs$income)
                    , locations = inputs$locations
                    , agePerson1 = as.numeric(inputs$agePerson1) # need to apply as.numeric to make sure NA is converted properly
                    , agePerson2 = as.numeric(inputs$agePerson2)
                    , agePerson3 = as.numeric(inputs$agePerson3)
                    , agePerson4 = as.numeric(inputs$agePerson4)
                    , agePerson5 = as.numeric(inputs$agePerson5)
                    , agePerson6 = as.numeric(inputs$agePerson6)
                    , agePerson7 = as.numeric(inputs$agePerson7)
                    , agePerson8 = as.numeric(inputs$agePerson8)
                    , agePerson9 = as.numeric(inputs$agePerson9)
                    , agePerson10 = as.numeric(inputs$agePerson10)
                    , agePerson11 = as.numeric(inputs$agePerson11)
                    , agePerson12 = as.numeric(inputs$agePerson12)
                    , married = as.numeric(inputs$married)
                    , disability1 = inputs$disability1
                    , disability2 = inputs$disability2
                    , disability3 = inputs$disability3
                    , disability4 = inputs$disability4
                    , disability5 = inputs$disability5
                    , disability6 = inputs$disability6
                    , disability7 = inputs$disability7
                    , disability8 = inputs$disability8
                    , disability9 = inputs$disability9
                    , disability10 = inputs$disability10
                    , disability11 = inputs$disability11
                    , disability12 = inputs$disability12
                    , blind1 = inputs$blind1
                    , blind2 = inputs$blind2
                    , blind3 = inputs$blind3
                    , blind4 = inputs$blind4
                    , blind5 = inputs$blind5
                    , blind6 = inputs$blind6
                    , ssdiPIA1 = inputs$ssdiPIA1 # Only adults with a work history may receive SSDI
                    , ssdiPIA2 = inputs$ssdiPIA2
                    , ssdiPIA3 = inputs$ssdiPIA3
                    , ssdiPIA4 = inputs$ssdiPIA4
                    , ssdiPIA5 = inputs$ssdiPIA5
                    , ssdiPIA6 = inputs$ssdiPIA6
                    , prev_ssi = 0 # Has anyone in the home ever received SSI - important for Medicaid disability provision
                    , empl_healthcare = inputs$empl_healthcare
                    , ownorrent = inputs$ownorrent
                    , assets.cash = inputs$assets.cash
                    , assets.car1 = inputs$assets.car1
                    , income.investment = inputs$income.investment
                    , income.gift = inputs$income.gift
                    , income.child_support = inputs$income.child_support
                    , disab.work.exp = inputs$disab.work.exp
                    , ruleYear = inputs$ruleYear) %>%
    mutate(Year = ruleYear)

  #make location two variables
  data<- data %>%
    separate(locations, c("countyortownName","stateAbbrev"), sep=", ")

  # Additional post-processing
  data<-data %>%
    mutate(income_tm12 = income)  # create lag of income for tax credits calculations


  # Create core variables
  data<-function.InitialTransformations(data)

  # Create extra vars (not in initial transformations b/c differs by CLIFF tools)
  data<- data %>%
    mutate(FilingStatus=(case_when(married==0 & numkids==0 ~ 1 # Single
                                   ,married==0 & numkids>0 ~ 3 # Head of Household
                                   ,married==1 ~ 2 # Married Filing Jointly
                                   ,TRUE~1)),
           uninsured = case_when( empl_healthcare==0 ~ 1
                                  ,empl_healthcare==1 ~ 0
                                  ,TRUE~0))

  return(data)

}

# Produce initial transformations of the inputs to create core variables that are used across the functions
function.InitialTransformations<-function(data){

  # remove old county fips codes for Connecticut - will work on re-doing PRD so we can remove this in the future
  table.countypop<-table.countypop %>%
    filter(!stcountyfips2010  %in% c("9_1", "9_3", "9_5" ,"9_7" ,"9_9", "9_11", "9_13" ,"9_15"))
  
  data<- data %>%
  left_join(table.countypop,by=c("countyortownName","stateAbbrev")) %>%
  left_join(table.msamap, by=c("stateAbbrev", "countyortownName"))

  # Calculate number of adults and kids
  data$numadults=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=19,na.rm=TRUE)
  data$numkids=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=18,na.rm=TRUE) # We assume kids are < 19. EITC defines it under age 19 & USDA adult category starts at 19
  data$numkidsunder13=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=12,na.rm=TRUE)
  data$ageofYoungestChild=rowMins(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12),na.rm=TRUE)

  # By default - Disability programs determine the value of the benefit on an individual level. We allocate total family income EQUALLY to each adult family member (income1-income6)
  # Default Assumption 1: Equal allocation of income
  # Default Assumption 2: All adults are working
  data$income1[!is.na(data$agePerson1) & data$agePerson1>=19]<-data$income[!is.na(data$agePerson1) & data$agePerson1>=19]/data$numadults[!is.na(data$agePerson1) & data$agePerson1>=19]
  data$income2[!is.na(data$agePerson2) & data$agePerson2>=19]<-data$income[!is.na(data$agePerson2) & data$agePerson2>=19]/data$numadults[!is.na(data$agePerson2) & data$agePerson2>=19]
  data$income3[!is.na(data$agePerson3) & data$agePerson3>=19]<-data$income[!is.na(data$agePerson3) & data$agePerson3>=19]/data$numadults[!is.na(data$agePerson3) & data$agePerson3>=19]
  data$income4[!is.na(data$agePerson4) & data$agePerson4>=19]<-data$income[!is.na(data$agePerson4) & data$agePerson4>=19]/data$numadults[!is.na(data$agePerson4) & data$agePerson4>=19]
  data$income5[!is.na(data$agePerson5) & data$agePerson5>=19]<-data$income[!is.na(data$agePerson5) & data$agePerson5>=19]/data$numadults[!is.na(data$agePerson5) & data$agePerson5>=19]
  data$income6[!is.na(data$agePerson6) & data$agePerson6>=19]<-data$income[!is.na(data$agePerson6) & data$agePerson6>=19]/data$numadults[!is.na(data$agePerson6) & data$agePerson6>=19]

  data<-data %>%
    mutate(famsize = numadults + numkids # Family size
           , hasdependent = case_when(  numkids>0 ~ 1 # Whether family has dependends (simple logic for now)
                                        , TRUE ~ 0)
           , AKorHI = case_when(  stateAbbrev=="AK" ~ "AK"
                                  , stateAbbrev=="HI" ~ "HI"
                                  , TRUE ~ "0")
           , totalassets = assets.car1 + assets.cash)

  data$income.child_support[data$numkids==0]<-0

  return(data)

}

# ASSIGN ALICE EXPENSES----
BenefitsCalculator.ALICEExpenses<-function(data){

  # Variables required to calculate each expense is specified in the function
  # Function will not run unless all inputs are specified

  ## CHILDCARE EXPENSE ----
  #the last argument (schoolagesummercare) defaults to part-time. override to full-time for experiments where we can estimate CCDF copay ourselves for summer time

  #person1
  data$childcare.exp.person1<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,1]

  #person2
  data$childcare.exp.person2<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,2]

  #person3
  data$childcare.exp.person3<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,3]

  #person4
  data$childcare.exp.person4<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,4]

  #person5
  data$childcare.exp.person5<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,5]

  #person6
  data$childcare.exp.person6<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,6]

  #person7
  data$childcare.exp.person7<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,7]
  #person8
  data$childcare.exp.person8<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,8]
  #person9
  data$childcare.exp.person9<-function.childcareExp.ALICE(data
                                                       , schoolagesummercare = "PT")[,9]
  #person10
  data$childcare.exp.person10<-function.childcareExp.ALICE(data
                                                        , schoolagesummercare = "PT")[,10]
  #person11
  data$childcare.exp.person11<-function.childcareExp.ALICE(data
                                                        , schoolagesummercare = "PT")[,11]
  #person12
  data$childcare.exp.person12<-function.childcareExp.ALICE(data
                                                        , schoolagesummercare = "PT")[,12]

  # Total cost of child care
  data<-data %>%
    mutate(exp.childcare = childcare.exp.person1+childcare.exp.person2+childcare.exp.person3+childcare.exp.person4+childcare.exp.person5+childcare.exp.person6+childcare.exp.person7+childcare.exp.person8+childcare.exp.person9+childcare.exp.person10+childcare.exp.person11+childcare.exp.person12)

  ## TRANSPORTATION EXPENSE ----
  data$exp.transportation<-function.transpExp.ALICE(data)

  # FOOD EXPENSE ----
  data$exp.basicfood<-function.foodExp.ALICE(data)

  # COST OF SCHOOL MEALS ----
  data$exp.schoolMeals<-function.schoolmealsExp(data)

  # WIC ----
  data$exp.wic<-function.wicExp(data)

  # For CLIFF, aggregate exp.food from fam food cost, WIC value, and school meals value so that food cost is always at least as much as food benefits
  if (budget.ALICE=="survival" | budget.ALICE=="stability"){
  data$exp.food<-data$exp.basicfood
  data<-data%>%select(-c(exp.basicfood))
  }else if(budget.ALICE=="survivalforcliff"){
  data$exp.food<-data$exp.basicfood + data$exp.schoolMeals + data$exp.wic
  data<-data%>%select(-c(exp.basicfood))
  }

  ## TECH EXPENSE ----
  data$exp.tech<-function.techExp.ALICE(data)

  ## HOUSING EXPENSE----
  #### NOTE: Output for the function is two objects: rent and utilities expense
  exp.housing<-function.housingExp.ALICE(data)
  data<-data%>%
    cbind(exp.housing)
  data$exp.rentormortgage<-data$exp.rent
  data$exp.housing<-data$exp.rent + data$exp.utilities

  ## HEALTHCARE EXPENSE----
  #- returns multiple outputs as object, append each element (output) as column to the data
  # -- exp.healthcare.employer & premium.employer may be recalculated in the healthcare benefits section
  exp.healthcare<-function.healthcareExp.ALICE(data
                                                , famsizevar = "famsize")
  data<-data%>%
    cbind(exp.healthcare)

  # For Self-Sufficiency Target
  data$exp.healthcare.SS<-data$ALICE.expense.healthcare.family


  ## MISCALLENEOUS (OTHER) EXPENSE ----
  # This is calculated at the end 10% of all budget items (Food,housing,Health Care,Transportation,Tech,Child care)
  data$exp.misc<- round(.1 * rowMaxs(cbind((data$ALICE.expense.healthcare.family+
                                data$exp.utilities+
                                data$exp.rentormortgage+
                                data$exp.tech+
                                data$exp.food+
                                data$exp.transportation+
                                data$exp.childcare), na.rm=TRUE)),0)


  return(data)

}

# CHILDCARE SUBSIDIES BLOCK ----
# This block calculate all childcare-related public benefits
# We assume that the family chooses the cheapest option:
# - If available, Pre-K is the best option (NOT YET INCLUDED)
# - If available, Head Start is the second best option
# - If available, CCDF subsidy is the third best option
BenefitsCalculator.Childcare<-function(data, APPLY_CHILDCARE=TRUE,APPLY_HEADSTART=TRUE,APPLY_PREK=TRUE, APPLY_CCDF=TRUE, APPLY_FATES=FALSE
                                       , contelig.ccdf = TRUE, contelig.headstart = TRUE, contelig.earlyheadstart = TRUE
                                       , k_ftorpt="FT", schoolagesummercare="PT", headstart_ftorpt="PT", preK_ftorpt="PT"){

if(APPLY_CHILDCARE==FALSE | (APPLY_HEADSTART==FALSE & APPLY_CCDF==FALSE & APPLY_PREK==FALSE)){ #Fates can only be TRUE if CCDF is also TRUE
  data$netexp.childcare <- data$exp.childcare
  data$value.CCDF<-0
  data$value.HeadStart<-0
  data$value.earlyHeadStart<-0
  data$value.PreK<-0
  data$numkidsinschool=rowSums(cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=18 & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=5, na.rm=TRUE)
  data$value.FATES<-0
  }else{


## Head Start ----

if(APPLY_HEADSTART==FALSE){

      data$value.HeadStartperson1<-0
      data$value.HeadStartperson2<-0
      data$value.HeadStartperson3<-0
      data$value.HeadStartperson4<-0
      data$value.HeadStartperson5<-0
      data$value.HeadStartperson6<-0
      data$value.HeadStartperson7<-0
      data$value.HeadStartperson8<-0
      data$value.HeadStartperson9<-0
      data$value.HeadStartperson10<-0
      data$value.HeadStartperson11<-0
      data$value.HeadStartperson12<-0
      data$value.HeadStart<-0

      data$value.earlyHeadStartperson1<-0
      data$value.earlyHeadStartperson2<-0
      data$value.earlyHeadStartperson3<-0
      data$value.earlyHeadStartperson4<-0
      data$value.earlyHeadStartperson5<-0
      data$value.earlyHeadStartperson6<-0
      data$value.earlyHeadStartperson7<-0
      data$value.earlyHeadStartperson8<-0
      data$value.earlyHeadStartperson9<-0
      data$value.earlyHeadStartperson10<-0
      data$value.earlyHeadStartperson11<-0
      data$value.earlyHeadStartperson12<-0
      data$value.earlyHeadStart<-0

      data$netexp.childcareperson1<-data$childcare.exp.person1
      data$netexp.childcareperson2<-data$childcare.exp.person2
      data$netexp.childcareperson3<-data$childcare.exp.person3
      data$netexp.childcareperson4<-data$childcare.exp.person4
      data$netexp.childcareperson5<-data$childcare.exp.person5
      data$netexp.childcareperson6<-data$childcare.exp.person6
      data$netexp.childcareperson7<-data$childcare.exp.person7
      data$netexp.childcareperson8<-data$childcare.exp.person8
      data$netexp.childcareperson9<-data$childcare.exp.person9
      data$netexp.childcareperson10<-data$childcare.exp.person10
      data$netexp.childcareperson11<-data$childcare.exp.person11
      data$netexp.childcareperson12<-data$childcare.exp.person12

      data$netexp.childcare<-data$exp.childcare
      }

else if(APPLY_HEADSTART==TRUE){

#Person 1
data$value.HeadStartperson1<-function.headstart(data=data
                                                , ageofPersonvar="agePerson1"
                                                , expchildcarevar="childcare.exp.person1"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson1<-function.headstart(data=data
                                                , ageofPersonvar="agePerson1"
                                                , expchildcarevar="childcare.exp.person1"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]

data$netexp.childcareperson1<-data$childcare.exp.person1-data$value.HeadStartperson1-data$value.earlyHeadStartperson1

#Person 2
data$value.HeadStartperson2<-function.headstart(data=data
                                                , ageofPersonvar="agePerson2"
                                                , expchildcarevar="childcare.exp.person2"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson2<-function.headstart(data=data
                                                , ageofPersonvar="agePerson2"
                                                , expchildcarevar="childcare.exp.person2"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]

data$netexp.childcareperson2<-data$childcare.exp.person2-data$value.HeadStartperson2-data$value.earlyHeadStartperson2

#Person 3
data$value.HeadStartperson3<-function.headstart(data=data
                                                , ageofPersonvar="agePerson3"
                                                , expchildcarevar="childcare.exp.person3"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson3<-function.headstart(data=data
                                                , ageofPersonvar="agePerson3"
                                                , expchildcarevar="childcare.exp.person3"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson3<-data$childcare.exp.person3-data$value.HeadStartperson3-data$value.earlyHeadStartperson3

#Person 4
data$value.HeadStartperson4<-function.headstart(data=data
                                                , ageofPersonvar="agePerson4"
                                                , expchildcarevar="childcare.exp.person4"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson4<-function.headstart(data=data
                                                , ageofPersonvar="agePerson4"
                                                , expchildcarevar="childcare.exp.person4"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson4<-data$childcare.exp.person4-data$value.HeadStartperson4-data$value.earlyHeadStartperson4

#Person 5
data$value.HeadStartperson5<-function.headstart(data=data
                                                , ageofPersonvar="agePerson5"
                                                , expchildcarevar="childcare.exp.person5"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson5<-function.headstart(data=data
                                                , ageofPersonvar="agePerson5"
                                                , expchildcarevar="childcare.exp.person5"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson5<-data$childcare.exp.person5-data$value.HeadStartperson5-data$value.earlyHeadStartperson5

#Person 6
data$value.HeadStartperson6<-function.headstart(data=data
                                                , ageofPersonvar="agePerson6"
                                                , expchildcarevar="childcare.exp.person6"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson6<-function.headstart(data=data
                                                , ageofPersonvar="agePerson6"
                                                , expchildcarevar="childcare.exp.person6"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson6<-data$childcare.exp.person6-data$value.HeadStartperson6-data$value.earlyHeadStartperson6

#Person 7
data$value.HeadStartperson7<-function.headstart(data=data
                                                , ageofPersonvar="agePerson7"
                                                , expchildcarevar="childcare.exp.person7"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson7<-function.headstart(data=data
                                                , ageofPersonvar="agePerson7"
                                                , expchildcarevar="childcare.exp.person7"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson7<-data$childcare.exp.person7-data$value.HeadStartperson7-data$value.earlyHeadStartperson7

#Person 8
data$value.HeadStartperson8<-function.headstart(data=data
                                                , ageofPersonvar="agePerson8"
                                                , expchildcarevar="childcare.exp.person8"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson8<-function.headstart(data=data
                                                     , ageofPersonvar="agePerson8"
                                                     , expchildcarevar="childcare.exp.person8"
                                                     , headstart_ftorpt = `headstart_ftorpt`
                                                     , contelig.headstart = `contelig.headstart`
                                                     , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson8<-data$childcare.exp.person8-data$value.HeadStartperson8-data$value.earlyHeadStartperson8

#Person 9
data$value.HeadStartperson9<-function.headstart(data=data
                                                , ageofPersonvar="agePerson9"
                                                , expchildcarevar="childcare.exp.person9"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson9<-function.headstart(data=data
                                                     , ageofPersonvar="agePerson9"
                                                     , expchildcarevar="childcare.exp.person9"
                                                     , headstart_ftorpt = `headstart_ftorpt`
                                                     , contelig.headstart = `contelig.headstart`
                                                     , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson9<-data$childcare.exp.person9-data$value.HeadStartperson9-data$value.earlyHeadStartperson9

#Person 10
data$value.HeadStartperson10<-function.headstart(data=data
                                                , ageofPersonvar="agePerson10"
                                                , expchildcarevar="childcare.exp.person10"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson10<-function.headstart(data=data
                                                     , ageofPersonvar="agePerson10"
                                                     , expchildcarevar="childcare.exp.person10"
                                                     , headstart_ftorpt = `headstart_ftorpt`
                                                     , contelig.headstart = `contelig.headstart`
                                                     , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson10<-data$childcare.exp.person10-data$value.HeadStartperson10-data$value.earlyHeadStartperson10

#Person 11
data$value.HeadStartperson11<-function.headstart(data=data
                                                , ageofPersonvar="agePerson11"
                                                , expchildcarevar="childcare.exp.person11"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson11<-function.headstart(data=data
                                                     , ageofPersonvar="agePerson11"
                                                     , expchildcarevar="childcare.exp.person11"
                                                     , headstart_ftorpt = `headstart_ftorpt`
                                                     , contelig.headstart = `contelig.headstart`
                                                     , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson11<-data$childcare.exp.person11-data$value.HeadStartperson11-data$value.earlyHeadStartperson11

#Person 12
data$value.HeadStartperson12<-function.headstart(data=data
                                                , ageofPersonvar="agePerson12"
                                                , expchildcarevar="childcare.exp.person12"
                                                , headstart_ftorpt = `headstart_ftorpt`
                                                , contelig.headstart = `contelig.headstart`
                                                , contelig.earlyheadstart = `contelig.headstart`)[[2]]

data$value.earlyHeadStartperson12<-function.headstart(data=data
                                                     , ageofPersonvar="agePerson12"
                                                     , expchildcarevar="childcare.exp.person12"
                                                     , headstart_ftorpt = `headstart_ftorpt`
                                                     , contelig.headstart = `contelig.headstart`
                                                     , contelig.earlyheadstart = `contelig.earlyheadstart`)[[1]]
data$netexp.childcareperson12<-data$childcare.exp.person12-data$value.HeadStartperson12-data$value.earlyHeadStartperson12

# Replace NAs with zeros because NA means not eligible (due to age or income)
data <- data %>%
  mutate(value.HeadStartperson1=if_else(is.na(value.HeadStartperson1), 0, value.HeadStartperson1)
         ,value.HeadStartperson2=if_else(is.na(value.HeadStartperson2), 0, value.HeadStartperson2)
         ,value.HeadStartperson3=if_else(is.na(value.HeadStartperson3), 0, value.HeadStartperson3)
         ,value.HeadStartperson4=if_else(is.na(value.HeadStartperson4), 0, value.HeadStartperson4)
         ,value.HeadStartperson5=if_else(is.na(value.HeadStartperson5), 0, value.HeadStartperson5)
         ,value.HeadStartperson6=if_else(is.na(value.HeadStartperson6), 0, value.HeadStartperson6)
         ,value.HeadStartperson7=if_else(is.na(value.HeadStartperson7), 0, value.HeadStartperson7)
         ,value.HeadStartperson8=if_else(is.na(value.HeadStartperson8), 0, value.HeadStartperson8)
         ,value.HeadStartperson9=if_else(is.na(value.HeadStartperson9), 0, value.HeadStartperson9)
         ,value.HeadStartperson10=if_else(is.na(value.HeadStartperson10), 0, value.HeadStartperson10)
         ,value.HeadStartperson11=if_else(is.na(value.HeadStartperson11), 0, value.HeadStartperson11)
         ,value.HeadStartperson12=if_else(is.na(value.HeadStartperson12), 0, value.HeadStartperson12)) %>%
  mutate(value.HeadStart = value.HeadStartperson1+value.HeadStartperson2+value.HeadStartperson3+value.HeadStartperson4+value.HeadStartperson5+value.HeadStartperson6+value.HeadStartperson7+value.HeadStartperson8+value.HeadStartperson9+value.HeadStartperson10+value.HeadStartperson11+value.HeadStartperson12) %>%

  mutate(value.earlyHeadStartperson1=if_else(is.na(value.earlyHeadStartperson1), 0, value.earlyHeadStartperson1)
       ,value.earlyHeadStartperson2=if_else(is.na(value.earlyHeadStartperson2), 0, value.earlyHeadStartperson2)
       ,value.earlyHeadStartperson3=if_else(is.na(value.earlyHeadStartperson3), 0, value.earlyHeadStartperson3)
       ,value.earlyHeadStartperson4=if_else(is.na(value.earlyHeadStartperson4), 0, value.earlyHeadStartperson4)
       ,value.earlyHeadStartperson5=if_else(is.na(value.earlyHeadStartperson5), 0, value.earlyHeadStartperson5)
       ,value.earlyHeadStartperson6=if_else(is.na(value.earlyHeadStartperson6), 0, value.earlyHeadStartperson6)
       ,value.earlyHeadStartperson7=if_else(is.na(value.earlyHeadStartperson7), 0, value.earlyHeadStartperson7)
       ,value.earlyHeadStartperson8=if_else(is.na(value.earlyHeadStartperson8), 0, value.earlyHeadStartperson8)
       ,value.earlyHeadStartperson9=if_else(is.na(value.earlyHeadStartperson9), 0, value.earlyHeadStartperson9)
       ,value.earlyHeadStartperson10=if_else(is.na(value.earlyHeadStartperson10), 0, value.earlyHeadStartperson10)
       ,value.earlyHeadStartperson11=if_else(is.na(value.earlyHeadStartperson11), 0, value.earlyHeadStartperson11)
       ,value.earlyHeadStartperson12=if_else(is.na(value.earlyHeadStartperson12), 0, value.earlyHeadStartperson12)) %>%
  mutate(value.earlyHeadStart = value.earlyHeadStartperson1+value.earlyHeadStartperson2+value.earlyHeadStartperson3+value.earlyHeadStartperson4+value.earlyHeadStartperson5+value.earlyHeadStartperson6+value.earlyHeadStartperson7+value.earlyHeadStartperson8+value.earlyHeadStartperson9+value.earlyHeadStartperson10+value.earlyHeadStartperson11+value.earlyHeadStartperson12)

#Calculate childcare cost net of Head Start
data<-data %>%
  mutate(netexp.childcare = exp.childcare-value.HeadStart-value.earlyHeadStart)

} #end Head Start & Early Head Start function


# PREK ----

    if(APPLY_PREK==FALSE){

      data$value.PreKperson1<-0
      data$value.PreKperson2<-0
      data$value.PreKperson3<-0
      data$value.PreKperson4<-0
      data$value.PreKperson5<-0
      data$value.PreKperson6<-0
      data$value.PreKperson7<-0
      data$value.PreKperson8<-0
      data$value.PreKperson9<-0
      data$value.PreKperson10<-0
      data$value.PreKperson11<-0
      data$value.PreKperson12<-0
      data$value.PreK<-0
      } 
    else if(APPLY_PREK==TRUE){

      #Person 1
      data$value.PreKperson1<-function.prek(data=data
                                            , agePersonvar="agePerson1"
                                            , headstartPersonvar="value.HeadStartperson1"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person1"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program


      #Person 2
      data$value.PreKperson2<-function.prek(data=data
                                            , agePersonvar="agePerson2"
                                            , headstartPersonvar="value.HeadStartperson2"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person2"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program


      #Person 3
      data$value.PreKperson3<-function.prek(data=data
                                            , agePersonvar="agePerson3"
                                            , headstartPersonvar="value.HeadStartperson3"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person3"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 4
      data$value.PreKperson4<-function.prek(data=data
                                            , agePersonvar="agePerson4"
                                            , headstartPersonvar="value.HeadStartperson4"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person4"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 5
      data$value.PreKperson5<-function.prek(data=data
                                            , agePersonvar="agePerson5"
                                            , headstartPersonvar="value.HeadStartperson5"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person5"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 6
      data$value.PreKperson6<-function.prek(data=data
                                            , agePersonvar="agePerson6"
                                            , headstartPersonvar="value.HeadStartperson6"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person6"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 7
      data$value.PreKperson7<-function.prek(data=data
                                            , agePersonvar="agePerson7"
                                            , headstartPersonvar="value.HeadStartperson7"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person7"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 8
      data$value.PreKperson8<-function.prek(data=data
                                            , agePersonvar="agePerson8"
                                            , headstartPersonvar="value.HeadStartperson8"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person8"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 9
      data$value.PreKperson9<-function.prek(data=data
                                            , agePersonvar="agePerson9"
                                            , headstartPersonvar="value.HeadStartperson9"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person9"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 10
      data$value.PreKperson10<-function.prek(data=data
                                            , agePersonvar="agePerson10"
                                            , headstartPersonvar="value.HeadStartperson10"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person10"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 11
      data$value.PreKperson11<-function.prek(data=data
                                            , agePersonvar="agePerson11"
                                            , headstartPersonvar="value.HeadStartperson11"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person11"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program

      #Person 12
      data$value.PreKperson12<-function.prek(data=data
                                            , agePersonvar="agePerson12"
                                            , headstartPersonvar="value.HeadStartperson12"
                                            , headstart_ftorpt = `headstart_ftorpt`
                                            , childcare.expvar="childcare.exp.person12"
                                            , preK_ftorpt = preK_ftorpt
                                            , schoolagesummercare=schoolagesummercare
                                            , contelig.headstart = `contelig.headstart`
                                            , contelig.earlyheadstart = `contelig.earlyheadstart`) # PT or FT program


      # Replace NAs with zeros because NA means not eligible (due to age or income)
      data <- data %>%
        mutate(value.PreKperson1=if_else(is.na(value.PreKperson1), 0, value.PreKperson1)
               ,value.PreKperson2=if_else(is.na(value.PreKperson2), 0, value.PreKperson2)
               ,value.PreKperson3=if_else(is.na(value.PreKperson3), 0, value.PreKperson3)
               ,value.PreKperson4=if_else(is.na(value.PreKperson4), 0, value.PreKperson4)
               ,value.PreKperson5=if_else(is.na(value.PreKperson5), 0, value.PreKperson5)
               ,value.PreKperson6=if_else(is.na(value.PreKperson6), 0, value.PreKperson6)
               ,value.PreKperson7=if_else(is.na(value.PreKperson7), 0, value.PreKperson7)
               ,value.PreKperson8=if_else(is.na(value.PreKperson8), 0, value.PreKperson8)
               ,value.PreKperson9=if_else(is.na(value.PreKperson9), 0, value.PreKperson9)
               ,value.PreKperson10=if_else(is.na(value.PreKperson10), 0, value.PreKperson10)
               ,value.PreKperson11=if_else(is.na(value.PreKperson11), 0, value.PreKperson11)
               ,value.PreKperson12=if_else(is.na(value.PreKperson12), 0, value.PreKperson12)) %>%
        mutate(value.PreK = value.PreKperson1+value.PreKperson2+value.PreKperson3+value.PreKperson4++value.PreKperson5++value.PreKperson6++value.PreKperson7+value.PreKperson8+value.PreKperson9+value.PreKperson10+value.PreKperson11+value.PreKperson12) %>%
        mutate(value.PreK = ifelse(exp.childcare<=value.PreK, exp.childcare, value.PreK))

            #Calculate childcare cost net of preK & headstart
      data=data %>%
        mutate(netexp.childcare = exp.childcare-value.PreK-value.HeadStart-value.earlyHeadStart,
               netexp.childcareperson1=childcare.exp.person1- value.PreKperson1-value.HeadStartperson1-value.earlyHeadStartperson1,
                netexp.childcareperson2=childcare.exp.person2- value.PreKperson2-value.HeadStartperson2-value.earlyHeadStartperson2,
                netexp.childcareperson3=childcare.exp.person3- value.PreKperson3-value.HeadStartperson3-value.earlyHeadStartperson3,
                netexp.childcareperson4=childcare.exp.person4- value.PreKperson4-value.HeadStartperson4-value.earlyHeadStartperson4,
                netexp.childcareperson5=childcare.exp.person5- value.PreKperson5-value.HeadStartperson5-value.earlyHeadStartperson5,
                netexp.childcareperson6=childcare.exp.person6- value.PreKperson6-value.HeadStartperson6-value.earlyHeadStartperson6,
                netexp.childcareperson7=childcare.exp.person7- value.PreKperson7-value.HeadStartperson7-value.earlyHeadStartperson7,
               netexp.childcareperson8=childcare.exp.person8- value.PreKperson8-value.HeadStartperson8-value.earlyHeadStartperson8,
               netexp.childcareperson9=childcare.exp.person9- value.PreKperson9-value.HeadStartperson9-value.earlyHeadStartperson9,
               netexp.childcareperson10=childcare.exp.person10- value.PreKperson10-value.HeadStartperson10-value.earlyHeadStartperson10,
               netexp.childcareperson11=childcare.exp.person11- value.PreKperson11-value.HeadStartperson11-value.earlyHeadStartperson11,
               netexp.childcareperson12=childcare.exp.person12- value.PreKperson12-value.HeadStartperson12-value.earlyHeadStartperson12
                )
    } #end PREK=true function

# The cost for school meals was done in the ALICE expense function and only includes kids>=5 years old. If kids are in PreK, we recalculate the cost
# for school meals by including these children who can be younger than 5 years old.
    data$numkidsinschool=rowSums(cbind(data$value.PreKperson1, data$value.PreKperson2, data$value.PreKperson3, data$value.PreKperson4, data$value.PreKperson5, data$value.PreKperson6, data$value.PreKperson7, data$value.PreKperson8, data$value.PreKperson9, data$value.PreKperson10, data$value.PreKperson11, data$value.PreKperson12)>0 | (cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)>=5 & cbind(data$agePerson1, data$agePerson2, data$agePerson3, data$agePerson4, data$agePerson5, data$agePerson6, data$agePerson7, data$agePerson8, data$agePerson9, data$agePerson10, data$agePerson11, data$agePerson12)<=18),na.rm=TRUE)
    data$numkidsinschool[is.na(data$numkidsinschool)]<-0
    data$exp.schoolMeals=data$exp.schoolMeals*data$numkidsinschool


# Calculate overage costs for the whole family
    # We decided against using overage because we don't have data on how common this is.
    # Set to zero for now, bc in future we find data on this
data$childcare.overage<-0


# CCDF ----
if(APPLY_CCDF==FALSE){

    data$value.CCDF<-0}

  else if(APPLY_CCDF==TRUE){

    # Calculate total copay
    data$value.CCDF<-function.CCDFcopay(data
                                        , contelig.ccdf = `contelig.ccdf`) # TRUE/FALSE


    # Adjust for take-up
    # Is take-up variable specified?
    if(is.null(data$ccdf_takeup)){
      data$ccdf_takeup<-1
    }
    data$value.CCDF[data$ccdf_takeup==0]<-0

    #Calculate net childcare expenses (net of CCDF and Head Start) for SNAP, CDCTC etc
    data<-data %>%
      mutate(netexp.childcare = exp.childcare-value.HeadStart-value.earlyHeadStart-value.PreK- value.CCDF)

  }


if(APPLY_FATES==FALSE){

  data$value.FATES<-0

}
    else if(APPLY_FATES==TRUE){

    # Calculate total copay
  data$value.FATES<- function.CCDFcopayFATES(data
                                      , contelig.ccdf = `contelig.ccdf`)

  #Calculate net childcare expenses (net of CCDF and Head Start) for SNAP, CDCTC etc
    data<-data %>%
  #replace values of FATES With 0 where there is CCDF
    mutate(value.FATES=case_when(value.CCDF>0 ~ 0,TRUE~ value.FATES)) %>%
    #Calculate net childcare expenses (net of CCDF and Head Start) for SNAP, CDCTC etc
    mutate(netexp.childcare = exp.childcare-value.HeadStart-value.earlyHeadStart-value.PreK-value.CCDF-value.FATES)


}

  }

  #return data for whole childcare block
  return(data)
}


# HEALTHCARE SUBSIDIES BLOCK ----
# Health Insurance Costs Minimization Algorithm
# 1. Medicaid/CHIP is the cheapest option - everyone who is eligible enrolls
# 2. For those who are not on Medicaid/CHIP - calculate premiums for:
#     i) employer plan
#     ii) health exchange
#     iii) out-of-pocket (proxied by SLCS)
# 3. Compare costs of employer plan vs health exchange
# 4. If Medicaid, ACA or Health Exchange are not available - out-of-pocket
BenefitsCalculator.Healthcare<-function(data, APPLY_HEALTHCARE=FALSE, APPLY_MEDICAID_ADULT=TRUE, APPLY_MEDICAID_CHILD=TRUE, APPLY_ACA=TRUE){

  if(APPLY_HEALTHCARE==FALSE){ # when APPY_HEALTHCARE==FALSE, only assign healthcare costs. No benefits.

    data$value.medicaid<-0
    data$value.medicaid.adult<-0
    data$value.medicaid.child<-0
    data$value.medicare<-0
    data$value.aca<-0

    # Assign employer healthcare if available----
    data$oop.health.family.ALICE<-function.healthcareExp.ALICE(data
                                                               , famsizevar = "famsize")[,"oop.health.family.ALICE"]



    data$exp.healthcare.employer<-function.healthcareExp.ALICE(data
                                                               , famsizevar = "famsize")[,2] #total cost of health insurance (employer paid + employee paid premium)

    data$premium.employer<-function.healthcareExp.ALICE(data
                                                        , famsizevar = "famsize")[,3] # employee premium

    data$exp.healthcare.SS <- data$ALICE.expense.healthcare.family

    # If option of healthcare through employer is not available, then set everything to NA
    data<-data %>%
      mutate(exp.healthcare.employer = case_when(empl_healthcare==0 ~ NA_real_, TRUE~exp.healthcare.employer)
             ,premium.employer = case_when(empl_healthcare==0 ~ NA_real_, TRUE~premium.employer)) %>%
      # Value of the employer healthcare
      mutate(value.employerhealthcare = exp.healthcare.employer - premium.employer)
    data$value.employerhealthcare[is.na(data$value.employerhealthcare)]<-0

    # Assign out-of-pocket costs ----
    # Assign costs of Second Lowest Cost Silverplan FOR EACH PERSON
    data$exp.healthcare.healthexchange.person1<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson1")
    data$exp.healthcare.healthexchange.person2<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson2")
    data$exp.healthcare.healthexchange.person3<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson3")
    data$exp.healthcare.healthexchange.person4<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson4")
    data$exp.healthcare.healthexchange.person5<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson5")
    data$exp.healthcare.healthexchange.person6<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson6")
    data$exp.healthcare.healthexchange.person7<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson7")
    data$exp.healthcare.healthexchange.person8<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson8")
    data$exp.healthcare.healthexchange.person9<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson9")
    data$exp.healthcare.healthexchange.person10<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson10")
    data$exp.healthcare.healthexchange.person11<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson11")
    data$exp.healthcare.healthexchange.person12<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson12")


    # Total family expenses on health exchange
    data$exp.healthcare.healthexchange<-rowSums(cbind(data$exp.healthcare.healthexchange.person1,data$exp.healthcare.healthexchange.person2
                                                      ,data$exp.healthcare.healthexchange.person3,data$exp.healthcare.healthexchange.person4
                                                      ,data$exp.healthcare.healthexchange.person5,data$exp.healthcare.healthexchange.person6
                                                      ,data$exp.healthcare.healthexchange.person7,data$exp.healthcare.healthexchange.person8
                                                      ,data$exp.healthcare.healthexchange.person9,data$exp.healthcare.healthexchange.person10
                                                      ,data$exp.healthcare.healthexchange.person11,data$exp.healthcare.healthexchange.person12), na.rm = TRUE)


    # Determine what health care sources are used !! add medicare here too!
    data<-data %>%
      # Generate out-of-pocket premium
      mutate(premium.outofpocket = exp.healthcare.healthexchange) %>%
      # Employer healthcare is available and Health Exchange is available, then compare two plans
      mutate(healthcare.source = case_when( (!is.na(premium.employer) & premium.employer <= premium.outofpocket) ~ "Employer" # If Health Exchange more expensive than Employer plan
                                            ,(!is.na(premium.employer) & premium.employer > premium.outofpocket) ~ "Out-of-pocket" # If Health Exchange is cheaper than Employer plan
                                            ,(is.na(premium.employer)) ~ "Out-of-pocket" # If both unavailable
                                            , TRUE ~ NA_character_)
      )


    # All possible combinations of the Healthcare costs
    data<-data %>%
      mutate(exp.healthcare = case_when( healthcare.source == "Employer" ~ exp.healthcare.employer
                                         ,healthcare.source == "Out-of-pocket" ~ exp.healthcare.healthexchange))

    data$exp.healthcare <- data$exp.healthcare + data$oop.health.family.ALICE

    data<-data %>%
      mutate(netexp.healthcare = exp.healthcare-value.employerhealthcare) # Total out-of-pocket costs



  }else{
    #Healthcare: must do this for each family member separately... unless they are on a family plan

    #if not on family plan then assign the employer plan or through health exchange:

    # Initialize Source of Healthcare for Each Person
    data<-data %>%
      mutate(healthcare.source.person1=NA_character_
             ,healthcare.source.person2=NA_character_
             ,healthcare.source.person3=NA_character_
             ,healthcare.source.person4=NA_character_
             ,healthcare.source.person5=NA_character_
             ,healthcare.source.person6=NA_character_
             ,healthcare.source.person7=NA_character_
             ,healthcare.source.person8=NA_character_
             ,healthcare.source.person9=NA_character_
             ,healthcare.source.person10=NA_character_
             ,healthcare.source.person11=NA_character_
             ,healthcare.source.person12=NA_character_
      )

    # Medicaid ----

    # Assign Medicaid Costs
    data$exp.healthcare.medicaid.person1<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson1")
    data$exp.healthcare.medicaid.person2<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson2")
    data$exp.healthcare.medicaid.person3<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson3")
    data$exp.healthcare.medicaid.person4<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson4")
    data$exp.healthcare.medicaid.person5<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson5")
    data$exp.healthcare.medicaid.person6<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson6")
    data$exp.healthcare.medicaid.person7<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson7")
    data$exp.healthcare.medicaid.person8<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson8")
    data$exp.healthcare.medicaid.person9<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson9")
    data$exp.healthcare.medicaid.person10<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson10")
    data$exp.healthcare.medicaid.person11<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson11")
    data$exp.healthcare.medicaid.person12<-function.healthcareExp.Medicaid(data
                                                                          , ageofpersonvar = "agePerson12")

    #Assign Medicaid premiums
    data$premium.medicaid.person1<-function.medicaid(data
                                                     , ageofpersonvar="agePerson1"
                                                     , disabilityofpersonvar="disability1"
                                                     , hadssivar="hadssi1")[[1]]
    data$premium.medicaid.person2<-function.medicaid(data
                                                     , ageofpersonvar="agePerson2"
                                                     , disabilityofpersonvar="disability2"
                                                     , hadssivar="hadssi2")[[1]]
    data$premium.medicaid.person3<-function.medicaid(data
                                                     , ageofpersonvar="agePerson3"
                                                     , disabilityofpersonvar="disability3"
                                                     , hadssivar="hadssi3")[[1]]
    data$premium.medicaid.person4<-function.medicaid(data
                                                     , ageofpersonvar="agePerson4"
                                                     , disabilityofpersonvar="disability4"
                                                     , hadssivar="hadssi4")[[1]]
    data$premium.medicaid.person5<-function.medicaid(data
                                                     , ageofpersonvar="agePerson5"
                                                     , disabilityofpersonvar="disability5"
                                                     , hadssivar="hadssi5")[[1]]
    data$premium.medicaid.person6<-function.medicaid(data
                                                     , ageofpersonvar="agePerson6"
                                                     , disabilityofpersonvar="disability6"
                                                     , hadssivar="hadssi6")[[1]]
    data$premium.medicaid.person7<-function.medicaid(data
                                                     , ageofpersonvar="agePerson7"
                                                     , disabilityofpersonvar="disability7"
                                                     , hadssivar="hadssi7")[[1]]
    data$premium.medicaid.person8<-function.medicaid(data
                                                     , ageofpersonvar="agePerson8"
                                                     , disabilityofpersonvar="disability8"
                                                     , hadssivar="hadssi8")[[1]]
    data$premium.medicaid.person9<-function.medicaid(data
                                                     , ageofpersonvar="agePerson9"
                                                     , disabilityofpersonvar="disability9"
                                                     , hadssivar="hadssi9")[[1]]
    data$premium.medicaid.person10<-function.medicaid(data
                                                     , ageofpersonvar="agePerson10"
                                                     , disabilityofpersonvar="disability10"
                                                     , hadssivar="hadssi10")[[1]]
    data$premium.medicaid.person11<-function.medicaid(data
                                                     , ageofpersonvar="agePerson11"
                                                     , disabilityofpersonvar="disability11"
                                                     , hadssivar="hadssi11")[[1]]
    data$premium.medicaid.person12<-function.medicaid(data
                                                     , ageofpersonvar="agePerson12"
                                                     , disabilityofpersonvar="disability12"
                                                     , hadssivar="hadssi12")[[1]]

    # Type of premium (Per Child/Per Family)
    # Later we'll need to determine the cheapest healthcare option for the person and then determine total costs to the family using Plan Type
    data$premium.medicaid.type<-function.medicaid(data
                                                  , ageofpersonvar="agePerson1"
                                                  , disabilityofpersonvar="disability1"
                                                  , hadssivar="hadssi1")[[2]]

    # Calculate total premium for Medicaid
    data<-data %>%
      mutate(premium.medicaid = case_when(premium.medicaid.type=="Per child" ~ rowSums(cbind(premium.medicaid.person1,premium.medicaid.person2
                                                                                             ,premium.medicaid.person3,premium.medicaid.person4
                                                                                             ,premium.medicaid.person5,premium.medicaid.person6
                                                                                             ,premium.medicaid.person7,premium.medicaid.person8
                                                                                             ,premium.medicaid.person9,premium.medicaid.person10
                                                                                             ,premium.medicaid.person11,premium.medicaid.person12), na.rm=TRUE)
                                          , premium.medicaid.type=="Per family" ~ rowMaxs(cbind(premium.medicaid.person1,premium.medicaid.person2
                                                                                                ,premium.medicaid.person3,premium.medicaid.person4
                                                                                                ,premium.medicaid.person5,premium.medicaid.person6
                                                                                                ,premium.medicaid.person7,premium.medicaid.person8
                                                                                                ,premium.medicaid.person9,premium.medicaid.person10
                                                                                                ,premium.medicaid.person11,premium.medicaid.person12), na.rm=TRUE)
                                          , TRUE ~ 0)
             # This will ensure that premium is zero if no one in the family is on Medicaid and rowMaxs can't be taken
             ,premium.medicaid = case_when(is.infinite(premium.medicaid) ~ 0, TRUE ~ premium.medicaid))

    # Adjust Medicaid expenses to be NA if person is not on Medicaid
    # Person is not on Medicaid if premium is NA
    data<-data %>%
      mutate(exp.healthcare.medicaid.person1 = case_when(is.na(premium.medicaid.person1) ~ NA_real_,TRUE~exp.healthcare.medicaid.person1)
             ,exp.healthcare.medicaid.person2 = case_when(is.na(premium.medicaid.person2) ~ NA_real_,TRUE~exp.healthcare.medicaid.person2)
             ,exp.healthcare.medicaid.person3 = case_when(is.na(premium.medicaid.person3) ~ NA_real_,TRUE~exp.healthcare.medicaid.person3)
             ,exp.healthcare.medicaid.person4 = case_when(is.na(premium.medicaid.person4) ~ NA_real_,TRUE~exp.healthcare.medicaid.person4)
             ,exp.healthcare.medicaid.person5 = case_when(is.na(premium.medicaid.person5) ~ NA_real_,TRUE~exp.healthcare.medicaid.person5)
             ,exp.healthcare.medicaid.person6 = case_when(is.na(premium.medicaid.person6) ~ NA_real_,TRUE~exp.healthcare.medicaid.person6)
             ,exp.healthcare.medicaid.person7 = case_when(is.na(premium.medicaid.person7) ~ NA_real_,TRUE~exp.healthcare.medicaid.person7)
             ,exp.healthcare.medicaid.person8 = case_when(is.na(premium.medicaid.person8) ~ NA_real_,TRUE~exp.healthcare.medicaid.person8)
             ,exp.healthcare.medicaid.person9 = case_when(is.na(premium.medicaid.person9) ~ NA_real_,TRUE~exp.healthcare.medicaid.person9)
             ,exp.healthcare.medicaid.person10 = case_when(is.na(premium.medicaid.person10) ~ NA_real_,TRUE~exp.healthcare.medicaid.person10)
             ,exp.healthcare.medicaid.person11 = case_when(is.na(premium.medicaid.person11) ~ NA_real_,TRUE~exp.healthcare.medicaid.person11)
             ,exp.healthcare.medicaid.person12 = case_when(is.na(premium.medicaid.person12) ~ NA_real_,TRUE~exp.healthcare.medicaid.person12))


    # Calculate total Family's Values of Medicaid & Medicaid Expenses
    data<-data %>%

      # Medicaid value per person
      mutate(value.medicaid.person1=exp.healthcare.medicaid.person1
             ,value.medicaid.person2=exp.healthcare.medicaid.person2
             ,value.medicaid.person3=exp.healthcare.medicaid.person3
             ,value.medicaid.person4=exp.healthcare.medicaid.person4
             ,value.medicaid.person5=exp.healthcare.medicaid.person5
             ,value.medicaid.person6=exp.healthcare.medicaid.person6
             ,value.medicaid.person7=exp.healthcare.medicaid.person7
             ,value.medicaid.person8=exp.healthcare.medicaid.person8
             ,value.medicaid.person9=exp.healthcare.medicaid.person9
             ,value.medicaid.person10=exp.healthcare.medicaid.person10
             ,value.medicaid.person11=exp.healthcare.medicaid.person11
             ,value.medicaid.person12=exp.healthcare.medicaid.person12)


    # Create separate values for Child Medicaid and Adult Medicaid
    data<-data %>%

      # Separate expenses for children and adults depending on age
      mutate(exp.healthcare.medicaid.child.person1 = case_when(agePerson1<19 ~ exp.healthcare.medicaid.person1, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person2 = case_when(agePerson2<19 ~ exp.healthcare.medicaid.person2, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person3 = case_when(agePerson3<19 ~ exp.healthcare.medicaid.person3, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person4 = case_when(agePerson4<19 ~ exp.healthcare.medicaid.person4, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person5 = case_when(agePerson5<19 ~ exp.healthcare.medicaid.person5, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person6 = case_when(agePerson6<19 ~ exp.healthcare.medicaid.person6, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person7 = case_when(agePerson7<19 ~ exp.healthcare.medicaid.person7, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person8 = case_when(agePerson8<19 ~ exp.healthcare.medicaid.person8, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person9 = case_when(agePerson9<19 ~ exp.healthcare.medicaid.person9, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person10 = case_when(agePerson10<19 ~ exp.healthcare.medicaid.person10, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person11 = case_when(agePerson11<19 ~ exp.healthcare.medicaid.person11, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.child.person12 = case_when(agePerson12<19 ~ exp.healthcare.medicaid.person12, TRUE ~ NA_real_))%>%

      mutate(exp.healthcare.medicaid.adult.person1 = case_when(agePerson1>=19 ~ exp.healthcare.medicaid.person1, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person2 = case_when(agePerson2>=19 ~ exp.healthcare.medicaid.person2, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person3 = case_when(agePerson3>=19 ~ exp.healthcare.medicaid.person3, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person4 = case_when(agePerson4>=19 ~ exp.healthcare.medicaid.person4, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person5 = case_when(agePerson5>=19 ~ exp.healthcare.medicaid.person5, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person6 = case_when(agePerson6>=19 ~ exp.healthcare.medicaid.person6, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person7 = case_when(agePerson7>=19 ~ exp.healthcare.medicaid.person7, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person8 = case_when(agePerson8>=19 ~ exp.healthcare.medicaid.person8, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person9 = case_when(agePerson9>=19 ~ exp.healthcare.medicaid.person9, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person10 = case_when(agePerson10>=19 ~ exp.healthcare.medicaid.person10, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person11 = case_when(agePerson11>=19 ~ exp.healthcare.medicaid.person11, TRUE ~ NA_real_)
             ,exp.healthcare.medicaid.adult.person12 = case_when(agePerson12>=19 ~ exp.healthcare.medicaid.person12, TRUE ~ NA_real_)) %>%

      # Separate premiums for children and adults depending on age
      mutate(premium.medicaid.child.person1 = case_when(agePerson1<19 ~ premium.medicaid.person1, TRUE ~ NA_real_)
             ,premium.medicaid.child.person2 = case_when(agePerson2<19 ~ premium.medicaid.person2, TRUE ~ NA_real_)
             ,premium.medicaid.child.person3 = case_when(agePerson3<19 ~ premium.medicaid.person3, TRUE ~ NA_real_)
             ,premium.medicaid.child.person4 = case_when(agePerson4<19 ~ premium.medicaid.person4, TRUE ~ NA_real_)
             ,premium.medicaid.child.person5 = case_when(agePerson5<19 ~ premium.medicaid.person5, TRUE ~ NA_real_)
             ,premium.medicaid.child.person6 = case_when(agePerson6<19 ~ premium.medicaid.person6, TRUE ~ NA_real_)
             ,premium.medicaid.child.person7 = case_when(agePerson7<19 ~ premium.medicaid.person7, TRUE ~ NA_real_)
             ,premium.medicaid.child.person8 = case_when(agePerson8<19 ~ premium.medicaid.person8, TRUE ~ NA_real_)
             ,premium.medicaid.child.person9 = case_when(agePerson9<19 ~ premium.medicaid.person9, TRUE ~ NA_real_)
             ,premium.medicaid.child.person10 = case_when(agePerson10<19 ~ premium.medicaid.person10, TRUE ~ NA_real_)
             ,premium.medicaid.child.person11 = case_when(agePerson11<19 ~ premium.medicaid.person11, TRUE ~ NA_real_)
             ,premium.medicaid.child.person12 = case_when(agePerson12<19 ~ premium.medicaid.person12, TRUE ~ NA_real_))%>%

      mutate(premium.medicaid.adult.person1 = case_when(agePerson1>=19 ~ premium.medicaid.person1, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person2 = case_when(agePerson2>=19 ~ premium.medicaid.person2, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person3 = case_when(agePerson3>=19 ~ premium.medicaid.person3, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person4 = case_when(agePerson4>=19 ~ premium.medicaid.person4, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person5 = case_when(agePerson5>=19 ~ premium.medicaid.person5, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person6 = case_when(agePerson6>=19 ~ premium.medicaid.person6, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person7 = case_when(agePerson7>=19 ~ premium.medicaid.person7, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person8 = case_when(agePerson8>=19 ~ premium.medicaid.person8, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person9 = case_when(agePerson9>=19 ~ premium.medicaid.person9, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person10 = case_when(agePerson10>=19 ~ premium.medicaid.person10, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person11 = case_when(agePerson11>=19 ~ premium.medicaid.person11, TRUE ~ NA_real_)
             ,premium.medicaid.adult.person12 = case_when(agePerson12>=19 ~ premium.medicaid.person12, TRUE ~ NA_real_)) %>%

      # Separate values for children and adults depending on age
      mutate(value.medicaid.child.person1 = case_when(agePerson1<19 ~ value.medicaid.person1, TRUE ~ NA_real_)
             ,value.medicaid.child.person2 = case_when(agePerson2<19 ~ value.medicaid.person2, TRUE ~ NA_real_)
             ,value.medicaid.child.person3 = case_when(agePerson3<19 ~ value.medicaid.person3, TRUE ~ NA_real_)
             ,value.medicaid.child.person4 = case_when(agePerson4<19 ~ value.medicaid.person4, TRUE ~ NA_real_)
             ,value.medicaid.child.person5 = case_when(agePerson5<19 ~ value.medicaid.person5, TRUE ~ NA_real_)
             ,value.medicaid.child.person6 = case_when(agePerson6<19 ~ value.medicaid.person6, TRUE ~ NA_real_)
             ,value.medicaid.child.person7 = case_when(agePerson7<19 ~ value.medicaid.person7, TRUE ~ NA_real_)
             ,value.medicaid.child.person8 = case_when(agePerson8<19 ~ value.medicaid.person8, TRUE ~ NA_real_)
             ,value.medicaid.child.person9 = case_when(agePerson9<19 ~ value.medicaid.person9, TRUE ~ NA_real_)
             ,value.medicaid.child.person10 = case_when(agePerson10<19 ~ value.medicaid.person10, TRUE ~ NA_real_)
             ,value.medicaid.child.person11 = case_when(agePerson11<19 ~ value.medicaid.person11, TRUE ~ NA_real_)
             ,value.medicaid.child.person12 = case_when(agePerson12<19 ~ value.medicaid.person12, TRUE ~ NA_real_))%>%

      mutate(value.medicaid.adult.person1 = case_when(agePerson1>=19 ~ value.medicaid.person1, TRUE ~ NA_real_)
             ,value.medicaid.adult.person2 = case_when(agePerson2>=19 ~ value.medicaid.person2, TRUE ~ NA_real_)
             ,value.medicaid.adult.person3 = case_when(agePerson3>=19 ~ value.medicaid.person3, TRUE ~ NA_real_)
             ,value.medicaid.adult.person4 = case_when(agePerson4>=19 ~ value.medicaid.person4, TRUE ~ NA_real_)
             ,value.medicaid.adult.person5 = case_when(agePerson5>=19 ~ value.medicaid.person5, TRUE ~ NA_real_)
             ,value.medicaid.adult.person6 = case_when(agePerson6>=19 ~ value.medicaid.person6, TRUE ~ NA_real_)
             ,value.medicaid.adult.person7 = case_when(agePerson7>=19 ~ value.medicaid.person7, TRUE ~ NA_real_)
             ,value.medicaid.adult.person8 = case_when(agePerson8>=19 ~ value.medicaid.person8, TRUE ~ NA_real_)
             ,value.medicaid.adult.person9 = case_when(agePerson9>=19 ~ value.medicaid.person9, TRUE ~ NA_real_)
             ,value.medicaid.adult.person10 = case_when(agePerson10>=19 ~ value.medicaid.person10, TRUE ~ NA_real_)
             ,value.medicaid.adult.person11 = case_when(agePerson11>=19 ~ value.medicaid.person11, TRUE ~ NA_real_)
             ,value.medicaid.adult.person12 = case_when(agePerson12>=19 ~ value.medicaid.person12, TRUE ~ NA_real_))

    # Total Premium for Adult
    data<-data %>%
      mutate(premium.medicaid.adult = case_when(premium.medicaid.type=="Per child" ~ rowSums(cbind(premium.medicaid.adult.person1,premium.medicaid.adult.person2
                                                                                                   ,premium.medicaid.adult.person3,premium.medicaid.adult.person4
                                                                                                   ,premium.medicaid.adult.person5,premium.medicaid.adult.person6
                                                                                                   ,premium.medicaid.adult.person7,premium.medicaid.adult.person8
                                                                                                   ,premium.medicaid.adult.person9,premium.medicaid.adult.person10
                                                                                                   ,premium.medicaid.adult.person11,premium.medicaid.adult.person12), na.rm=TRUE)
                                                , premium.medicaid.type=="Per family" ~ rowMaxs(cbind(premium.medicaid.adult.person1,premium.medicaid.adult.person2
                                                                                                      ,premium.medicaid.adult.person3,premium.medicaid.adult.person4
                                                                                                      ,premium.medicaid.adult.person5,premium.medicaid.adult.person6
                                                                                                      ,premium.medicaid.adult.person7,premium.medicaid.adult.person8
                                                                                                      ,premium.medicaid.adult.person9,premium.medicaid.adult.person10
                                                                                                      ,premium.medicaid.adult.person11,premium.medicaid.adult.person12), na.rm=TRUE)
                                                , TRUE ~ 0)
             # This will ensure that premium is zero if no one in the family is on medicaid and rowMaxs can't be taken
             ,premium.medicaid.adult = case_when(is.infinite(premium.medicaid.adult) ~ 0, TRUE ~ premium.medicaid.adult))


    # Total Premium for Children
    data<-data %>%
      mutate(premium.medicaid.child = case_when(premium.medicaid.type=="Per child" ~ rowSums(cbind(premium.medicaid.child.person1,premium.medicaid.child.person2
                                                                                                   ,premium.medicaid.child.person3,premium.medicaid.child.person4
                                                                                                   ,premium.medicaid.child.person5,premium.medicaid.child.person6
                                                                                                   ,premium.medicaid.child.person7,premium.medicaid.child.person8
                                                                                                   ,premium.medicaid.child.person9,premium.medicaid.child.person10
                                                                                                   ,premium.medicaid.child.person11,premium.medicaid.child.person12), na.rm=TRUE)
                                                , premium.medicaid.type=="Per family" ~ rowMaxs(cbind(premium.medicaid.child.person1,premium.medicaid.child.person2
                                                                                                      ,premium.medicaid.child.person3,premium.medicaid.child.person4
                                                                                                      ,premium.medicaid.child.person5,premium.medicaid.child.person6
                                                                                                      ,premium.medicaid.child.person7,premium.medicaid.child.person8
                                                                                                      ,premium.medicaid.child.person9,premium.medicaid.child.person10
                                                                                                      ,premium.medicaid.child.person11,premium.medicaid.child.person12), na.rm=TRUE)
                                                , TRUE ~ 0)
             # This will ensure that premium is zero if noone in the family is on medicaid and rowMaxs can't be taken
             ,premium.medicaid.child = case_when(is.infinite(premium.medicaid.child) ~ 0, TRUE ~ premium.medicaid.child))




    if(APPLY_MEDICAID_ADULT==FALSE){ # Set everything as NA

      data<-data %>%

        mutate(premium.medicaid.adult.person1 = NA_real_
               ,premium.medicaid.adult.person2 = NA_real_
               ,premium.medicaid.adult.person3 = NA_real_
               ,premium.medicaid.adult.person4 = NA_real_
               ,premium.medicaid.adult.person5 = NA_real_
               ,premium.medicaid.adult.person6 = NA_real_
               ,premium.medicaid.adult.person7 = NA_real_
               ,premium.medicaid.adult.person8 = NA_real_
               ,premium.medicaid.adult.person9 = NA_real_
               ,premium.medicaid.adult.person10 = NA_real_
               ,premium.medicaid.adult.person11 = NA_real_
               ,premium.medicaid.adult.person12 = NA_real_
               ,premium.medicaid.adult = 0) %>%

        mutate(value.medicaid.adult.person1 = NA_real_
               ,value.medicaid.adult.person2 = NA_real_
               ,value.medicaid.adult.person3 = NA_real_
               ,value.medicaid.adult.person4 = NA_real_
               ,value.medicaid.adult.person5 = NA_real_
               ,value.medicaid.adult.person6 = NA_real_
               ,value.medicaid.adult.person7 = NA_real_
               ,value.medicaid.adult.person8 = NA_real_
               ,value.medicaid.adult.person9 = NA_real_
               ,value.medicaid.adult.person10 = NA_real_
               ,value.medicaid.adult.person11 = NA_real_
               ,value.medicaid.adult.person12 = NA_real_
               ,value.medicaid.adult = 0 ) %>%

        mutate(exp.healthcare.medicaid.adult.person1 = NA_real_
               ,exp.healthcare.medicaid.adult.person2 = NA_real_
               ,exp.healthcare.medicaid.adult.person3 = NA_real_
               ,exp.healthcare.medicaid.adult.person4 = NA_real_
               ,exp.healthcare.medicaid.adult.person5 = NA_real_
               ,exp.healthcare.medicaid.adult.person6 = NA_real_
               ,exp.healthcare.medicaid.adult.person7 = NA_real_
               ,exp.healthcare.medicaid.adult.person8 = NA_real_
               ,exp.healthcare.medicaid.adult.person9 = NA_real_
               ,exp.healthcare.medicaid.adult.person10 = NA_real_
               ,exp.healthcare.medicaid.adult.person11 = NA_real_
               ,exp.healthcare.medicaid.adult.person12 = NA_real_
               ,exp.healthcare.medicaid.adult = 0 )

    }


    if(APPLY_MEDICAID_CHILD==FALSE){ # Set everything as NA

      data<-data %>%

        mutate(premium.medicaid.child.person1 = NA_real_
               ,premium.medicaid.child.person2 = NA_real_
               ,premium.medicaid.child.person3 = NA_real_
               ,premium.medicaid.child.person4 = NA_real_
               ,premium.medicaid.child.person5 = NA_real_
               ,premium.medicaid.child.person6 = NA_real_
               ,premium.medicaid.child.person7 = NA_real_
               ,premium.medicaid.child.person8 = NA_real_
               ,premium.medicaid.child.person9 = NA_real_
               ,premium.medicaid.child.person10 = NA_real_
               ,premium.medicaid.child.person11 = NA_real_
               ,premium.medicaid.child.person12 = NA_real_
               ,premium.medicaid.child = 0) %>%

        mutate(value.medicaid.child.person1 = NA_real_
               ,value.medicaid.child.person2 = NA_real_
               ,value.medicaid.child.person3 = NA_real_
               ,value.medicaid.child.person4 = NA_real_
               ,value.medicaid.child.person5 = NA_real_
               ,value.medicaid.child.person6 = NA_real_
               ,value.medicaid.child.person7 = NA_real_
               ,value.medicaid.child.person8 = NA_real_
               ,value.medicaid.child.person9 = NA_real_
               ,value.medicaid.child.person10 = NA_real_
               ,value.medicaid.child.person11 = NA_real_
               ,value.medicaid.child.person12 = NA_real_
               ,value.medicaid.child = 0) %>%

        mutate(exp.healthcare.medicaid.child.person1 = NA_real_
               ,exp.healthcare.medicaid.child.person2 = NA_real_
               ,exp.healthcare.medicaid.child.person3 = NA_real_
               ,exp.healthcare.medicaid.child.person4 = NA_real_
               ,exp.healthcare.medicaid.child.person5 = NA_real_
               ,exp.healthcare.medicaid.child.person6 = NA_real_
               ,exp.healthcare.medicaid.child.person7 = NA_real_
               ,exp.healthcare.medicaid.child.person8 = NA_real_
               ,exp.healthcare.medicaid.child.person9 = NA_real_
               ,exp.healthcare.medicaid.child.person10 = NA_real_
               ,exp.healthcare.medicaid.child.person11= NA_real_
               ,exp.healthcare.medicaid.child.person12 = NA_real_
               ,exp.healthcare.medicaid.child = 0)

    }

    # Adjust for Medicaid take-up

    # Is take-up variable specified?
    if(is.null(data$medicaid_adult_takeup)){
      data$medicaid_adult_takeup<-1
    }

      temp<-data[data$medicaid_adult_takeup==0,]

      temp<-temp %>%

        mutate(premium.medicaid.adult.person1 = NA_real_
               ,premium.medicaid.adult.person2 = NA_real_
               ,premium.medicaid.adult.person3 = NA_real_
               ,premium.medicaid.adult.person4 = NA_real_
               ,premium.medicaid.adult.person5 = NA_real_
               ,premium.medicaid.adult.person6 = NA_real_
               ,premium.medicaid.adult.person7 = NA_real_
               ,premium.medicaid.adult.person8 = NA_real_
               ,premium.medicaid.adult.person9 = NA_real_
               ,premium.medicaid.adult.person10 = NA_real_
               ,premium.medicaid.adult.person11 = NA_real_
               ,premium.medicaid.adult.person12 = NA_real_) %>%

        mutate(value.medicaid.adult.person1 = NA_real_
               ,value.medicaid.adult.person2 = NA_real_
               ,value.medicaid.adult.person3 = NA_real_
               ,value.medicaid.adult.person4 = NA_real_
               ,value.medicaid.adult.person5 = NA_real_
               ,value.medicaid.adult.person6 = NA_real_
               ,value.medicaid.adult.person7 = NA_real_
               ,value.medicaid.adult.person8 = NA_real_
               ,value.medicaid.adult.person9 = NA_real_
               ,value.medicaid.adult.person10 = NA_real_
               ,value.medicaid.adult.person11 = NA_real_
               ,value.medicaid.adult.person12 = NA_real_) %>%

        mutate(exp.healthcare.medicaid.adult.person1 = NA_real_
               ,exp.healthcare.medicaid.adult.person2 = NA_real_
               ,exp.healthcare.medicaid.adult.person3 = NA_real_
               ,exp.healthcare.medicaid.adult.person4 = NA_real_
               ,exp.healthcare.medicaid.adult.person5 = NA_real_
               ,exp.healthcare.medicaid.adult.person6 = NA_real_
               ,exp.healthcare.medicaid.adult.person7 = NA_real_
               ,exp.healthcare.medicaid.adult.person8 = NA_real_
               ,exp.healthcare.medicaid.adult.person9 = NA_real_
               ,exp.healthcare.medicaid.adult.person10 = NA_real_
               ,exp.healthcare.medicaid.adult.person11 = NA_real_
               ,exp.healthcare.medicaid.adult.person12 = NA_real_)


      # Is take-up variable specified?
      if(is.null(data$medicaid_child_takeup)){
        data$medicaid_child_takeup<-1
      }

      temp<-data[data$medicaid_child_takeup==0,]

      temp<-temp %>%

        mutate(premium.medicaid.child.person1 = NA_real_
               ,premium.medicaid.child.person2 = NA_real_
               ,premium.medicaid.child.person3 = NA_real_
               ,premium.medicaid.child.person4 = NA_real_
               ,premium.medicaid.child.person5 = NA_real_
               ,premium.medicaid.child.person6 = NA_real_
               ,premium.medicaid.child.person7 = NA_real_
               ,premium.medicaid.child.person8 = NA_real_
               ,premium.medicaid.child.person9 = NA_real_
               ,premium.medicaid.child.person10 = NA_real_
               ,premium.medicaid.child.person11 = NA_real_
               ,premium.medicaid.child.person12 = NA_real_) %>%

        mutate(value.medicaid.child.person1 = NA_real_
               ,value.medicaid.child.person2 = NA_real_
               ,value.medicaid.child.person3 = NA_real_
               ,value.medicaid.child.person4 = NA_real_
               ,value.medicaid.child.person5 = NA_real_
               ,value.medicaid.child.person6 = NA_real_
               ,value.medicaid.child.person7 = NA_real_
               ,value.medicaid.child.person8 = NA_real_
               ,value.medicaid.child.person9 = NA_real_
               ,value.medicaid.child.person10 = NA_real_
               ,value.medicaid.child.person11 = NA_real_
               ,value.medicaid.child.person12 = NA_real_) %>%

        mutate(exp.healthcare.medicaid.child.person1 = NA_real_
               ,exp.healthcare.medicaid.child.person2 = NA_real_
               ,exp.healthcare.medicaid.child.person3 = NA_real_
               ,exp.healthcare.medicaid.child.person4 = NA_real_
               ,exp.healthcare.medicaid.child.person5 = NA_real_
               ,exp.healthcare.medicaid.child.person6 = NA_real_
               ,exp.healthcare.medicaid.child.person7 = NA_real_
               ,exp.healthcare.medicaid.child.person8 = NA_real_
               ,exp.healthcare.medicaid.child.person9 = NA_real_
               ,exp.healthcare.medicaid.child.person10 = NA_real_
               ,exp.healthcare.medicaid.child.person11= NA_real_
               ,exp.healthcare.medicaid.child.person12 = NA_real_)

      data[data$medicaid_child_takeup==0,]<-temp



    # Total Family's Medicaid value taking into account that premium sometimes is paid per family/per person
    data<-data %>%
      mutate(value.medicaid.adult=rowSums(cbind(value.medicaid.adult.person1,value.medicaid.adult.person2,value.medicaid.adult.person3
                                                ,value.medicaid.adult.person4,value.medicaid.adult.person5,value.medicaid.adult.person6
                                                ,value.medicaid.adult.person7,value.medicaid.adult.person8
                                                ,value.medicaid.adult.person9,value.medicaid.adult.person10
                                                ,value.medicaid.adult.person11,value.medicaid.adult.person12), na.rm = TRUE)-premium.medicaid.adult) %>%

      mutate(value.medicaid.child=rowSums(cbind(value.medicaid.child.person1,value.medicaid.child.person2,value.medicaid.child.person3
                                                ,value.medicaid.child.person4,value.medicaid.child.person5,value.medicaid.child.person6
                                                ,value.medicaid.child.person7,value.medicaid.child.person8
                                                ,value.medicaid.child.person9,value.medicaid.child.person10
                                                ,value.medicaid.child.person11,value.medicaid.child.person12), na.rm = TRUE)-premium.medicaid.child)%>%
      # Total Family's Medicaid value taking into account that premium sometimes is paid per family/per person
      mutate(value.medicaid=rowSums(cbind(value.medicaid.adult,value.medicaid.child), na.rm = TRUE)) %>%



      # Total Family's Medicaid expenses (separatley for adults and children and then together)
      mutate(exp.healthcare.medicaid.adult=rowSums(cbind(exp.healthcare.medicaid.adult.person1,exp.healthcare.medicaid.adult.person2,exp.healthcare.medicaid.adult.person3
                                                   ,exp.healthcare.medicaid.adult.person4,exp.healthcare.medicaid.adult.person5,exp.healthcare.medicaid.adult.person6
                                                   ,exp.healthcare.medicaid.adult.person7,exp.healthcare.medicaid.adult.person8
                                                   ,exp.healthcare.medicaid.adult.person9,exp.healthcare.medicaid.adult.person10
                                                   ,exp.healthcare.medicaid.adult.person11,exp.healthcare.medicaid.adult.person12), na.rm = TRUE)) %>%

      mutate(exp.healthcare.medicaid.child=rowSums(cbind(exp.healthcare.medicaid.child.person1,exp.healthcare.medicaid.child.person2,exp.healthcare.medicaid.child.person3
                                                               ,exp.healthcare.medicaid.child.person4,exp.healthcare.medicaid.child.person5,exp.healthcare.medicaid.child.person6
                                                               ,exp.healthcare.medicaid.child.person7,exp.healthcare.medicaid.child.person8
                                                               ,exp.healthcare.medicaid.child.person9,exp.healthcare.medicaid.child.person10
                                                               ,exp.healthcare.medicaid.child.person11,exp.healthcare.medicaid.child.person12), na.rm = TRUE)) %>%

      mutate(exp.healthcare.medicaid=rowSums(cbind(exp.healthcare.medicaid.adult,exp.healthcare.medicaid.child), na.rm = TRUE))

    # OPTIMIZATION STEP 1: Those who are eligible for Medicaid take it ----
    data<-data %>%
      mutate(healthcare.source.person1 = case_when(!is.na(value.medicaid.adult.person1) | !is.na(value.medicaid.child.person1) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person2 = case_when(!is.na(value.medicaid.adult.person2) | !is.na(value.medicaid.child.person2) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person3 = case_when(!is.na(value.medicaid.adult.person3) | !is.na(value.medicaid.child.person3) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person4 = case_when(!is.na(value.medicaid.adult.person4) | !is.na(value.medicaid.child.person4) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person5 = case_when(!is.na(value.medicaid.adult.person5) | !is.na(value.medicaid.child.person5) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person6 = case_when(!is.na(value.medicaid.adult.person6) | !is.na(value.medicaid.child.person6) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person7 = case_when(!is.na(value.medicaid.adult.person7) | !is.na(value.medicaid.child.person7) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person8 = case_when(!is.na(value.medicaid.adult.person8) | !is.na(value.medicaid.child.person8) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person9 = case_when(!is.na(value.medicaid.adult.person9) | !is.na(value.medicaid.child.person9) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person10 = case_when(!is.na(value.medicaid.adult.person10) | !is.na(value.medicaid.child.person10) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person11 = case_when(!is.na(value.medicaid.adult.person11) | !is.na(value.medicaid.child.person11) ~ "Medicaid/CHIP", TRUE ~ NA_character_)
             ,healthcare.source.person12 = case_when(!is.na(value.medicaid.adult.person12) | !is.na(value.medicaid.child.person12) ~ "Medicaid/CHIP", TRUE ~ NA_character_))

    # Set value of Medicaid to NA if person is not on Medicaid (matters for calculating the famsize on employer plan)
    data<-data %>%
    mutate(value.medicaid.person1= case_when(healthcare.source.person1=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person1, TRUE ~ NA_real_)
           ,value.medicaid.person2= case_when(healthcare.source.person2=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person2, TRUE ~ NA_real_)
           ,value.medicaid.person3= case_when(healthcare.source.person3=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person3, TRUE ~ NA_real_)
           ,value.medicaid.person4= case_when(healthcare.source.person4=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person4, TRUE ~ NA_real_)
           ,value.medicaid.person5= case_when(healthcare.source.person5=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person5, TRUE ~ NA_real_)
           ,value.medicaid.person6= case_when(healthcare.source.person6=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person6, TRUE ~ NA_real_)
           ,value.medicaid.person7= case_when(healthcare.source.person7=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person7, TRUE ~ NA_real_)
           ,value.medicaid.person8= case_when(healthcare.source.person8=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person8, TRUE ~ NA_real_)
           ,value.medicaid.person9= case_when(healthcare.source.person9=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person9, TRUE ~ NA_real_)
           ,value.medicaid.person10= case_when(healthcare.source.person10=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person10, TRUE ~ NA_real_)
           ,value.medicaid.person11= case_when(healthcare.source.person11=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person11, TRUE ~ NA_real_)
           ,value.medicaid.person12= case_when(healthcare.source.person12=="Medicaid/CHIP" ~ exp.healthcare.medicaid.person12, TRUE ~ NA_real_)
           )


    ## ACA Subsidy ----

    # Assign costs of Second Lowest Cost Silverplan FOR EACH PERSON
    data$exp.healthcare.healthexchange.person1<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson1")
    data$exp.healthcare.healthexchange.person2<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson2")
    data$exp.healthcare.healthexchange.person3<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson3")
    data$exp.healthcare.healthexchange.person4<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson4")
    data$exp.healthcare.healthexchange.person5<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson5")
    data$exp.healthcare.healthexchange.person6<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson6")
    data$exp.healthcare.healthexchange.person7<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson7")
    data$exp.healthcare.healthexchange.person8<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson8")
    data$exp.healthcare.healthexchange.person9<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson9")
    data$exp.healthcare.healthexchange.person10<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson10")
    data$exp.healthcare.healthexchange.person11<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson11")
    data$exp.healthcare.healthexchange.person12<-function.healthcareExp.SLCS(data
                                                                            , ageofpersonvar = "agePerson12")

    # If person has already taken Medicaid - no expenses on the health-exchange
    data<-data %>%
      mutate(exp.healthcare.healthexchange.person1 = case_when(healthcare.source.person1=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person1)
             ,exp.healthcare.healthexchange.person2 = case_when(healthcare.source.person2=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person2)
             ,exp.healthcare.healthexchange.person3 = case_when(healthcare.source.person3=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person3)
             ,exp.healthcare.healthexchange.person4 = case_when(healthcare.source.person4=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person4)
             ,exp.healthcare.healthexchange.person5 = case_when(healthcare.source.person5=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person5)
             ,exp.healthcare.healthexchange.person6 = case_when(healthcare.source.person6=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person6)
             ,exp.healthcare.healthexchange.person7 = case_when(healthcare.source.person7=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person7)
             ,exp.healthcare.healthexchange.person8 = case_when(healthcare.source.person8=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person8)
             ,exp.healthcare.healthexchange.person9 = case_when(healthcare.source.person9=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person9)
             ,exp.healthcare.healthexchange.person10 = case_when(healthcare.source.person10=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person10)
             ,exp.healthcare.healthexchange.person11 = case_when(healthcare.source.person11=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person11)
             ,exp.healthcare.healthexchange.person12 = case_when(healthcare.source.person12=="Medicaid/CHIP" ~ NA_real_, TRUE ~ exp.healthcare.healthexchange.person12))



    # Total family expenses on health exchange
    data$exp.healthcare.healthexchange<-rowSums(cbind(data$exp.healthcare.healthexchange.person1,data$exp.healthcare.healthexchange.person2
                                                      ,data$exp.healthcare.healthexchange.person3,data$exp.healthcare.healthexchange.person4
                                                      ,data$exp.healthcare.healthexchange.person5,data$exp.healthcare.healthexchange.person6
                                                      ,data$exp.healthcare.healthexchange.person7,data$exp.healthcare.healthexchange.person8
                                                      ,data$exp.healthcare.healthexchange.person9,data$exp.healthcare.healthexchange.person10
                                                      ,data$exp.healthcare.healthexchange.person11,data$exp.healthcare.healthexchange.person12), na.rm = TRUE)

    # No health exchange before 2014 (but it is used as an approximation for out-of-pocket costs)
    #data$exp.healthcare.healthexchange[data$ruleYear<2014]<-0

    # Premium for the whole family
    data$premium.aca<-function.aca(data)

    if(APPLY_ACA==FALSE){
      data$premium.aca<-NA_real_
    }

    # Adjust for take-up

    # Is take-up variable specified?
    if(is.null(data$aca_takeup)){
      data$aca_takeup<-1
    }

    data$premium.aca[data$aca_takeup==0]<-NA_real_

    data$value.aca<-rowMaxs(cbind(data$exp.healthcare.healthexchange-data$premium.aca,0))
    data$value.aca[is.na(data$value.aca)]<-0

    ## Employer Sponsored Plan ----
    # Single coverage
    # Family coverage
    # Employee-plus-one coverage
    # Calculate how many family members are on Medicaid (how many non-NAs):

    # Calculate non-NAs for the numadults
    data$numadults.onMedicaid<-rowSums(!is.na(cbind(data$value.medicaid.adult.person1,data$value.medicaid.adult.person2,data$value.medicaid.adult.person3
                                                  ,data$value.medicaid.adult.person4,data$value.medicaid.adult.person5,data$value.medicaid.adult.person6
                                                  ,data$value.medicaid.adult.person7,data$value.medicaid.adult.person8,data$value.medicaid.adult.person9
                                                  ,data$value.medicaid.adult.person10,data$value.medicaid.adult.person11,data$value.medicaid.adult.person12)))

    # Calculate non-NAs for the numkids
    data$nukids.onMedicaid<-rowSums(!is.na(cbind(data$value.medicaid.child.person1,data$value.medicaid.child.person2,data$value.medicaid.child.person3
                                                    ,data$value.medicaid.child.person4,data$value.medicaid.child.person5,data$value.medicaid.child.person6
                                                    ,data$value.medicaid.child.person7,data$value.medicaid.child.person8,data$value.medicaid.child.person9
                                                 ,data$value.medicaid.child.person10,data$value.medicaid.child.person11,data$value.medicaid.child.person12)))


    data$famsize.onMedicaid<-data$numadults.onMedicaid+data$nukids.onMedicaid

    # How many family members (potentially) subscribe for the employer plan? Everyone who is not on Medicaid
    data$famsize.foremployerhealthcare<-data$famsize-data$famsize.onMedicaid

    # create copy of famsize variable to use in the exp function below bc we need actual famsize variable within the expense calculation for ALICE but need famsize.foremployerhealthcare for PRD
    data$famsize2<-data$famsize
    # Total costs of employer health insurance
    data$exp.healthcare.employer<-function.healthcareExp.ALICE(data
                                                               , famsizevar = "famsize2")[,2]
    # Total costs of employee premium from the employer plan health insurance
    data$premium.employer<-function.healthcareExp.ALICE(data
                                                        , famsizevar = "famsize2")[,3] # employee premium

    data$exp.healthcare.SS <- data$ALICE.expense.healthcare.family
    data$exp.healthcare.employer <- NULL
    data$premium.employer <- NULL

    data$exp.healthcare.employer<-function.healthcareExp.ALICE(data
                                                              , famsizevar = "famsize.foremployerhealthcare")[,2]
    # Employee premium
    data$premium.employer<-function.healthcareExp.ALICE(data
                                                        , famsizevar = "famsize.foremployerhealthcare")[,3]


    # If option of healthcare through employer is not available, then set everything to NA
    data<-data %>%
      mutate(exp.healthcare.employer = case_when(empl_healthcare==0 ~ NA_real_, TRUE~exp.healthcare.employer)
             ,premium.employer = case_when(empl_healthcare==0 ~ NA_real_, TRUE~premium.employer)) %>%
      # Value of the employer healthcare
      mutate(value.employerhealthcare = exp.healthcare.employer - premium.employer)
    data$value.employerhealthcare[is.na(data$value.employerhealthcare)]<-0

    # OPTIMIZATION STEP 2: Compare total costs of employer plan and health exchange plan----

    # Determine what healhcare sources are used
    data<-data %>%
      # Generate out-of-pocket premium
      mutate(premium.outofpocket = exp.healthcare.healthexchange) %>%
      # Employer healthcare is available and Health Exchange is available, then compare two plans
      mutate(healthcare.source = case_when((!is.na(premium.employer) & !is.na(premium.aca) & premium.employer > premium.aca) ~ "Marketplace"
                                           ,(!is.na(premium.employer) & !is.na(premium.aca) & premium.employer <= premium.aca) ~ "Employer"
                                           ,(is.na(premium.employer) & !is.na(premium.aca)) ~ "Marketplace" # If Employer Healthcare is unavailable
                                           ,(!is.na(premium.employer) & is.na(premium.aca) & premium.employer <= premium.outofpocket) ~ "Employer" # If ACA subsidy is unavailable and Health Exchange more expensive than Employer plan
                                           ,(!is.na(premium.employer) & is.na(premium.aca) & premium.employer > premium.outofpocket) ~ "Out-of-pocket" # If ACA subsidy is unavailable and Health Exchange is cheaper than Employer plan
                                           ,(is.na(premium.employer) & is.na(premium.aca)) ~ "Out-of-pocket" # If both unavailable
                                           , TRUE ~ NA_character_)
      )

    # If at least one person is on Medicaid
    condition<-((!is.na(data$healthcare.source.person1) & data$healthcare.source.person1=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person2) & data$healthcare.source.person2=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person3) & data$healthcare.source.person3=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person4) & data$healthcare.source.person4=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person5) & data$healthcare.source.person5=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person6) & data$healthcare.source.person6=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person7) & data$healthcare.source.person7=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person8) & data$healthcare.source.person8=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person9) & data$healthcare.source.person9=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person10) & data$healthcare.source.person10=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person11) & data$healthcare.source.person11=="Medicaid/CHIP")
                | (!is.na(data$healthcare.source.person12) & data$healthcare.source.person12=="Medicaid/CHIP"))

    # Medicaid is not the only source
    data$healthcare.source[condition & data$famsize.onMedicaid!=data$famsize]<-paste0(data$healthcare.source[condition & data$famsize.onMedicaid!=data$famsize], " & Medicaid/CHIP")
    # Everyone in the family is on Medicaid
    data$healthcare.source[condition & data$famsize.onMedicaid==data$famsize]<-"Medicaid/CHIP"

    # Before 2014 we proxy it as out-of-pocket
    data$healthcare.source[data$healthcare.source == "Marketplace" & data$ruleYear<2014]<-"Out-of-pocket"
    data$healthcare.source[data$healthcare.source == "Marketplace & Medicaid/CHIP" & data$ruleYear<2014]<-"Out-of-pocket & Medicaid/CHIP"

    # All possible combinations of the Healthcare costs
    data<-data %>%
      mutate(exp.healthcare = case_when(healthcare.source == "Marketplace" ~ exp.healthcare.healthexchange
                                        ,healthcare.source == "Employer" ~ exp.healthcare.employer
                                        ,healthcare.source == "Out-of-pocket" ~ exp.healthcare.healthexchange
                                        ,healthcare.source == "Medicaid/CHIP" ~ exp.healthcare.medicaid
                                        ,healthcare.source == "Marketplace & Medicaid/CHIP" ~ exp.healthcare.healthexchange + exp.healthcare.medicaid
                                        ,healthcare.source == "Employer & Medicaid/CHIP" ~ exp.healthcare.employer + exp.healthcare.medicaid
                                        ,healthcare.source == "Out-of-pocket & Medicaid/CHIP" ~ exp.healthcare.healthexchange + exp.healthcare.medicaid))

    # Set values of certain benefits to zero depending on a choice of healthcare source
    data$value.aca[data$healthcare.source != "Marketplace" & data$healthcare.source != "Marketplace & Medicaid/CHIP"]<-0
    data$value.employerhealthcare[data$healthcare.source != "Employer" & data$healthcare.source != "Employer & Medicaid/CHIP"]<-0

    # Set values of certain premiums to zero depending on a choice of healthcare source
    data$premium.aca[data$healthcare.source != "Marketplace" & data$healthcare.source != "Marketplace & Medicaid/CHIP"]<-NA
    data$premium.employer[data$healthcare.source != "Employer" & data$healthcare.source != "Employer & Medicaid/CHIP"]<-NA
    data$premium.outofpocket[data$healthcare.source != "Out-of-pocket" & data$healthcare.source != "Out-of-pocket & Medicaid/CHIP"]<-NA

    data$exp.healthcare <- data$exp.healthcare + data$oop.health.family.ALICE


    data<-data %>%
      mutate(netexp.healthcare = exp.healthcare-value.aca-value.medicaid-value.employerhealthcare) # Total out-of-pocket costs

    #View(data[,c("income", "healthcare.source", "netexp.healthcare", "exp.healthcare.employer", "exp.healthcare.healthexchange", "exp.healthcare.medicaid", "value.aca", "value.medicaid", "value.medicaid.adult", "value.medicaid.child", "value.employerhealthcare", "premium.aca", "premium.employer", "premium.outofpocket", "empl_healthcare")])

  }

  return(data)

}

# OTHER Benefits ----
# (liheap, tanf, will add ssi and ssdi here when finished)
BenefitsCalculator.OtherBenefits<-function(data, APPLY_TANF, APPLY_SSDI, APPLY_SSI){


  # Do ssdi before tanf because ssdi counts as unearned income for tanf calculation

  # # SSDI
  if (APPLY_SSDI==FALSE){
    data$value.ssdi<-0
  }else if(APPLY_SSDI==TRUE){
    value.ssdi<-function.ssdiBenefit(data)
    data$value.ssdi<-NULL
    data <- data %>%
      cbind(value.ssdi)

  }


  if(APPLY_TANF==FALSE){
    data$value.tanf<-0
  }else if(APPLY_TANF==TRUE){

    data$index <- as.numeric(row.names(data))

    if(!("householdid" %in% colnames(data))){
      data$householdid <- 1
    }

    data <- data[order(data$householdid, data$Year),]

    data$value.tanf<-function.tanfBenefit(data)

    data <- data[order(data$index), ]
    data$index <- NULL
    
    # Adjust for take-up
    # Is take-up variable specified?
    if(is.null(data$tanf_takeup)){
      data$tanf_takeup<-1
    }
    data$value.tanf[data$tanf_takeup==0]<-0

  }

  # SSI
  if(APPLY_SSI==FALSE){

    # Set all the output to zero
    data$value.ssi<-0
    data$value.ssiAdlt1<-0
    data$value.ssiAdlt2<-0
    data$value.ssiAdlt3<-0
    data$value.ssiAdlt4<-0
    data$value.ssiAdlt5<-0
    data$value.ssiAdlt6<-0

    data$value.ssiChild1<-0
    data$value.ssiChild2<-0
    data$value.ssiChild3<-0
    data$value.ssiChild4<-0
    data$value.ssiChild5<-0
    data$value.ssiChild6<-0
    # When SSI is TRUE 'hadssi' is created in benefits_function.R instead of here
    data$hadssi1<-ifelse(data$disability1==1 & !is.na(data$disability1) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi2<-ifelse(data$disability2==1 & !is.na(data$disability2) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi3<-ifelse(data$disability3==1 & !is.na(data$disability3) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi4<-ifelse(data$disability4==1 & !is.na(data$disability4) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi5<-ifelse(data$disability5==1 & !is.na(data$disability5) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi6<-ifelse(data$disability6==1 & !is.na(data$disability6) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi7<-ifelse(data$disability7==1 & !is.na(data$disability7) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi8<-ifelse(data$disability8==1 & !is.na(data$disability8) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi9<-ifelse(data$disability9==1 & !is.na(data$disability9) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi10<-ifelse(data$disability10==1 & !is.na(data$disability10) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi11<-ifelse(data$disability11==1 & !is.na(data$disability11) & data$prev_ssi==1
                         ,1 ,0)
    data$hadssi12<-ifelse(data$disability12==1 & !is.na(data$disability12) & data$prev_ssi==1
                         ,1 ,0)

  }else if(APPLY_SSI==TRUE){

    value.ssi<-function.ssiBenefit(data)
    data$value.ssi<-NULL
    data <- data%>%
      cbind(value.ssi) # merge on all output (total values + allocation across family members)
  }


  return(data)

} #end other benefits function


# FOOD and HOUSING----
# (Housing Vouchers (Section 8, RAP), SNAP, SLP, WIC)

BenefitsCalculator.FoodandHousing<-function(data, APPLY_SECTION8=FALSE, APPLY_LIHEAP=FALSE, APPLY_SNAP=FALSE, APPLY_SLP=FALSE, APPLY_WIC=FALSE, APPLY_RAP=FALSE, APPLY_FRSP=FALSE, frsp_share = 0.3){

#some programs rely on liheap, but liheap is run last
  data$value.liheap<-0

  # Section 8
  if(APPLY_SECTION8==FALSE & APPLY_RAP==FALSE & APPLY_FRSP==FALSE){
    data$value.section8<-0
    data$netexp.rentormortgage<-data$exp.rentormortgage
    data$netexp.housing<-data$exp.housing
    data$netexp.utilities<-data$exp.utilities
    }else { if(APPLY_SECTION8==TRUE & APPLY_RAP==FALSE & APPLY_FRSP==FALSE){

    data$value.section8<-function.section8Benefit(data=data)

    #data<-data[!is.na(data$value.section8),]
    data$value.section8[is.na(data$value.section8)] <- 0

    # Adjust for take-up
    # Is take-up variable specified?
    if(is.null(data$section8_takeup)){
      data$section8_takeup<-1
    }
    data$value.section8[data$section8_takeup==0]<-0

    data$share.rent<-(data$exp.rentormortgage)/(data$exp.rentormortgage+data$exp.utilities) # share of rent in total housing expense
    data$share.rent[is.na(data$share.rent)]<-0 # in case the denominator is zero, just set the whole share to 0

    data$share.utilities<-(data$exp.utilities)/(data$exp.rentormortgage+data$exp.utilities)  # share of utilities in total housing expense
    data$share.utilities[is.na(data$share.utilities)]<-0 # in case the denominator is zero, just set the whole share to 0

    # Allocate Section 8 voucher to net rent & net utilities reduction in accordance with the share of utilities and rent from above
    data$netexp.rentormortgage<-rowMaxs(cbind(data$exp.rentormortgage-data$share.rent*data$value.section8,0))
    data$netexp.utilities<-rowMaxs(cbind(data$exp.utilities-data$share.utilities*data$value.section8,0))

    data$netexp.housing<-data$netexp.rentormortgage+data$netexp.utilities

  }else{ if(APPLY_SECTION8==FALSE & APPLY_RAP==TRUE & APPLY_FRSP==FALSE){

    data$value.section8<-function.RAPBenefit(data=data)

    data<-data[!is.na(data$value.section8),]
    
    # Adjust for take-up
    # Is take-up variable specified?
    if(is.null(data$section8_takeup)){
      data$section8_takeup<-1
    }
    data$value.section8[data$section8_takeup==0]<-0

    data$share.rent<-(data$exp.rentormortgage)/(data$exp.rentormortgage+data$exp.utilities) # share of rent in total housing expense
    data$share.rent[is.na(data$share.rent)]<-0 # in case the denominator is zero, just set the whole share to 0

    data$share.utilities<-(data$exp.utilities)/(data$exp.rentormortgage+data$exp.utilities)  # share of utilities in total housing expense
    data$share.utilities[is.na(data$share.utilities)]<-0 # in case the denominator is zero, just set the whole share to 0

    # Allocate Section 8 voucher to net rent & net utilities reduction in accordance with the share of utilities and rent from above
    data$netexp.rentormortgage<-rowMaxs(cbind(data$exp.rentormortgage-data$share.rent*data$value.section8,0))
    data$netexp.utilities<-rowMaxs(cbind(data$exp.utilities-data$share.utilities*data$value.section8,0))

    data$netexp.housing<-data$netexp.rentormortgage+data$netexp.utilities

  }else { if(APPLY_SECTION8==FALSE & APPLY_RAP==FALSE & APPLY_FRSP==TRUE){

    data$value.section8<-function.FRSPBenefit(data
                                              , shareOfRent = frsp_share
                                              , CareerMapIndicator1 = CareerMAP)  # User input - varies from 40 to 60%
    
    # Adjust for take-up
    # Is take-up variable specified?
    if(is.null(data$section8_takeup)){
      data$section8_takeup<-1
    }
    data$value.section8[data$section8_takeup==0]<-0
    
    data$share.rent<-(data$exp.rentormortgage)/(data$exp.rentormortgage+data$exp.utilities) # share of rent in total housing expense
    data$share.rent[is.na(data$share.rent)]<-0 # in case the denominator is zero, just set the whole share to 0
    
    data$share.utilities<-(data$exp.utilities)/(data$exp.rentormortgage+data$exp.utilities)  # share of utilities in total housing expense
    data$share.utilities[is.na(data$share.utilities)]<-0 # in case the denominator is zero, just set the whole share to 0
    
    # Allocate Section 8 voucher to net rent & net utilities reduction in accordance with the share of utilities and rent from above
    data$netexp.rentormortgage<-rowMaxs(cbind(data$exp.rentormortgage-data$share.rent*data$value.section8,0))
    data$netexp.utilities<-rowMaxs(cbind(data$exp.utilities-data$share.utilities*data$value.section8,0))
    
    data$netexp.housing<-data$netexp.rentormortgage+data$netexp.utilities
    
  }

  }}}
  # SNAP
  if(APPLY_SNAP==FALSE){
    data$value.snap<-0
    data$netexp.food<-data$exp.food
  }else { if(APPLY_SNAP==TRUE){

    data$value.snap<-rowMins(cbind(function.snapBenefit(data), data$exp.food)) # Benefit cannot be greater than expense

    # Adjust for take-up
    # Is take-up variable specified?
    if(is.null(data$snap_takeup)){
      data$snap_takeup<-1
    }
    data$value.snap[data$snap_takeup==0]<-0

    data$netexp.food<-rowMaxs(cbind(data$exp.food-data$value.snap,0))
  }}


  if(APPLY_SLP==FALSE){
    data$value.schoolmeals<-0
  }else{ if(APPLY_SLP==TRUE){
    # School Lunches
    data$value.schoolmeals<-rowMins(cbind(function.schoolmeals(data), data$netexp.food)) # Benefit cannot be greater than expense. Net expense for food created in SNAP function

    data$netexp.food<-rowMaxs(cbind(data$netexp.food-data$value.schoolmeals,0))

  }}


  if(APPLY_WIC==FALSE){ # Note: WIC - must go after medicaid, snap &  TANF
    data$value.wic<-0
  }else{ if(APPLY_WIC==TRUE){

    data$value.wic<-rowMins(cbind(function.wicBenefit(data), data$netexp.food))


  data$netexp.food<-rowMaxs(cbind(data$netexp.food-data$value.wic,0))

  }}

  # LIHEAP - only workign for CT right now.
  if(APPLY_LIHEAP==FALSE & APPLY_SECTION8==FALSE){
    data$netexp.utilities<-data$exp.utilities
    data$value.liheap<-0
  }else{ if(APPLY_LIHEAP==FALSE & APPLY_SECTION8==TRUE){
      data$netexp.utilities<-data$netexp.utilities
      data$value.liheap<-0
  }else {
    data$value.liheap<-function.liheapBenefit(data)
    data$netexp.utilities<-data$netexp.utilities-data$value.liheap
  }}



  return(data)

}


# TAXES AND TAX CREDITS ----

# Variables required to calculate each tax and tax credit are specified in the function
# Function will not run unless all inputs are specified
BenefitsCalculator.TaxesandTaxCredits<-function(data, APPLY_EITC=FALSE, APPLY_CTC=FALSE, APPLY_CDCTC=FALSE, APPLY_TAXES=TRUE){ #DEFAULT TAXES TO TRUE


# Federal Taxes and Tax Credits ----

  # fed INCOME TAXES
  if(APPLY_TAXES==FALSE){
    data$tax.income.fed<-0
    data$tax.federal<-0
    data$tax.FICA<-0
    data$tax.federal_tm12<-0
    data$tax.income.state_tm12<-0
    data$tax.income.local_tm12<-0
  }else if(APPLY_TAXES==TRUE){

# Current Year Taxes (for take-home pay) 
#Federal Income Tax

    data$TaxableamtofSSDI<-0
data$tax.income.fed<-function.fedinctax(data
                                        , incomevar = "income")


# FICA TAx
data$tax.FICA<-function.ficatax(data
                                , employmentincomevar = "income")



data$tax.federal<-data$tax.income.fed # Input into the Federal Tax Credits


# Past Year Taxes (for Tax Credits)

#Federal Income Tax
data$tax.income.fed_tm12<-function.fedinctax(data
                                        , incomevar = "income_tm12")

# FICA TAx
data$tax.FICA_tm12<-function.ficatax(data
                                , employmentincomevar = "income_tm12")

data$tax.federal_tm12<-data$tax.income.fed_tm12 # Input into the Federal Tax Credits


}
# Adjust for possible non-filers (kids for example)

# Federal EITC
if(APPLY_EITC==FALSE){
data$value.eitc.fed<-0
}else if(APPLY_EITC==TRUE){

data$value.eitc.fed<-function.fedeitc(data
                                      , incomevar = "income_tm12"
                                      , investmentincomevar ="income.investment"
                                      , ageofRespondentvar = "agePerson1"
                                      , ageofSpousevar = "agePerson2")

}

# CTC
if(APPLY_CTC==FALSE){
  data$value.ctc.fed<-0
}else if(APPLY_CTC==TRUE){

  # Federal CTC
  data$value.ctc.fed<-function.fedctc(data
                                      , incomevar = "income_tm12"
                                      , totalfederaltaxvar ="tax.federal_tm12")
}

# CDCTC
if(APPLY_CDCTC==FALSE){
  data$value.cdctc.fed<-0
}else if(APPLY_CDCTC==TRUE){

  # Federal CDCTC
  data$value.cdctc.fed<-function.fedcdctc(data
                                          , incomevar="income_tm12"
                                          , qualifyingexpensesvar="netexp.childcare"
                                          , totalfederaltaxvar ="tax.federal_tm12")

}

data$value.taxcredits.fed<-data$value.eitc.fed+data$value.ctc.fed+data$value.cdctc.fed
# State & Local Taxes and State Tax Credits ----

if(APPLY_TAXES==FALSE){
  data$tax.income.state<-0
  data$tax.income.local<-0
 }else if(APPLY_TAXES==TRUE){

# Current Year State Income Tax (for take-home pay)

#Current year - state
data$tax.income.state<-function.stateinctax(data
                                            , incomevar = "income"
                                            , fedincometaxvar = "tax.income.fed"
                                            , fedtaxcreditsvar = "value.taxcredits.fed")
data$tax.income.state[is.na(data$tax.income.state)]<-0

# Current Year- local

data$tax.income.local<-function.localinctax(data
                                            ,incomevar = "income")
data$tax.income.local[is.na(data$tax.income.local)]<-0

# Past Year

#past year - state
data$tax.income.state_tm12<-function.stateinctax(data
                                            , incomevar = "income_tm12"
                                            , fedincometaxvar = "tax.income.fed"
                                            , fedtaxcreditsvar = "value.taxcredits.fed")
data$tax.income.state_tm12[is.na(data$tax.income.state_tm12)]<-0


data$tax.income.local_tm12<-function.localinctax(data
                                            ,incomevar = "income_tm12")
data$tax.income.local_tm12[is.na(data$tax.income.local)]<-0



}

# State EITC
if(APPLY_EITC==FALSE){
  data$value.eitc.state<-0
}else if(APPLY_EITC==TRUE){
# State EITC
data$value.eitc.state<-function.stateeitc(data
                                          , incomevar = "income_tm12"
                                          , investmentincomevar ="income.investment"
                                          , federaleitcvar = "value.eitc.fed"
                                          , stateincometaxvar = "tax.income.state_tm12"
                                          , ageofRespondentvar = "agePerson1"
                                          , ageofSpousevar = "agePerson2"
                                          , ageofYoungestChildvar = "ageofYoungestChild")
data$value.eitc.state[is.na(data$value.eitc.state)]<-0

}

# ER 3/28/24: temporarily removing state ctc from the PRD
# State CTC
if(APPLY_CTC==FALSE){
  data$value.ctc.state<-0
}else if(APPLY_CTC==TRUE){

  # State CTC
  # data$value.ctc.state<-function.statectc(data
  #                                         , incomevar = "income_tm12"
  #                                         , stateincometaxvar = "tax.income.state_tm12"
  #                                         , federalctcvar = "value.ctc.fed")
  
  data$value.ctc.state<-0
  data$value.ctc.state[is.na(data$value.ctc.state)]<-0

}

# State CDCTC
if(APPLY_CDCTC==FALSE){
  data$value.cdctc.state<-0
}else if(APPLY_CDCTC==TRUE){
  # State CDCTC
  data$value.cdctc.state<-function.statecdctc(data
                                              , incomevar="income_tm12"
                                              , qualifyingexpensesvar="netexp.childcare"
                                              , stateincometaxvar="tax.income.state_tm12"
                                              , federalcdctcvar = "value.cdctc.fed")
  data$value.cdctc.state[is.na(data$value.cdctc.state)]<-0
}


# Adjust for take-up
# Is take-up variables specified?
if(is.null(data$eitc_takeup)){
  data$eitc_takeup<-1
}

# Is take-up variables specified?
if(is.null(data$ctc_takeup)){
  data$ctc_takeup<-1
}

# Is take-up variables specified?
if(is.null(data$cdctc_takeup)){
  data$cdctc_takeup<-1
}

data$value.eitc.fed[data$eitc_takeup==0]<-0
data$value.eitc.state[data$eitc_takeup==0]<-0

data$value.ctc.fed[data$ctc_takeup==0]<-0
data$value.ctc.state[data$ctc_takeup==0]<-0

data$value.cdctc.fed[data$cdctc_takeup==0]<-0
data$value.cdctc.state[data$cdctc_takeup==0]<-0


return(data)

}

# CREATE VARIABLES FOR PLOTTING ----
function.createVars<-function(data){

data<-data %>%
    mutate( income.aftertax.noTC = income +income.gift+income.investment+income.child_support - tax.income.fed - tax.income.state -tax.income.local - tax.FICA
            # uses exp.healthcare.SS: healthcare expense based on costs of employer sponsored health insurance; plotted in expenses bar chart in CLIFF Dashboard and Planner
            ,SelfSufficiency = exp.childcare+exp.healthcare.SS+exp.food+exp.rentormortgage+exp.transportation+exp.misc+exp.utilities+exp.tech

              ,total.transfers = value.CCDF+value.HeadStart+value.earlyHeadStart+value.PreK+
              value.liheap+value.section8+value.snap+value.wic+value.schoolmeals+value.tanf+
              value.aca+value.medicaid+value.ssi+value.ssdi+
              value.cdctc.fed+value.cdctc.state+value.eitc.fed+value.eitc.state+value.ctc.fed+value.ctc.state+value.FATES
            , total.taxes = tax.income.fed+tax.income.state+tax.income.local + tax.FICA
            # uses exp.healthcare: healthcare expense based on source of healthcare the person is on (Medicaid,ACA,etc.); used in NetResources
            ,total.expenses = exp.childcare+exp.healthcare+exp.food+exp.rentormortgage+
              exp.transportation+exp.misc+exp.utilities+exp.tech
            ,total.expenses.SS = exp.childcare+exp.healthcare.SS+exp.food+exp.rentormortgage+
              exp.transportation+exp.misc+exp.utilities +exp.tech

            # Total values of tax credits
            ,value.cdctc=value.cdctc.fed+value.cdctc.state
            ,value.ctc=value.ctc.fed+value.ctc.state
            ,value.eitc=value.eitc.fed+value.eitc.state

            ,NetResources = income+income.gift+income.investment+income.child_support+value.employerhealthcare+total.transfers-total.taxes-total.expenses
            ,NetResources.FATES = income+income.gift+income.investment+income.child_support+value.employerhealthcare+total.transfers-total.taxes-total.expenses +value.FATES

            #,netexp.housing=netexp.rentormortgage+netexp.utilities
            ,AfterTaxIncome=(income+income.gift+income.investment+income.child_support)-(tax.income.fed+tax.income.state+tax.income.local+tax.FICA)

            )

            # Bound take-home pay below by 0 - just in case
            data$AfterTaxIncome<-rowMaxs(cbind(data$AfterTaxIncome,0))
            data$income.aftertax.noTC<-rowMaxs(cbind(data$income.aftertax.noTC,0))


return(data)
}


# For the CLIFF planner ----
function.createVars.CLIFF<-function(data){

  data<-data %>%
    mutate( income.aftertax.noTC = income+income.gift+income.investment+income.child_support - tax.income.fed - tax.income.state -tax.income.local - tax.FICA

            # Totals
            ,total.transfers = value.CCDF+value.HeadStart+value.earlyHeadStart+value.PreK+
              value.liheap+value.section8+value.tanf+value.snap+value.schoolmeals+value.wic+
              value.aca+value.medicaid+value.ssi+value.ssdi+
              value.cdctc.fed+value.cdctc.state+value.eitc.fed+value.eitc.state+value.ctc.fed+value.ctc.state
            ,total.taxes = tax.income.fed+tax.income.state+tax.income.local+tax.FICA
            ,total.expenses = exp.childcare+exp.healthcare+exp.food+exp.rentormortgage+
              exp.transportation+exp.misc+exp.utilities+exp.tech

            # Total values of tax credits
            ,value.cdctc=value.cdctc.fed+value.cdctc.state
            ,value.ctc=value.ctc.fed+value.ctc.state
            ,value.eitc=value.eitc.fed+value.eitc.state

            # Other elements of the budget constraint
            ,value.tuition.net = value.tuition-value.grants-value.loans.student

            ,NetResources = income+income.gift+income.investment+income.child_support+value.employerhealthcare+total.transfers-value.tuition.net-total.taxes-total.expenses-studentLoanRepayment-value.loans

            #,netexp.housing=netexp.rentormortgage+netexp.utilities
            ,AfterTaxIncome=(income+income.gift+income.investment+income.child_support)-(tax.income.fed+tax.income.state+tax.income.local+tax.FICA)

    )

  # Bound take-home pay below by 0 - just in case
  data$AfterTaxIncome<-rowMaxs(cbind(data$AfterTaxIncome,0))
  data$income.aftertax.noTC<-rowMaxs(cbind(data$income.aftertax.noTC,0))


  return(data)
}

