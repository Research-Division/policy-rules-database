###################
# Read in necessary libraries, create functions to format data frame based on 
# user inputs and functions that create charts
###################

rm(list=ls())


# Load libraries, functions, and set gloabl parameters ----
source(paste0("BenefitsCalculator/loadFilesandFunctions.R"), local=TRUE) # Load auxiliary files and required functions
source(paste0("BenefitsCalculator/libraries.R"), local=TRUE) # Load required packages

loc_meta <- fread('locations_list.csv')
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


#childcare needs assumptions
k_ftorpt <- "FT" #not used anywhere in benefits calc right now
schoolagesummercare <- "PT" #don't change - ben calc not yet set up to handle FT
headstart_ftorpt <-"PT" #use the same for both headstart & earlyheadstart 
preK_ftorpt <- "PT"
contelig.headstart <- TRUE
contelig.earlyheadstart <- TRUE
contelig.ccdf <- TRUE
budget.ALICE<-"survivalforcliff"
ruleYear <- as.numeric(format(Sys.Date(), "%Y"))

# THIS SECTION OF INPUTS CAN BE USED FOR TESTING. WILL NOT AFFECT THE TOOLS ----
# inputs<-list()
# inputs$numadults<-1
# inputs$numkids<-2
# inputs$city.name<-"Broward County, FL"
# inputs$empl_healthcare<-0
# inputs$assets<-0
# inputs$age_adult_1<-25
# inputs$age_adult_2<-NA_real_
# inputs$age_adult_3<-NA_real_
# inputs$age_adult_4<-NA_real_
# inputs$age_adult_5<-NA_real_
# inputs$age_adult_6<-NA_real_
# inputs$age_child_1<-8
# inputs$age_child_2<-NA_real_
# inputs$age_child_3<-NA_real_
# inputs$age_child_4<-NA_real_
# inputs$age_child_5<-NA_real_
# inputs$age_child_6<-NA_real_
# inputs$married<-0
# inputs$housingexp<-NA_real_
# inputs$childcareexp<-NA_real_
# inputs$benefit<-c("Child and Dependent Care Tax Credit (CDCTC)")
# fam_disab<-"No"
# inputs$disability1<-0 # should be 0 or 1
# inputs$disability2<-0
# inputs$disability3<-0
# inputs$disability4<-0
# inputs$disability5<-0
# inputs$disability6<-0
# inputs$disability7<-0
# inputs$disability8<-0
# inputs$disability9<-0
# inputs$disability10<-0
# inputs$disability11<-0
# inputs$disability12<-0
# inputs$disab.work.exp<-0 # amt spent to give disabled ssi recipient ability to work
# inputs$prev_ssi<-0 # 0 or 1, has any disabled person in the fam ever received ssi?
# inputs$ssdiPIA1<-0
# inputs$ssdiPIA2<-0
# inputs$ssdiPIA3<-0
# inputs$ssdiPIA4<-0
# inputs$ssdiPIA5<-0
# inputs$ssdiPIA6<-0
# inputs$blind1<-0
# inputs$blind2<-0
# inputs$blind3<-0
# inputs$blind4<-0
# inputs$blind5<-0
# inputs$blind6<-0
# inputs$income.child_support<-0
# inputs$income.gift<-0
# inputs$income.investment<-0
# ruleYear <- as.numeric(format(Sys.Date(), "%Y"))
# END OF TESTING SECTION


# create initial data frame from user inputs ----
cross.section <- function(inputs, contelig.ccdf=TRUE, contelig.headstart=TRUE, contelig.earlyheadstart=TRUE){

  inputs <<- inputs
  contelig.ccdf <<- contelig.ccdf
  contelig.headstart <<- contelig.headstart
  contelig.earlyheadstart <<- contelig.earlyheadstart
  
  csdata<-expand_grid(
      numadults = inputs$numadults
    , numkids = inputs$numkids
    , locations = inputs$city.name
    , income = seq(1000,100000,by=1000)
    , ruleYear = ruleYear
    , Year = ruleYear
    , married = inputs$married
    ) 
  
  # Default programs. NOTE: APPLY_CHILDCARE & APPLY_HEALTHCARE should always be set to TRUE
      APPLY_CHILDCARE<-TRUE # Childcare block - Head Start and CCDF
      APPLY_LIHEAP<-FALSE
      APPLY_HEALTHCARE<-TRUE # switch to apply healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
      APPLY_SECTION8<-FALSE 
      APPLY_RAP<-FALSE 
      APPLY_FATES<-FALSE 
      APPLY_SNAP<-FALSE 
      APPLY_EITC<-FALSE
      APPLY_CTC<-FALSE
      APPLY_CDCTC<-FALSE
      APPLY_HEADSTART<-FALSE
      APPLY_PREK<-FALSE
      APPLY_CCDF<-FALSE
      APPLY_SLP<-FALSE
      APPLY_TAXES<-FALSE
      APPLY_MEDICAID_ADULT <- FALSE
      APPLY_MEDICAID_CHILD<-FALSE
      APPLY_ACA<-FALSE
      APPLY_WIC<-FALSE
      APPLY_TANF<-FALSE
      APPLY_SSI<-FALSE
      APPLY_SSDI<-FALSE
      APPLY_TANF<-FALSE
      # If user selects benefits, change prgram to TRUE
      if("Supplemental Nutrition Assistance Program (SNAP)" %in% inputs$benefit) {APPLY_SNAP<-TRUE}else{APPLY_SNAP<-FALSE}
      if("Free or Reduced Price School Meals" %in% inputs$benefit) {APPLY_SLP<-TRUE}
      if("Earned Income Tax Credit (EITC)" %in% inputs$benefit) {APPLY_EITC<-TRUE}
      if("Child Tax Credit (CTC)" %in% inputs$benefit) {APPLY_CTC<-TRUE}
      if("Child and Dependent Care Tax Credit (CDCTC)" %in% inputs$benefit) {APPLY_CDCTC<-TRUE}
      if("Head Start/Early Head Start" %in% inputs$benefit) {APPLY_HEADSTART<-TRUE}
      if("Section 8 Housing Voucher" %in% inputs$benefit) {APPLY_SECTION8<-TRUE}
      if("Child Care Subsidy (CCDF)" %in% inputs$benefit) {APPLY_CCDF<-TRUE}
      if("Medicaid for Adults" %in% inputs$benefit) {APPLY_MEDICAID_ADULT<-TRUE}
      if("Medicaid for Children/CHIP"  %in% inputs$benefit) {APPLY_MEDICAID_CHILD<-TRUE}
      if("Health Insurance Marketplace Subsidy" %in% inputs$benefit) {APPLY_ACA<-TRUE}
      if("Women, Infants and Children Nutrition Program (WIC)" %in% inputs$benefit) {APPLY_WIC<-TRUE}
      if("Home Energy Assistance" %in% inputs$benefit) {APPLY_LIHEAP<-TRUE}
      if("Temporary Assistance for Needy Families (TANF)" %in% inputs$benefit) {APPLY_TANF<-TRUE}
      if("State-Funded Pre-Kindergarten" %in% inputs$benefit) {APPLY_PREK<-TRUE}
      if("FATES" %in% inputs$benefit) {APPLY_FATES<-TRUE}
      if("RAP" %in% inputs$benefit) {APPLY_RAP<-TRUE}
      if("Supplemental Security Income (SSI)" %in% inputs$benefit) {APPLY_SSI<-TRUE}
      if("Social Security Disability Insurance (SSDI)" %in% inputs$benefit) {APPLY_SSDI<-TRUE}
    
      if("RAP" %in% inputs$benefit & "Section 8 Housing Voucher" %in% inputs$benefit) {APPLY_RAP<-TRUE
      APPLY_SECTION8<-FALSE}
      
      
  csdata<-csdata %>% 
    
  # Demographics ----
  # Initialize age of each member of the household
  
  mutate(agePerson1=NA_real_  
         ,agePerson2=NA_real_
         ,agePerson3=NA_real_ 
         ,agePerson4=NA_real_
         ,agePerson5=NA_real_
         ,agePerson6=NA_real_
         ,agePerson7=NA_real_
         ,agePerson8=NA_real_
         ,agePerson9=NA_real_
         ,agePerson10=NA_real_
         ,agePerson11=NA_real_
         ,agePerson12=NA_real_) %>% 
    
    # Tax Filing Status:
    # 1 - single
    # 2 - married filing jointly (make a note in Dashboard)
    # 3 - Heads of Household (for later)
    # 4 - Married Filing Separately (for later)
    
  mutate(FilingStatus=case_when(married==0 & numkids==0 ~ 1 # Single 
                                ,married==0 & numkids>0 ~ 3 # Head of Household
                                ,married==1 ~ 2 # Married Filing Jointly
                                ,TRUE ~ 1)) %>%
    
  
  # Finances -----
  mutate(empl_healthcare=inputs$empl_healthcare # initialize and override later
         , income.investment=0
         , income.gift=0
         , income.child_support=0
         , ownorrent="rent"
         , assets.cash=0
         , assets.car1=0) %>% 
    
    mutate(income1=NA_real_
           ,income2=NA_real_
           ,income3=NA_real_
           ,income4=NA_real_
           ,income5=NA_real_
           ,income6=NA_real_) %>%
    

 
  # Family types settings (Manually set up start ages of family members ----

  mutate(agePerson1=case_when(inputs$numadults>=1  ~ as.numeric(inputs$age_adult_1)
                              ,TRUE ~ agePerson1),
         
         agePerson2=case_when(inputs$numadults>=2 ~ as.numeric(inputs$age_adult_2)
                              ,TRUE ~ agePerson2),
         
         agePerson3=case_when(inputs$numadults>=3 ~ as.numeric(inputs$age_adult_3)
                              ,TRUE ~ agePerson3),
         
         agePerson4=case_when(inputs$numadults>=4 ~ as.numeric(inputs$age_adult_4)
                              ,TRUE ~ agePerson4),
         
         agePerson5=case_when(inputs$numadults>=5 ~ as.numeric(inputs$age_adult_5)
                              ,TRUE ~ agePerson5),
         
         agePerson6=case_when(inputs$numadults>=6 ~ as.numeric(inputs$age_adult_6)
                              ,TRUE ~ agePerson6),
         
         agePerson7=case_when(inputs$numkids>=1 ~ as.numeric(inputs$age_child_1) # NEEDS TO BE ADULT
                              ,TRUE ~ agePerson7),
         
         agePerson8=case_when(inputs$numkids>=2 ~ as.numeric(inputs$age_child_2) # NEEDS TO BE ADULT
                              ,TRUE ~ agePerson8),
         
         agePerson9=case_when(inputs$numkids>=3 ~ as.numeric(inputs$age_child_3) # NEEDS TO BE ADULT
                              ,TRUE ~ agePerson9),
         
         agePerson10=case_when(inputs$numkids>=4 ~ as.numeric(inputs$age_child_4) # 7 - 12 ARE CHILDREN
                              ,TRUE ~ agePerson10),
         
         agePerson11=case_when(inputs$numkids>=5 ~ as.numeric(inputs$age_child_5) # 7 - 12 ARE CHILDREN
                               ,TRUE ~ agePerson11),
         
         agePerson12=case_when(inputs$numkids>=6 ~ as.numeric(inputs$age_child_6) # 7 - 12 ARE CHILDREN
                               ,TRUE ~ agePerson12),
        
         
  ) %>%
    
  # PRD Dashboard specific assumptions about other family income if SSI or SSDI is selected ----
  mutate(countPIAadults=case_when(as.numeric(inputs$numadults)==1 ~ sum(c(inputs$ssdiPIA1)!=0) # count number of adults who get SSDI payments
                                  ,as.numeric(inputs$numadults)==2 ~ sum(c(inputs$ssdiPIA1,inputs$ssdiPIA2)!=0)
                                  ,as.numeric(inputs$numadults)==3 ~ sum(c(inputs$ssdiPIA1,inputs$ssdiPIA2,inputs$ssdiPIA3)!=0)
                                  ,as.numeric(inputs$numadults)==4 ~ sum(c(inputs$ssdiPIA1,inputs$ssdiPIA2,inputs$ssdiPIA3, inputs$ssdiPIA4)!=0)
                                  ,as.numeric(inputs$numadults)==5 ~ sum(c(inputs$ssdiPIA1,inputs$ssdiPIA2,inputs$ssdiPIA3, inputs$ssdiPIA4 , inputs$ssdiPIA5)!=0)
                                  ,as.numeric(inputs$numadults)==6 ~ sum(c(inputs$ssdiPIA1,inputs$ssdiPIA2,inputs$ssdiPIA3, inputs$ssdiPIA4, inputs$ssdiPIA5, inputs$ssdiPIA6)!=0)
                                  )
         , income1 = case_when(countPIAadults==0 ~ income/as.numeric(numadults) # SSDI is correct at $0; SSI is correct
                               ,countPIAadults==as.numeric(numadults) ~ income/as.numeric(numadults) # Both are correct
                               ,countPIAadults<as.numeric(numadults) ~ income/countPIAadults # SSDI is correct; SSI will be underestimated
                               )
         , income2 = case_when(countPIAadults==0 ~ income/as.numeric(numadults)
                               ,countPIAadults==as.numeric(numadults) ~ income/as.numeric(numadults)
                               ,countPIAadults<as.numeric(numadults) ~ income/countPIAadults
         )
         , income3 = case_when(countPIAadults==0 ~ income/as.numeric(numadults)
                               ,countPIAadults==as.numeric(numadults) ~ income/as.numeric(numadults)
                               ,countPIAadults<as.numeric(numadults) ~ income/countPIAadults
         )
         , income4 = case_when(countPIAadults==0 ~ income/as.numeric(numadults)
                               ,countPIAadults==as.numeric(numadults) ~ income/as.numeric(numadults)
                               ,countPIAadults<as.numeric(numadults) ~ income/countPIAadults
         )
         , income5 = case_when(countPIAadults==0 ~ income/as.numeric(numadults)
                               ,countPIAadults==as.numeric(numadults) ~ income/as.numeric(numadults)
                               ,countPIAadults<as.numeric(numadults) ~ income/countPIAadults
         )
         , income6 = case_when(countPIAadults==0 ~ income/as.numeric(numadults)
                               ,countPIAadults==as.numeric(numadults) ~ income/as.numeric(numadults)
                               ,countPIAadults<as.numeric(numadults) ~ income/countPIAadults
         )

        )
  
  # ANYTHING THAT GOES UP TO 7 (I.E DISABILITY) NEEDS TO BE INCREASED TO 12. ANYTHING THAT GOES UP TO 3 (I.E. INCOME, BLIND) NEEDS TO GO UP TO 6
  csdata$disability1 <- inputs$disability1 
  csdata$disability2 <- inputs$disability2
  csdata$disability3 <- inputs$disability3 
  csdata$disability4 <- inputs$disability4 
  csdata$disability5 <- inputs$disability5 
  csdata$disability6 <- inputs$disability6 
  csdata$disability7 <- inputs$disability7 
  csdata$disability8 <- inputs$disability8 
  csdata$disability9 <- inputs$disability9 
  csdata$disability10 <- inputs$disability10 
  csdata$disability11 <- inputs$disability11 
  csdata$disability12 <- inputs$disability12 
  csdata$disab.work.exp <- inputs$disab.work.exp
  csdata$prev_ssi <- inputs$prev_ssi
  csdata$ssdiPIA1 <- inputs$ssdiPIA1
  csdata$ssdiPIA2 <- inputs$ssdiPIA2
  csdata$ssdiPIA3 <- inputs$ssdiPIA3
  csdata$ssdiPIA4 <- inputs$ssdiPIA4
  csdata$ssdiPIA5 <- inputs$ssdiPIA5
  csdata$ssdiPIA6 <- inputs$ssdiPIA6
  csdata$blind1  <- inputs$blind1
  csdata$blind2  <- inputs$blind2
  csdata$blind3  <- inputs$blind3
  csdata$blind4  <- inputs$blind4
  csdata$blind5  <- inputs$blind5
  csdata$blind6  <- inputs$blind6
  
  csdata$Year<-as.numeric(csdata$Year)
  
  # Additional formatting to the data frame ----
  csdata<-csdata %>% 
    separate(locations, c("countyortownName","stateAbbrev"), sep=", ") 
  
  csdata<-as.data.frame(csdata)
  
  csdata<-function.InitialTransformations(csdata)

  csdata$year.index <- 1
  
  csdata$income.otherfamily <- 0
 
  csdata$income_tm12 <- csdata$income # create lag of income for tax credits calculations
  
 
  # Apply expenses ----
  csdata<-BenefitsCalculator.ALICEExpenses(csdata)
  
  # Override childcare and housing expenses with user inputs if they do not use our estimates
  if (!is.na(inputs$housingexp)) {csdata$exp.rentormortgage <- inputs$housingexp*12 - csdata$exp.utilities} #this includes utilities too .. can combine together bc this is just for acc calculation snap  &section 8
  if (!is.na(inputs$childcareexp)) {csdata$exp.childcare <- inputs$childcareexp*12}

  # If user inputted values - override
  if (!is.na(inputs$assets)){
    csdata$assets.cash <- inputs$assets
    csdata$totalassets <- inputs$assets
    } 
  if (!is.na(inputs$income.child_support)){csdata$income.child_support <- inputs$income.child_support*12} 
  if (!is.na(inputs$income.investment)){csdata$income.investment <- inputs$income.investment*12} 
  if (!is.na(inputs$income.gift)){csdata$income.gift <- inputs$income.gift*12} 
  
  
  # Apply Benefits Calculator (PRD) ----
  csdata<-BenefitsCalculator.OtherBenefits(csdata, APPLY_TANF, APPLY_SSDI, APPLY_SSI) 
  csdata<-BenefitsCalculator.Childcare(csdata, APPLY_CHILDCARE, APPLY_HEADSTART, APPLY_PREK, APPLY_CCDF,APPLY_FATES, contelig.ccdf = contelig.ccdf,contelig.headstart = `contelig.headstart`, contelig.earlyheadstart = `contelig.earlyheadstart`) #option to add APPLY_FATES
  csdata<-BenefitsCalculator.Healthcare(csdata, APPLY_HEALTHCARE, APPLY_MEDICAID_ADULT, APPLY_MEDICAID_CHILD, APPLY_ACA)
  csdata<-BenefitsCalculator.FoodandHousing(csdata, APPLY_SECTION8, APPLY_LIHEAP, APPLY_SNAP, APPLY_SLP, APPLY_WIC, APPLY_RAP) # OPTION TO END WITH APPLY_RAP
  csdata<-BenefitsCalculator.TaxesandTaxCredits(csdata, APPLY_EITC, APPLY_CTC, APPLY_CDCTC)

   # Create additional variables for plotting
  csdata<-function.createVars(csdata)

  
     return(csdata)
  
}



# Plot second chart & display error messages if needed ----
cross.section.NR.values <- function(d, inputs){
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  
  validate(
    need(error_no_adults == 0, "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  d <<- d
  
  extraheight <- 10000
    
    d$`Employment Income` <- d$income # Rename variables for the plotly
    d$`Net Resources` <- round(d$NetResources,0)
  
    fl <- round_any(min(d$NetResources), 1000, f = floor)
    
    
    if(max(d$NetResources)>4000){  
     ceil <- round_any(max(d$NetResources), 1000, f=ceiling)
    }else{
      ceil <- 4000
      }
    cross.section.net.res <- ggplot(d, aes(x=`Employment Income` , y=`Net Resources`))  +
      geom_line(color='darkred',size=1.5) +
      
      xlim(0,100000)+
      ylim(fl, ceil) + 
      
      scale_x_continuous(labels = label_number(accuracy = 1, prefix="$", suffix = "K", scale = 1e-3), breaks = seq(0,100000, by=10000))+ # That allows to scale numbers on axis up and down
      scale_y_continuous(labels = label_number(accuracy = 1, prefix="$", suffix = "K", scale = 1e-3))+
      
      xlab("Employment Income")  + ylab("Net Resources") +
      
      theme(plot.title = element_text(hjust = .5, size = 16), 
            axis.text.x = element_text(angle = 45, vjust = .5), 
            axis.text.y = element_text(angle = 45, vjust = .5), 
            axis.text = element_text(size = 14, colour = "black"), 
            axis.title = element_text(size = 14),
            panel.grid.major = element_line(colour = "grey", linetype = 2),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.position = "bottom") +
      
            labs(title="", caption=inputs$city.name , color = '') 
      
    cross.section.net.res <- cross.section.net.res + geom_hline(yintercept = 0, lty =2, size = 1.5)
    
    cross.section.net.res <- ggplotly(cross.section.net.res) %>% 
      layout(legend = list(orientation = "h", x = 0.4, y = -0.7)) %>%  # GGPLOTLY doesn't know how to deal with legend location so have to do that
      layout(annotations = list(x = 1, y = -0.3, text = inputs$city.name, 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=15, color="black")))
      
      
  return(print(cross.section.net.res))
  
}


# Function for the first chart ----

csbenefit.breakdown.values <- function(d, inputs, contelig.ccdf=TRUE, contelig.headstart=TRUE, contelig.earlyheadstart=TRUE){
  
  validate(
    need(error_kids_num == 0 & error_adults_num == 0, "The tool supports the family profiles with up to 6 adults and 6 children. Please specify valid values.")
  )
  
  
  validate(
    need(error_no_adults == 0, "Tool does not support family profiles without adults. Please provide information about adults in the family.")
  )
  
  
  
  validate(
    need(error_kids == 0 & error_adults == 0, "The number of children and/or adults is missing. Please specify valid values.")
  )
  
  d$totalcsbenefits <- d$value.medicaid.adult+d$value.medicaid.child+d$value.snap+d$value.wic+d$value.tanf+
    d$value.section8+d$value.CCDF+d$value.aca+d$value.eitc+d$value.ctc+d$value.cdctc+d$value.HeadStart+
    d$value.schoolmeals+d$value.earlyHeadStart+d$value.ssi+d$value.ssdi
  

  d <- subset(d, select = c("income","totalcsbenefits",
                            "value.aca","value.medicaid.adult", 
                            "value.medicaid.child",
                            "value.section8",
                            "value.tanf",
                            "value.HeadStart",
                            "value.earlyHeadStart",
                            "value.PreK",
                            "value.CCDF", 
                            "value.snap",
                            "value.wic",
                            "value.schoolmeals",
                            "value.cdctc",
                            "value.ctc",
                            "value.eitc",
                            "value.liheap",
                            "value.FATES",
                            "value.ssi",
                            "value.ssdi"
  ))
  
  
  names(d)[names(d)=="value.medicaid.adult"] <- "Medicaid for Adults"
  names(d)[names(d)=="value.medicaid.child"] <- "Medicaid for Children/CHIP"
  names(d)[names(d)=="value.snap"] <- "SNAP"
  names(d)[names(d)=="value.wic"] <- "WIC"
  names(d)[names(d)=="value.tanf"] <- "TANF"
  names(d)[names(d)=="value.cdctc"] <- "CDCTC"
  names(d)[names(d)=="value.section8"] <- "Housing Voucher"
  names(d)[names(d)=="value.CCDF"] <- "CCDF"
  names(d)[names(d)=="value.aca"] <- "Health Insurance Marketplace Subsidy"
  names(d)[names(d)=="value.eitc"] <- "EITC"
  names(d)[names(d)=="value.ctc"] <- "CTC"
  names(d)[names(d)=="value.schoolmeals"] <- "Free or Reduced Price School Meals"
  names(d)[names(d)=="value.liheap"] <- "LIHEAP"
  names(d)[names(d)=="value.HeadStart"] <- "Head Start"
  names(d)[names(d)=="value.earlyHeadStart"] <- "Early Head Start"
  names(d)[names(d)=="value.PreK"] <- "PreK"
  names(d)[names(d)=="value.FATES"] <- "FATES"
  names(d)[names(d)=="value.ssi"] <- "SSI"
  names(d)[names(d)=="value.ssdi"] <- "SSDI"
  
  #reshape data
    csbenefit.decomp <- melt(d, id.vars=c("income", "totalcsbenefits"),
                           variable.name="Program",value.name="Transfer")

  
  ## Rename Factors 
  if(sum(d$totalcsbenefits) > 0 & !is.na(sum(d$totalcsbenefits))){
  csbenefit.decomp$Transfer <- replace(csbenefit.decomp$Transfer, csbenefit.decomp$Transfer==0, NA)
  }
  
  csbenefit.decomp <- csbenefit.decomp[is.na(csbenefit.decomp$Transfer) == FALSE, ]
  csbenefit.decomp$Program <- droplevels(csbenefit.decomp$Program)
  
  csbenefit_colors <- c("Medicaid for Adults"="#332288", 
                        "Medicaid for Children/CHIP"="#E69F00", "SNAP"="#D55E00", "WIC"="#AA4499","Housing Voucher"="#CC79A7", "TANF"="#0072B2",
                        "CCDF"="#882255", "Health Insurance Marketplace Subsidy"="#117733", "EITC"="chocolate4", "CTC"="#44AA99", "CDCTC" = "#F0E442", 
                        "Free or Reduced Price School Meals" = "#999933", "LIHEAP" = "#CC6677", "Head Start" = "#88CCEE", "Early Head Start" = "#56E4B9", "PreK" = "black", "FATES" = "darkblue"
                        ,"TANF"="#CC79A7","SSI"="red3","SSDI"="turquoise4"
                        )
  

  # Rename variables for Plotly
  csbenefit.decomp$`Value` <- round(csbenefit.decomp$Transfer,0)
  csbenefit.decomp$`Employment Income` <- csbenefit.decomp$income
  
   fl <- min(d$income)
   ceil <- max(d$income)
   if(ceil < 4000){
     ceil <- 4000
   }

   
  csbenefits.plot <- ggplot(csbenefit.decomp, aes(fill = Program, y = `Value`, x = `Employment Income`, text = paste("<br>Employment Income $:", format(income, digits = 1, big.mark = ",", scientific = FALSE),
                                                                                                                     "<br>Program:", Program,
                                                                                                                     "<br>Value: $", format(Transfer, digits = 1, big.mark = ",", scientific = FALSE))))+ 
    geom_bar(position = "stack", stat = "identity") +
    
    xlim(0,100000)+
    ylim(fl, ceil)
  
  if(max(d$totalcsbenefits)>0){
    csbenefits.plot <- csbenefits.plot + scale_fill_manual(values = csbenefit_colors)
      
  }
  
   csbenefits.plot <- csbenefits.plot +
     xlab("Employment Income") + ylab("Dollar Value") +
    
    scale_x_continuous(labels = label_number(accuracy = 1, prefix="$", suffix = "K", scale = 1e-3), limits = c(0,100000), breaks = seq(0,100000, by=10000)) # That allows to scale numbers on axis up and down
    
  if(max(d$totalcsbenefits)<2500 & max(d$totalcsbenefits)>0){
  csbenefits.plot <- csbenefits.plot + scale_y_continuous(labels = label_number(accuracy = 0.1, prefix="$", suffix = "K", scale = 1e-3))
  }else if(max(d$totalcsbenefits)>=2500){
    csbenefits.plot <- csbenefits.plot +  scale_y_continuous(labels = label_number(accuracy = 1, prefix="$", suffix = "K", scale = 1e-3))
  }else if(max(d$totalcsbenefits)==0){
    csbenefits.plot <- csbenefits.plot + scale_y_continuous(labels = label_number(accuracy = 1, prefix="$", suffix = "K", scale = 1e-3), 
                                                            limits = c(0,4000), 
                                                breaks = seq(0,4000, by=1000)) # That allows to scale numbers on axis up and down
    
  }
    
    if(max(d$totalcsbenefits)>0){
  csbenefits.plot <- csbenefits.plot + theme(plot.title = element_text(hjust = .5, size = 16),
          
          axis.text.x = element_text(angle = 45, vjust = .5), 
          axis.text.y = element_text(angle = 45, vjust = .5), 
          axis.text = element_text(size = 14, colour = "black"),
          axis.title = element_text(size = 14),
          panel.grid.major = element_line(colour = "grey", linetype = 2),
          panel.grid.minor = element_line(colour = "grey", linetype = 2),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.key = element_rect(fill = "white"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.position = "bottom") +
    
    labs(title = NULL,
         caption = paste0(inputs$city.name),
         fill = "Program:")  
    }else{
      
      csbenefits.plot <- csbenefits.plot + theme(plot.title = element_text(hjust = .5, size = 16),
                                                 
                                                 axis.text.x = element_text(angle = 45, vjust = .5), 
                                                 axis.text.y = element_text(angle = 45, vjust = .5), 
                                                 axis.text = element_text(size = 14, colour = "black"),
                                                 axis.title = element_text(size = 14),
                                                 panel.grid.major = element_line(colour = "grey", linetype = 2),
                                                 panel.grid.minor = element_line(colour = "grey", linetype = 2),
                                                 panel.background = element_blank(),
                                                 axis.line = element_line(colour = "black"),
                                                 legend.key = element_rect(fill = "white"),
                                                 legend.text = element_text(size = 14),
                                                 legend.title = element_text(size = 14),
                                                 legend.position = "none") +
        
        labs(title = NULL,
             caption = paste0(inputs$city.name),
             fill = "Program:")  
      
    }
    
  csbenefits.plot <- ggplotly(csbenefits.plot, tooltip = "text" ) %>% 
    layout(legend = list(orientation = "h", x = 0.1, y = -0.5)) %>% 
    layout(annotations = list(x = 1, y = -0.5, text = inputs$city.name, 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=15, color="black")))
  
  return(print(csbenefits.plot)) 
  
}
