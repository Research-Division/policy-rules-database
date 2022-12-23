library(testthat)
library(here)

# get directory for this project (in this case, it's where .git is)
proj_dir <- here::here()
setwd(proj_dir)
# call benefits functions.R script
# run all tests in test_dir

# Step 1 - create a test data frame
current_directory<-getwd()

## Load expense parameters ----
#load(paste0(current_directory,"/prd_parameters/expenses.rdata"))
load("./prd_parameters/expenses.rdata")

## Load benefits parameters ----
load(paste0(current_directory,"/prd_parameters/benefit.parameters.rdata"))

## Load eligible tables of SMI,FPL / crosswalks ----
load(paste0(current_directory,"/prd_parameters/tables.rdata"))

## Load default parameters for the PRD----
load(paste0(current_directory,"/prd_parameters/parameters.defaults.rdata"))

# verify that everything is loaded in the console

## Call all the functions----
source(paste0(current_directory,"/libraries.R"), local=TRUE) # Load required packages
source(paste0(current_directory,"/functions/benefits_functions.R"), local=TRUE) # Benefits calculations
source(paste0(current_directory,"/functions/expense_functions.R"), local=TRUE) # Expenses calculations
source(paste0(current_directory,"/functions/BenefitsCalculator_functions.R"), local=TRUE) # Benefits Calculator functions

# SPECIFY PROJECT----
PROJECT<-"Unit_test"
## 1. Settings----

# Load inputs YAML file
inputs <- read_yaml(paste0(current_directory,"/projects/",PROJECT,".yml"))


# Global settings
k_ftorpt <- inputs$k_ftorpt 
schoolagesummercare <- inputs$schoolagesummercare 
headstart_ftorpt <- inputs$headstart_ftorpt  
preK_ftorpt <- inputs$preK_ftorpt 
contelig.headstart <- inputs$contelig.headstart 
contelig.earlyheadstart <- inputs$contelig.earlyheadstart 
contelig.ccdf <- inputs$contelig.ccdf 
USEALICE <- inputs$USEALICE

# Transfer programs switches
APPLY_CHILDCARE<-inputs$APPLY_CHILDCARE #Childcare block - Head Start and CCDF
APPLY_CCDF<-inputs$APPLY_CCDF
APPLY_HEADSTART<-inputs$APPLY_HEADSTART 
APPLY_PREK<-inputs$APPLY_PREK 
APPLY_LIHEAP<-FALSE  #doesn't work if true
APPLY_HEALTHCARE<-inputs$APPLY_HEALTHCARE 
APPLY_MEDICAID_ADULT<-inputs$APPLY_MEDICAID_ADULT 
APPLY_MEDICAID_CHILD<-inputs$APPLY_MEDICAID_CHILD 
APPLY_ACA<-inputs$APPLY_ACA 
APPLY_SECTION8<-inputs$APPLY_SECTION8 
APPLY_RAP<-inputs$APPLY_RAP 
APPLY_FRSP<-inputs$APPLY_FRSP  
APPLY_SNAP<-inputs$APPLY_SNAP 
APPLY_SLP<-inputs$APPLY_SLP 
APPLY_WIC<-inputs$APPLY_WIC 
APPLY_EITC<-inputs$APPLY_EITC 
APPLY_TAXES<-inputs$APPLY_TAXES 
APPLY_CTC<-inputs$APPLY_CTC
APPLY_CDCTC<-inputs$APPLY_CDCTC 
APPLY_FATES<-inputs$APPLY_FATES 
APPLY_TANF<-inputs$APPLY_TANF 
APPLY_SSI<-inputs$APPLY_SSI 
APPLY_SSDI<-inputs$APPLY_SSDI


## 2. Create data from inputs----
data<-function.createData(inputs)

## 3. Attach default expenses----
if (USEALICE == FALSE){
  data<-BenefitsCalculator.DefaultExpenses(data)
}
if (USEALICE == TRUE){
  data<-BenefitsCalculator.ALICEExpenses(data)
}

load("./unit_test/BenefitsCalculator.OtherBenefits_unittest.rdata")
comparison_function_1=data_unittest


load("./unit_test/BenefitsCalculator.Childcare_unittest.rdata")
comparison_function_2=data_unittest


load("./unit_test/BenefitsCalculator.Healthcare_unittest.rdata")
comparison_function_3=data_unittest

load("./unit_test/BenefitsCalculator.FoodandHousing_unittest.rdata")
comparison_function_4=data_unittest

load("./unit_test/BenefitsCalculator.TaxesandTaxCredits_unittest.rdata")
comparison_function_5=data_unittest

load("./unit_test/function.createVars_unittest.rdata")
comparison_function_6=data_unittest


test_results <- test_dir(file.path(proj_dir, 'tests'), reporter="summary")

