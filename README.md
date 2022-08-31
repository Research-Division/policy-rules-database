# About the Policy Rules Database (PRD)
The Policy Rules Database (PRD) is an open-source repository that contains up-to-date program rules and provisions for all major federal and state public assistance programs, tax provisions, and tax credits in a single easy-to-use database.

To learn more, go to the [PRD website](https://www.atlantafed.org/economic-mobility-and-resilience/advancing-careers-for-low-income-families/policy-rules-database.aspx)

Analyze the structure of the U.S. social safety net using [PRD Dashboard](https://emar-data-tools.shinyapps.io/prd_dashboard/)

The terms of the PRD use are defined by the GNU General Public Licene v3.0. If you are interested in alternative licensing arrangements, please contact elias.ilin@atl.frb.org

**Suggested citation:**

Ilin, Elias and Ellyn Terry. 2021. 'The Policy Rules Database.' Federal Reserve Bank of Atlanta.
Available at www.frbatlanta.org/economic-mobility-and-resilience/advancing-careers-forlow-income-families/policy-rules-database.aspx. 

# How to Use This Repository?

This folder contains the following files:

**1. "prd_parameters" folder**

Contains main Policy Rules Database parameters files in RData format. These files are then used by the PRD functions to produce the calculations

The following files are located in this folder:

1. benefit.parameters.RData - eligibility & benefits parameters for each public assistance programs and tax credit included into the PRD
2. expenses.RData - parameters from the supplementary cost-of-living database that can be used to retrieve information required to implement PRD calculations
3. parameters.defaults.RData - default parameters used elsewhere in the PRD calculations (such as number of school days, number of summer days etc)
4. tables.RData - supplemental tables and crosswalks (e.g. SMIs, FPLs, state FIPS to state name mapping etc)


**2. "prd_parameters_excel" folder**

Contains the PRD parameters in the alternative Excel format


**3. "functions" folder**

Contains main Policy Rules Database functions written in R programming language. These functions read-in parameters from (1) and determine eligibility/calculate the dollar value of benefits for each program. 

The following files are located in this folder:

1. benefits_functions.R - functions that read-in PRD parameters and determine eligibility/calculate value of benefits for each public assistance program and tax credit contained in the PRD
2. expense_functions.R - functions that assign default expenses using Cost of Living Database
3. BenefitsCalculator_functions.R - higher-level program that aggregates smaller functions into blocks (Expenses, Childcare, Healthcare, Food and Housing, Taxes and Tax Credits, Other Benefits) 


**4. applyBenefitsCalculator.R**

High-level program that runs the Benfits Calculator in its entirety


**5. libraries.R**

Packages and libraries required to run example codes below


**6. "projects" folder**

YML files that are used to provide the specification for the PRD (i.e. specify the family demographics and benefits composition to run through the PRD). TEST.yml is then read-in by the applyBenefitsCalculator.R program and produces calculations

To test changes/run PRD you can do the following:

1. In projects folder, copy TEST.yml to YOUR_NAME.yml
2. Open YOUR_NAME.yml file in any text editor; adust specifications as needed
2. Open applyBenefitsCalculator.R; specify PROJECT object as YOUR_NAME
4. Run the program. Results are saved to the "output" folder as results_YOUR_NAME.csv file


**7. "output" folder**

applyBenefitsCalculator.R program saves the resulted output into this folder

