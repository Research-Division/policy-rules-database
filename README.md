# Policy Rules Database (PRD)
The Policy Rules Database (PRD) is an open-source repository that contains up-to-date rules and provisions for all major federal and state public assistance programs, taxes, and tax credits in a single easy-to-use database. 

* [Subscribe](http://eepurl.com/ib_0eT) to receive email updates about new features of the Policy Rules Database

* To learn more, visit the [PRD website](https://www.atlantafed.org/economic-mobility-and-resilience/advancing-careers-for-low-income-families/policy-rules-database.aspx)

* Read [PRD Technical Manual](https://github.com/FRB-Atlanta-Advancing-Careers/policy-rules-database/blob/9bb1e96a5218a8b486b2b3bcbf28fdcd5a2ec906/PRD%20Technical%20Manual.pdf) to get started with the PRD and to learn details about each program included in the database

* Analyze the structure of the U.S. social safety net using [PRD Dashboard](https://emar-data-tools.shinyapps.io/prd_dashboard/)

The terms of the PRD use are defined by the GNU General Public License v3.0. If you are interested in alternative licensing arrangements, please contact cliff@atl.frb.org

## Citing the Policy Rules Database

Ilin, Elias and Ellyn Terry. 2021. 'The Policy Rules Database.' Federal Reserve Bank of Atlanta.
Available at www.frbatlanta.org/economic-mobility-and-resilience/advancing-careers-forlow-income-families/policy-rules-database.aspx. 

## How to Use This Repository?

**Step 1: Download the repository**

Download and unzip this repository to your local machine. Make sure you have R and RStudio installed.

**Step 2: Specify settings of the simulation - family characteristics and public benefits**

Open TEST.yml file located in the *projects* folder. Adjust each input accordingly.

**Step 3: Run the simulation**

Open applyBenefitsCalculator.R and run the program.

**Step 4: Explore the results**

Results of the simulation are saved as a csv file in the *output* folder.

**Step 5 (Optional): Modify and Explore**

The PRD GitHub repository gives users flexibility in how to use the database. That includes:

* Using PRD Excel files with the policy parameters located in *prd_parameters_excel* folder

* Modifying benefits_functions.R located in *functions* folder to produce policy simulations

* Applying PRD calculations to external datasets (CPS, ACS, SIPP etc) for microsimulation purposes 

## Description of Each File Included in This Repository

This folder contains the following files:

**1. "PRD Technical Manual.pdf"**

This document provides detailed information on each public assistance program and tax credit included in the PRD. The document also describes the programming steps necessary to implement the calculations for each program in any programming language.


**2. "prd_parameters" folder**

Contains main Policy Rules Database parameters files in RData format. These files are then used by the PRD functions to produce the calculations

The following files are located in this folder:

1. benefit.parameters.RData - eligibility & benefits parameters for each public assistance programs and tax credit included into the PRD
2. expenses.RData - parameters from the supplementary cost-of-living database that can be used to retrieve information required to implement PRD calculations
3. parameters.defaults.RData - default parameters used elsewhere in the PRD calculations (such as number of school days, number of summer days etc)
4. tables.RData - supplemental tables and crosswalks (e.g. SMIs, FPLs, state FIPS to state name mapping etc)


**3. "prd_parameters_excel" folder**

Contains the PRD parameters in the alternative Excel format


**4. "functions" folder**

Contains main Policy Rules Database functions written in R programming language. These functions read-in parameters from (1) and determine eligibility/calculate the dollar value of benefits for each program. 

The following files are located in this folder:

1. benefits_functions.R - functions that read-in PRD parameters and determine eligibility/calculate value of benefits for each public assistance program and tax credit contained in the PRD
2. expense_functions.R - functions that assign default expenses using Cost of Living Database
3. BenefitsCalculator_functions.R - higher-level program that aggregates smaller functions into blocks (Expenses, Childcare, Healthcare, Food and Housing, Taxes and Tax Credits, Other Benefits) 


**5. applyBenefitsCalculator.R**

High-level program that runs the Benfits Calculator in its entirety


**6. libraries.R**

Packages and libraries required to run example codes below


**7. "projects" folder**

YML files that are used to provide the specification for the PRD (i.e. specify the family demographics and benefits composition to run through the PRD). TEST.yml is then read-in by the applyBenefitsCalculator.R program and produces calculations

To test changes/run PRD you can do the following:

1. In projects folder, copy TEST.yml to YOUR_NAME.yml
2. Open YOUR_NAME.yml file in any text editor; adust specifications as needed
2. Open applyBenefitsCalculator.R; specify PROJECT object as YOUR_NAME
4. Run the program. Results are saved to the "output" folder as results_YOUR_NAME.csv file


**8. "output" folder**

applyBenefitsCalculator.R program saves the resulted output into this folder


## How to Use the Unit Test

A unit test is a way of testing the smallest piece of code that can be logically isolated in a system.

These steps should be carried out only once:

1. Execute the following command library(here)
2. Create an object with the path where the files are downloaded.

proj_dir <-"/Users/....."

3. Execute the following command setwd(proj_dir)
4. Execute the following command set_here()
5. Close your R session

To test the functions:

Check the Readme file inside the folder tests

**9. "Up to date Programs**


Federal Taxes & Tax Credits*									
Federal Income Tax	FICA	Federal CDCTC	Federal CTC	Federal EITC					
2010-2022	2010-2023	1976-2022	1997-2022	2018-2022					
									
									
									
*Note: For the current year, the PRD uses the tax rules for the previous year									
									
State Income Tax – all states and DC									
2011-2022									
									
State Tax Credits									
CDCTC – States:	2021								
Arkansas									
California									
Colorado									
Delaware									
District of Columbia									
Georgia									
Hawaii									
Iowa									
Kansas									
Kentucky									
Louisiana									
Maine									
Maryland									
Minnesota									
New Jersey									
Nebraska									
New Mexico									
New York									
Ohio									
Oklahoma									
Oregon									
Rhode Island									
South Carolina									
Vermont									
CTC – States:	2021								
California									
Colorado									
Idaho									
New York									
North Carolina									
Oklahoma									
EITC – States:	2022								
California									
Colorado									
Connecticut									
Delaware									
District of Columbia									
Hawaii									
Illinois									
Indiana									
Iowa									
Kansas									
Louisiana									
Maine									
Maryland									
Massachusetts									
Michigan									
Minnesota									
Montana									
Nebraska									
New Jersey									
New Mexico									
New York									
Ohio									
Oklahoma									
Oregon									
Rhode Island									
South Carolina									
Vermont									
Virginia									
Wisconsin									
									
									
Benefits									
SNAP	School Lunch Program	WIC	Pre-K	Head Start /	Section 8	Medicaid	ACA	SSI	SSDI
				Early Head Start					
2011-2023	2011-2023	2011-2023	2019	2013-2022	2009-2023	2011-2023	2014-2023	1984-2023	1975-2023
									
									
CCDF									
AL	2021-2022								
AK	2021-2022								
AR	2021-2022								
AZ	2020								
CA	2020								
CO	2021								
CT	2021								
DE	2021-2022								
DC	2018 & 2021								
FL	2011-2022								
GA	2020 & 2022								
HI	2020-2022								
IA	2021-2022								
ID	2020								
IL	2019-2022								
IN	2020								
KS	2020								
KY	2020								
LA	2020 & 2022								
ME	2020								
MA	2020-2022								
MD	2018 & 2022								
MI	2021-2022								
MN	2020								
MO	2020								
MS	2021								
MT	2020								
NC	2020								
ND	2021-2022								
NE	2021								
NV	2021-2022								
NH	2021								
NJ	2021-2022								
NM	2020								
NY	2020 & 2022								
OH	2020								
OK	2020								
OR	2021								
PA	2021-2022								
RI	2021								
SC	2020								
SD	2021-2022								
TN	2021								
TX	2020								
UT	2020								
VA	2020								
VT	2021-2022								
WA	2011-2022								
WI	2020								
WV	2020-2022								
WY	2021								
									
									
									
									
![image](https://github.com/FRB-Atlanta-Advancing-Careers/policy-rules-database/assets/59230162/89adfaa4-117f-40a0-8170-3332c194b626)






