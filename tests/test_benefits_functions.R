
# testing  functions

test_that("OtherBenefits",{
  expect_equal(BenefitsCalculator.OtherBenefits(data, APPLY_TANF, APPLY_SSDI, APPLY_SSI), comparison_function_1)
})
data=BenefitsCalculator.OtherBenefits(data, APPLY_TANF, APPLY_SSDI, APPLY_SSI)


 test_that("Childcare",{
   expect_equal(BenefitsCalculator.Childcare(data, APPLY_CHILDCARE, APPLY_HEADSTART, APPLY_PREK, APPLY_CCDF,APPLY_FATES), comparison_function_2)
 })
 data=BenefitsCalculator.Childcare(data, APPLY_CHILDCARE, APPLY_HEADSTART, APPLY_PREK, APPLY_CCDF,APPLY_FATES)


 test_that("Healthcare",{
   expect_equal(BenefitsCalculator.Healthcare(data, APPLY_HEALTHCARE, APPLY_MEDICAID_ADULT, APPLY_MEDICAID_CHILD, APPLY_ACA), comparison_function_3)
 })

 data=BenefitsCalculator.Healthcare(data, APPLY_HEALTHCARE, APPLY_MEDICAID_ADULT, APPLY_MEDICAID_CHILD, APPLY_ACA)
 

test_that("FoodandHousing",{
  expect_equal(BenefitsCalculator.FoodandHousing(data, APPLY_SECTION8, APPLY_LIHEAP, APPLY_SNAP, APPLY_SLP, APPLY_WIC, APPLY_RAP, APPLY_FRSP), comparison_function_4)
})

data=BenefitsCalculator.FoodandHousing(data, APPLY_SECTION8, APPLY_LIHEAP, APPLY_SNAP, APPLY_SLP, APPLY_WIC, APPLY_RAP, APPLY_FRSP)

 test_that("TaxesandTax",{
   expect_equal(BenefitsCalculator.TaxesandTaxCredits(data, APPLY_EITC, APPLY_CTC, APPLY_CDCTC, APPLY_TAXES), comparison_function_5)
 })

 data=BenefitsCalculator.TaxesandTaxCredits(data, APPLY_EITC, APPLY_CTC, APPLY_CDCTC, APPLY_TAXES)
 

test_that("createVars",{
  expect_equal(function.createVars(data), comparison_function_6)
})



                                                      						                                                                                                            					                                                             	                                                            		                      		                                                                                                                        				                                                                                   				                                                                                      				                                                                                                          					 