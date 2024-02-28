

server <- function(input, output, session){
  
  # called at the beginning of UI.R
  output$main_logo <- renderImage({
    return(list(src='www/logo2.png',width='80%', height='80%', contentType = "image/png"))}, deleteFile = FALSE)
  
  observeEvent(input$info1, {
    showNotification(strong("Access to the employer-sponsored health insurance can affect receipiency of the ACA"), type="message", duration = 7, closeButton = TRUE)
  })
  
  
  toListen <- reactive({ # Recalculate if "Update Results" are clicked
    list(input$getresults)  
  })
  
  observeEvent(toListen(),{
    
    inputs<-list() # initialize the list of inputs
    
    # Collect inputs recieved from the user and flag potential errors ----
   inputs$state<-isolate(as.character(input$state))

     if(inputs$state=='AK'){inputs$city.name<-input$county1}
     if(inputs$state=='AL'){inputs$city.name<-input$county2}
     if(inputs$state=='AR'){inputs$city.name<-input$county3}
     if(inputs$state=='AZ'){inputs$city.name<-input$county4}
     if(inputs$state=='CA'){inputs$city.name<-input$county5}
     if(inputs$state=='CO'){inputs$city.name<-input$county6}
     if(inputs$state=='CT'){inputs$city.name<-input$county7}
     if(inputs$state=='DC'){inputs$city.name<-input$county8}
     if(inputs$state=='DE'){inputs$city.name<-input$county9}
     if(inputs$state=='FL'){inputs$city.name<-input$county10}
     if(inputs$state=='GA'){inputs$city.name<-input$county11}
     if(inputs$state=='HI'){inputs$city.name<-input$county12}
     if(inputs$state=='IA'){inputs$city.name<-input$county13}
     if(inputs$state=='ID'){inputs$city.name<-input$county14}
     if(inputs$state=='IL'){inputs$city.name<-input$county15}
     if(inputs$state=='IN'){inputs$city.name<-input$county16}
     if(inputs$state=='KS'){inputs$city.name<-input$county17}
     if(inputs$state=='KY'){inputs$city.name<-input$county18}
     if(inputs$state=='LA'){inputs$city.name<-input$county19}
     if(inputs$state=='MA'){inputs$city.name<-input$county20}
     if(inputs$state=='MD'){inputs$city.name<-input$county21}
     if(inputs$state=='ME'){inputs$city.name<-input$county22}
     if(inputs$state=='MI'){inputs$city.name<-input$county23}
     if(inputs$state=='MN'){inputs$city.name<-input$county24}
     if(inputs$state=='MO'){inputs$city.name<-input$county25}
     if(inputs$state=='MS'){inputs$city.name<-input$county26}
     if(inputs$state=='MT'){inputs$city.name<-input$county27}
     if(inputs$state=='NC'){inputs$city.name<-input$county28}
     if(inputs$state=='ND'){inputs$city.name<-input$county29}
     if(inputs$state=='NE'){inputs$city.name<-input$county30}
     if(inputs$state=='NH'){inputs$city.name<-input$county31}
     if(inputs$state=='NJ'){inputs$city.name<-input$county32}
     if(inputs$state=='NM'){inputs$city.name<-input$county33}
     if(inputs$state=='NV'){inputs$city.name<-input$county34}
     if(inputs$state=='NY'){inputs$city.name<-input$county35}
     if(inputs$state=='OH'){inputs$city.name<-input$county36}
     if(inputs$state=='OK'){inputs$city.name<-input$county37}
     if(inputs$state=='OR'){inputs$city.name<-input$county38}
     if(inputs$state=='PA'){inputs$city.name<-input$county39}
     if(inputs$state=='RI'){inputs$city.name<-input$county40}
     if(inputs$state=='SC'){inputs$city.name<-input$county41}
     if(inputs$state=='SD'){inputs$city.name<-input$county42}
     if(inputs$state=='TN'){inputs$city.name<-input$county43}
     if(inputs$state=='TX'){inputs$city.name<-input$county44}
     if(inputs$state=='UT'){inputs$city.name<-input$county45}
     if(inputs$state=='VA'){inputs$city.name<-input$county46}
     if(inputs$state=='VT'){inputs$city.name<-input$county47}
     if(inputs$state=='WA'){inputs$city.name<-input$county48}
     if(inputs$state=='WI'){inputs$city.name<-input$county49}
     if(inputs$state=='WV'){inputs$city.name<-input$county50}
     if(inputs$state=='WY'){inputs$city.name<-input$county51}
     
    inputs$fam_disab <- isolate(as.character(input$fam_disab))
    inputs$prev_ssi <- isolate(as.character(input$prev_ssi))
    inputs$marital_status <- isolate(as.character(input$marital_status))
    
    inputs$age_child_1<-isolate(as.numeric(input$age_child_1))
    inputs$age_child_2<-isolate(as.numeric(input$age_child_2))
    inputs$age_child_3<-isolate(as.numeric(input$age_child_3))
    inputs$age_child_4<-isolate(as.numeric(input$age_child_4))
    inputs$age_child_5<-isolate(as.numeric(input$age_child_5))
    inputs$age_child_6<-isolate(as.numeric(input$age_child_6))
    
    inputs$age_adult_1<-isolate(as.numeric(input$age_adult_1))
    inputs$age_adult_2<-isolate(as.numeric(input$age_adult_2))
    inputs$age_adult_3<-isolate(as.numeric(input$age_adult_3))
    inputs$age_adult_4<-isolate(as.numeric(input$age_adult_4))
    inputs$age_adult_5<-isolate(as.numeric(input$age_adult_5))
    inputs$age_adult_6<-isolate(as.numeric(input$age_adult_6))
    
    inputs$numkids<-isolate(as.numeric(input$numkids))
    inputs$numadults<-isolate(as.numeric(input$numadults))
    
   
    if(inputs$numkids>6){inputs$numkids<-6} 
    if(inputs$numadults>6){inputs$numadults<-6} 
    
    
    # Logic related to customization
    inputs$customize<-isolate(input$customize)
    
    inputs$housingexp<-isolate(as.numeric(input$housingexp))
    
    inputs$assets<-isolate(as.numeric(input$assets))
    
    inputs$childcareexp<-isolate(as.numeric(input$childcareexp))
    
    inputs$income.child_support<-isolate(as.numeric(input$income.child_support))
    
    inputs$income.investment<-isolate(as.numeric(input$income.investment))
    
    inputs$income.gift<-isolate(as.numeric(input$income.gift))
    
    
    
    inputs$empl_healthcare<-isolate(input$empl_healthcare)
    if(inputs$empl_healthcare=="Yes" & inputs$customize==TRUE){inputs$empl_healthcare<-1}
    else{inputs$empl_healthcare<-0}
    
    # initialize variables
    inputs$married <- 0
    inputs$disability1 <- 0
    inputs$disability2 <- 0
    inputs$disability3 <- 0
    inputs$disability4 <- 0
    inputs$disability5 <- 0
    inputs$disability6 <- 0
    inputs$disability7 <- 0
    inputs$disability8 <- 0
    inputs$disability9 <- 0
    inputs$disability10 <- 0
    inputs$disability11 <- 0
    inputs$disability12 <- 0
    inputs$disab.work.exp<-0
    inputs$ssdiPIA1<-0
    inputs$ssdiPIA2<-0
    inputs$ssdiPIA3<-0
    inputs$ssdiPIA4<-0
    inputs$ssdiPIA5<-0
    inputs$ssdiPIA6<-0
    inputs$blind1<-0
    inputs$blind2<-0
    inputs$blind3<-0
    inputs$blind4<-0
    inputs$blind5<-0
    
    
    if(inputs$marital_status=="Yes"){
      inputs$married<-1
    }else{
      inputs$married<-0
    }
    
 if(inputs$fam_disab=="Yes"){
    inputs$disability1<-isolate(as.logical(input$disab1))
    if(inputs$disability1==TRUE){
      inputs$disability1<-1
    }else{
      inputs$disability1<-0
    }
    
    inputs$disability2<-isolate(as.logical(input$disab2))
    if(inputs$disability2==TRUE){
      inputs$disability2<-1
    }else{
      inputs$disability2<-0
    }
    
    inputs$disability3<-isolate(as.logical(input$disab3))
    if(inputs$disability3==TRUE){
      inputs$disability3<-1
    }else{
      inputs$disability3<-0
    }
    
    inputs$disability4<-isolate(as.logical(input$disab4))
    if(inputs$disability4==TRUE){
      inputs$disability4<-1
    }else{
      inputs$disability4<-0
    }
    
    inputs$disability5<-isolate(as.logical(input$disab5))
    if(inputs$disability5==TRUE){
      inputs$disability5<-1
    }else{
      inputs$disability5<-0
    }
    
    inputs$disability6<-isolate(as.logical(input$disab6))
    if(inputs$disability6==TRUE){
      inputs$disability6<-1
    }else{
      inputs$disability6<-0
    }
    
    inputs$disability7<-isolate(as.logical(input$disab7))
    if(inputs$disability7==TRUE){
      inputs$disability7<-1
    }else{
      inputs$disability7<-0
    }
    
    inputs$disability8<-isolate(as.logical(input$disab8))
    if(inputs$disability8==TRUE){
      inputs$disability8<-1
    }else{
      inputs$disability8<-0
    }
    
    inputs$disability9<-isolate(as.logical(input$disab9))
    if(inputs$disability9==TRUE){
      inputs$disability9<-1
    }else{
      inputs$disability9<-0
    }
    
    inputs$disability10<-isolate(as.logical(input$disab10))
    if(inputs$disability10==TRUE){
      inputs$disability10<-1
    }else{
      inputs$disability10<-0
    }
    
    inputs$disability11<-isolate(as.logical(input$disab11))
    if(inputs$disability11==TRUE){
      inputs$disability11<-1
    }else{
      inputs$disability11<-0
    }
    
    inputs$disability12<-isolate(as.logical(input$disab12))
    if(inputs$disability12==TRUE){
      inputs$disability12<-1
    }else{
      inputs$disability12<-0
    }
    
    
    
    inputs$blind1<-isolate(as.logical(input$blind1))
    if(inputs$blind1==TRUE){
      inputs$blind1<-1
      inputs$disability1<-1 # Automatically assume that the person is also disabled by SSI definition
    }else{
      inputs$blind1<-0
    }
    
    inputs$blind2<-isolate(as.logical(input$blind2))
    if(inputs$blind2==TRUE){
      inputs$blind2<-1
      inputs$disability2<-1 # Automatically assume that the person is also disabled by SSI definition
    }else{
      inputs$blind2<-0
    }
    
    inputs$blind3<-isolate(as.logical(input$blind3))
    if(inputs$blind3==TRUE){
      inputs$blind3<-1
      inputs$disability3<-1 # Automatically assume that the person is also disabled by SSI definition
    }else{
      inputs$blind3<-0
    }
    inputs$blind4<-isolate(as.logical(input$blind4))
    if(inputs$blind4==TRUE){
      inputs$blind4<-1
      inputs$disability4<-1 # Automatically assume that the person is also disabled by SSI definition
    }else{
      inputs$blind4<-0
    }
    inputs$blind5<-isolate(as.logical(input$blind5))
    if(inputs$blind5==TRUE){
      inputs$blind5<-1
      inputs$disability5<-1 # Automatically assume that the person is also disabled by SSI definition
    }else{
      inputs$blind5<-0
    }
    inputs$blind6<-isolate(as.logical(input$blind6))
    if(inputs$blind6==TRUE){
      inputs$blind6<-1
      inputs$disability6<-1 # Automatically assume that the person is also disabled by SSI definition
    }else{
      inputs$blind6<-0
    }
    
}else{
     inputs$disability1<-0
     inputs$disability2<-0
     inputs$disability3<-0
     inputs$disability4<-0
     inputs$disability5<-0
     inputs$disability6<-0
     inputs$disability7<-0
     inputs$disability8<-0
     inputs$disability9<-0
     inputs$disability10<-0
     inputs$disability11<-0
     inputs$disability12<-0
     
     inputs$blind1<-0
     inputs$blind2<-0
     inputs$blind3<-0
     inputs$blind4<-0
     inputs$blind5<-0
     inputs$blind6<-0
}
    
     if(inputs$prev_ssi=="Yes"){
       inputs$prev_ssi<-1}
     else
     {inputs$prev_ssi<-0}
     
     
   
    inputs$ssdiPIA1<-0
    inputs$ssdiPIA2<-0
    inputs$ssdiPIA3<-0
    inputs$ssdiPIA4<-0
    inputs$ssdiPIA5<-0
    inputs$ssdiPIA6<-0

    #inputs$benefit1<-isolate(as.character(input$benefit1))
    #inputs$benefit2<-isolate(as.character(input$benefit2))
    # inputs$benefit3<-isolate(as.character(input$benefit3))
   #inputs$benefit4<-isolate(as.character(input$benefit4)) # FLORIDA
  #  inputs$benefit5<-isolate(as.character(input$benefit5)) # CONNECTICUT
    

    # Collect benefits package from the user ----
    
    #ccdf & tanf states
    
    if(inputs$state != "FL" & inputs$state != "CT"){
      inputs$benefit<-isolate(as.character(input$benefit1))}
        #fl has fates
    else if(inputs$state == "FL"){
      inputs$benefit<-isolate(as.character(input$benefit4))}
    #ct has rap
    else if(inputs$state == "CT"){
      inputs$benefit<-isolate(as.character(input$benefit5))}
    #states with netiher ccdf or tanf
    #  else{inputs$benefit<-inputs$benefit3}
s8_error <- 0
if(inputs$state == 'CT' & 'Section 8 Housing Voucher' %in% inputs$benefit & 'RAP' %in% inputs$benefit){
  s8_error <- 1
  
}

if('Supplemental Security Income (SSI)' %in% inputs$benefit & inputs$fam_disab=="Yes"){
  inputs$disab.work.exp<-isolate(as.numeric(input$disab.work.exp))

  if(is.na(inputs$disab.work.exp) | inputs$disab.work.exp < 0){
    inputs$disab.work.exp<-0
    inputs$disab.work.exp.error <- 1
  }
}else{
  inputs$disab.work.exp <- 0
}

# Initialize potential user error flags ----
error_kids <<- 0
error_adults <<- 0
error_kids_num <<- 0
error_adults_num <<- 0
error_no_adults <<- 0
inputs$disab.work.exp.error <- 0


if(inputs$numkids > 6 | inputs$numkids < 0 | is.na(inputs$numkids)){
  if(is.na(inputs$numkids)){
    error_kids <<- 1
    inputs$numkids <- 0
  }else{
    error_kids_num <<- 1
  }
}

if(inputs$numadults > 6 |  is.na(inputs$numadults)){
  if(is.na(inputs$numadults)){
    error_adults <<- 1
    inputs$age_adult_1 <- 25
    inputs$numadults <-1 
  }else{
    error_adults_num <<- 1
  }
}

if(inputs$numadults < 1 & !is.na(inputs$numadults)){
  error_no_adults <<- 1
  inputs$age_adult_1 <- 25
  inputs$numadults <-1 
}

inputs$ssdi_error_1 <- 0
inputs$ssdi_error_2 <- 0
inputs$ssdi_error_3 <- 0
inputs$ssdi_error_4 <- 0
inputs$ssdi_error_5 <- 0
inputs$ssdi_error_6 <- 0

# Disability inputs and potential user errors ----

    if("Social Security Disability Insurance (SSDI)" %in% inputs$benefit & inputs$fam_disab=="Yes"){
      
      if(inputs$numadults>=1 & inputs$disability1==1){
        inputs$ssdiPIA1<-isolate(as.numeric(input$ssdiPIA1))
        
        if(is.na(inputs$ssdiPIA1) | inputs$ssdiPIA1<= 0){
          inputs$ssdi_error_1 <- 1
          inputs$ssdiPIA1<-0
        }else{
          inputs$ssdi_error_1 <- 0
        }
        
      }else{
        inputs$ssdiPIA1<-0
      }
      
      if(inputs$numadults>=2 & inputs$disability2==1){
        inputs$ssdiPIA2<-isolate(as.numeric(input$ssdiPIA2))
        
        
        if(is.na(inputs$ssdiPIA2) | inputs$ssdiPIA2<= 0){
          inputs$ssdi_error_2 <- 1
          inputs$ssdiPIA2<-0
        }else{
          inputs$ssdi_error_2 <- 0
        }
        
        
      }else{
        inputs$ssdiPIA2<-0
      }
      if(inputs$numadults>=3 & inputs$disability3==1){
        inputs$ssdiPIA3<-isolate(as.numeric(input$ssdiPIA3))
        
        if(is.na(inputs$ssdiPIA3) | inputs$ssdiPIA3<= 0){
          inputs$ssdi_error_3 <- 1
          inputs$ssdiPIA3<-0
        }else{
          inputs$ssdi_error_3 <- 0
        }
        
      }else{
        inputs$ssdiPIA3<-0
      }
      
      if(inputs$numadults>=4 & inputs$disability4==1){
        inputs$ssdiPIA4<-isolate(as.numeric(input$ssdiPIA4))
        
        if(is.na(inputs$ssdiPIA4) | inputs$ssdiPIA4<= 0){
          inputs$ssdi_error_4 <- 1
          inputs$ssdiPIA4<-0
        }else{
          inputs$ssdi_error_4 <- 0
        }
        
      }else{
        inputs$ssdiPIA4<-0
      }
      
      if(inputs$numadults>=5 & inputs$disability5==1){
        inputs$ssdiPIA5<-isolate(as.numeric(input$ssdiPIA5))
        
        if(is.na(inputs$ssdiPIA5) | inputs$ssdiPIA5<= 0){
          inputs$ssdi_error_5 <- 1
          inputs$ssdiPIA5<-0
        }else{
          inputs$ssdi_error_5 <- 0
        }
        
      }else{
        inputs$ssdiPIA5<-0
      }
      if(inputs$numadults>=6 & inputs$disability6==1){
        inputs$ssdiPIA6<-isolate(as.numeric(input$ssdiPIA6))
        
        if(is.na(inputs$ssdiPIA6) | inputs$ssdiPIA6<= 0){
          inputs$ssdi_error_6 <- 1
          inputs$ssdiPIA6<-0
        }else{
          inputs$ssdi_error_6 <- 0
        }
        
      }else{
        inputs$ssdiPIA6<-0
      }
    }


ssdiPIA1 <<- inputs$ssdiPIA1
ssdiPIA2 <<- inputs$ssdiPIA2
ssdiPIA3 <<- inputs$ssdiPIA3
ssdiPIA4 <<- inputs$ssdiPIA4
ssdiPIA5 <<- inputs$ssdiPIA5
ssdiPIA6 <<- inputs$ssdiPIA6

ssdi_error_1 <<- inputs$ssdi_error_1
ssdi_error_2 <<-inputs$ssdi_error_2
ssdi_error_3 <<- inputs$ssdi_error_3
ssdi_error_4 <<- inputs$ssdi_error_4
ssdi_error_5 <<- inputs$ssdi_error_5
ssdi_error_6 <<- inputs$ssdi_error_6

inputs$ssdi_error <- 0

if(inputs$ssdi_error_1 == 1 | inputs$ssdi_error_2 == 1 | inputs$ssdi_error_3 == 1 | inputs$ssdi_error_4 == 1 | inputs$ssdi_error_5 ==1 | inputs$ssdi_error_6 == 1){
  inputs$ssdi_error <- 1
}else{
  inputs$ssdi_error <- 0
}

   inputs$disab_error <- 0
   if(inputs$fam_disab == "Yes" & 
      inputs$disability1==0 &
      inputs$disability2==0&
      inputs$disability3==0&
      inputs$disability4==0&
      inputs$disability5==0&
      inputs$disability6==0&
      inputs$disability7==0&
      inputs$disability8==0&
      inputs$disability9==0&
      inputs$disability10==0&
      inputs$disability11==0&
      inputs$disability12==0){
     inputs$disab_error <- 1
   }
   
   
   
   inputs$ssdi_no_adults <- 0
   
   if('Social Security Disability Insurance (SSDI)' %in% inputs$benefit & inputs$fam_disab=="Yes"){
     
     if(inputs$disability1 == 0 & inputs$disability2 == 0 & inputs$disability3 == 0 & inputs$disability4 == 0 & inputs$disability5 == 0 & inputs$disability6 == 0){
       inputs$ssdi_no_adults <- 1
     }else{
       inputs$ssdi_no_adults <- 0
     }
     
   }
 
   inputs$ssi_ssdi_error <- 0
   
   if(inputs$fam_disab == "No" & ('Supplemental Security Income (SSI)' %in% inputs$benefit | 'Social Security Disability Insurance (SSDI)' %in% inputs$benefit)){
     inputs$ssi_ssdi_error <- 1
   }else{
     inputs$ssi_ssdi_error <- 0
   }
   
   
   inputs$fam_disab_error <- 0
   if(inputs$fam_disab=="empty"){
     inputs$fam_disab_error <- 1
     inputs$fam_disab <- "No"
   }else{
     inputs$fam_disab_error <- 0
   }
   
   
   # Initial vs Conditional Eligibility for 3 programs ----
   ccdf_elig<-isolate(input$ccdf_elig)
   if(ccdf_elig=="cont"){contelig.ccdf<-TRUE}else{contelig.ccdf<-FALSE}
   
   headstart_elig<-isolate(input$headstart_elig)
   if(headstart_elig=="cont"){contelig.headstart<-TRUE}else{contelig.headstart<-FALSE}
   
   earlyheadstart_elig<-isolate(input$earlyheadstart_elig)
   if(earlyheadstart_elig=="cont"){contelig.earlyheadstart<-TRUE}else{contelig.earlyheadstart<-FALSE}
   
 
   # Create cross section data ----
    csdata<- cross.section(inputs, contelig.ccdf = `contelig.ccdf`,contelig.headstart = `contelig.headstart`, contelig.earlyheadstart = `contelig.earlyheadstart`)
    
    # Plot Cross Sectional Net Resource ----
   
   # validate statements appear if there is a user error
    output$cross.section.net.resources <- renderPlotly({
    
      validate(
        need(inputs$disab_error ==0, "If you have selected that at least one member of your family is disabled, make sure to correctly select that family member if disabled.")
      )
      
      validate(
        need(inputs$ssdi_no_adults ==0, "You have selected SSDI, but none of the adults have a disability.")
      )
      
      validate(
        need(
          inputs$ssi_ssdi_error==0, "If you are receiving SSI or SSDI you must select 'Yes' to the Question 'Does anyone have a disability?'"
        )
      )
      
      validate(
        need(
         s8_error == 0, "You have selected both Section 8 Housing Voucher and RAP. Please unselect one of these programs to proceed."
        )
      )
      
      
      validate(
        need(inputs$ssdi_error==0, "SSDI values need to be positive.")
      )
      
      validate(
        need(inputs$disab.work.exp.error==0, "Please input a non-negative value for amount spent per month on specialized equipment or services that enable household member(s) with disabilities to work")
      )
      
      
      validate(
        need(inputs$fam_disab_error==0, "Please select whether or not at least one person in your family has a disability")
      )
      
      
      validate(
        if(inputs$numkids==1) {need(!is.na(inputs$age_child_1), "Input ages of all children")}
        else if(inputs$numkids==2) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2), "Input ages of all children")}
        else if(inputs$numkids==3) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2) & !is.na(inputs$age_child_3), "Input ages of all children")}
        else if(inputs$numkids==4) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2) & !is.na(inputs$age_child_3) & !is.na(inputs$age_child_4), "Input ages of all children")}
        else if(inputs$numkids==5) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2) & !is.na(inputs$age_child_3) & !is.na(inputs$age_child_4) & !is.na(inputs$age_child_5), "Input ages of all children")}
        else if(inputs$numkids>=6) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2) & !is.na(inputs$age_child_3) & !is.na(inputs$age_child_4) & !is.na(inputs$age_child_5) & !is.na(inputs$age_child_6), "Input ages of all children")}
      )
      
      validate(
        if(inputs$numadults==1) {need(!is.na(inputs$age_adult_1), "Input ages of all adults")}
        else if(inputs$numadults==2) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2), "Input ages of all adults")}
        else if(inputs$numadults==3) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2) & !is.na(inputs$age_adult_3), "Input ages of all adults")}
        else if(inputs$numadults==4) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2) & !is.na(inputs$age_adult_3) & !is.na(inputs$age_adult_4), "Input ages of all adults")}
        else if(inputs$numadults==5) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2) & !is.na(inputs$age_adult_3) & !is.na(inputs$age_adult_4) & !is.na(inputs$age_adult_5), "Input ages of all adults")}
        else if(inputs$numadults>=6) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2) & !is.na(inputs$age_adult_3) & !is.na(inputs$age_adult_4) & !is.na(inputs$age_adult_5) & !is.na(inputs$age_adult_6), "Input ages of all adults")}
      )
      
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$assets), "Input amount in checking and savings accounts")}
      )
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$housingexp), "Input custom value of rent")}
      )
      
      validate(
        if((inputs$customize==TRUE & inputs$numkids>=1 & "Child Care Subsidy (CCDF)" %notin% inputs$benefit)) {need(!is.na(inputs$childcareexp), "Input custom value of childcare expenses")}
      )
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$income.child_support), "Input the value of income from child support")}
      )
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$income.investment), "Input the value of investment income")}
      )
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$income.gift), "Input the value of cash suppor/gift income")}
      )
      
      cross.section.NR.values(csdata, inputs)
      
    })
    
    output$csbenefit.breakdown <- renderPlotly({
      
      
      validate(
        need(
          s8_error == 0, "    "
        )
      )
      
      
      validate(
        need(inputs$disab_error ==0, "   ")
      )
      
      validate( 
        need(inputs$ssdi_no_adults ==0, "      ")
      )
      
      validate(
        need(
          inputs$ssi_ssdi_error==0, "               "
        )
      )
      
      validate(
        need(inputs$disab.work.exp.error==0, "      ")
      )
      
      validate(
        need(inputs$ssdi_error==0, "     ")
      )
      
      validate(
        need(inputs$fam_disab_error==0, "        ")
      )
      
      
      validate(
        if(inputs$numkids==1) {need(!is.na(inputs$age_child_1), "Input ages of all children")}
        else if(inputs$numkids==2) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2), "Input ages of all children")}
        else if(inputs$numkids==3) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2) & !is.na(inputs$age_child_3), "Input ages of all children")}
        else if(inputs$numkids==4) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2) & !is.na(inputs$age_child_3) & !is.na(inputs$age_child_4), "Input ages of all children")}
        else if(inputs$numkids==5) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2) & !is.na(inputs$age_child_3) & !is.na(inputs$age_child_4) & !is.na(inputs$age_child_5), "Input ages of all children")}
        else if(inputs$numkids>=6) {need(!is.na(inputs$age_child_1) & !is.na(inputs$age_child_2) & !is.na(inputs$age_child_3) & !is.na(inputs$age_child_4) & !is.na(inputs$age_child_5) & !is.na(inputs$age_child_6), "Input ages of all children")}
        
      )
      
      validate(
        if(inputs$numadults==1) {need(!is.na(inputs$age_adult_1), "Input ages of all adults")}
        else if(inputs$numadults==2) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2), "Input ages of all adults")}
        else if(inputs$numadults==3) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2) & !is.na(inputs$age_adult_3), "Input ages of all adults")}
        else if(inputs$numadults==4) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2) & !is.na(inputs$age_adult_3) & !is.na(inputs$age_adult_4), "Input ages of all adults")}
        else if(inputs$numadults==5) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2) & !is.na(inputs$age_adult_3) & !is.na(inputs$age_adult_4) & !is.na(inputs$age_adult_5), "Input ages of all adults")}
        else if(inputs$numadults>=6) {need(!is.na(inputs$age_adult_1) & !is.na(inputs$age_adult_2) & !is.na(inputs$age_adult_3) & !is.na(inputs$age_adult_4) & !is.na(inputs$age_adult_5) & !is.na(inputs$age_adult_6), "Input ages of all adults")}
      )
      
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$assets), "Input amount in checking and savings accounts")}
      )
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$housingexp), "Input custom value of rent")}
      )
      
      validate(
        if((inputs$customize==TRUE & inputs$numkids>=1 & "Child Care Subsidy (CCDF)" %notin% inputs$benefit )) {need(!is.na(inputs$childcareexp), "Input custom value of childcare expenses")}
      )
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$income.child_support), "Input the value of income from child support")}
      )
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$income.investment), "Input the value of investment income")}
      )
      
      validate(
        if(inputs$customize==TRUE) {need(!is.na(inputs$income.gift), "Input the value of cash suppor/gift income")}
      )
      
      csbenefit.breakdown.values(csdata, inputs)
      
    })
    
    
    
    data_toPrint<-csdata %>%
      mutate(familyType = paste0("Family of ", input$numadults, " adult(s),", " and ", input$numkids, " child(ren)")) %>%
      
      select(familyType, stateAbbrev, countyortownName, FilingStatus, empl_healthcare, ownorrent,	assets.cash,	assets.car1, income, NetResources
             , exp.childcare,	exp.transportation,	exp.food,	exp.misc,	exp.utilities,	exp.rentormortgage,	exp.healthcare, exp.schoolMeals,	exp.tech,	exp.housing
             , value.tanf, value.liheap, value.CCDF,	value.HeadStart,	value.earlyHeadStart,	value.PreK, premium.medicaid.adult,	premium.medicaid.child,	value.medicaid.adult,	value.medicaid.child, premium.aca, value.aca
             , value.employerhealthcare, value.section8, value.snap, value.schoolmeals, value.wic, tax.income.fed, tax.income.state,	tax.FICA,	value.eitc.fed,	value.ctc.fed,	value.cdctc.fed
             , value.eitc.state,	value.ctc.state,	value.cdctc.state, value.ssi, value.ssdi)
    
    
    output$printData <- downloadHandler(
      
      filename = function(){
        paste0("prd_calculations.xlsx")
      },
      
      content = function(file){
        require(openxlsx)
        list_datasets <- data_toPrint
        write.xlsx(list_datasets, file=file)
        
      }
      
    )
    
    # Rule Years in PRD Table ----
    # use the benefits package selected to display information shown on the table in the last tab on the tool
    
    is.sequential <- function(x){
      all(abs(diff(x)) == 1)
    }
     
    Benefits <- c("Supplemental Nutrition Assistance Program (SNAP)"
                  ,"Free or Reduced Price School Meals"
                  ,"Women, Infants and Children Nutrition Program (WIC)"
                  ,"Temporary Assistance for Needy Families (TANF)"
                  ,"Child Care Subsidy (CCDF)"
                  ,"Head Start/Early Head Start"
                  ,"State-Funded Pre-Kindergarten" 
                  ,"Section 8 Housing Voucher"
                  ,"Medicaid"
                  ,"Health Insurance Marketplace Subsidy"
                  ,"Federal Earned Income Tax Credit (EITC)"
                  ,"Federal Child Tax Credit (CTC)"
                  ,"Federal Child and Dependent Care Tax Credit (CDCTC)"
                  ,"State CDCTC"
                  ,"State CTC"
                  ,"State EITC"
                  ,"Supplemental Security Income (SSI)"
                  ,"Social Security Disability Insurance (SSDI)"
                  ,"FICA"
                  ,"Federal Income Tax"
    )
    # ACA
    ACA_RuleYear <- unique(acaData$ruleYear)
    aca_values <- NULL
    if(length(ACA_RuleYear) > 1){
      if(is.sequential(ACA_RuleYear)==TRUE){
        aca_values <- paste0(min(ACA_RuleYear),"-",max(ACA_RuleYear))
      }else{
        for(i in 1:length(ACA_RuleYear)-1){
        aca_values <- paste0(aca_values, ACA_RuleYear[i], ", ")
        }
        aca_values <- gsub("^.{0,2}", "", aca_values)
        x <- length(ACA_RuleYear)
        aca_values <- paste0(aca_values, ACA_RuleYear[x])  
       
                             
     }
    }else{
     aca_values <- max(ACA_RuleYear) 
    }
    
    
    #CCDF 
    state<-isolate(as.character(input$state))
    
    if(state == "AL"){
      CCDF_RuleYear <- unique(ccdfData_AL$ruleYear)
    }else if(state == "AK"){
      CCDF_RuleYear <- unique(ccdfData_AK$ruleYear)
    }else if(state == "AR"){
      CCDF_RuleYear <- unique(ccdfData_AR$ruleYear)
    }else if(state == "AZ"){
      CCDF_RuleYear <- unique(ccdfData_AZ$ruleYear)
    }else if(state == "CA"){
      CCDF_RuleYear <- unique(ccdfData_CA$ruleYear)
    }else if(state == "CO"){
      CCDF_RuleYear <- unique(ccdfData_CO$ruleYear)
    }else if(state == "CT"){
      CCDF_RuleYear <- unique(ccdfData_CT$ruleYear)
    }else if(state == "DC"){
      CCDF_RuleYear <- unique(ccdfData_DC$ruleYear)
    }else if(state == "DE"){
      CCDF_RuleYear <- unique(ccdfData_DE$ruleYear)
    }else if(state == "FL"){
      CCDF_RuleYear <- unique(ccdfData_FL$ruleYear)
    }else if(state == "GA"){
      CCDF_RuleYear <- unique(ccdfData_GA$ruleYear)
    }else if(state == "HI"){
      CCDF_RuleYear <- unique(ccdfData_HI$ruleYear)
    }else if(state == "IA"){
      CCDF_RuleYear <- unique(ccdfData_IA$ruleYear)
    }else if(state == "ID"){
      CCDF_RuleYear <- unique(ccdfData_ID$ruleYear)
    }else if(state == "IL"){
      CCDF_RuleYear <- unique(ccdfData_IL$ruleYear)
    }else if(state == "IN"){
      CCDF_RuleYear <- unique(ccdfData_IN$ruleYear)
    }else if(state == "KS"){
      CCDF_RuleYear <- unique(ccdfData_KS$ruleYear)
    }else if(state == "KY"){
      CCDF_RuleYear <- unique(ccdfData_KY$ruleYear)
    }else if(state == "LA"){
      CCDF_RuleYear <- unique(ccdfData_LA$ruleYear)
    }else if(state == "MA"){
      CCDF_RuleYear <- unique(ccdfData_MA$ruleYear)
    }else if(state == "MD"){
      CCDF_RuleYear <- unique(ccdfData_MD$ruleYear)
    }else if(state == "ME"){
      CCDF_RuleYear <- unique(ccdfData_ME$ruleYear)
    }else if(state == "MI"){
      CCDF_RuleYear <- unique(ccdfData_MI$ruleYear)
    }else if(state == "MN"){
      CCDF_RuleYear <- unique(ccdfData_MN$ruleYear)
    }else if(state == "MO"){
      CCDF_RuleYear <- unique(ccdfData_MO$ruleYear)
    }else if(state == "MS"){
      CCDF_RuleYear <- unique(ccdfData_MS$ruleYear)
    }else if(state == "MT"){
      CCDF_RuleYear <- unique(ccdfData_MT$ruleYear)
    }else if(state == "NC"){
      CCDF_RuleYear <- unique(ccdfData_NC$ruleYear)
    }else if(state == "ND"){
      CCDF_RuleYear <- unique(ccdfData_ND$ruleYear)
    }else if(state == "NE"){
      CCDF_RuleYear <- unique(ccdfData_NE$ruleYear)
    }else if(state == "NH"){
      CCDF_RuleYear <- unique(ccdfData_NH$ruleYear)
    }else if(state == "NJ"){
      CCDF_RuleYear <- unique(ccdfData_NJ$ruleYear)
    }else if(state == "NM"){
      CCDF_RuleYear <- unique(ccdfData_NM$ruleYear)
    }else if(state == "NV"){
      CCDF_RuleYear <- unique(ccdfData_NV$ruleYear)
    }else if(state == "NY"){
      CCDF_RuleYear <- unique(ccdfData_NY$ruleYear)
    }else if(state == "OH"){
      CCDF_RuleYear <- unique(ccdfData_OH$ruleYear)
    }else if(state == "OK"){
      CCDF_RuleYear <- unique(ccdfData_OK$ruleYear)
    }else if(state == "OR"){
      CCDF_RuleYear <- unique(ccdfData_OR$ruleYear)
    }else if(state == "PA"){
      CCDF_RuleYear <- unique(ccdfData_PA$ruleYear)
    }else if(state == "RI"){
      CCDF_RuleYear <- unique(ccdfData_RI$ruleYear)
    }else if(state == "SC"){
      CCDF_RuleYear <- unique(ccdfData_SC$ruleYear)
    }else if(state == "SD"){
      CCDF_RuleYear <- unique(ccdfData_SD$ruleYear)
    }else if(state == "TN"){
      CCDF_RuleYear <- unique(ccdfData_TN$ruleYear)
    }else if(state == "TX"){
      CCDF_RuleYear <- unique(ccdfData_TX$ruleYear)
    }else if(state == "UT"){
      CCDF_RuleYear <- unique(ccdfData_UT$ruleYear)
    }else if(state == "VA"){
      CCDF_RuleYear <- unique(ccdfData_VA$ruleYear)
    }else if(state == "VT"){
      CCDF_RuleYear <- unique(ccdfData_VT$ruleYear)
    }else if(state == "WA"){
      CCDF_RuleYear <- unique(ccdfData_WA$ruleYear)
    }else if(state == "WI"){
      CCDF_RuleYear <- unique(ccdfData_WI$ruleYear)
    }else if(state == "WV"){
      CCDF_RuleYear <- unique(ccdfData_WV$ruleYear)
    }else if(state == "WY"){
      CCDF_RuleYear <- unique(ccdfData_WY$ruleYear)
    }
    
    ccdf_values <- NULL
    if(length(CCDF_RuleYear) > 1){
      if(is.sequential(CCDF_RuleYear)==TRUE){
        ccdf_values <- paste0(min(CCDF_RuleYear),"-",max(CCDF_RuleYear))
      }else{
        for(i in 1:length(CCDF_RuleYear)-1){
          ccdf_values <- paste0(ccdf_values, CCDF_RuleYear[i], ", ")
        }
        ccdf_values <- gsub("^.{0,2}", "", ccdf_values)
        x <- length(CCDF_RuleYear)
        ccdf_values <- paste0(ccdf_values, CCDF_RuleYear[x])  
        
        
      }
    }else{
      ccdf_values <- max(CCDF_RuleYear) 
    }
    
    
    #Federal CDCTC
    FEDCDCTC_RuleYear <- unique(fedcdctcData$ruleYear)
    fedcdctc_values <- NULL
    if(length(FEDCDCTC_RuleYear) > 1){
      if(is.sequential(FEDCDCTC_RuleYear)==TRUE){
        fedcdctc_values <- paste0(min(FEDCDCTC_RuleYear),"-",max(FEDCDCTC_RuleYear))
      }else{
        for(i in 1:length(FEDCDCTC_RuleYear)-1){
          fedcdctc_values <- paste0(fedcdctc_values, FEDCDCTC_RuleYear[i], ", ")
        }
        fedcdctc_values <- gsub("^.{0,2}", "", fedcdctc_values)
        x <- length(FEDCDCTC_RuleYear)
        fedcdctc_values <- paste0(fedcdctc_values, FEDCDCTC_RuleYear[x])  
        
        
      }
    }else{
      fedcdctc_values <- max(FEDCDCTC_RuleYear) 
    }
    
    # Federal CTC
   
    FEDCTC_RuleYear <- unique(fedctcData$ruleYear)
    fedctc_values <- NULL
    if(length(FEDCTC_RuleYear) > 1){
      if(is.sequential(FEDCTC_RuleYear)==TRUE){
        fedctc_values <- paste0(min(FEDCTC_RuleYear),"-",max(FEDCTC_RuleYear))
      }else{
        for(i in 1:length(FEDCTC_RuleYear)-1){
          fedctc_values <- paste0(fedctc_values, FEDCTC_RuleYear[i], ", ")
        }
        fedctc_values <- gsub("^.{0,2}", "", fedctc_values)
        x <- length(FEDCTC_RuleYear)
        fedctc_values <- paste0(fedctc_values, FEDCTC_RuleYear[x])  
        
        
      }
    }else{
      fedctc_values <- max(FEDCTC_RuleYear) 
    }
    
    # Federal EITC
   
    FEDEITC_RuleYear <- unique(fedeitcData$ruleYear)
    fedeitc_values <- NULL
    if(length(FEDEITC_RuleYear) > 1){
      if(is.sequential(FEDEITC_RuleYear)==TRUE){
        fedeitc_values <- paste0(min(FEDEITC_RuleYear),"-",max(FEDEITC_RuleYear))
      }else{
        for(i in 1:length(FEDEITC_RuleYear)-1){
          fedeitc_values <- paste0(fedeitc_values, FEDEITC_RuleYear[i], ", ")
        }
        fedeitc_values <- gsub("^.{0,2}", "", fedeitc_values)
        x <- length(FEDEITC_RuleYear)
        fedeitc_values <- paste0(fedeitc_values, FEDEITC_RuleYear[x])  
        
        
      }
    }else{
      fedeitc_values <- max(FEDEITC_RuleYear) 
    }
    
    # Federal Income Tax
   
    fedinctax_RuleYear <- unique(fedinctaxData$ruleYear)
    fedinctax_values <- NULL
    if(length(fedinctax_RuleYear) > 1){
      if(is.sequential(fedinctax_RuleYear)==TRUE){
        fedinctax_values <- paste0(min(fedinctax_RuleYear),"-",max(fedinctax_RuleYear))
      }else{
        for(i in 1:length(fedinctax_RuleYear)-1){
          fedinctax_values <- paste0(fedinctax_values, fedinctax_RuleYear[i], ", ")
        }
        fedinctax_values <- gsub("^.{0,2}", "", fedinctax_values)
        x <- length(fedinctax_RuleYear)
        fedinctax_values <- paste0(fedinctax_values, fedinctax_RuleYear[x])  
        
        
      }
    }else{
      fedinctax_values <- max(fedinctax_RuleYear) 
    }
    
    
    # FICA
   
    ficatax_RuleYear <- unique(ficataxData$ruleYear)
    ficatax_values <- NULL
    if(length(ficatax_RuleYear) > 1){
      if(is.sequential(ficatax_RuleYear)==TRUE){
        ficatax_values <- paste0(min(ficatax_RuleYear),"-",max(ficatax_RuleYear))
      }else{
        for(i in 1:length(ficatax_RuleYear)-1){
          ficatax_values <- paste0(ficatax_values, ficatax_RuleYear[i], ", ")
        }
        ficatax_values <- gsub("^.{0,2}", "", ficatax_values)
        x <- length(ficatax_RuleYear)
        ficatax_values <- paste0(ficatax_values, ficatax_RuleYear[x])  
        
        
      }
    }else{
      ficatax_values <- max(ficatax_RuleYear) 
    }
    
    
    # Head Start / Early Head Start
   
    HEADSTART_RuleYear <- unique(headstartData$ruleYear)
    headstart_values <- NULL
    if(length(HEADSTART_RuleYear) > 1){
      if(is.sequential(HEADSTART_RuleYear)==TRUE){
        headstart_values <- paste0(min(HEADSTART_RuleYear),"-",max(HEADSTART_RuleYear))
      }else{
        for(i in 1:length(HEADSTART_RuleYear)-1){
          headstart_values <- paste0(headstart_values, HEADSTART_RuleYear[i], ", ")
        }
        headstart_values <- gsub("^.{0,2}", "", headstart_values)
        x <- length(HEADSTART_RuleYear)
        headstart_values <- paste0(headstart_values, HEADSTART_RuleYear[x])  
        
        
      }
    }else{
      headstart_values <- max(HEADSTART_RuleYear) 
    }
    
    
    
    # Medicaid 
   
    MEDICAID_RuleYear <- unique(medicaidData$ruleYear)
    medicaid_values <- NULL
    if(length(MEDICAID_RuleYear) > 1){
      if(is.sequential(MEDICAID_RuleYear)==TRUE){
        medicaid_values <- paste0(min(MEDICAID_RuleYear),"-",max(MEDICAID_RuleYear))
      }else{
        for(i in 1:length(MEDICAID_RuleYear)-1){
          medicaid_values <- paste0(medicaid_values, MEDICAID_RuleYear[i], ", ")
        }
        medicaid_values <- gsub("^.{0,2}", "", medicaid_values)
        x <- length(MEDICAID_RuleYear)
        medicaid_values <- paste0(medicaid_values, MEDICAID_RuleYear[x])  
        
        
      }
    }else{
      medicaid_values <- max(MEDICAID_RuleYear) 
    }
    
    
    
    # Pre-K
    preKData$ruleYear <- 2019
    PREK_RuleYear <- unique(preKData$ruleYear)
    prek_values <- NULL
    if(length(PREK_RuleYear) > 1){
      if(is.sequential(PREK_RuleYear)==TRUE){
        prek_values <- paste0(min(PREK_RuleYear),"-",max(PREK_RuleYear))
      }else{
        for(i in 1:length(PREK_RuleYear)-1){
          prek_values <- paste0(prek_values, PREK_RuleYear[i], ", ")
        }
        prek_values <- gsub("^.{0,2}", "", prek_values)
        x <- length(PREK_RuleYear)
        prek_values <- paste0(prek_values, PREK_RuleYear[x])  
        
        
      }
    }else{
      prek_values <- max(PREK_RuleYear) 
    }
    
    
    
    xyzxyz <<- 99
    # School LUnch Program
   
    SCHOOLMEALS_RuleYear <- unique(schoolmealData$ruleYear)
    schoolmeals_values <- NULL
    if(length(SCHOOLMEALS_RuleYear) > 1){
      if(is.sequential(SCHOOLMEALS_RuleYear)==TRUE){
        schoolmeals_values <- paste0(min(SCHOOLMEALS_RuleYear),"-",max(SCHOOLMEALS_RuleYear))
      }else{
        for(i in 1:length(SCHOOLMEALS_RuleYear)-1){
          schoolmeals_values <- paste0(schoolmeals_values, SCHOOLMEALS_RuleYear[i], ", ")
        }
        schoolmeals_values <- gsub("^.{0,2}", "", schoolmeals_values)
        x <- length(SCHOOLMEALS_RuleYear)
        schoolmeals_values <- paste0(schoolmeals_values, SCHOOLMEALS_RuleYear[x])  
        
        
      }
    }else{
      schoolmeals_values <- max(SCHOOLMEALS_RuleYear) 
    }
    
    
    
    xyzxyz <<- 99999
    # Section 8
   
    S8_RuleYear <- unique(section8Data$ruleYear)
    section8_values <- NULL
    if(length(S8_RuleYear) > 1){
      if(is.sequential(S8_RuleYear)==TRUE){
        section8_values <- paste0(min(S8_RuleYear),"-",max(S8_RuleYear))
      }else{
        for(i in 1:length(S8_RuleYear)-1){
          section8_values <- paste0(section8_values, S8_RuleYear[i], ", ")
        }
        section8_values <- gsub("^.{0,2}", "", section8_values)
        x <- length(S8_RuleYear)
        section8_values <- paste0(section8_values, S8_RuleYear[x])  
        
        
      }
    }else{
      section8_values <- max(S8_RuleYear) 
    }
    
    # SNAP
   
    SNAP_RuleYear <- unique(snapData$ruleYear)
    snap_values <- NULL
    if(length(SNAP_RuleYear) > 1){
      if(is.sequential(SNAP_RuleYear)==TRUE){
        snap_values <- paste0(min(SNAP_RuleYear),"-",max(SNAP_RuleYear))
      }else{
        for(i in 1:length(SNAP_RuleYear)-1){
          snap_values <- paste0(snap_values, SNAP_RuleYear[i], ", ")
        }
        snap_values <- gsub("^.{0,2}", "", snap_values)
        x <- length(SNAP_RuleYear)
        snap_values <- paste0(snap_values, SNAP_RuleYear[x])  
        
        
      }
    }else{
      snap_values <- max(SNAP_RuleYear) 
    }
    
    # For state tax credit stuff
    
    state<-isolate(as.character(input$state))
    
    if(state == "AL"){
      state_code <- 1
    }else if(state == "AK"){
      state_code <- 2
    }else if(state == "AR"){
      state_code <- 5
    }else if(state == "AZ"){
      state_code <- 4
    }else if(state == "CA"){
      state_code <- 6
    }else if(state == "CO"){
      state_code <- 8
    }else if(state == "CT"){
      state_code <- 9
    }else if(state == "DC"){
      state_code <- 11
    }else if(state == "DE"){
      state_code <- 10
    }else if(state == "FL"){
      state_code <- 12
    }else if(state == "GA"){
      state_code <- 13
    }else if(state == "HI"){
      state_code <- 15
    }else if(state == "IA"){
      state_code <- 19
    }else if(state == "ID"){
      state_code <- 16
    }else if(state == "IL"){
      state_code <- 17
    }else if(state == "IN"){
      state_code <- 18
    }else if(state == "KS"){
      state_code <- 20
    }else if(state == "KY"){
      state_code <- 21
    }else if(state == "LA"){
      state_code <- 22
    }else if(state == "MA"){
      state_code <- 24
    }else if(state == "MD"){
      state_code <- 25
    }else if(state == "ME"){
      state_code <- 23
    }else if(state == "MI"){
      state_code <- 26
    }else if(state == "MN"){
      state_code <- 27
    }else if(state == "MO"){
      state_code <- 29
    }else if(state == "MS"){
      state_code <- 28
    }else if(state == "MT"){
      state_code <- 30
    }else if(state == "NC"){
      state_code <- 37
    }else if(state == "ND"){
      state_code <- 38
    }else if(state == "NE"){
      state_code <- 31
    }else if(state == "NH"){
      state_code <- 33
    }else if(state == "NJ"){
      state_code <- 34
    }else if(state == "NM"){
      state_code <- 35
    }else if(state == "NV"){
      state_code <- 32
    }else if(state == "NY"){
      state_code <- 36
    }else if(state == "OH"){
      state_code <- 39
    }else if(state == "OK"){
      state_code <- 40
    }else if(state == "OR"){
      state_code <- 41
    }else if(state == "PA"){
      state_code <- 42
    }else if(state == "RI"){
      state_code <- 44
    }else if(state == "SC"){
      state_code <- 45
    }else if(state == "SD"){
      state_code <- 46
    }else if(state == "TN"){
      state_code <- 47
    }else if(state == "TX"){
      state_code <- 48
    }else if(state == "UT"){
      state_code <- 49
    }else if(state == "VA"){
      state_code <- 51
    }else if(state == "VT"){
      state_code <- 50
    }else if(state == "WA"){
      state_code <- 53
    }else if(state == "WI"){
      state_code <- 55
    }else if(state == "WV"){
      state_code <- 54
    }else if(state == "WY"){
      state_code <- 56
    }
    
    # State CDCTC
 statecdctcData$ruleYear <- 2021
 if(state_code %in% statecdctcData$stateFIPS){
 statecdctc_RuleYear <- unique(statecdctcData$ruleYear[statecdctcData$stateFIPS == state_code])
 statecdctc_values <- NULL
 if(length(statecdctc_RuleYear) > 1){
   if(is.sequential(statecdctc_RuleYear)==TRUE){
     statecdctc_values <- paste0(min(statecdctc_RuleYear),"-",max(statecdctc_RuleYear))
   }else{
     for(i in 1:length(statecdctc_RuleYear)-1){
       statecdctc_values <- paste0(statecdctc_values,statecdctc_RuleYear[i], ", ")
     }
     statecdctc_values <- gsub("^.{0,2}", "",statecdctc_values)
     x <- length(statecdctc_RuleYear)
     statecdctc_values <- paste0(statecdctc_values,statecdctc_RuleYear[x])  
     
     
   }
 }else{
   statecdctc_values <- max(statecdctc_RuleYear) 
 }  
 }else{
  statecdctc_values <- "Not Availiable"  
 }
 
 
    # State CTC
 if(state_code %in% statectcData$stateFIPS){
   statectc_RuleYear <- unique(statectcData$ruleYear[statectcData$stateFIPS == state_code])
   statectc_values <- NULL
   if(length(statectc_RuleYear) > 1){
     if(is.sequential(statectc_RuleYear)==TRUE){
       statectc_values <- paste0(min(statectc_RuleYear),"-",max(statectc_RuleYear))
     }else{
       for(i in 1:length(statectc_RuleYear)-1){
         statectc_values <- paste0(statectc_values,statectc_RuleYear[i], ", ")
       }
       statectc_values <- gsub("^.{0,2}", "",statectc_values)
       x <- length(statectc_RuleYear)
       statectc_values <- paste0(statectc_values,statectc_RuleYear[x])  
       
       
     }
   }else{
     statectc_values <- max(statectc_RuleYear) 
   }  
 }else{
   statectc_values <- "Not Availiable" 
 }
    
    
    # State EITC
 if(state_code %in% stateeitcData$stateFIPS){
   stateeitc_RuleYear <- unique(stateeitcData$ruleYear[stateeitcData$stateFIPS == state_code])
   stateeitc_values <- NULL
   if(length(stateeitc_RuleYear) > 1){
     if(is.sequential(stateeitc_RuleYear)==TRUE){
       stateeitc_values <- paste0(min(stateeitc_RuleYear),"-",max(stateeitc_RuleYear))
     }else{
       for(i in 1:length(stateeitc_RuleYear)-1){
         stateeitc_values <- paste0(stateeitc_values,stateeitc_RuleYear[i], ", ")
       }
       stateeitc_values <- gsub("^.{0,2}", "",stateeitc_values)
       x <- length(stateeitc_RuleYear)
       stateeitc_values <- paste0(stateeitc_values,stateeitc_RuleYear[x])  
       
       
     }
   }else{
     stateeitc_values <- max(stateeitc_RuleYear) 
   }  
 }else{
   stateeitc_values <- "Not Availiable"  
 }
 

    # State Income Tax
   stateinctax_RuleYear <- unique(stateinctaxData$ruleYear[stateinctaxData$stateFIPS == state_code])
   stateinctax_RuleYear <- stateinctax_RuleYear[!is.na(stateinctax_RuleYear)]
   stateinctax_values <- NULL
   zyx <<- 1
    if(length(stateinctax_RuleYear) > 1){
      zyx <<- 2
      if(is.sequential(stateinctax_RuleYear)==TRUE){
        zyx <<- 3
       stateinctax_values <- paste0(min(stateinctax_RuleYear),"-",max(stateinctax_RuleYear))
      }else{
        zyx <<- 4
        for(i in 1:length(stateinctax_RuleYear)-1){
         stateinctax_values <- paste0(stateinctax_values,stateinctax_RuleYear[i], ", ")
        }
       stateinctax_values <- gsub("^.{0,2}", "",stateinctax_values)
        x <- length(stateinctax_RuleYear)
       stateinctax_values <- paste0(stateinctax_values,stateinctax_RuleYear[x])  
        
        
      }
    }else{
     stateinctax_values <- max(stateinctax_RuleYear) 
    }
    
   
    # SSDI
    SSDI_RuleYear <- unique(ssdiData$ruleYear)
    ssdi_values <- NULL
    if(length(SSDI_RuleYear) > 1){
      if(is.sequential(SSDI_RuleYear)==TRUE){
        ssdi_values <- paste0(min(SSDI_RuleYear),"-",max(SSDI_RuleYear))
      }else{
        for(i in 1:length(SSDI_RuleYear)-1){
          ssdi_values <- paste0(ssdi_values, SSDI_RuleYear[i], ", ")
        }
        ssdi_values <- gsub("^.{0,2}", "", ssdi_values)
        x <- length(SSDI_RuleYear)
        ssdi_values <- paste0(ssdi_values, SSDI_RuleYear[x])  
        
        
      }
    }else{
      ssdi_values <- max(SSDI_RuleYear) 
    }
    

    # SSI
    SSI_RuleYear <- unique(ssiData$ruleYear)
    ssi_values <- NULL
    if(length(SSI_RuleYear) > 1){
      if(is.sequential(SSI_RuleYear)==TRUE){
        ssi_values <- paste0(min(SSI_RuleYear),"-",max(SSI_RuleYear))
      }else{
        for(i in 1:length(SSI_RuleYear)-1){
          ssi_values <- paste0(ssi_values, SSI_RuleYear[i], ", ")
        }
        ssi_values <- gsub("^.{0,2}", "", ssi_values)
        x <- length(SSI_RuleYear)
        ssi_values <- paste0(ssi_values, SSI_RuleYear[x])  
        
        
      }
    }else{
      ssi_values <- max(SSI_RuleYear) 
    }
    
   
    # TANF
    tanfData$ruleYear <- tanfData$LatestYear
    tanf_RuleYear <- unique(tanfData$ruleYear[tanfData$stateFIPS == state_code])
    tanf_values <- NULL
    if(length(tanf_RuleYear) > 1){
      if(is.sequential(tanf_RuleYear)==TRUE){
        tanf_values <- paste0(min(tanf_RuleYear),"-",max(tanf_RuleYear))
      }else{
        for(i in 1:length(tanf_RuleYear)-1){
          tanf_values <- paste0(tanf_values,tanf_RuleYear[i], ", ")
        }
        tanf_values <- gsub("^.{0,2}", "",tanf_values)
        x <- length(tanf_RuleYear)
        tanf_values <- paste0(tanf_values,tanf_RuleYear[x])  
        
        
      }
    }else{
      tanf_values <- max(tanf_RuleYear) 
    }
    
   
    # WIC
    WIC_RuleYear <- unique(wicData$ruleYear)
    wic_values <- NULL
    if(length(WIC_RuleYear) > 1){
      if(is.sequential(WIC_RuleYear)==TRUE){
        wic_values <- paste0(min(WIC_RuleYear),"-",max(WIC_RuleYear))
      }else{
        for(i in 1:length(WIC_RuleYear)-1){
          wic_values <- paste0(wic_values, WIC_RuleYear[i], ", ")
        }
        wic_values <- gsub("^.{0,2}", "", wic_values)
        x <- length(WIC_RuleYear)
        wic_values <- paste0(wic_values, WIC_RuleYear[x])  
        
        
      }
    }else{
      wic_values <- max(WIC_RuleYear) 
    }
    
    
    RuleYears <- c(snap_values
                   ,schoolmeals_values
                   ,wic_values
                   ,tanf_values
                   ,ccdf_values
                   ,headstart_values
                   ,prek_values
                   ,section8_values
                   ,medicaid_values
                   ,aca_values
                   ,fedeitc_values
                   ,fedctc_values
                   ,fedcdctc_values
                   ,statecdctc_values
                   ,statectc_values
                   ,stateeitc_values
                   ,ssi_values
                   ,ssdi_values
                   ,ficatax_values
                   ,fedinctax_values
      
    )
   
    table <- as.data.frame(cbind(Benefits, RuleYears))
    
    colnames(table)[1] <- "Benefits, Taxes, & Tax Credits"
    colnames(table)[2] <- "Years of Data"
  
    output$table <- renderTable(table, width="70%")
    
  })
}



