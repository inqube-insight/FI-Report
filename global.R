library(apexcharter)
library(flexdashboard)
library(jsonlite)
library(mongolite)
library(rjson)
library(reactable)
library(reactablefmtr)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(stringr)
library(tidyverse)
library(tippy)


mongo_url <- "mongodb://MongodbAdmin:t%262uO%7Cr_8%5D4%5DJ68@SG-PROD-NLB-56d694edf100cb2d.elb.ap-southeast-1.amazonaws.com:27017/?tls=true&directConnection=true&tlsAllowInvalidHostnames=true"


sales.data =mongo("Sales_Summary",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
Sales.Data=sales.data$find('{}')


Sales.Data =Sales.Data %>% 
  mutate(Extract_Style_Code=str_sub(Style_Code,1,9),
         Year=format(as.Date(paste(Week, 1, sep="-"), "%Y-%U-%u"),"%Y"),
         Month=format(as.Date(paste(Week, 1, sep="-"), "%Y-%U-%u"),"%Y-%B")
  )%>%
  with_groups(c(Buyer,Extract_Style_Code,P_L_Category,Season,Plant),
              summarise,
              Actual_Sales_Value=sum(Actual_Sales_Value),
              Planned_Sales_Value=sum(Planned_Sales_Value),
              across(where(is.numeric),sum),
              across(everything(), first))  

rm.data =mongo("RM_Summary",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))

RM.data =rm.data$find('{}')

RM.data=RM.data %>% mutate(Extract_Style_Code=str_sub(Style_Code,1,9))



source('server.R')
source('ui.R')

shinyApp(ui = ui, server = server)

