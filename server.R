server <- function(input, output, session) {
  
  observeEvent(Sales.Data, {
    
    
    updatePickerInput(session, 'customer',
                      choices = Sales.Data$Buyer%>% unique(),
                      selected = Sales.Data$Buyer%>% unique())
    
  
  })
  
  observeEvent(input$customer, {
    
    season.data = Sales.Data %>% filter(Buyer %in% input$customer)
    
    updatePickerInput(session, 'season',
                      choices = season.data$Season %>% unique(),
                      selected = season.data$Season %>% unique())
    
  })
  
  observeEvent(input$season, {
    
    plant.data = Sales.Data %>% filter(Buyer %in% input$customer & Season %in% input$season)
    
    updatePickerInput(session, 'plant',
                      choices = plant.data$Plant %>% unique(),
                      selected = plant.data$Plant %>% unique())
    
  })
  
  
  observeEvent(input$plant, {
    
    extract.style.data = Sales.Data %>% 
      filter(Buyer %in% input$customer & Season %in% input$season & Plant %in% input$plant )
    
    updatePickerInput(session, 'extract.style',
                      choices = extract.style.data$Extract_Style_Code %>% unique(),
                      selected = extract.style.data$Extract_Style_Code %>% unique())
    
  })
    
  
    
  observeEvent(input$extract.style, {
    
    
    year.data = Sales.Data %>% 
      filter(Buyer %in% input$customer & Season %in% input$season & 
               Plant %in% input$plant &
               Extract_Style_Code %in% input$extract.style )
    
    
    updatePickerInput(session, 'year',
                      choices = year.data$Year %>% unique(),
                      selected = year.data$Year %>% unique())
    
    
  })
    
  observeEvent(input$year, {
    
    
    month.data = Sales.Data %>% 
      filter(Buyer %in% input$customer & Season %in% input$season & 
               Plant %in% input$plant &
               Extract_Style_Code %in% input$extract.style &
               Year %in% input$year)
    
    updatePickerInput(session, 'month',
                      choices = month.data$Month %>% unique() %>% sort(),
                      selected = month.data$Month %>% unique())
    
  })
  
  
  observeEvent(input$month, {

    week.data = Sales.Data %>%
      filter(Buyer %in% input$customer & Season %in% input$season &
               Plant %in% input$plant &
               Extract_Style_Code %in% input$extract.style &
               Year %in% input$year  &
               Month %in% input$month)
    print('gdhjdf')
    print(head(week.data))

    updatePickerInput(session, 'md.week',
                      choices = week.data$Week %>% unique() %>% sort(),
                      selected = week.data$Week %>% unique() %>% max())
    
    updatePickerInput(session, 'mds.week',
                      choices = week.data$Week %>% unique(),
                      selected = week.data$Week %>% unique()%>% max())

  })
  
  
  # 
  # observeEvent(input$month, {
  #   
  #   print(head(Sales.Data))
  #   oc.data = Sales.Data %>% 
  #     filter(Buyer %in% input$customer & Season %in% input$season & 
  #              Plant %in% input$plant &
  #              Extract_Style_Code %in% input$extract.style &
  #              Year %in% input$year  &
  #              Month %in% input$month)
  #   
  #   updatePickerInput(session, 'oc.no',
  #                     choices = oc.data$OCNo %>% unique(),
  #                     selected = oc.data$OCNo %>% unique())
  #   
  # })
  
  
  total.style.list.tbl.df <- reactive({
    
    
    req(input$extract.style)
    
    RM.Data =RM.data%>%
      with_groups(c(Buyer,Extract_Style_Code,OCNo),
                  summarise,
                  across(everything(),first)
      )
    RM.Data =RM.Data%>%
      with_groups(c(Buyer,Extract_Style_Code,P_L_Category,Season,Plant),
                  summarise,
                  Costed_RM_Value=sum(Material$Costed_RM_Value,na.rm=TRUE),
                  Actual_RM_Value=sum(Material$Actual_RM_Value,na.rm=TRUE)
      )
    
    Sales.Data =Sales.Data %>%
      with_groups(c(Buyer,Extract_Style_Code,P_L_Category,Season,Plant),
                  summarise,
                  Actual_Sales_Value=sum(Actual_Sales_Value),
                  Planned_Sales_Value=sum(Planned_Sales_Value),
                  Ship_Qty=sum(Ship_Qty,na.rm=TRUE),
                  DOC_Order_Qty=sum(DOC_Order_Qty,na.rm=TRUE),
                  across(everything(), first)
                  )

    Sales.Data %>% 
      full_join(RM.Data,by=c('Buyer','Extract_Style_Code','P_L_Category','Season','Plant'))  %>%
      # FOB=sum(Ship_Qty*FOB)/Sum(Ship_qty)#%>%
      mutate(PRE_RMC =round(Costed_RM_Value/DOC_Order_Qty,4)	,
             POST_RMC=round(Actual_RM_Value/Ship_Qty,4),
             PRE_RMC_PER=round(Costed_RM_Value/Planned_Sales_Value *100,4),
             POST_RMC_PER=round(Actual_RM_Value/Actual_Sales_Value*100,4),
             PRE_FOB=round(Planned_Sales_Value/Ship_Qty,4),
             POST_FOB=round(Actual_Sales_Value/DOC_Order_Qty,4),
             Order_to_Ship =paste0(round(Ship_Qty/DOC_Order_Qty*100,2),'%')
             ) %>% 
      filter(Buyer %in% input$customer &
               Season %in% input$season &
               Plant %in% input$plant &
               Extract_Style_Code %in% input$extract.style &
               # Style_Code %in% input$full.style &
               Year %in% input$year &
               Month %in% input$month 
               ) %>% 
      select((matches('Buyer|Extract_Style_Code|PRE_RMC|POST_RMC|FOB|Value|Qty|Plant|Season|Category|Ship')))
    
  })
   
    output$total.style.list.tbl <- renderReactable({
      
      req(input$extract.style)
      
      details.data=total.style.list.tbl.df() %>% 
        with_groups(c(Buyer,Plant),
                    summarise,
                    Actual_Sales=round(sum(Actual_Sales_Value,na.rm=TRUE),2))
      
      data <- unique(details.data[, c("Buyer", "Plant","Actual_Sales")])
      
      reactable(details.data, details = function(index) {
        plant_data <- total.style.list.tbl.df()[total.style.list.tbl.df()$Plant == data$Plant[index]&total.style.list.tbl.df()$Buyer == data$Buyer[index], ]
        htmltools::div(style = "padding: 1rem",
                       reactable(plant_data %>% select(-matches('Buyer|Plant|Value|Qty')), 
                                 outlined = TRUE,
                                 defaultExpanded= TRUE,
                                 theme = reactableTheme(
                                   searchInputStyle = list(width = "30%"),
                                   headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                                   footerStyle = list(fontWeight = "normal", background = "#f0f0f0")
                                 ), 
                                 ),
                       
                       )
        })
      
  
      
    })
    
    output$download.total.style.list <- downloadHandler(
      
      filename = function() { 'Total Style List.xlsx' },
      content = function(cont) {
        total.style.list.tbl.df() %>%
          writexl::write_xlsx(cont,col_names = TRUE,format_headers = TRUE) 
      }
    )
    
    output$actual.sales.value <- renderValueBox({
      
      if(isTruthy(input$oc.no)){
        
        Value =Sales.Data
        
        actual.sale.value=paste0('$ ',round(sum(Value$Actual_Sales_Value,na.rm=TRUE)/1000,2),'k')
        
        valueBox(tags$p(actual.sale.value, style = "font-size: 50%;"), tags$p("Actual Sales", style = "font-size: 80%;border-color: #FF0000;"),color='black',icon = icon("dollar"))
        
      }
      else{
        valueBox(tags$p('0k', style = "font-size: 50%;"), tags$p("Actual Sales", style = "font-size: 80%;"),color='black',icon = icon('dollar'))
        
      }
      
    })
 
    


    repeat.style.list.tbl.df <- reactive({
      
      x=Sales.Data %>%  
        filter(Plant %in% input$plant &
                 Buyer %in% input$customer &  
                 Extract_Style_Code %in% input$extract.style &
                 # Style_Code %in% input$full.style & 
                 Season %in% input$season)%>%
        with_groups(c(Buyer,Month),
                    summarise,
                    count =n()
        
      )%>% 
        pivot_wider(names_from=Month,values_from = count)

      return(x)
    })
    
    output$repeat.style.list.tbl <- renderReactable({
      
      req(input$season)
      
      repeat.style.list.tbl.df()  %>%
        reactable(searchable = F, highlight = T,
                  wrap = T, outlined = F, borderless = TRUE,
                  pagination = T, sortable = F,
                  # columnGroups = list(
                  #   colGroup("2023", columns = grepl("2023",Month,ignore.case = TRUE), sticky = "left"),
                  # ),  
                  theme = reactableTheme(
                    searchInputStyle = list(width = "30%"),
                    headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                    footerStyle = list(fontWeight = "normal", background = "#f0f0f0")
                  ))
      
    })
    
    output$download.repeat.style.list <- downloadHandler(
      
      filename = function() { 'Repeat Style List.xlsx' },
      content = function(cont) {
        repeat.style.list.tbl.df() %>%
          writexl::write_xlsx(cont,col_names = TRUE,format_headers = TRUE) 
      }
    )
    
    material.details.tbl.df <- reactive({
      
      req(input$md.week)
      withProgress(message = 'Please wait... gathering data...', value = 0,{
        
        
      setProgress(.20, detail = "gathering data")
        
      recon.data =mongo("Recon_Data",db='Weekly_Reconcilation',url=mongo_url,options = ssl_options(weak_cert_validation = TRUE))
      
      query=paste('{"Buyer":1,"Style_Code":1,"OCNo":1,"OCQty":1 , "Export" :1 ,"FOB":1 ,
      "Process_Name" :1 ,"Plant" :1 ,"Sub_Category" :1 , "Item_Code" :1, "Item_Name":1, 
      "Mat_Color_Code":1,"Mat_Color_Name":1,"Mat_Size_Code" :1,"Mat_Size_Name" :1,"UOM" :1 , 
      "Costed_YY" :1, "Wastage" :1 , "Costed_YY_Include_Wastage" :1, "Unit_Value" :1,
      "Total_Order_Qty" :1 ,"Received_Qty" :1 ,
      "Category" :1 ,"Indent_Type" :1 ,
      "Tot_Required_Qty" :1, "RM_Net_Issued" :1,"RM_Net_Issued_Value" :1,"Status":1,"Week":1}',sep='')

      query1=''
      
      
      setProgress(.30, detail = "Arrange data")
      
      if(input$md=="Material Details"){
        
        query1 <- paste('{"Week" :"',input$md.week,'"}', sep='')
        
      } 
      else{
        
        query1 <- paste('{"Week" :"',input$mds.week,'"}', sep='')
      }

      Recon.Data=recon.data$find(query=query1,fields=query)
      
      # print(head(Recon.Data))
      # print('fdhd')
      setProgress(.20, detail = "calculating value")
      
      Recon.Data =Recon.Data %>%
        mutate(across(-matches('Buyer|Category|UOM|Code|Name|No|Type|Status|Plant|Season|On|Date|Week|id'), as.double))
      
      Material.Details= Recon.Data %>% filter(Status=='Active' & Week %in% input$md.week) %>%
        group_by(OCNo) %>%
        mutate(Export= ifelse(Process_Name=='FINISH GOODs',sum(Export),Export),
               Export=max(Export)) %>% 
        ungroup()
      
      Material.Details= Material.Details %>% group_by(OCNo) %>%
        mutate(Actual_Sales= max(Export*FOB),
               Planned_Sales=if_else(Indent_Type =='Adhoc Indent',0,OCQty*FOB),
               Planned_Sales=max(Planned_Sales),
               Tot_Required_Qty=ifelse(Process_Name=='Adhoc Indent',0,Tot_Required_Qty)
               )  %>% ungroup()%>%
        filter(Process_Name!='FINISH GOODS') %>% select(-matches('id|Status|Week|Process_Name'))
      
      setProgress(.60, detail = "grouping data")
      
      Material.Details.final =Material.Details %>% 
        with_groups(c(OCNo,Style_Code,Category,Sub_Category,Item_Code,Mat_Color_Code,Mat_Size_Code),
                    summarise,
                    Costed_YY=sum(Costed_YY,na.rm=TRUE),
                    Costed_YY_Include_Wastage=sum(Costed_YY_Include_Wastage,na.rm=TRUE),
                    Total_BOM_Qty=sum(Tot_Required_Qty,na.rm=TRUE),
                    Actual_Consumed_Qty=sum(RM_Net_Issued,na.rm=TRUE),
                    Actual_Consumed_Value=sum(RM_Net_Issued_Value,na.rm=TRUE),
                    across(matches('Buyer|Style|Qty|FOB|Sales|Plant|Export|Item_Name|Unit_Value|UOM'),first)
                    ) %>%
        mutate(wastage=(Costed_YY_Include_Wastage-Costed_YY)/Costed_YY,
               Cost_Per_Garment=Costed_YY_Include_Wastage*Unit_Value,
               Total_Bom_Value=Total_BOM_Qty*Unit_Value,
               Rate=Actual_Consumed_Value/Actual_Consumed_Qty,
               Actual_Cost_Per_Garment=Actual_Consumed_Value/Export,
               BOM_Vs_Actual_Cost_Per_Piece=Cost_Per_Garment-Actual_Cost_Per_Garment,
               BOM_RMC=Total_Bom_Value/Planned_Sales,
               Post_RMC=Actual_Consumed_Value/Actual_Sales
               )
      
      })

    })
    
    output$download.material.details <- downloadHandler(
      
      filename = function() { 'Material Details.xlsx' },
      content = function(cont) {
        material.details.tbl.df() %>%
          writexl::write_xlsx(cont,col_names = TRUE,format_headers = TRUE) 
      }
    )
    
    output$material.details.tbl <- renderReactable({
      
      print(head(material.details.tbl.df()))
      
      material.details.tbl.df() %>%
        setNames(nm = gsub("_", " ", colnames(.))) %>%
        reactable(searchable = F, highlight = T,
                  wrap = F, outlined = F, borderless = TRUE,
                  pagination = T, sortable = F,
                  filterable = T,striped=T,
                  # defaultColDef= colDef(),
                  
                  # columnGroups = list(
                  #   colGroup("2023", columns = grepl("2023",Month,ignore.case = TRUE), sticky = "left"),
                  # ),  
                  theme = reactableTheme(
                    searchInputStyle = list(width = "30%"),
                    headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                    footerStyle = list(fontWeight = "normal", background = "#f0f0f0")
                  ))

    })
    
    material.details.summary.tbl.df <- reactive({
      withProgress(message = 'Please wait... gathering data...', value = 0,{
        
        
        material.details.tbl.df() %>% 
          with_groups(c(Buyer,Style_Code,OCNo,Category,Sub_Category,UOM),
                      summarise,
                      Actual_Consumed_Qty=sum(Actual_Consumed_Qty,na.rm=TRUE),
                      Actual_Consumed_Value=sum(Actual_Consumed_Value,na.rm=TRUE),
                      Costed_YY=sum(OCQty*Costed_YY)/sum(OCQty),
                      Costed_YY_Include_Wastage=sum(OCQty*Costed_YY_Include_Wastage)/sum(OCQty),
                      Unit_Value=sum(OCQty*Unit_Value)/sum(OCQty),
                      Total_BOM_Qty=sum(Total_BOM_Qty),
                      across(matches('OCQty|Export|FOB|Sales|Plant'),first)) %>%
          mutate(Total_BOM_Value=Costed_YY_Include_Wastage*Unit_Value,
                 wastage=(Costed_YY_Include_Wastage-Costed_YY)/Costed_YY,
                 Cost_Per_Garment=Costed_YY_Include_Wastage*Unit_Value,
                 Total_Bom_Value=Total_BOM_Qty*Unit_Value,
                 Rate=Actual_Consumed_Value/Actual_Consumed_Qty,
                 Actual_Cost_Per_Garment=Actual_Consumed_Value/Export,
                 BOM_Vs_Actual_Cost_Per_Piece=Cost_Per_Garment-Actual_Cost_Per_Garment,
                 BOM_RMC=Total_Bom_Value/Planned_Sales,
                 Post_RMC=Actual_Consumed_Value/Actual_Sales
          )
        
        
      })
        
      
    })
    
    output$download.material.details.summary <- downloadHandler(
      
      filename = function() { 'Material Details Summary.xlsx' },
      content = function(cont) {
        material.details.summary.tbl.df() %>%
          setNames(nm = gsub("_", " ", colnames(.))) %>%
          writexl::write_xlsx(cont,col_names = TRUE,format_headers = TRUE) 
      }
    )
    
    output$material.details.summary.tbl <- renderReactable({
      
      material.details.summary.tbl.df() %>%
        setNames(nm = gsub("_", " ", colnames(.))) %>%
        reactable(searchable = F, highlight = T,
                  wrap = F, outlined = F, borderless = TRUE,
                  pagination = T, sortable = F,
                  filterable = T,striped=T,

                  # columnGroups = list(
                  #   colGroup("2023", columns = grepl("2023",Month,ignore.case = TRUE), sticky = "left"),
                  # ),  
                  theme = reactableTheme(
                    searchInputStyle = list(width = "30%"),
                    headerStyle = list(background = "#44546A", color = "white", fontWeight = "normal"),
                    footerStyle = list(fontWeight = "normal", background = "#f0f0f0")
                  ))
      
      
      
    })
  
}