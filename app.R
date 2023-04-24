#pesticide selection tool
library(shiny)
library(tidyr)
library(dplyr)
library(knitr)
library(rmarkdown)
library(readxl)
library(stargazer)
pesticide<-read_excel("data/pesticides.xlsx")
#can change to reading in original crop budget values to update annually
burn<-read_excel("data/burn long.xlsx")
pre<-read_excel("data/pre long.xlsx")
post<-read_excel("data/post long.xlsx")
names<-read_excel("data/weed names.xlsx")
info<-read_excel("data/pesticide info.xlsx")


ui <- fluidPage(
  
  # App title ----
  titlePanel(includeMarkdown("header.md")),
  
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Burndown Herbicides",tableOutput("table1")),
              tabPanel("Pre-emergence Herbicides", tableOutput("table2")),
              tabPanel("Post-emergence Herbicides", tableOutput("table3"))),
  hr(),
  fluidRow(
    
    column(1,
           checkboxGroupInput("weeds.b","Broadleaf Weeds",choices=names$weed[names$type=="broadleaf"])),
    column(1,
           checkboxGroupInput("weeds.g","Grass Weeds",choices=names$weed[names$type=="grass"])),
           column(3,
                  h4('Burndown Products'),
                  selectInput("herb1","Burndown herbicide",pesticide$name[pesticide$burndown==1],
                              selected="none"),
                  uiOutput("rate1"),
                  uiOutput("cost1"),
                  selectInput("herb2","Burndown herbicide",pesticide$name[pesticide$burndown==1],
                              selected="none"),
                  uiOutput("rate2"),
                  uiOutput("cost2"),
                  selectInput("herb3","Burndown herbicide",pesticide$name[pesticide$burndown==1],
                              selected="none"),
                  uiOutput("rate3"),
                  uiOutput("cost3"),),
           column(3,
                  h4('Pre Emergence Products'),
                  selectInput("herb4","Preplant herbicide",pesticide$name[pesticide$PRE==1],
                              selected="none"),
                  uiOutput("rate4"),
                  uiOutput("cost4"),
                  selectInput("herb5","Preplant herbicide",pesticide$name[pesticide$PRE==1],
                              selected="none"),
                  uiOutput("rate5"),
                  uiOutput("cost5"),
                  selectInput("herb6","Preplant herbicide",pesticide$name[pesticide$PRE==1],
                              selected="none"),
                  uiOutput("rate6"),
                  uiOutput("cost6")),
           column(3,
                  h4('Postemergence products'),
                  selectInput("herb7","Postemergence herbicide",pesticide$name[pesticide$POST==1],
                              selected="none"),
                  uiOutput("rate7"),
                  uiOutput("cost7"),
                  selectInput("herb8","Postemergence herbicide",pesticide$name[pesticide$POST==1],
                              selected="none"),
                  uiOutput("rate8"),
                  uiOutput("cost8"),
                  selectInput("herb9","Postemergence herbicide",pesticide$name[pesticide$POST==1],
                              selected="none"),
                  uiOutput("rate9"),
                  uiOutput("cost9"),
                  helpText("Click the button below to save",
                           "information entered into budget"),
                  downloadButton("report", "Generate report"))
           )
    )
  
  
  

server <- function(input, output) {
  
  
  #dynamic user interface for burndown herbicides
  output$rate1<-renderUI({numericInput("rate1",paste("Rate (",pesticide$unit[pesticide$name==input$herb1],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb1])
    
  })
  output$cost1<-renderUI({numericInput("cost1",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb1],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb1])
    
  })
  output$rate2<-renderUI({numericInput("rate2",paste("Rate (",pesticide$unit[pesticide$name==input$herb2],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb2])
    
  })
  output$cost2<-renderUI({numericInput("cost2",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb2],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb2])
    
  })
  
  output$rate3<-renderUI({numericInput("rate3",paste("Rate (",pesticide$unit[pesticide$name==input$herb3],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb3])
    
  })
  output$cost3<-renderUI({numericInput("cost3",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb3],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb3])
    
  })

  #dynamic user interface for pre-emergence herbicides
  output$rate4<-renderUI({numericInput("rate4",paste("Rate (",pesticide$unit[pesticide$name==input$herb4],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb4])
    
  })
  output$cost4<-renderUI({numericInput("cost4",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb4],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb4])
    
  })
  output$rate5<-renderUI({numericInput("rate5",paste("Rate (",pesticide$unit[pesticide$name==input$herb5],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb5])
    
  })
  output$cost5<-renderUI({numericInput("cost5",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb5],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb5])
    
  })
  output$rate6<-renderUI({numericInput("rate6",paste("Rate (",pesticide$unit[pesticide$name==input$herb6],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb6])
    
  })
  output$cost6<-renderUI({numericInput("cost6",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb6],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb6])
    
  })
  
  #dynamic user interface for postemergence herbicides
  
  output$rate7<-renderUI({numericInput("rate7",paste("Rate (",pesticide$unit[pesticide$name==input$herb7],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb7])
    
  })
  output$cost7<-renderUI({numericInput("cost7",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb7],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb7])
    
  })
  output$rate8<-renderUI({numericInput("rate8",paste("Rate (",pesticide$unit[pesticide$name==input$herb8],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb8])
    
  })
  output$cost8<-renderUI({numericInput("cost8",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb8],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb8])
    
  })
  output$rate9<-renderUI({numericInput("rate9",paste("Rate (",pesticide$unit[pesticide$name==input$herb9],
                                                     ")",sep=""),pesticide$rate[pesticide$name==input$herb9])
    
  })
  output$cost9<-renderUI({numericInput("cost9",paste("Cost ($/",pesticide$unit_bulk[pesticide$name==input$herb9],
                                                     ")",sep=""),pesticide$price_bulk[pesticide$name==input$herb9])
    
  })
  

  
    #need to create reactive values that take into account price with user-defined rates and prices
  HERB1<-reactive({input$cost1/pesticide$convert[pesticide$name==input$herb1]*input$rate1
  })
  HERB2<-reactive({input$cost2/pesticide$convert[pesticide$name==input$herb2]*input$rate2
  })
  HERB3<-reactive({input$cost3/pesticide$convert[pesticide$name==input$herb3]*input$rate3
  })
  HERB4<-reactive({input$cost4/pesticide$convert[pesticide$name==input$herb4]*input$rate4
  })
  HERB5<-reactive({input$cost5/pesticide$convert[pesticide$name==input$herb5]*input$rate5
  })
  HERB6<-reactive({input$cost6/pesticide$convert[pesticide$name==input$herb6]*input$rate6
  })
  HERB7<-reactive({input$cost7/pesticide$convert[pesticide$name==input$herb7]*input$rate7
  })
  HERB8<-reactive({input$cost8/pesticide$convert[pesticide$name==input$herb8]*input$rate8
  })
  HERB9<-reactive({input$cost9/pesticide$convert[pesticide$name==input$herb9]*input$rate9
  })
#  INS1<-reactive({input$cost_ins1/pesticide$convert[pesticide$name==input$insect1]*input$rate_ins1
#  })

  
  #dataframe of pesticide inputs to pass to RMarkdown 
#  pest_input<-reactive({
#    a<-tibble(
#      Input=c(input$herb1,input$herb4,input$herb7),
#      Group=c(pesticide$group[pesticide$name==input$herb1],pesticide$group[pesticide$name==input$herb4],
#              pesticide$group[pesticide$name==input$herb7]),
#      Rate=c(input$rate1,input$rate4,input$rate7),
#      Unit=c(pesticide$unit[pesticide$name==input$herb1],pesticide$unit[pesticide$name==input$herb4],
#             pesticide$unit[pesticide$name==input$herb7]),
#      Cost=round(c(HERB1(),HERB4(),HERB7())
#                 ,digits=2))
#    a<-filter(a,Input!="none")
#    a
#  })
  #recommendation for managing herbicide resistant weeds if box is checked (passed to markdown)
  resistance<-reactive({if(pesticide$group[pesticide$name==input$herb4]=="2"){"**Many weeds have developed
    resistance to ALS-inhibiting or Group 2 herbicides"}
    else{NULL}
  })

  #reactive tables of herbicides to pass to markdown
  #dataframe of pesticide inputs to pass to RMarkdown 
  burn_input<-reactive({
    a<-tibble(
      Input=c(input$herb1,input$herb2,input$herb3),
      `Active ingredient`=c(pesticide$chem[pesticide$name==input$herb1],pesticide$chem[pesticide$name==input$herb2],
                            pesticide$chem[pesticide$name==input$herb3]),
      Group=c(pesticide$group[pesticide$name==input$herb1],pesticide$group[pesticide$name==input$herb2],
              pesticide$group[pesticide$name==input$herb3]),
      Rate=c(input$rate1,input$rate2,input$rate3),
      Unit=c(pesticide$unit[pesticide$name==input$herb1],pesticide$unit[pesticide$name==input$herb2],
             pesticide$unit[pesticide$name==input$herb3]),
      Cost=round(c(HERB1(),HERB2(),HERB3())
                 ,digits=2))
    a<-filter(a,Input!="none")
    a
  })
  
  pre_input<-reactive({
    a<-tibble(
      Input=c(input$herb4,input$herb5,input$herb6),
      `Active ingredient`=c(pesticide$chem[pesticide$name==input$herb4],
                            pesticide$chem[pesticide$name==input$herb5],pesticide$chem[pesticide$name==input$herb6]),
      Group=c(pesticide$group[pesticide$name==input$herb4],
              pesticide$group[pesticide$name==input$herb5],pesticide$group[pesticide$name==input$herb6]),
      Rate=c(input$rate4,input$rate5,input$rate6),
      Unit=c(pesticide$unit[pesticide$name==input$herb4],
             pesticide$unit[pesticide$name==input$herb5],pesticide$unit[pesticide$name==input$herb6]),
      Cost=round(c(HERB4(),HERB5(),HERB6())
                 ,digits=2))
    a<-filter(a,Input!="none")
    a
  })
  
  post_input<-reactive({
    a<-tibble(
      Input=c(input$herb7,input$herb8,input$herb9),
      `Active ingredient`=c(pesticide$chem[pesticide$name==input$herb7],
                            pesticide$chem[pesticide$name==input$herb8],pesticide$chem[pesticide$name==input$herb9]),
      Group=c(pesticide$group[pesticide$name==input$herb7],
              pesticide$group[pesticide$name==input$herb8],pesticide$group[pesticide$name==input$herb9]),
      Rate=c(input$rate7,input$rate8,input$rate9),
      Unit=c(pesticide$unit[pesticide$name==input$herb7],
             pesticide$unit[pesticide$name==input$herb8],pesticide$unit[pesticide$name==input$herb9]),
      Cost=round(c(HERB7(),HERB8(),HERB9())
                 ,digits=2))
    a<-filter(a,Input!="none")
    a
  })
  
  adjuvant_pre<-reactive({
    herbicides<-c(input$herb4,input$herb5,input$herb6)
    adj<-info%>%
      filter(name%in%herbicides)%>%
      select(NIS:MSO_unit)
    rates<-tibble(rate=c(max(adj$NIS,na.rm=TRUE),max(adj$COC,na.rm=TRUE),max(adj$MSO,na.rm=TRUE)),
                  product=c("non-ionic surfactant","crop oil concentrate","methylated seed oil"),
                  unit=rep("qt/100 gal",3))
    rates<-mutate(rates,text=paste(rate,unit,product,sep=" "))
    rates<-filter(rates,rate!=-Inf)
    a<-if(dim(rates)[1]>0){print(c("*This application requires one of the following adjuvants:", rates$text))}else{NULL}
    a
  })
  
  adjuvant_burn<-reactive({
    herbicides<-c(input$herb1,input$herb2,input$herb3)
    adj<-info%>%
      filter(name%in%herbicides)%>%
      select(NIS:MSO_unit)
    rates<-tibble(rate=c(max(adj$NIS,na.rm=TRUE),max(adj$COC,na.rm=TRUE),max(adj$MSO,na.rm=TRUE)),
                  product=c("non-ionic surfactant","crop oil concentrate","methylated seed oil"),
                  unit=rep("qt/100 gal",3))
    rates<-mutate(rates,text=paste(rate,unit,product,sep=" "))
    rates<-filter(rates,rate!=-Inf)
    a<-if(dim(rates)[1]>0){print(c("*This application requires one of the following adjuvants:", rates$text))}else{NULL}
    a
  })
  
  adjuvant_post<-reactive({
    herbicides<-c(input$herb7,input$herb8,input$herb9)
    adj<-info%>%
      filter(name%in%herbicides)%>%
      select(NIS:MSO_unit)
    rates<-tibble(rate=c(max(adj$NIS,na.rm=TRUE),max(adj$COC,na.rm=TRUE),max(adj$MSO,na.rm=TRUE)),
                  product=c("non-ionic surfactant","crop oil concentrate","methylated seed oil"),
                  unit=rep("qt/100 gal",3))
    rates<-mutate(rates,text=paste(rate,unit,product,sep=" "))
    rates<-filter(rates,rate!=-Inf)
    a<-if(dim(rates)[1]>0){print(c("*This application requires one of the following adjuvants:", rates$text))}else{NULL}
    a
  })
  
  #warnings for similar modes of action, and for group 2 herbicides
#  group2<-reactive({
#  a<-c(pesticide$group[pesticide$name==input$herb7],
#       pesticide$group[pesticide$name==input$herb8],pesticide$group[pesticide$name==input$herb9])
#  if(input$resistance==TRUE){"**Any fields with herbicide resistant
#    weeds should reconsider post application products"}
#    else{NULL}
#  })
  
  #create reactive dataframes of values for table outputs
  burndown.spray<-reactive({
    a<-burn%>%
      filter(weed%in%c(input$weeds.b,input$weeds.g))%>%
      select(Herbicide,rating,weed)%>%
      group_by(Herbicide)%>%
      filter(sum(rating)>0)%>%
      pivot_wider(.,id_cols=weed,names_from=Herbicide,values_from=rating)
    a
  })

    pre.spray<-reactive({
    b<-pre%>%
      filter(weed%in%c(input$weeds.b,input$weeds.g))%>%
      select(Herbicide,rating,weed)%>%
      group_by(Herbicide)%>%
      filter(sum(rating)>0)%>%
      pivot_wider(.,id_cols=weed,names_from=Herbicide,values_from=rating)
    b
  })
  
  post.spray<-reactive({
    c<-post%>%
      filter(weed%in%c(input$weeds.b,input$weeds.g))%>%
      select(Herbicide,rating,weed)%>%
      group_by(Herbicide)%>%
      filter(sum(rating)>0)%>%
      pivot_wider(.,id_cols=weed,names_from=Herbicide,values_from=rating)
    c
  })
  output$table1<-renderTable({
    burndown.spray()},digits=0
  )
  
  output$table2<- renderTable({
    pre.spray()},digits=0
    
  )
  
  output$table3<- renderTable({
    post.spray()},digits=0
    
  )
  
  
  output$report <- downloadHandler(
    
    filename = "Pesticide_report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "budget report.Rmd")
      file.copy("budget report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(burn_input=burn_input(),
                     pre_input=pre_input(),
                     post_input=post_input(),
                     resist=resistance(),
                     broad=input$weeds.b,
                     grass=input$weeds.g,
                     adj_pre=adjuvant_pre(),
                     adj_burn=adjuvant_burn(),
                     adj_post=adjuvant_post())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,output_file=file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)



