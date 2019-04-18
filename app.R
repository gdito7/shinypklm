library(shinydashboard)
library(shiny)
library(glmnet)
library(shinyWidgets)
library(plotly)
library(dplyr)
ui <- dashboardPage(
  dashboardHeader(title = "Lasso Regression "),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem(
      "Lasso Regression",
      tabName = 'sgregression',
      icon = icon("line-chart")
    ),
    menuItem(
      "Developers",
      tabName = 'dev',
      icon = icon("user")
    )
    #,    menuItem(
    # "Group-Lasso Regression",
    #  tabName = 'gpregression',
    #  icon = icon("line-chart")
    #)
  )),
  dashboardBody(# Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = 'home',
              tabsetPanel(
                tabPanel(p("About"),

h1(" "),
tags$div(
  'The purpose of this application is to ease users in doing variables/features selection in a case of high dimensional regression.  
      Instead of working with R-language scripting, this application provides a 
    user interface that works with glmnet package of R behind that.',tags$br(),
  
   'So far, the application covers only LASSO methods,
but is planned to include also other approaches such as Group LASSO and Fused LASSO.
In this application, users are assisted with several 
tools, i.e Data Import, Exploration, Analytics, and Output.',tags$br(),
  
'This application has been developed by a research group of staffs and 
students of the Department of Statistics, Faculty of Mathematics and Natural Sciences, 
Bogor Agricultural University (IPB), Indonesia, as part the research grant titled',tags$b('
"Statistical Methods for Variable-Group Selection in High Dimensional Regression: applications in bioinformatics and experimental data analysis for herbal medicine development".'),
'  The grant has been funded by Ministry of Research, Technology, and Higher Education for the year of 2018. and fully supported by Assoc. Prof. Dr. Agus Salim, La Trobe University.
Any comment and suggestion would be appreciated and could be delivered by email to 
Bagus Sartono (',tags$b('bagusco@gmail.com'),').'
),
tags$br( ),
tags$br(),
HTML('
    <b style="margin-bottom:0">Version:</b>
    <p>0.6</p>

     '),
tags$br(),
tags$br(),
tags$b("supported by:", align = "left"),
tags$br(),
tags$br(),
tags$img(
  src = "Logo-IPB-baru.png",
  width = 60,
  height = 60,
  align = "left"
),
tags$img(
  src = "Logo-Ristekdikti.png",
  width = 60,
  height = 60,
  align = "left"
  
)
),
tabPanel(p( "User Guides"),
                       h2(
                         "User Guides"
                       ),includeHTML("user_guide.html")
         
                       
              ),
tabPanel(p( "Tutorial"),
         h2(
           "Video Tutorial"
         ),  tags$video(src="GUI.mp4",type="video/mp4",width="720px",height="480px",controls="controls")

         
         
)
              )
              ),
      tabItem(tabName = "data",
              tabsetPanel(
                tabPanel(p(icon('cloud-upload'), "Upload"),
                         fluidRow(box(
                           fileInput(
                             "file1",
                             "Choose CSV Files",
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")
                             
                           )
                         ))),
                tabPanel(p(icon("table"), "Dataset"),
                         fluidRow(DT::dataTableOutput("thedata"))),
                tabPanel(
                  p(icon("stats"), "Summary"),
                  
                  h4('Summary Statistics'),
                  
                  DT::dataTableOutput("smry")
                  ),
                  tabPanel(
                    p(icon("line-chart"), "Graphics"),
                    fluidPage(
                      box(uiOutput("plot_y")),
                      box(uiOutput("plot_x")),
                      box(plotlyOutput("plot3",height = "500px"),width=12)
                    )
                    
                )
              ))
      ,
      tabItem(tabName = 'sgregression',
              tabsetPanel(
                tabPanel(p("Model Spesification"),
                        fluidRow(
                           box(selectInput(
                             "family",
                             "Choose Type of Response",
                             list(
                               'Quantitative Response' = 'gaussian',
                               'Binary Response' = 'binomial',
                               'Count Response'  = 'poisson',
                               'Multiclass Response' = 'multinomial',
                               'Multi-Quantitative Response' = 'mgaussian'
                             )
                           )),
                           
                           box(selectInput('intercept',"Include Intercept",
                                           list('Yes'=1,
                                                'No'=0)
                                           ))
                           ,
                           
                           box(uiOutput("choose_resp")),
                           
                           
                           box(selectInput('stdzres',"Standardize Response",
                                           list('No'=0,
                                                'Yes'=1)
                           ))
                           ,
                           box(uiOutput("exclude_pred")),
                           box(selectInput('stdzpred',"Standardize Predictor",
                                           list('Yes'=1,
                                                'No'=0)
                           ))
                           
                                                 )),
                tabPanel('Cross Validation',
                         fluidPage(
                           box(
                             numericInput(
                               'nfolds',
                               'Number of fold (Max 100)',
                               10,
                               min = 5,
                               max = 100
                             )
                           ),
                           box(pickerInput(
                             "measure",
                             "Choose Type of Measure",
                             list(
                               'Mean Squared Error' = 'mse',
                               'Mean Absolute Error' = 'mae',
                               'Deviance (Binary or Count Response only)' = 'deviance',
                               'Class (Multiclass Response only)' = 'class',
                               'Area Under Curve (Binomial Response only)'  = 'auc'
                               
                             )
                           )),
                          box( plotOutput("plot1"),title=p("Cross Validation Plot"),
                               width = 12)
                         )),  tabPanel('Model Output',
                            fluidPage(
                              box(uiOutput("lambda")),#infoBoxOutput("ibox"),
                              box(plotlyOutput("plot"),width=12,title="Non-zero Coeeficient Plot"),
                             
                              box(downloadButton(outputId = 'download',label="Download"),title="Download Coefficient")
                
                            )
                         )
                
              )),tabItem(tabName = 'dev',
                         
                         
                         h2("Developers Team"),
#                         tags$img(
 #                          src = "Logo-IPB-baru.png",
#                           width = 60,
#                           height = 60,
 #                          align = "left"
#                         ),
#                         tags$br(),
#                         tags$br(),
#                         tags$br(),
#                         tags$br(),
                         tags$p("Gerry Alfa Dito (gerryalfadito@gmail.com)"),
                         tags$p("Bagus Sartono (bagusco@gmail.com)"),
                         tags$p("Rahma Anisa (r.rahma.anisa@gmail.com)"),
                         tags$p("Ani Safitri (anisafitristk52@gmail.com)"),
                         tags$p("Farit Mochamad Afendi (fmafendi@apps.ipb.ac.id)"),
                         tags$p("Agus Salim (A.Salim@latrobe.edu.au)"),
                         tags$p("Rahmat H.S. (aule.hesha@gmail.com)"),
                         tags$p("Ade Susanti (adesusanti23@gmail.com)")
                          
                         #,tags$img(
                        #   src = "Logo-IPB-baru.png",
                        #   width = 60,
                        #   height = 60,
                        #   align = "left"
                        # ),
                        # tags$p("Bagus Sartono"),
                        # tags$img(
                        #   src = "Logo-IPB-baru.png",
                        #   width = 60,
                        #   height = 60,
                        #   align = "left"
                        # ),tags$img(
                        #   src = "Logo-IPB-baru.png",
                        #   width = 60,
                        #   height = 60,
                        #   align = "left"
                        # ),
                        # tags$p("Rahma Anisa"),
                        # tags$img(
                        #   src = "Logo-IPB-baru.png",
                        #   width = 60,
                        #   height = 60,
                        #   align = "left"
                        # ),
                        # tags$p("Farit Mochamad Afendi"),
                         #tags$img(
                        #   src = "Logo-IPB-baru.png",
                        #   width = 60,
                        #   height = 60,
                        #   align = "left"
                        # ),                         tags$p("Gerry Alfa Dito"),
                      #   tags$img(
                      #     src = "Logo-IPB-baru.png",
                      #     width = 60,
                      #     height = 60,
                      #     align = "left"
                      #   ),tags$img(
                      #     src = "Logo-IPB-baru.png",
                      #     width = 60,
                      #     height = 60,
                      #     align = "left"
                      #   ),
                      #   tags$p("Bagus Sartono"),
                      #   tags$img(
                      #     src = "Logo-IPB-baru.png",
                      #     width = 60,
                      #     height = 60,
                      #     align = "left"
                      #   ),tags$img(
                      #     src = "Logo-IPB-baru.png",
                      #     width = 60,
                      #     height = 60,
                      #     align = "left"
                      #   ),
                      #   tags$p("Rahma Anisa"),
                      #   tags$img(
                      #     src = "Logo-IPB-baru.png",
                      #     width = 60,
                      #     height = 60,
                      #     align = "left"
                      #   ),
                      #   tags$p("Farit Mochamad Afendi"),
                      #   tags$img(
                      #     src = "Logo-IPB-baru.png",
                      #     width = 60,
                      #     height = 60,
                      #     align = "left"
                      #   )
                         
                         
                         
                         
                         
              )
    ))
)






summarize = function(x, comma = 2) {
  x = as.data.frame(x)
  num = NULL
  for (i in 1:ncol(x)) {
    num = c(num, is.numeric(x[, i]))
  }
  x = x[, which(num == T)]
  # cd=sapply(apply(x,2,unique),length)/apply(x,2,length)
  #  cdc=ifelse(cd>0.2,T,F)
  # x=x[,which(cdc==T)]
  Variable = colnames(x)
  N = apply(x, 2, length)
  Mean = round(apply(x, 2, mean, na.rm = T), comma)
  Sd = round(apply(x, 2, sd, na.rm = T), comma)
  Q1 = round(apply(x, 2, quantile, 0.25, na.rm = T), comma)
  Median = round(apply(x, 2, median, na.rm = T), comma)
  Q3 = round(apply(x, 2, quantile, 0.75, na.rm = T), comma)
  `IQR` = round(apply(x, 2, IQR, na.rm = T), comma)
  Min = round(apply(x, 2, min, na.rm = T), comma)
  Max = round(apply(x, 2, max, na.rm = T), comma)
  N.missing = apply(apply(x, 2, is.na), 2, sum)
  outlier = function(x) {
    lo = quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T)
    hi = quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T)
    out = (x < lo | x > hi)
    return(out)
  }
  N.outlier = apply(apply(x, 2, outlier), 2, sum, na.rm = T)
  dt = data.frame(Variable,
                  N,
                  Mean,
                  Sd,
                  Q1,
                  Median,
                  Q3,
                  `IQR`,
                  Min,
                  Max,
                  N.missing,
                  N.outlier)
  row.names(dt) = NULL
  return(dt)
}




server <- function(input, output) {
  dta=reactive({
    inFile = input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
   data.table::fread(input = inFile$datapath)

  })
  output$thedata = DT::renderDataTable({
    dta()
  }, style = "bootstrap", options = list(scrollX = TRUE))
  
  
  output$plot_y=renderUI({
    dat = dta()
    clnm = colnames(dat)
    selectInput("in_y", "Choose Response Variable", clnm, multiple = F)
  
  })
  
  output$plot_x=renderUI({
    dat = dta()
    clnm = colnames(dat)
    selectInput("in_x", "Choose Predictor Variable", clnm, multiple = F)
    
  })
  
  
  output$plot3 <- renderPlotly({
    dat=dta()
    dat=as.data.frame(dat)
    X=dat[,which(colnames(dat)==input$in_x)]
    Y=dat[,which(colnames(dat)==input$in_y)]
    s1 <- subplot(
      plot_ly(x = X,type = "box"), 
      plotly_empty(), 
      plot_ly(x = X, y = Y,type="scatter",marker=list(size=10)), 
      plot_ly(y = Y,type = "box"),
      nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
      shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
    )
    
    sf <- subplot(
      plot_ly(x = X,type = "histogram")%>%layout(bargap=0.5), 
      plotly_empty(), s1,
      plot_ly(y = Y,type = "histogram")%>%layout(bargap=0.5), 
      nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
      shareX = TRUE, shareY = TRUE
    )
    layout(sf, showlegend = FALSE)
  })
  
  
  
  
  output$choose_resp = renderUI({
  #  inFile = input$file1
    
  #  if (is.null(input$file1)) {
  #    return()
  #  }
    dat = dta()# data.table::fread(input = dta())
    clnm = colnames(dat)
    selectInput("response", "Choose Response Variable", clnm, multiple = T)
    
    #checkboxGroupInput("response","Choose Response Variable",choices=clnm)
  })
  dta1=reactive({
    dat = dta()
    if(is.null(input$response)){
      dat
    }else{
    select(dat,-one_of(input$response))
      }
  })

  output$exclude_pred = renderUI({
    dat1 = dta1()
    dat1=dat1
    clnm1 = colnames(dat1)
    selectInput("predictor", "Exclude Predictor Variable", clnm1, multiple = T)
  })
  
  output$smry = DT::renderDataTable({
#    inFile = input$file1
    
 #   if (is.null(input$file1)) {
  #    return()
   # }
    dat = dta() #data.table::fread(input = inFile$datapath)
    summarize(dat)
    
  }, style = "bootstrap", options = list(scrollX = TRUE))
  

  model= reactive({
    dat = dta() 
    dat1=dta1()
    #data.table::fread(input = inFile$datapath)
    dat = as.data.frame(dat)
    dat1=as.data.frame(dat1)
    y = data.matrix(dat[,which(colnames(dat)==input$response)])
    if(is.null(input$predictor)){
    x=data.matrix(dat1)
    }else{
    x = data.matrix(dat1[,-which(colnames(dat1)==input$predictor)])
    }
    cv = cv.glmnet(
      x=x,
      y=y,
      type.measure = input$measure,
      nfolds = input$nfolds,
      family = input$family,
      alpha = 1,
      grouped = FALSE,
      intercept=input$intercept,
      standardize=input$stdzpred,
      standardize.response=input$stdzres
    )
    cv
  }) 
  
  output$plot1 = renderPlot({
     cv=model() 
    plot(cv)
  })
output$lambda=
  renderUI({
  
  cv=model()
  lambda.min=round(log(min(cv$lambda)),2)
  lambda.max=round(log(max(cv$lambda)),2)
  lambda.opt=round(log(cv$lambda.min),2)
  sliderInput(
    'lmbd', 'Choose Lasso parameter', lambda.min, lambda.max,
    value = lambda.opt,round = T,ticks = F,step=0.05,animate = 
      animationOptions(interval = 300))
    })
  output$plot <- renderPlotly({

    cv=model()
    lmbd1=try(input$lmbd,silent = TRUE)
    lmbd=try(exp(lmbd1),silent = TRUE)
    coef.lasso = coef(cv,s=lmbd)
    coef.lasso.name=coef.lasso@Dimnames[[1]]
    coef.lasso.df=data.frame("Name"=coef.lasso.name,"value"=as.numeric(coef.lasso))
    coef.lasso.df.nonzero=coef.lasso.df[which(coef.lasso.df$value!=0),]
    coef.lasso.df.nonzero.sort=coef.lasso.df.nonzero%>%arrange(desc(abs(value)))
    #coef.lasso.df.nonzero.sort$Name=factor(coef.lasso.df.nonzero.sort$Name,
    #                                       levels =coef.lasso.df.nonzero.sort$Name[order(coef.lasso.df.nonzero.sort$value,decreasing = T)]) 
    
    coef.fin=coef.lasso.df.nonzero.sort
   group.coef=ifelse(coef.fin$value>0,"Positive","Negative")
   if(nrow(coef.fin)==1){
   bar.value=0.9
   }else if(nrow(coef.fin)==2){
     bar.value=0.7
   }else if(nrow(coef.fin)==3){
     bar.value=0.6
   }else{
     bar.value=0.5
   }
   
    plot_ly(x=coef.fin$Name,y=abs(coef.fin$value)
            ,type = 'bar',color=as.factor(group.coef),
            colors=c("#9AACB8","#3C455C"))%>%
      layout(showlegend=TRUE,xaxis=list(showticklabels=T,
                                        categoryoder="array",categoryarray=coef.fin$Name),bargap=bar.value)

    })
#  output$lambdaoptim=renderUI({
#    cv=model()
#    round(log(cv$lambda.min),2)
    
#  })
  coef1=reactive({cv=model()
  coef.lasso = coef(cv)
  coef.lasso.num=as.numeric(coef.lasso)
  coef.lasso.num.nonzero=coef.lasso.num[coef.lasso.num!=0]
  coef.lasso.nonzero.name=coef.lasso@Dimnames[[1]][coef.lasso.num!=0]
  Coef.Lasso=data.frame(coef.lasso.nonzero.name,coef.lasso.num.nonzero)
  colnames(Coef.Lasso)=c("Coefficient","Values")
  Coef.Lasso
  })

output$download <- downloadHandler(
  
  filename = function() {
    paste("coef",.Sys.Date(),".csv", sep = "")
  },
  content=function(file){
    write.csv(coef1(), file, row.names = FALSE)
  },contentType="csv")

}

shinyApp(ui, server = server,options = list(shiny.sanitize.errors=TRUE))