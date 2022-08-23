library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyFeedback)
library(ggplot2)
library(readxl)
library(tools)
library(doParallel)
registerDoParallel(cores=4)
library(plotly)
library(testandroll.pkg)

sampledata <- read.csv("sampledata.csv", header=TRUE)
sampledataasymm <- read.csv("sampledataasymm.csv", header=TRUE)

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  #all the tooltips
  bsTooltip(id="N", title="N: total addressable population", placement="bottom", trigger="hover"),
  bsTooltip(id="s", title="s: standard deviation from collected data", placement="bottom", trigger="hover"),
  bsTooltip(id="sigma", title="\u03c3: from previous data or estimations", placement="bottom", trigger="hover"),
  bsTooltip(id="mu", title="\u03bc: from previous data or estimations", placement="bottom", trigger="hover"),
  bsTooltip(id="usern", title="n: compare profit metrics under test and roll with a user-selected sample size", placement="bottom", trigger="hover"),
  bsTooltip(id="d", title="d: smallest treatment difference test will detect", placement="bottom", trigger="hover"),
  bsTooltip(id="conf", title="confidence: probability of detecting true negatives", placement="bottom", trigger="hover"),
  bsTooltip(id="power", title="power: probability of detecting true postivies", placement="bottom", trigger="hover"),
  
  #beginning of ui
  titlePanel("Test and Roll Calculator"), "The calculator is based on the research paper ", tags$a(href="https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3274875", "Test and Roll: Profit Maximizing A/B Tests"), " by Elea Feit and Ron Berman",
  tags$br("Citation: Feit, Elea McDonnell, and Ron Berman. 'Test & roll: Profit-maximizing a/b tests.' Marketing Science 38, no. 6 (2019): 1038-1058"),
  "Developed by ", tags$a(href="https://www.linkedin.com/in/maxine-fang/","Maxine Fang"), 'and Bolun Xiao.',
  tabsetPanel(
    tabPanel("Two-arm Test and Roll",
             headerPanel(""),
             column (4, wellPanel(strong("Population"), style = "background: #Dcecf4",
                                  numericInput(inputId="N", label="N: Deployment Population",
                                               value=1000)
             ),
             
             #Priors and Posteriors
             
             wellPanel(strong("Priors and posteriors"), style = "background: #Dcecf4",
                       radioButtons("choice", "Data Distribution", choiceNames=c("Symmetric", "Asymmetric"),
                                    choiceValues=c("symm", "asymm")),
                       tabsetPanel(id="tab",
                                   tabPanel(value="inputtab", title="Use Inputed Values",
                                            
                                            conditionalPanel("input.choice == 'symm'", numericInput(inputId="s", label="s: Posterior Standard Deviation",
                                                                                                    value=.1),
                                                             numericInput(inputId="mu", label="\u03bc: Prior Mean",
                                                                          value=.1),
                                                             textOutput("mu_positive"),
                                                             numericInput(inputId="sigma", label="\u03c3: Prior Standard Deviation",
                                                                          value=.05)),
                                            conditionalPanel("input.choice == 'asymm'",
                                                             splitLayout(numericInput(inputId="s1", label="s1",
                                                                                      value=.1),
                                                                         numericInput(inputId="s2", label="s2", value=.2)),
                                                             splitLayout(numericInput(inputId="mu1", label="\u03bc1",
                                                                                      value=.1),
                                                                         numericInput(inputId="mu2", label="\u03bc2",
                                                                                      value=.2)),
                                                             textOutput("mu1_positive"), textOutput("mu2_positive"),
                                                             splitLayout(numericInput(inputId="sigma1", label="\u03c31",
                                                                                      value=.05),
                                                                         numericInput(inputId="sigma2", label="\u03c32",
                                                                                      value=.1))),
                                            uiOutput("n")
                                   ),
                                   
                                   #Uploaded Data
                                   
                                   tabPanel(value="uploadtab",title="Use Uploaded Data", strong("Choose CSV/Excel File"), tags$br(), "1. Please make sure the column containing the mean responses
                                             is named \"Mean.Response\" (download sample data for an example).", tags$br(), "2. Data has to follow the binomial distribution.",
                                            
                                            fileInput("file", "",
                                                      multiple = FALSE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv", ".xls", ".xlsx")),
                                            downloadButton("download", "Download Sample Data"),
                                            tableOutput("sumstats"),
                                            uiOutput("ndata")
                                   ))
                       
                       #User-selected sample size
                       
             ), wellPanel(strong("User selected sample size comparison"),style = "background: #Dcecf4",
                          conditionalPanel("input.choice == 'symm'",
                                           numericInput(inputId="usern", label="n: user selected sample size",
                                                        value=100)),
                          conditionalPanel("input.choice == 'asymm'",
                                           splitLayout(numericInput(inputId="usern1", label="n1",
                                                                    value=100),
                                                       numericInput(inputId="usern2", label="n2", value=150)))
             ),
             
             #Hypothesis Test comparison
             
             wellPanel(strong("Hypothesis Test Comparison"), style = "background: #Dcecf4",
                       numericInput(inputId="d", label="d: Minimum Detectable Difference",
                                    value=.01),
                       numericInput(inputId="conf", label="confidence: 1 - Type I error rate",
                                    value=.95),
                       numericInput(inputId="power", label="power: 1 - Type II error rate",
                                    value=.80),
                       uiOutput("hn")
             )),
             
             column(8,
                    wellPanel(style = "background: white", fluidRow(column (8, plotOutput("bigplot")),
                                                                    column (4, "Legend", div(
                                                                      p("",
                                                                        span(style="color: #00FF00",
                                                                             intToUtf8(9679)),
                                                                        span(textOutput("green_text", inline = TRUE))
                                                                      ),
                                                                      p("",
                                                                        span(style="color: #FF0000",
                                                                             intToUtf8(9679)),
                                                                        span(textOutput("red_text", inline = TRUE))
                                                                      ),
                                                                      p(span(style="color: #0000FF",
                                                                             intToUtf8(9679)),
                                                                        span(textOutput("blue_text", inline = TRUE))
                                                                      ),
                                                                      p(span(style="color: #FF00FF",
                                                                             "---"
                                                                      ),
                                                                      "Profit-Perfect Information"),
                                                                      p(span(style="color: #05B0B7",
                                                                             "---"
                                                                      ),
                                                                      "No Information"
                                                                      ),
                                                                      align = "left"),conditionalPanel("input.choice == 'symm'", span(textOutput("Attention_symm"), style="color:royalblue")), conditionalPanel("input.choice == 'asymm'", span(textOutput("Attention_asymm"), style="color:royalblue"))))),
                    
                    
                    
                    
                    fluidRow(
                      column(5, wellPanel( plotOutput("priormRes"), style = "background: white")),
                      column(5, wellPanel(plotOutput("priorRes"), style = "background: white")),
                      column(5, wellPanel(plotOutput("priorEff"), style = "background: white"))),
                    wellPanel(strong("Summary Table"), tableOutput("summary"), style = "background: white")
                    
             )
             
    ),
    
    tabPanel("K-arm Test and Roll",
             fluidRow(column(12, "Not yet implemented..."))
             
    ),
    tabPanel("Instructions",
             fluidRow(column(8, tags$br(), tags$b("Utilization of the Test and Roll Calculator "), tags$br("Input the \"Priors and posteriors\" panel's parameters (population,
             standard deviation, etc.) You may manually enter the priors and posteriors or upload a CSV, Xls, or Xlsx file.
             Ensure that your uploaded data follows the binomial distribution, and the column containing the mean responses for each treatment is labeled \"Mean.Response.\"
            After uploading data or entering values, each panel will automatically update."),
                             tags$br(), tags$b("Reading outputs -- Figures"), tags$br("The \"Profit per customer versus sample size\" figure illustrates the profit per customer
                                                                           for each test sample size within the context of \"Profit Maximizing A/B Tests.\" You can also compare test
                                                                           sample sizes and profits under the \"User-Selected,\" \"Hypothesis-Test,\" and \"Profit Maximizing A/B Tests\"
                                                                           circumstances. There are some differences between the symmetric and asymmetric conditions. The
                                                                           range of the x-axis for the symmetric condition is 0 to N/2, which corresponds to n, half of the
                                                                           total test sample size. Therefore, the curve contains three dots representing different sample
                                                                           size conditions and their corresponding profits. The x-axis for the asymmetric condition represents the
                                                                           total test sample size: (n1+n2), which ranges from 0 to N. On the curve is only the dot representing
                                                                           the \"Profit Maximizing Sample Size\" and its profit."),
                             tags$br("The figures which depict prior densities with customer profits show how priors distribute across customer profits.
                                     The figures display the customer profit between (\u03bc \u00b1 4s)."),
                             tags$br(), tags$b("Reading outputs -- Summary Table"),
                             tags$br("The summary table displays summary statistics for sample sizes generated using different methods.
                                     Note that Thomson Sampling metrics don't display because of the lengthy calculation times.")))
             
    )
    
    
  ))

server <- function(input, output, session) {
  #the reactive variables
  s1 <- reactiveVal(.1)
  s2 <- reactiveVal(.1)
  mu1 <- reactiveVal(.1)
  mu2 <- reactiveVal(.1)
  sigma1 <- reactiveVal(.05)
  sigma2 <- reactiveVal(.05)
  usern1 <- reactiveVal(100)
  usern2 <- reactiveVal(100)
  
  n1 <- reactiveVal()
  n2 <- reactiveVal()
  nh1 <- reactiveVal()
  nh2 <- reactiveVal()
  
  
  #setting reactive variables
  toListen <- reactive({
    list(input$choice,input$tab,input$file,input$s,input$mu, input$sigma,
         input$usern, input$s1, input$s2, input$mu1, input$mu2, input$sigma1,
         input$sigma2, input$usern1, input$usern2)
  })
  
  #ensure the mean responses to be positive
  mu_positive <- reactive({
    positive <- input$mu > 0
    shinyFeedback::feedbackWarning("mu", !positive, "The mu should be positive")
  })
  
  mu1_positive <- reactive({
    positive <- input$mu1 > 0
    shinyFeedback::feedbackWarning("mu1", !positive, "The mu1 should be positive")
  })
  
  mu2_positive <- reactive({
    positive <- input$mu2 > 0
    shinyFeedback::feedbackWarning("mu2", !positive, "The mu2 should be positive")
  })
  
  observeEvent(toListen(), {
    
    if(input$choice=="symm"&&input$tab=="inputtab") {
      s1(input$s)
      s2(input$s)
      mu1(input$mu)
      mu2(input$mu)
      sigma1(input$sigma)
      sigma2(input$sigma)
      usern1(input$usern)
      usern2(input$usern)
    }
    
    else if(input$choice=="asymm"&&input$tab=="inputtab") {
      s1(input$s1)
      s2(input$s2)
      mu1(input$mu1)
      mu2(input$mu2)
      sigma1(input$sigma1)
      sigma2(input$sigma2)
      usern1(input$usern1)
      usern2(input$usern2)
    }
    
    else if(input$tab=="uploadtab") {
      req(input$file)
      path <- (input$file)$datapath
      ext <- tools::file_ext(path)
      
      if (ext=="csv") {
        data_raw <- read.csv(path)
      }
      
      if (ext=="xls") {
        data_raw <- read_xls(path)
      }
      
      if (ext=="xlsx") {
        data_raw <- read_xlsx(path)
      }
      
      resp <- data_raw$Mean.Response
      group <- data_raw$Group
      df <- data.frame(group,resp)
      
      if(input$choice=="symm") {
        mu <- mean(resp)
        sigma <- sd(resp)
        s <- mean(sqrt(resp*(1-resp)))
        
        s1(s)
        s2(s)
        mu1(mu)
        mu2(mu)
        sigma1(sigma)
        sigma2(sigma)
        
        usern1(input$usern)
        usern2(input$usern)
      }
      else if(input$choice=="asymm") {
        A <- df[df$group=="A",2]
        B <- df[df$group=="B",2]
        mu1 <- mean(A)
        mu2 <- mean(B)
        sigma1 <- sd(A)
        sigma2 <- sd(B)
        s1 <- mean(sqrt(A*(1-A)))
        s2 <- mean(sqrt(B*(1-B)))
        
        s1(s1)
        s2(s2)
        sigma1(sigma1)
        sigma2(sigma2)
        mu1(mu1)
        mu2(mu2)
        
        usern1(input$usern)
        usern2(input$usern)
      }
    }
    
    
  })
  
  
  #Text for the legend
  output$green_text <- renderText({
    "User-selected sample size"
  })
  
  output$blue_text <- renderText({
    "Hypothesis test sample size"
  })
  
  output$red_text <- renderText ({
    "Profit-maximizing sample size"
  })
  
  #Outputs for test and roll
  output$mu_positive <- renderText(mu_positive())
  output$mu1_positive <- renderText(mu1_positive())
  output$mu2_positive <- renderText(mu2_positive())
  
  output$n <- renderUI({
    N <- input$N
    n <- test_size_nn(N, c(s1(),s2()), c(mu1(),mu2()), c(sigma1(),sigma2()))
    n <- round(n)
    n1(n[1])
    n2(n[2])
    n <- paste("n = (", toString(n[1]), ",", toString(n[2]), ")")
    h4(strong(n))
  })
  
  
  output$sumstats <- renderTable({
    if(is.null(input$file)) {
      dfstat <- data.frame(NA)
      names(dfstat) <- "table = NA (No data uploaded)"
    }
    else if(input$choice=="symm") {
      
      dfstat <- data.frame(mu1(),sigma1(),s1())
      names(dfstat) <- c("mu","sigma","s")
    }
    else {
      dfstat <- data.frame(c(mu1(),mu2()), c(sigma1(),sigma2()),
                           c(s1(),s2()))
      names(dfstat) <- c("mu","sigma","s")
    }
    dfstat
    
  })
  
  output$ndata <- renderUI({
    if(is.null(input$file)) {
      h4(strong("n = NA (No data uploaded)"),
         h4(span(style="color:orange", "The data has to follow the binomial distribution !")))
    }
    else{
      ndata <- test_size_nn(input$N, c(s1(),s2()), c(mu1(),mu2()), c(sigma1(),sigma2()))
      ndata <- round(ndata)
      n1(ndata[1])
      n2(ndata[2])
      ndata <- paste ("n = (", toString(ndata[1]), ",", toString(ndata[2]), ")")
      h4(strong(ndata))
    }
  })
  
  
  #Outputs for hypothesis testing
  output$hn <- renderUI({
    
    if(input$tab=="uploadtab"&&is.null(input$file)) {
      h4(strong("n = NA (No data uploaded)"))
    }
    
    else {
      d <- input$d
      conf <- input$conf
      power <- input$power
      N <- input$N
      
      if (input$choice == "symm") {
        nh <- test_size_nht(s1(), d, conf, power, N)
        nh <- round(nh)
        nh1(nh)
        nh2(nh)
        nh <- paste("(",nh,",",nh,")") #test_size_nht returns 1 value
      }
      
      else {
        nh <- test_size_nht(c(s1(), s2()), d, conf, power, N)
        nh <- round(nh)
        nh1(nh[1])
        nh2(nh[2])
        nh <- paste("(",nh[1],",",nh[2],")")
      }
      
      h4(strong(nh))
    }
  })
  
  
  #Output for attentions
  output$Attention_asymm <- renderText({
    "The figure on the left depicts the relationship between the profit per customer and the test sample size within the
    framework: \"Profit Maximizing A/B Tests.\" That is, how the profit per consumer changes as the test sample size changes. In the
     asymmetric situation, the x-axis represents the total test sample size: (n1+n2). And obviouly, the red dot representing the \"Profit-Maximizing Sample Size\" condition is always the peak point of the curve.
     You can compare different test sample sizes and profits in the \"Hypothesis A/B Test,\" \"User-Selected Sample Size,\" and \"Profit Maximizing A/B Tests.\""
    
  })
  
  output$Attention_symm <- renderText({
    "The figure on the left depicts the relationship between the profit per consumer and the test sample size within the
    framework: \"Profit Maximizing A/B Tests.\" That is, how the profit per consumer changes as the test sample size changes. In the symmetric
    situation, the x-axis represents n, half of the test sample size. Therefore, the curve contains three dots representing different
    sample size conditions and their corresponding profits. You can compare different test sample sizes and profits in the \"Hypothesis A/B Test,\"
    \"User-Selected Sample Size,\" and \"Profit Maximizing A/B Tests.\""
  })
  
  
  
  #Outputs for plots
  output$priormRes <- renderPlot({
    if(input$tab=="uploadtab"&&is.null(input$file)) {}
    else {
      plot_prior_mean_resp_nn(c(mu1(),mu2()), c(sigma1(),sigma2()))
    }
  })
  
  output$priorRes <- renderPlot({
    if(input$tab=="uploadtab"&&is.null(input$file)) {}
    else {
      plot_prior_resp_nn(c(s1(),s2()),c(mu1(),mu2()), c(sigma1(),sigma2()))
    }
  })
  
  output$priorEff <- renderPlot({
    if(input$tab=="uploadtab"&&is.null(input$file)) {}
    else {
      plot_prior_effect_nn(c(mu1(),mu2()), c(sigma1(),sigma2()))
    }
  })
  
  #Downloads sample data
  output$download <- downloadHandler(
    
    filename=function() {
      if(input$choice=="symm") {
        "sampledata.csv"
      }
      else {
        "sampledataasymm.csv"
      }
    },
    content=function(file){
      if(input$choice=="symm") {
        write.csv(sampledata, file)
      }
      else {
        write.csv(sampledataasymm,file)
      }
    }
  )
  
  
  #Plots the big profit plot
  output$bigplot <- renderPlot({
    N <- input$N
    
    #Finding profits for points to plot on the graph
    profitmax <- profit_nn(c(n1(),n2()), input$N, c(s1(),s2()), c(mu1(),mu2()), c(sigma1(),sigma2()))
    nhprofit <- profit_nn(c(nh1(),nh2()), input$N, c(s1(),s2()), c(mu1(),mu2()), c(sigma1(),sigma2()))
    userprofit <-  profit_nn(c(usern1(),usern2()), input$N, c(s1(),s2()), c(mu1(),mu2()), c(sigma1(),sigma2()))
    perfectprofit <- profit_perfect_nn(mu1(), sigma1()) #Asymm case not yet implemented
    
    if(input$tab=="uploadtab"&&is.null(input$file)) {
    }
    
    else if(input$choice=="asymm") {
      ratio <- n1() / (n1() + n2())
      n1max <- floor(ratio * input$N) #Round down to avoid sum(n) > N error
      n1val <- seq(1, n1max)
      n2val <- ((1-ratio)/ratio)*n1val
      nval <- data.frame(n1val,n2val)
      totaln <- rowSums(nval)
      
      profits <- apply(nval, MARGIN=1, FUN=profit_nn, N=input$N, s=c(s1(),s2()), mu=c(mu1(),mu2()), sigma=c(sigma1(),sigma2()))
      df <- data.frame(totaln, profits)
      
      ggplot (data=df, aes(x=totaln, y=profits)) + geom_line() + xlim(0,N) + labs(title=~bold("Profit per Customer vs. Sample Size")) +
        xlab("Sample Size (n1+n2)") + ylab("Profit per Customer") + theme(axis.title=element_text(size=14)) + theme(axis.text=element_text(size=11)) +
        theme(plot.title = element_text(hjust = 0.5, face="bold", size=18)) + theme(plot.title = element_text(face = "bold")) +
        geom_point(aes(x = n1()+n2(), y = profitmax),
                   color = "#FF0000",
                   shape = 19,
                   size = 3) +
        geom_point(aes(x = usern1()+usern2(), y = userprofit),
                   color = "#00FF00",
                   shape = 19,
                   size = 3) +
        geom_point(aes(x = nh1()+nh2(), y = nhprofit),
                   color = "#0000FF",
                   shape = 19,
                   size = 3) +
        theme_minimal()
    }
    else {
      nval <- seq(1, N/2)
      profits <- sapply(nval, FUN=profit_nn, N=N, s=c(s1(),s2()), mu=c(mu1(),mu2()), sigma=c(sigma1(),sigma2()))
      df <- data.frame(nval, profits)
      
      
      
      
      ggplot (data=df, aes(x=nval, y=profits)) + geom_line() +  labs(title=~bold("Profit per Customer vs. Sample Size")) +
        xlab("Sample Size (n)") + ylab("Profit per Customer") + theme(axis.title=element_text(size=14)) + theme(axis.text=element_text(size=11)) +
        theme(plot.title = element_text(hjust = 0.5, face="bold", size=18)) + theme(plot.title = element_text(face = "bold")) +
        geom_point(aes(x = n1(), y = profitmax),
                   color = "#FF0000",
                   shape = 19,
                   size = 3) +
        geom_point(aes(x = usern1(), y = userprofit),
                   color = "#00FF00",
                   shape = 19,
                   size = 3) +
        geom_point(aes(x = nh1(), y = nhprofit),
                   color = "#0000FF",
                   shape = 19,
                   size = 3) +
        geom_hline(aes(yintercept = perfectprofit),
                   color = "#FF00FF",
                   linetype = 2) +
        #No Information Profit Line
        geom_hline(aes(yintercept = mu1()),
                   color = "#05B0B7",
                   linetype = 2) +
        theme_minimal()
    }
  })
  
  output$summary <- renderTable({
    if(input$tab=="uploadtab"&&is.null(input$file)) {}
    else {
      #Calculating values for dataframe
      
      #Total Profit
      profperfc <- round(profit_perfect_nn(mu1(), sigma1()),2)
      profperf <- round(profperfc*input$N,2)#change for asymm case
      if(input$choice=="asymm") {
        profperf <- NA
      }
      proftandrc <- round(profit_nn(c(n1(),n2()), input$N, c(s1(),s2()), c(mu1(),mu2()), c(sigma1(),sigma2())),2)
      proftandr <- round(proftandrc*input$N,2)
      profhypc <- round(profit_nn(c(nh1(),nh2()), input$N, c(s1(),s2()), c(mu1(),mu2()), c(sigma1(),sigma2())),2)
      profhyp <- round(profhypc*input$N,2)
      profuserc <- round(profit_nn(c(usern1(),usern2()), input$N, c(s1(),s2()), c(mu1(),mu2()), c(sigma1(),sigma2())),2)
      profuser <- round(profuserc*input$N,2)
      notestc <- mu1()
      notest <- notestc*input$N #assume deploy choice 1
      
      #Test Profit
      proftest.tandr <- round(mu1()*n1()+mu2()*n2(),2)
      proftest.hyp <- round(mu1()*nh1()+mu2()*nh2(),2)
      proftest.user <- round(mu1()*usern1()+mu2()*usern2(),2)
      
      #Deploy Profit (see below)
      
      #Percent Gains
      gainperf <- round((profperf-notest)/notest,4)
      gainperf <- paste(gainperf*100, "%")
      gaintandr <- round((proftandr-notest)/notest,4)
      gaintandr <- paste(gaintandr*100,"%")
      gainhyp <- round((profhyp-notest)/notest,4)
      gainhyp <- paste(gainhyp*100, "%")
      gainuser <- round((profuser-notest)/notest,4)
      gainuser <- paste(gainuser*100,"%")
      
      #Error Rate
      if(input$choice=="asymm") { #error_rate_nn does not support asymmetric s
        errortandr <- NA
        errorhyp <- NA
        erroruser <- NA
      }
      
      errortandr <- round(error_rate_nn(n1(), s1(), sigma1()),4)
      errortandr <- paste(errortandr*100, "%")
      errorhyp <- round(error_rate_nn(nh1(), s1(), sigma1()),4)
      errorhyp <- paste(errorhyp*100, "%")
      erroruser <- round(error_rate_nn(usern1(), s1(), sigma1()),4)
      erroruser <- paste(erroruser*100, "%")
      
      
      #Creating dataframe for summary output
      dfsum <- data.frame(
        "Method"=c(
          "Perfect Information",
          "Thompson Sampling",
          "Test & Roll",
          "Hypothesis Test",
          "Selected Sample Size",
          "No Test"),
        "Sample Size A (n1)"=
          c("NA", "NA", n1(), nh1(), usern1(), 0),
        "Sample Size B (n2)"=
          c("NA", "NA", n2(), nh2(), usern2(), 0),
        "Profit per Customer"=c(profperfc, "NA", proftandrc, profhypc, profuserc, notestc),
        "Total Profit"=c(profperf, "NA", proftandr, profhyp, profuser, notest),
        "Test Profit"=c(0, "NA", proftest.tandr, proftest.hyp, proftest.user, 0),
        "Deploy Profit"=c(profperf, "NA", proftandr-proftest.tandr, profhyp-
                            proftest.hyp, profuser-proftest.user, notest),
        "Percentage of Gains Realized"=c(gainperf, "NA", gaintandr, gainhyp, gainuser, "0 %"),
        "Error Rate"=c("0 %", "NA", errortandr, errorhyp, erroruser, "50 %")
      )
      
      
      
      colnames(dfsum)<-c("Method","Sample Size A (n1)", "Sample Size B (n2)", "Profit per Customer", "Total Profit",
                         "Test Profit", "Deploy Profit", "Percentage of Gains Realized", "Error Rate")
      
      dfsum
    }
  })
  
}

shinyApp(ui=ui, server=server)
