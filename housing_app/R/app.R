# Name: Jinru Zhang


#########################################################################################################
########                                  Import Package                                   ##############      
#########################################################################################################

library(shiny)
library(tidyverse)
library(ggplot2)
# devtools::install_github("lionel-/ggstance")
library(ggstance)
library(purrr)

#########################################################################################################
########                                  Read Data                                        ##############      
#########################################################################################################

setwd("C:/Users/jinru/hw06-sunflower118/housing_app")
estate <- read_csv("./data/estate.csv")

#########################################################################################################
########                                  Data Cleansing                                   ##############      
#########################################################################################################

estate <- estate %>%
  mutate("Price($K)" = Price  / 1000,
         "Pool" = as.factor(ifelse(Pool == 1, "Pool", "No Pool")),
         "AC" = as.factor(ifelse(AC == 1, "AC", "No AC")),
         "Highway" = as.factor(ifelse(Highway == 1, "Highway Adj", "Highway Not Adj")),
         "Style" = as.factor(Style)) %>%
  select(-Price) %>%
  relocate("Price($K)")

# Variable dictionary
# 1: Numeric and not contain 0
# 2: Numeric and contain 0
# 3: Not numeric

var_name <- c()
var_cat <- c()
for ( i in 1:dim(estate)[2]){
  var_name <- c(var_name, colnames(estate)[i])
  if (is.numeric(simplify2array(estate[,i])) == T & (sum(estate[,i] == 0) == 0)){
    var_cat <- c(var_cat, 1)
  } else if (is.numeric(simplify2array(estate[,i])) == T & (sum(estate[,i] == 0) > 0)){
    var_cat <- c(var_cat, 2)
  } else if(is.numeric(simplify2array(estate[,i])) == F){
    var_cat <- c(var_cat, 3)
  }
}


var_dict <- data.frame(var_name, var_cat)


# var_dict[var_name == "Area" ,2]
#########################################################################################################
########                                Utilities Function                                 ##############      
#########################################################################################################

#################################################################################################
########                         1. T Test function                                ##############      
#################################################################################################

ttest_func <- function(variable = "Price($K)", data = estate, log_tran = F, null_value = 0){
  
  # validate(need(is.numeric(simplify2array(data %>%select(variable))) == T, "Variable is not numeric"))
  if(var_dict[var_name == variable ,2] == 3){
    validate("Variable is not numeric")
  }

  # if (is.numeric(simplify2array(data %>%select(variable))) == F){
  #   return("Variable is not numeric")
  # }
  # 
  if (log_tran){
    if(var_dict[var_name == variable ,2] == 2){
      validate("Variable contains one or more values of 0")
    }
    data_use <- log(data %>%select(variable))
  } else {
    data_use <- data %>%select(variable)
  }
  
  # validate(need(any(data_use==-Inf) == FALSE, "Variable contains one or more values of 0."))
  
  if (any(data_use==-Inf) == TRUE){
    return("Variable contains one or more values of 0.")
  }
  
  
  t_test_output <- t.test(data_use, mu = null_value)
  p_val <- round(t_test_output$p.value,2)
  est   <- round(t_test_output$estimate,2)
  L95   <- round(t_test_output$conf.int[1],2)
  U95   <- round(t_test_output$conf.int[2],2)
  
  output <- tribble(
    ~"P-value", ~"Estimate",  ~"95% Lower", ~"95% Upper",
    p_val, est,  L95, U95)
  
  return(as.data.frame(output))
}

#################################################################################################
########                         2. Regression Function                            ##############      
#################################################################################################


regfunc <- function(varX = "Area", varY = "Price($K)", logX = F, logY = F, data = estate){
  # if(var_dict[var_name == varX ,2] == 1 & var_dict[var_name == varY ,2] == 1 ){
  if (logX & logY){
    model <- lm(log(estate[[varY]])~log(estate[[varX]]))
    summary_output <- summary(model)
    resplot <- ggplot(model, aes(.fitted, .resid)) + geom_point() +
      xlab("Fitted Value") + ylab("Residuals") + ggtitle("Residuals vs fitted")
    qq_plot <- ggplot( mapping = aes(qqnorm(rstandard(model))[[1]], resid(model)))+
      geom_point(na.rm = TRUE) + geom_qq_line(aes(x = qqnorm(rstandard(model))[[1]], sample=resid(model))) +
      xlab("theoretical") + ylab("sample") + ggtitle("QQ Plot")
  } else if(logX){
    model <- lm(estate[[varY]]~log(estate[[varX]]))
    summary_output <- summary(model)
    resplot <- ggplot(model, aes(.fitted, .resid)) + geom_point() +
      xlab("Fitted Value") + ylab("Residuals") + ggtitle("Residuals vs fitted")
    qq_plot <- ggplot( mapping = aes(qqnorm(rstandard(model))[[1]], resid(model)))+
      geom_point(na.rm = TRUE) + geom_qq_line(aes(x = qqnorm(rstandard(model))[[1]], sample=resid(model))) +
      xlab("theoretical") + ylab("sample") + ggtitle("QQ Plot")
  }else if(logY){
    model <- lm(log(estate[[varY]])~estate[[varX]])
    summary_output <- summary(model)
    resplot <- ggplot(model, aes(.fitted, .resid)) + geom_point() +
      xlab("Fitted Value") + ylab("Residuals") + ggtitle("Residuals vs fitted")
    qq_plot <- ggplot( mapping = aes(qqnorm(rstandard(model))[[1]], resid(model)))+
      geom_point(na.rm = TRUE) + geom_qq_line(aes(x = qqnorm(rstandard(model))[[1]], sample=resid(model))) +
      xlab("theoretical") + ylab("sample") + ggtitle("QQ Plot")
  } else {
    model <- lm(estate[[varY]]~estate[[varX]])
    summary_output <- summary(model)
    resplot <- ggplot(model, aes(.fitted, .resid)) + geom_point() +
      xlab("Fitted Value") + ylab("Residuals") + ggtitle("Residuals vs fitted")
    qq_plot <- ggplot( mapping = aes(qqnorm(rstandard(model))[[1]], resid(model)))+
      geom_point(na.rm = TRUE) + geom_qq_line(aes(x = qqnorm(rstandard(model))[[1]], sample=resid(model))) +
      xlab("theoretical") + ylab("sample") + ggtitle("QQ Plot")
  }
  # }
  
  
  return_list <- list("summary_output"= summary_output,
                      "resplot" = resplot,
                      "qq_plot" = qq_plot)
  return(return_list)
}

#########################################################################################################
########                                Program : UI                                       ##############      
#########################################################################################################

ui <- fluidPage(
  titlePanel("EDA of Estate Data"),
  tabsetPanel(type = "tabs",
              tabPanel("Univariate", 
                       sidebarLayout(
                         sidebarPanel(
                           fillPage(
                             varSelectInput("t1var1", "Variable?", data = estate, selected = 'Price($K)'),
                             checkboxInput("t1log", "Log_Transform?", value = F),
                             sliderInput("t1bins", "Number of Bins?", value = 40, min = 0, max = 100),
                             numericInput("t1null", "Null Value", value = 0),
                             tableOutput("t1summary1")
                           )),
                         mainPanel(plotOutput("t1plot1"))
                       )
              ),
              tabPanel("Bivariate",
                       sidebarLayout(
                         sidebarPanel(
                           varSelectInput("t2var1", "X Variable?", data = estate, selected = 'Area'),
                           checkboxInput("t2log1", "Log_Transform?", value = F),
                           varSelectInput("t2var2", "Y Variable?", data = estate, selected = 'Price($K)'),
                           checkboxInput("t2log2", "Log_Transform?", value = F),
                           checkboxInput("t2ols", "Fit OLS?", value = F),
                           tableOutput("t2validnum"),
                           tableOutput("t2validx0"),
                           tableOutput("t2validy0")
                         ),
                         mainPanel( plotOutput("t2plot1"))
                       ),
                       fluidRow(column(4, verbatimTextOutput("t2lm")),
                                column(4, plotOutput("t2resplot")),
                                column(4,plotOutput("t2qqplot"))
                       )
              ),
              tabPanel("Spreasheet",
                       dataTableOutput("t3table")))
  
)


#########################################################################################################
########                                Program : Server                                   ##############      
#########################################################################################################

server <- function(input, output) {
  
  # tab1_data <- reactive({
  #                        if(is.numeric(simplify2array(estate %>% select(input$t1var1))) == F){
  #                          estate %>% select(input$t1var1)
  #                        } else if (is.numeric(simplify2array(estate %>% select(input$t1var1))) == T & input$t1log == T){
  #                          log(estate %>% select(input$t1var1))
  #                        } else {
  #                          estate %>% select(input$t1var1)
  #                        }
  # })
  # 
  output$t1summary1 <- renderTable({ttest_func(variable = input$t1var1, log_tran = input$t1log, null_value = input$t1null)})
  
  
  
  output$t1plot1 <- renderPlot({
    base = ggplot(data = estate, aes(x = !!input$t1var1)) 
    
    # if (is.numeric(simplify2array(estate %>% select(input$t1var1))) == F){
    #   base + geom_bar()
    # } else if (is.numeric(simplify2array(estate %>% select(input$t1var1))) == T & input$t1log == T){
    #   base + geom_histogram(bins = input$t1bins) + 
    #     scale_x_continuous(trans='log2') + xlab(paste("Log(",input$t1var1,")", sep = ""))
    # } else {
    #   base + geom_histogram(bins = input$t1bins)
    # }
    
    
    if (var_dict[var_name == input$t1var1 ,2] == 3 ){
      base + geom_bar()
    } else if (var_dict[var_name == input$t1var1 ,2] %in% c(1,2) & input$t1log == T){
      base + geom_histogram(bins = input$t1bins) + 
        scale_x_continuous(
          # breaks = seq(round(min(estate[input$t1var1])), round(max(estate[input$t1var1])),
          #        round((max(estate[input$t1var1] - min(estate[input$t1var1]))/5))), 
          trans='log10') + xlab(paste("Log(",input$t1var1,")", sep = ""))
    } else {
      base + geom_histogram(bins = input$t1bins)
    }
    
  })
  
  
  
  output$t2validnum <- renderTable({
    validate(need(!(is.numeric(simplify2array(estate %>%select(input$t2var1))) == F & 
                      is.numeric(simplify2array(estate %>%select(input$t2var2))) == F),
                  "Both Variable X and Variable Y are not numeric"))
    validate(need(is.numeric(simplify2array(estate %>%select(input$t2var1))) == T, "Variable X is not numeric"))
    validate(need(is.numeric(simplify2array(estate %>%select(input$t2var2))) == T, "Variable Y is not numeric"))
  })
  
  
  output$t2validx0 <- renderTable({
    if(is.numeric(simplify2array(estate %>%select(input$t2var1)))){
      if (input$t2log1){
        data_use <- log(estate %>%select(input$t2var1))
      } else {
        data_use <- estate %>%select(input$t2var1)
      }
      validate(need(any(data_use==-Inf) == FALSE, "Variable X contains one or more values of 0."))
    }
  })
  
  
  output$t2validy0 <- renderTable({
    if(is.numeric(simplify2array(estate %>%select(input$t2var2)))){
      if (input$t2log2){
        data_use <- log(estate %>%select(input$t2var2))
      } else {
        data_use <- estate %>%select(input$t2var2)
      }
      validate(need(any(data_use==-Inf) == FALSE, "Variable Y contains one or more values of 0."))
    }
  })
  
  
  output$t2plot1 <- renderPlot({
    base = ggplot(data = estate, aes(x = !!input$t2var1, y = !!input$t2var2))
    if (var_dict[var_name == input$t2var1 ,2] == 3 & var_dict[var_name == input$t2var2 ,2] == 3){
      base + geom_jitter()
    } else if (var_dict[var_name == input$t2var1 ,2] %in% c(2,1) &  var_dict[var_name == input$t2var2 ,2] == 3){
      if (input$t2log1){
        base +geom_boxploth() + scale_x_continuous(trans = "log10") + xlab(paste("Log(",input$t2var1,")", sep = ""))
      } else {
        base +geom_boxploth()
      }
      
    } else if (var_dict[var_name == input$t2var1 ,2] == 3 &  var_dict[var_name == input$t2var2 ,2] %in% c(2,1)){
      if (input$t2log2){
        base + geom_boxplot() + scale_y_continuous(trans = "log10") + ylab(paste("Log(",input$t2var2,")", sep = ""))
      } else {
        base + geom_boxplot()
      }
    } else if (var_dict[var_name == input$t2var1 ,2] %in% c(2,1) &  var_dict[var_name == input$t2var2 ,2] %in% c(2,1)){
      if (input$t2log1 & input$t2log2){
        base2 <- base + geom_point() + scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") +
          xlab(paste("Log(",input$t2var1,")", sep = "")) + ylab(paste("Log(",input$t2var2,")", sep = ""))
        if (input$t2ols) {
          base2 + geom_smooth(formula = y ~ x, method = "lm",se = FALSE)
        } else {
          base2
        }
      } else if (input$t2log1) {
        base2 <- base + geom_point() + scale_x_continuous(trans = "log10") + xlab(paste("Log(",input$t2var1,")", sep = ""))
        if (input$t2ols) {
          base2 + geom_smooth(formula = y ~ x, method = "lm",se = FALSE)
        } else {
          base2
        }
      } else if (input$t2log2){
        base2 <- base + geom_point() + scale_y_continuous(trans = "log10") + ylab(paste("Log(",input$t2var1,")", sep = ""))
        if (input$t2ols) {
          base2 + geom_smooth(formula = y ~ x, method = "lm",se = FALSE)
        } else {
          base2
        }
      } else {
        base2 <- base + geom_point() 
        if (input$t2ols) {
          base2 + geom_smooth(formula = y ~ x, method = "lm",se = FALSE)
        } else {
          base2
        }
      }
    }
  })
  
  
  output$t3table <- renderDataTable(estate[,map_lgl(estate,is.numeric)], options = list(pageLength = 10))
  
  
  output$t2lm <- renderPrint({
    if (input$t2ols){
      if (var_dict[var_name == input$t2var1 ,2] == 1 & var_dict[var_name == input$t2var2 ,2] == 1 ){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$summary_output
      } else if (var_dict[var_name == input$t2var1 ,2] == 1 & 
                 var_dict[var_name == input$t2var2 ,2] == 2 &
                 input$t2log2 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$summary_output
      } else if (var_dict[var_name == input$t2var1 ,2] == 2 & 
                 var_dict[var_name == input$t2var2 ,2] == 1 &
                 input$t2log1 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$summary_output
      } else if (var_dict[var_name == input$t2var1 ,2] == 2 & 
                 var_dict[var_name == input$t2var2 ,2] == 2 &
                 input$t2log1 == F & input$t2log2 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$summary_output
      } else {
        invisible()
      }
      
    } else {
      invisible()
    }
  })
  
  
  output$t2resplot <- renderPlot({
    if (input$t2ols){
      if (var_dict[var_name == input$t2var1 ,2] == 1 & var_dict[var_name == input$t2var2 ,2] == 1 ){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$resplot
      } else if (var_dict[var_name == input$t2var1 ,2] == 1 & 
                 var_dict[var_name == input$t2var2 ,2] == 2 &
                 input$t2log2 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$resplot
      } else if (var_dict[var_name == input$t2var1 ,2] == 2 & 
                 var_dict[var_name == input$t2var2 ,2] == 1 &
                 input$t2log1 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$resplot
      } else if (var_dict[var_name == input$t2var1 ,2] == 2 & 
                 var_dict[var_name == input$t2var2 ,2] == 2 &
                 input$t2log1 == F & input$t2log2 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$resplot
      } else {
        invisible()
      }
      
    } else {
      invisible()
    }
  })
  
  
  output$t2qqplot <- renderPlot({
    if (input$t2ols){
      if (var_dict[var_name == input$t2var1 ,2] == 1 & var_dict[var_name == input$t2var2 ,2] == 1 ){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$qq_plot
      } else if (var_dict[var_name == input$t2var1 ,2] == 1 & 
                 var_dict[var_name == input$t2var2 ,2] == 2 &
                 input$t2log2 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$qq_plot
      } else if (var_dict[var_name == input$t2var1 ,2] == 2 & 
                 var_dict[var_name == input$t2var2 ,2] == 1 &
                 input$t2log1 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$qq_plot
      } else if (var_dict[var_name == input$t2var1 ,2] == 2 & 
                 var_dict[var_name == input$t2var2 ,2] == 2 &
                 input$t2log1 == F & input$t2log2 == F){
        regfunc(varX = input$t2var1, varY = input$t2var2,logX = input$t2log1, logY = input$t2log2)$qq_plot
      } else {
        invisible()
      }
      
    } else {
      invisible()
    }
  })
  
}
#########################################################################################################
########                                Program : Run Code                                 ##############      
#########################################################################################################

shinyApp(ui, server)