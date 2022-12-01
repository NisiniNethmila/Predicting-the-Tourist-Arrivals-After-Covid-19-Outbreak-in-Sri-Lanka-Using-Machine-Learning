#load the required libraries
library(shiny)
library(shinythemes)
library(RMySQL)
library(DBI)
library(plotly)
library(shinyalert)
library(forecast)
library(caret)
library(tseries)
library(highcharter)
library(lubridate)
library(zoo)
library(DT)
library(tidyverse)
library(shinyauthr)
library(shinycssloaders)

# #to make the connection to the database
# sltdfs_db <- dbConnect(MySQL(), dbname='sltdfs',
#                        user='root', password='',
#                        host='localhost',
#                        port=3306)

#write data into database
# write_query_fun <- function(name, df){
#   sltdfs_db <- dbConnect(MySQL(), dbname='sltdfs',
#                          user='root', password='',
#                          host='localhost',
#                          port=3306)
#   
#   dbWriteTable(conn = sltdfs_db, name = name, value = df, overwrite = FALSE, append = TRUE,
#                row.names = FALSE)
#   
#   
# }

# #to make a reference to users
# sltdfs_database <- tbl(sltdfs_db, "users")
# 
# user_credentials <- sltdfs_database %>%
#   select(user, password) %>%
#   collect()
# 
# user_credentials_df <- data.frame(user_credentials)

# user database for logins
user_base <- tibble::tibble(
  user = c("Sandamini","Sumith","user1","user2"),
  password = sapply(c("sandamini123", "sumith123","pass1","pass2"), sodium::password_store),)

# Options for Spinner
options(spinner.color="green", spinner.color.background="#FFFFFF", spinner.size=1)

#load the data sets
arrivals <- read.csv("tourist_arrivals.csv")
arrivals_details <- read.csv("tourist_arrivals_details.csv")
cou_arrivals <- read.csv("country_by_arrivals.csv")
cont_arrivals <- read.csv("continent_by_arrivals.csv")
y2022_arrivals <- read.csv("y2022_arrivals.csv")
arima_forecast <- read.csv("arima_forecasting.csv")
nnar_forecast <- read.csv("nnar_forecasting.csv")

#-------------------------home page plots------------------------------------------

month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')
y_2020 <- arrivals$Tourist_Arrivals[517:528]
y_2021 <- arrivals$Tourist_Arrivals[529:540]
y_2022 <- y2022_arrivals$Tourist_Arrivals

data <- data.frame(month, y_2020, y_2021, y_2022)

#To alphabetized the order
data$month <- factor(data$month, levels = data[["month"]])

fig <- plot_ly(data, x = ~month, y = ~y_2020, name = '2020', type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(205, 12, 24)', width = 4)) 
fig <- fig %>% add_trace(y = ~y_2021, name = '2021', line = list(color = 'rgb(22, 96, 167)', width = 4))
fig <- fig %>% add_trace(y = ~y_2022, name = '2022', line = list(color = 'rgb(0,128,0)', width = 4))
fig <- fig %>% layout(xaxis = list(title = "Months"),
                      yaxis = list (title = "Number of Tourist Arrivals"))


continent <- c('Americas', 'Africa', 'Asia & Pacific', 'Europe', 'Middle East')
yr_2020 <- cont_arrivals$Tourist_Arrivals[1:5]
yr_2021 <- cont_arrivals$Tourist_Arrivals[6:10]

data_2 <- data.frame(continent, yr_2020, yr_2021)

#The default order will be alphabetized unless specified as below:
data_2$continent <- factor(data_2$continent, levels = data_2[["continent"]])

fig_2 <- plot_ly(data_2, x = ~continent, y = ~yr_2020, name = '2020', type = 'bar')
fig_2 <- fig_2 %>% add_trace(y = ~yr_2021, name = '2021') 
fig_2 <- fig_2 %>% layout(xaxis = list(title = "Region"),
                          yaxis = list (title = "Number of Tourist Arrivals"))


#-----------------pre-processing task-------------------------------

#check NA values in arrivals
any(is.na(arrivals))


#-------------splitting arrivals data frame into training and testing--------------

arrivals_train <- arrivals[1:516, ]
arrivals_test <- arrivals[517:540, ]

#convert to time series format
arrivals_timeseries <- ts(arrivals_train$Tourist_Arrivals, start = c(1977,1),
                          end = c(2019,12), frequency = 12)


#-----------------------------ARIMA model------------------------------------------

arima_model <- auto.arima(arrivals_timeseries)
summary(arima_model)

#testing the predicted data
test_forecast_arima <- forecast(arima_model, h=2*12)

#predictions up to 2030
forecast_arima <- forecast(arima_model, h=9*12)



#---------------------------Neural Network AR model--------------------------------

set.seed(124)

nnar_model = nnetar(arrivals_timeseries)
summary(nnar_model)

#testing the predicted data
test_forecast_nnar = forecast(nnar_model, h =2*12)

#predictions up to 2030
forecast_nnar <- forecast(nnar_model, h=9*12)



#-----------------------number categorization for sidebar-------------------------

Number <- c("25", "37", "49", "61", "73", "85", "97", "109", "121")
Time_Range <- c("January 2022","January 2023","January 2024","January 2025",
                "January 2026","January 2027","January 2028","January 2029","January 2030")
month_df <- data.frame(Number, Time_Range)



#----------------additional tabs to be added after login--------------------------

#--------------------------------home tab-----------------------------------------

home_tab <- tabPanel(
  title = "Home",
  value = "home",
  sidebarLayout(
    sidebarPanel(
      img(src = "SLTDFS.png", width = "100px", height = "100px"),
      "SL Tourism Demand Forecasting System",
      tags$hr(),
      tags$p("Welcome to the Sri Lanka Tourism Demand Forecasting System, a part of the research project of Predicting the tourist arrivals After Covid-19 Outbreak 
                                    in Sri Lanka Using Machine Learning."),
      
      actionButton("show", "More details",class="btn btn-primary"),
      tags$hr(),
      tags$p("Get the recent updates"),
      actionButton("clicks", "Notifications", class="btn btn-primary"),
      tags$hr(),
      tags$p("Please send your comments and suggestions to the following email"),
      tags$a("nisini.2018416@iit.ac.lk",
             href = "mailto:nisini.2018416@iit.ac.lk"),
      tags$hr(),
    ),
    
    mainPanel(
      tags$h3("Welcome"),
      tags$br(),
      
      tabsetPanel(
        tabPanel("Tourist Arrivals in Sri Lanka", br(), fig),
        tabPanel("Tourist Arrivals in Sri Lanka by Region", br(), fig_2),
      ),
    ),
  ),
)

#-------------------------tourism_forecast_tab--------------------------------------

tourism_forecast_tab <- tabPanel(
  title = "Tourism Forecast",
  value = "tourism_forecast",
  tabsetPanel(
    tabPanel("Tourism Forecasting with ARIMA",
             sidebarLayout(
               sidebarPanel(
                 p("You can get the predicted number of tourist arrivals according to the selected number of months ahead."),
                 p ("You can select the required months using the following sidebar (according to the given details)."),
                 hr(),
                 
                 sliderInput(
                   inputId = "forecast_n_months",
                   label = strong("Months to Forecast Ahead"),
                   min = 25,
                   max = 132,
                   value = 30),
                 
                 tableOutput("month_view_1"),
                 
               ),
               
               mainPanel(
                 h3("Tourism Forecasting with ARIMA"),
                 tabsetPanel(
                   tabPanel("Forecasting Plot",
                            br(),
                            p("Please note that the calculated forecasts appear in red."),
                            withSpinner(highchartOutput("forecastPlot"), type = 4),
                            textOutput("summary_arima"),
                   ),
                   tabPanel("Forecasting Points",
                            br(),
                            p("Forecasting Points that are calculated from the model."),
                            withSpinner(tableOutput("view_arima_model"), type = 4),
                   ),
                   tabPanel("Forecasting Report by Year",
                            br(),
                            selectInput("select_year_with_updateSelectInput_arima",
                                        label = "Choose a year:",
                                        choices = NULL),
                            p("Download Forecasting Reports"),
                            withSpinner(DTOutput("report_arima"), type = 4),
                   ),
                   tabPanel("Forecasting Report",
                            br(),
                            p("Download Forecasting Reports"),
                            withSpinner(DTOutput("report_arima_full"), type = 4),
                   ),
                 ),
               ),
             ),
             
    ),
    
    tabPanel("Tourism Forecasting with Neural Networks Autoregression",
             sidebarLayout(
               sidebarPanel(
                 p("You can get the predicted number of tourist arrivals according to the selected number of months ahead."), 
                 p ("You can select the required months using the following sidebar (according to the given details)."),
                 hr(),
                 sliderInput(
                   inputId = "forecast_n_months_2",
                   label = strong("Months to Forecast Ahead"),
                   min = 25,
                   max = 132,
                   value = 40),
                 
                 tableOutput("month_view_2"),
               ),
               
               mainPanel(
                 h3("Tourism Forecasting with Neural Networks Autoregression"),
                 tabsetPanel(
                   tabPanel("Forecasting Plot",
                            br(),
                            p("Please note that the calculated forecasts appear in red."),
                            withSpinner(highchartOutput("tn_forecastPlot"), type = 4),
                            textOutput("summary_nnar"),
                   ),
                   tabPanel("Forecasting Points",
                            br(),
                            p("Forecasting Points that are calculated from the model."),
                            withSpinner(tableOutput("view_tn_model"), type = 4),
                   ),
                   tabPanel("Forecasting Report by Year",
                            br(),
                            selectInput("select_year_with_updateSelectInput_nnar",
                                        label = "Choose a year:",
                                        choices = NULL),
                            p("Download Forecasting Reports"),
                            withSpinner(DTOutput("report_nnar"), type = 4),
                   ),
                   tabPanel("Forecasting Report",
                            br(),
                            p("Download Forecasting Reports"),
                            withSpinner(DTOutput("report_nnar_full"), type = 4),
                   ),
                 ),
               ),
             ),
    ),
  ),
)

#--------------------------------dashboard_tab-------------------------------------

dashboard_tab <- tabPanel(
  title = "Dashboard",
  value = "dashboard",
  tabsetPanel(
    tabPanel("Past Tourist Arrivals",
             br(),
             tags$p("Please note that this past tourist arrivals dashboard has 7 pages."),
             fluidRow(
               column(1, offset = 1,
                      
                      tags$iframe(
                        title="Past Tourist Arrivals Dashboard - 2017 - 2021", 
                        width="1024", height="768", 
                        src="https://app.powerbi.com/view?r=eyJrIjoiNTA4OGUzNWQtYjdmYS00N2VkLTllZWItMTM0NmVjNWM3NTA1IiwidCI6ImFhYzBjNTY0LTZjNWUtNGIwNS04ZGMzLTQwODA4N2Y3N2Y3NiIsImMiOjEwfQ%3D%3D", 
                        frameborder="0", allowFullScreen="true",),
               ),
             ),
    ),
    tabPanel("Predicted Tourist Arrivals",
             br(),
             tags$p("Please note that this predicted tourist arrivals dashboard has 3 pages."),
             fluidRow(
               column(1, offset = 1,
                      tags$iframe(
                        title="Predicted details dashboard - Predicted Tourist Arrivals Forecasts",
                        width="1024", height="768", 
                        src="https://app.powerbi.com/view?r=eyJrIjoiMjljM2Q3NjQtNjUxNC00NTEyLWJiMWYtMzk3ZTA3NGMzODMzIiwidCI6ImFhYzBjNTY0LTZjNWUtNGIwNS04ZGMzLTQwODA4N2Y3N2Y3NiIsImMiOjEwfQ%3D%3D&pageName=ReportSection", 
                        frameborder="0", allowFullScreen="true",),
               ),
             ),
             
    ),
  ),
)

#----------------------------past_reports_tab---------------------------------------

past_reports_tab <- tabPanel(
  title = "Past Tourist Arrivals Reports",
  value = "past_report",
  tabsetPanel(
    tabPanel("Tourist Arrivals by Year and Month",
             sidebarLayout(
               sidebarPanel(
                 selectInput("select_year_with_updateSelectInput_1",
                             label = "Choose a year:",
                             choices = NULL),
                 
                 downloadButton("download_data_1", "Download Full Data", class="btn btn-primary"),
               ),
               mainPanel(
                 h3("Tourist Arrivals by Year and Month"),
                 p("Download Past Reports"),
                 withSpinner(DTOutput("report_1"), type = 4),
               ),
             ),
    ),
    tabPanel("Tourist Arrivals by Continent, Region and Country",
             sidebarLayout(
               sidebarPanel(
                 selectInput("select_year_with_updateSelectInput_2",
                             label = "Choose a year:",
                             choices = NULL),
                 selectInput("selected_continent",
                             label = "Choose a continent:",
                             choices = NULL),
                 selectInput("selected_region",
                             label = "Choose a region:",
                             choices = NULL),
                 selectInput("selected_country",
                             label = "Choose a country:",
                             choices = NULL),
                 downloadButton("download_data_2", "Download Full Data", class="btn btn-primary"),
               ),
               mainPanel(
                 h3("Tourist Arrivals by Continent, Region and Country"),
                 p("Download Past Reports"),
                 withSpinner(DTOutput("report_2"), type = 4),
               ),
             ),
    ),
  ),
)

#----------------------------guide_tab---------------------------------------

guide_tab <- tabPanel(
  title = "How to Use",
  value = "guide",
  tags$h3("How to get the predictions?"),
  tags$h4("Under the",
          tags$strong("'Tourism Forecast'"), "section, you will be able to get the predicted number of tourist arrivals by using two different 
    statistical models."),
  br(),
  tags$h4(tags$strong("Tourism Forecasting with ARIMA (Autoregressive Integrated Moving Average)"),
          hr(),
          tags$ol(tags$strong("Forecasting Plot"), "- Here, you will be able to get the tourist arrivals 
            predictions that are calculated from the ARIMA model. You can select the required number of months 
            to forecast ahead using the given sidebar. It starts from 25 because the ARIMA model has been built 
            using the training data up to December 2019 and the data from January 2020 to December 2021 has 
            been considered as testing data. Therefore, 24 months had to be reserved for predictions that 
            were calculated for the testing data. You can refer to the given number and the time range to 
            get an idea of the number that you should select using the sidebar in order to get the 
            predictions for your required month or year (ex: 25 refers to January 2022, 26 refers to 
            February 2022 and so on). If you want to get the predictions for September 2022, then you can 
            simply select 33 (24+9) on the side bar. The ARIMA plot will then automatically generate and 
            provide predictions up to September 2022. After generating the ARIMA plot, you can see the 
            predictions for each month (based on your selection) by moving your cursor over the plot. 
            The generated predictions will appear in red colour. The details that are displayed below 
            the plot are the summary of the ARIMA model."),
          br(),
          tags$ol(tags$strong("Forecasting Points"), "- Here, you will be able to view all the predictions 
              that have been calculated from the ARIMA model up to the year 2030. Here, the first row 
              displays the calculated forecasts for January 2022. Likewise, each row displays the 
              calculated forecast up to the year 2030. These predictions can be viewed separately 
              for each year under the 'Forecasting Report by Year' tab."),
          br(),
          tags$ol(tags$strong("Forecasting Report by Year"), "- Here, you can view the ARIMA forecast reports 
              for each month of the selected year by selecting the required year that you want to get 
              the predictions for. You can choose the year using the given dropdown option. By clicking 
              on the expand icon (green plus icon), you can see the predictions for the hidden months. 
              You can use the 'Excel' option and the 'PDF' option to download the forecast reports 
              in both Excel and PDF format."),
          br(),
          tags$ol(tags$strong("Forecasting Report"), "- Here, you can view all the ARIMA forecast reports 
              that are calculated for up to the year 2030. You can use the 'Excel' option and the 'PDF' 
              option to download all the forecasts in both Excel and PDF format."),
  ),
  
  br(),
  br(),
  
  tags$h4(tags$strong("Tourism Forecasting with NNAR (Neural Network Autoregression)"),
          hr(),
          tags$ol(tags$strong("Forecasting Plot"), "- Here, you will be able to get the 
                    tourist arrivals predictions that are calculated from the NNAR model. 
                    You can select the required number of months to forecast ahead using 
                    the given sidebar. The NNAR plot will then automatically generate and 
                    provide predictions up to the selected number of months forecasted 
                    ahead. After generating the NNAR plot, you can see the predictions 
                    for each month (based on your selection) by moving your cursor 
                    over the plot. The generated predictions will appear in red colour. 
                    The details that are displayed below the plot are the summary of 
                    the NNAR model."),
          br(),
          tags$ol(tags$strong("Forecasting Points"), "- Here, you will be able to view all the predictions 
                    that have been calculated from the NNAR model up to the year 2030. Each row displays the 
                    monthly calculated forecast for each year up to 2030 (ex: the first row displays the 
                    predictions for 2022 and so on). These predictions can be viewed separately for each year 
                    under the Forecasting Report by Year tab."),
          br(),
          tags$ol(tags$strong("Forecasting Report by Year"), "- Here, you can view the NNAR forecast reports 
              for each month of the selected year by selecting the required year that you want to get 
              the predictions for. You can choose the year using the given dropdown option. By clicking 
              on the expand icon (green plus icon), you can see the predictions for the hidden months. 
              You can use the 'Excel' option and the 'PDF' option to download the forecast reports 
              in both Excel and PDF format."),
          br(),
          tags$ol(tags$strong("Forecasting Report"), "- Here, you can view all the NNAR forecast reports 
              that are calculated for up to the year 2030. You can use the 'Excel' option and the 'PDF' 
              option to download all the forecasts in both Excel and PDF format."),
          br(),
          br(),
  ),
  
)




#-----------------------------------server----------------------------------------

server <- function(input, output, session) {
  # hack to add the logout button to the navbar on app launch 
  insertUI(
    selector = ".navbar .container-fluid .navbar-collapse",
    ui = tags$ul(
      class="nav navbar-nav navbar-right",
      tags$li(
        div(
          style = "padding: 10px; padding-top: 5px; padding-bottom: 5px;",
          shinyauthr::logoutUI("logout")
        )
      )
    )
  )
  
  observeEvent(input$registration_button, {
    if (input$first_name == ""){
      shinyalert("You need to enter your first name!", type = "error")
    } else if (input$last_name == ""){
      shinyalert("You need to enter your last name!", type = "error")
    } else if (input$email == ""){
      shinyalert("You need to enter your email!", type = "error")
    } else if (input$contact_number == ""){
      shinyalert("You need to enter your contact number!", type = "error")
    } else if (input$user == ""){
      shinyalert("You need to enter your username!", type = "error")
    } else if (input$password == ""){
      shinyalert("You need to enter a password!", type = "error")
    } else if (input$con_password == "" | input$con_password != input$password){
      shinyalert("You need to enter the same password!", type = "error")
    }else {
      df <- data.frame(
        "first_name" = input$first_name,
        "last_name" = input$last_name,
        "email" = input$email,
        "contact_number" = input$contact_number,
        "user"= input$user,
        "password" = input$password,
        "created_at" = Sys.time()
      )
      
      write_query_fun("users", df)
      
      shinyalert("You have registered successfully!", type = "success")
      session$reload()
    }
    
  })
  
  # call the shinyauthr login and logout server modules
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = "user",
    pwd_col = "password",
    sodium_hashed = TRUE,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  observeEvent(credentials()$user_auth, {
    
    # if user logs in successfully
    if (credentials()$user_auth) {
      
      # remove the login tab
      removeTab("tabs", "login")
      
      # remove the register tab
      removeTab("tabs", "register")
      
      # add home tab 
      appendTab("tabs", home_tab, select = TRUE)
      
      # more details
      observeEvent(input$show, {
        showModal(modalDialog(
          title = "Predicting the Tourist Arrivals After Covid-19 Outbreak 
      in Sri Lanka Using Machine Learning",
          
          tags$a("Click here to watch the Pitch Video",
                 href = "https://youtu.be/D53RPHAycHQ"),
          tags$br(),
          tags$a("Click here to watch the Demo Video",
                 href = "https://youtu.be/TCOfhV1Cmfo"),
          tags$hr(),
          tags$p("Predicting tourist arrivals has been done previously by many researchers. However, tourist arrivals 
          have never been predicted after the covid 19 outbreak. The aim of this research project is to analyze 
          the effectiveness of predicting tourist arrivals after the covid 19 outbreak in Sri Lanka and design, 
          develop and evaluate a web-based tourism demand forecasting system that that can predict the 
          number of tourist arrivals accurately after the covid 19 outbreak. Therefore, in this research study, 
          we propose a web-based tourism demand forecasting system that can predict the number of tourist 
          arrivals for the upcoming months accurately. It integrates with two Power BI dashboards in order 
          to gain deeper business insight and improve decision-making process efficiency. This system will 
          allow users to perform forecasting using different statistical models, considering the time series as 
          the baseline model, and trying to improve the forecasting accuracy using machine learning 
          techniques. This research project was chosen using the agile software development approach to be 
          more flexible to make changes at any moment. The mix approach was chosen as the appropriate 
          primary data collection methodology for this project because data was gathered through interviews 
          as well as questionnaires. The past tourist arrivals data from January 1977 to December 2021 was 
          collected in order to build the models. The predictions were calculated from the 
          ARIMA(3,1,1)(0,1,2)[12] model and NNAR(14,1,8)[12]. The error measurements of ME, RMSE, 
          MAE, MPE, MAPE, MASE, and ACF1 are used to evaluate the models. This research study found 
          that the NNAR(14,1,8)[12] model performs better than the ARIMA(3,1,1)(0,1,2)[12] model. 
          Therefore, the NNAR(14,1,8)[12] model is the best model for forecasting tourist arrivals after the 
          covid 19 outbreak because it has comparably low error measurement values.
          "),
          tags$p("This research project is conducted in partial fulfillment of the requirements for the
        BSc (Hons) in Business Information Systems degree
        at the University of Westminster."),
          tags$p("Student: Nisini Silva (2018416)",
                 tags$br(),"Supervisor: Dr. Sachintha Pitigala"),
          tags$br(),
          tags$a("Click here to read more about the Nisini",
                 href = "https://sites.google.com/my.westminster.ac.uk/nisini-silva-data-scientist/home"),
          easyClose = TRUE))
      })
      
      #notification
      observeEvent(input$clicks, {
        showNotification("The predictions of tourist arrivals for the upcoming months have been updated.", 
                         type = "message")
      })
      
      # add tourism_forecast_tab
      appendTab("tabs", tourism_forecast_tab)
      
      output$forecastPlot <- renderHighchart({
        forecast_arima_plot <- forecast(arima_model,
                                        input$forecast_n_months) %>%
          hchart %>%
          hc_add_series(ts(arrivals$Tourist_Arrivals, start = c(1977,1),
                           end = c(2021,12), frequency = 12), color = "green", 
                        name = "Actual Tourist Arrivals Data") %>%
          hc_add_series(ts(arrivals_train$Tourist_Arrivals, start = c(1977,1),
                           end = c(2019,12), frequency = 12), color = "blue", 
                        name = "Training Data") %>%
          hc_add_series(ts(arrivals_test$Tourist_Arrivals, start = c(2020,1),
                           end = c(2021,12), frequency = 12), color = "black", 
                        name = "Testing Data") %>%
          hc_add_series(data = test_forecast_arima, color = "orange", 
                        name = "Predicted Testing Data") %>%
          hc_add_theme(hc_theme_google()) %>%
          hc_title(text = "Tourist Arrivals Forecasting using ARIMA")
      })
      
      output$view_arima_model <- renderTable({
        summary(forecast_arima)
      })
      
      
      output$summary_arima <- renderPrint({ 
        summary(arima_model)
      })
      
      
      output$tn_forecastPlot <- renderHighchart2({
        forecast_nnar_plot <- forecast(nnar_model,
                                       input$forecast_n_months_2) %>% 
          hchart %>%
          hc_add_series(ts(arrivals$Tourist_Arrivals, start = c(1977,1),
                           end = c(2021,12), frequency = 12), color = "green", 
                        name = "Actual Tourist Arrivals Data") %>%
          hc_add_series(ts(arrivals_train$Tourist_Arrivals, start = c(1977,1),
                           end = c(2019,12), frequency = 12), color = "blue", 
                        name = "Training Data") %>%
          hc_add_series(ts(arrivals_test$Tourist_Arrivals, start = c(2020,1),
                           end = c(2021,12), frequency = 12), color = "black", 
                        name = "Testing Data") %>%
          hc_add_series(data = test_forecast_arima, color = "orange", 
                        name = "Predicted Testing Data") %>%
          hc_add_theme(hc_theme_google()) %>%
          
          hc_title(text = "Tourist Arrivals Forecasting using Neural Networks Autoregression")
      })
      
      output$view_tn_model <- renderTable({
        summary(forecast_nnar)
      })
      
      output$summary_nnar <- renderPrint({ 
        summary(nnar_model)
      })
      
      output$month_view_1 <- renderTable(month_df)
      output$month_view_2 <- renderTable(month_df)
      
      updateSelectInput(session,
                        "select_year_with_updateSelectInput_arima",
                        choices = unique(arima_forecast$Year))
      
      output$report_arima <- renderDT({
        
        arima_forecast %>%
          filter(Year == input$select_year_with_updateSelectInput_arima) %>%
          datatable(rownames = FALSE,
                    extensions = c('Responsive', 'Buttons'), options = list(
                      orientation ='landscape',
                      dom = 'Bfrtip',
                      buttons = 
                        list ('excel', list(
                          extend = 'pdf',
                          pageSize = 'A4',
                          orientation = 'landscape',
                          filename = 'ARIMA_Forecasting_Report_by_Year'
                        )
                        )
                    )
          )
        
      })
      
      output$report_arima_full <- renderDT({
        
        arima_forecast %>%
          datatable(rownames = FALSE,
                    extensions = c('Responsive', 'Buttons'), options = list(
                      orientation ='landscape',
                      dom = 'Bfrtip',
                      buttons = 
                        list ('excel', list(
                          extend = 'pdf',
                          pageSize = 'A4',
                          orientation = 'landscape',
                          filename = 'ARIMA_Forecasting_Report'
                        )
                        )
                    )
          )
        
      })
      
      updateSelectInput(session,
                        "select_year_with_updateSelectInput_nnar",
                        choices = unique(nnar_forecast$Year))
      
      output$report_nnar <- renderDT({
        
        nnar_forecast %>%
          filter(Year == input$select_year_with_updateSelectInput_nnar) %>%
          datatable(rownames = FALSE,
                    extensions = c('Responsive', 'Buttons'), options = list(
                      orientation ='landscape',
                      dom = 'Bfrtip',
                      buttons = 
                        list ('excel', list(
                          extend = 'pdf',
                          pageSize = 'A4',
                          orientation = 'landscape',
                          filename = 'NNAR_Forecasting_Report_by_Year'
                        )
                        )
                    )
          )
        
      })
      
      output$report_nnar_full <- renderDT({
        
        nnar_forecast %>%
          datatable(rownames = FALSE,
                    extensions = c('Responsive', 'Buttons'), options = list(
                      orientation ='landscape',
                      dom = 'Bfrtip',
                      buttons = 
                        list ('excel', list(
                          extend = 'pdf',
                          pageSize = 'A4',
                          orientation = 'landscape',
                          filename = 'NNAR_Forecasting_Report'
                        )
                        )
                    )
          )
        
      })
      
      # add dashboard_tab
      appendTab("tabs", dashboard_tab)
      
      # add Past Reports tab 
      appendTab("tabs", past_reports_tab)
      
      updateSelectInput(session,
                        "select_year_with_updateSelectInput_1",
                        choices = unique(arrivals_details$Year))
      
      updateSelectInput(session,
                        "select_year_with_updateSelectInput_2",
                        choices = unique(cou_arrivals$Year))
      
      updateSelectInput(session,
                        "selected_region",
                        choices = unique(cou_arrivals$Region))
      
      observeEvent(c(input$selected_region),
                   {
                     countries_in_regions <- cou_arrivals %>%
                       filter(Region == input$selected_region) %>%
                       pull(Country)
                     
                     updateSelectInput(session,
                                       "selected_country",
                                       choices = countries_in_regions)
                   })
      
      updateSelectInput(session,
                        "selected_continent",
                        choices = unique(cou_arrivals$Continent))
      
      observeEvent(c(input$selected_continent),
                   {
                     regions_in_continents <- cou_arrivals %>%
                       filter(Continent == input$selected_continent) %>%
                       pull(Region)
                     
                     updateSelectInput(session,
                                       "selected_region",
                                       choices = regions_in_continents)
                     
                   })
      
      output$download_data_1 <- downloadHandler(
        filename = "tourist-arrivals-by-year-and-month.csv",
        content = function(file){
          file.copy("tourist_arrivals_details.csv",
                    file)
        }
      )
      
      output$download_data_2 <- downloadHandler(
        filename = "tourist-arrivals-by-continent-and-region.csv",
        content = function(file){
          file.copy("country_by_arrivals.csv",
                    file)
        }
      )
      
      output$report_1 <- renderDT({
        
        arrivals_details %>%
          arrange(desc(Year)) %>%
          filter(Year == input$select_year_with_updateSelectInput_1) %>%
          datatable(rownames = FALSE,
                    extensions = c('Responsive', 'Buttons'), options = list(
                      pageLength = 12,
                      orientation ='landscape',
                      dom = 'Bfrtip',
                      buttons = 
                        list('excel', list(
                          extend = 'pdf',
                          pageSize = 'A4',
                          orientation = 'landscape',
                          filename = 'Tourist_Arrivals_by_Year_and_Month'
                        ))
                    )
          )
        
      })
      
      output$report_2 <- renderDT({
        
        cou_arrivals %>%
          select(Year, Month, Continent, Region, Country, Tourist_Arrivals) %>%
          arrange(desc(Year)) %>%
          filter(Year == input$select_year_with_updateSelectInput_2) %>%
          filter(Region == input$selected_region) %>%
          filter(Country == input$selected_country) %>%
          group_by(Year, Month, Continent, Region) %>%
          datatable(
            rownames = FALSE,
            extensions = c('Responsive', 'Buttons'), options = list(
              pageLength = 12,
              orientation ='landscape',
              dom = 'Bfrtip',
              buttons = 
                list('excel', list(
                  extend = 'pdf',
                  pageSize = 'A4',
                  orientation = 'landscape',
                  filename = 'Tourist_Arrivals_by_Continent_Region_and_Country'
                ))
            )
          )
        
      })
      
      # add guide_tab
      appendTab("tabs", guide_tab)
      
    }
  })
  
  
  observeEvent(input$registration_button, {
    if (input$first_name == ""){
      shinyalert("You need to enter your first name!", type = "error")
    } else if (input$last_name == ""){
      shinyalert("You need to enter your last name!", type = "error")
    } else if (input$email == ""){
      shinyalert("You need to enter your email!", type = "error")
    } else if (input$contact_number == ""){
      shinyalert("You need to enter your contact number!", type = "error")
    } else if (input$user == ""){
      shinyalert("You need to enter your username!", type = "error")
    } else if (input$password == ""){
      shinyalert("You need to enter a password!", type = "error")
    } else if (input$con_password == "" | input$con_password != input$password){
      shinyalert("You need to enter the same password!", type = "error")
    }else {
      df <- data.frame(
        "first_name" = input$first_name,
        "last_name" = input$last_name,
        "email" = input$email,
        "contact_number" = input$contact_number,
        "user"= input$user,
        "password" = input$password,
        "created_at" = Sys.time()
      )
      
      shinyalert("You have registered successfully!", type = "success")
      session$reload()
    }
    
  })
}
