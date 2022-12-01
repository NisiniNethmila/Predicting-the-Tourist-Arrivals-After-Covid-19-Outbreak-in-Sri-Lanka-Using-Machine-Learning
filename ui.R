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

# login tab ui to be rendered on launch
login_tab <- tabPanel(
  title = icon("lock"), 
  value = "login", 
  shinyauthr::loginUI("login"),
  fluidRow(
    column(12, offset = 5,
           p("Credentials for Demo Account"),
           br(),
           p("Username: user1"),
           p("Password: pass1"),
    ),
  ),
)

register_tab <- tabPanel(
  title = icon("user"),
  value = "register", 
  shinyauthr::loginUI("login"),
  fluidRow(
    column(width = 4, offset = 4,
           h2("Please Register"),
           textInput("first_name", "First Name:", placeholder = "Enter your first name"),
           textInput("last_name", "Last Name:", placeholder = "Enter your last name"),
           textInput("email", "Email:", placeholder = "Enter your email"),
           textInput("contact_number", "Contact Number", placeholder = "Enter your contact number"),
           textInput("user", "User Name", placeholder = "Enter your user name"),
           passwordInput("password", "Password", placeholder = "Enter a password"),
           passwordInput("con_password", "Confirm Password", placeholder = "Enter the same password"),
    ),
    column(width = 3, offset = 5,
           actionButton("registration_button","Register", class="btn btn-primary"),
    ),
  ),
)

# initial app UI with only login tab
ui <- navbarPage(
  theme = shinytheme("flatly"),
  
  # add this
  tags$head(tags$style(HTML('.navbar-static-top {background-color: green;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: green;}'))),
  
  title = "Sri Lanka Tourism Demand Forecasting System",
  id = "tabs", # must give id here to add/remove tabs in server
  collapsible = TRUE,
  login_tab,
  register_tab
)



