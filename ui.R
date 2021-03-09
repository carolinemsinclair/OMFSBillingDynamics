library(shiny)
library(rsconnect)
library(lubridate)
library(dbplyr)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

ui=fluidPage(
  titlePanel("Billing Dynamics: OMFS Report Generator"),
  fluidRow(
    h2("How do I make the file to upload?"),
    p("1. Select Patient the way you normally would"),
    p("2. View Bill"),
    p("3. Export Bill to csv"),
    p("4. Keep all default values and hit ok"),
    p("5. Save file to desktop in billing"),
    p("6. Copy over to where you can easily access the bill (I like putting it on my main desktop)"),
    h2("Upload Bill"),
    fileInput(inputId = "bill",label="Choose File"),
    #choose bill
    
    downloadButton("downloadbill", "Download Bill")),
    h5("Beta 05.03")
  )