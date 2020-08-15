
library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("DT")

#install libraries for database update
library("RMySQL")
library("DBI")
library("dplyr")

#1. SHINY UI FUNCTION ----

shinyUI <- fluidPage(
  
  setBackgroundColor(
    color = "ghostwhite"
  ),
  
  
  
  sidebarLayout(
    sidebarPanel(
      
      #1a select pub & login ----
      tags$h2("Login"),
      uiOutput(outputId = "SelectedPub"),
      textInput(inputId = "PsWd", label = "Password"),
      
      actionButton(inputId = "login", label = "login"),
      div(style="margin-bottom:10px"),
      
      textOutput(outputId = "passCheck"),
      
      #1b display summary of orders available to drill into ----
      tags$h2("Open Orders List"),
      dataTableOutput("summary")
      
      #data file
      #fileInput(inputId = "file", label = "Upload Pricelist (csv file)"),
      #helpText("max file size is 60MB")
      ),
    mainPanel(
    #tabsetPanel(id = "inTabset",
     #           tabPanel(title = "Order_Detail", value = "panel1",
                         titlePanel("Selected Order Detail"),
                         
      #1c Display Drilldown Data - i.e. details of selected order ----
      textOutput("Detail of Order Selected"),
      dataTableOutput("drilldown"), 
      
      #1d insert button to close the order ----
      actionButton(inputId = 'CollectComp', label = 'Close Order'),
      div(style="margin-bottom:10px"),
      
      #1e insert button to refresh order list ----
      helpText("The list of orders to the left will refresh each time an existing order is closed.  To refresh the list of orders without closing an order, click below"),
      actionButton(inputId = 'refreshOrders', label = 'Refresh Order List')
    
      )
    )
)

shinyServer <- function(input, output, session) {

  #2. Log in and set the pub name ----
  
  values <- reactiveValues()
  values$openUp <- 0
  #set working drive to be working drive for app
  setwd("/home/rstudio/ShinyApps/Order")
  
  pubList <- as.list(read.csv(file = "test_centre_list.csv", header = T))
  
  output$SelectedPub <- renderUI({selectInput(inputId = 'TestCentre',
                                                     label = 'Pub',
                                                     choices = pubList)})
  
  
  observeEvent(input$login, {

    if(input$login > 3) {Sys.sleep(10*input$login - 30)}
    output$passCheck <- renderText({
      validate(need(input$PsWd == Sys.getenv(paste0(input$TestCentre, "PsWd")), message = "Error: incorrect password"))
      ""
    })
    
    validate(need(input$PsWd == Sys.getenv(paste0(input$TestCentre, "PsWd")), message = "Error: incorrect password"))
  
    #3. INITIAL SET UP UP - PULL IN DATA FROM SQL FOR OPEN ORDERS AND DISPLAY SUMMARY OF ORDERS TO DRILL DOWN ----
      if(input$CollectComp == 0){
        #2. Bring in table of open orders from sql database
        
        options(mysql = list(
          "host" = "database-2.c7cch80rsap5.eu-west-2.rds.amazonaws.com",
          "port" = 3306,
          "user" = Sys.getenv("MY_UID"),
          "password" = Sys.getenv("MY_PWD")
        ))
        
        values$tbl_pub <- paste0(input$TestCentre, "Orders")
        
        values$db <- "BAR"
        cn <- dbConnect(drv      = RMySQL::MySQL(),
                        username = options()$mysql$user,
                        password = options()$mysql$password,
                        host     = options()$mysql$host,
                        port     = options()$mysql$port,
                        dbname = values$db
        )
        
        values$query <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus = 'Open'", tbl = SQL(values$tbl_pub))
        values$open_orders_from_sql <- dbGetQuery(cn, values$query)
        
        cons<-dbListConnections(MySQL())
        for(con in cons) dbDisconnect(con)
        
        # summarise table
        
        values$summary_orders <- group_by(values$open_orders_from_sql, OrderNumber) %>%
          summarise(Count = n())
        
        
        # display the data that is available to be drilled down
        output$summary <- DT::renderDataTable(values$summary_orders, selection = 'single', rownames = FALSE)  
        
        values$num_orders <- dim(values$open_orders_from_sql)[[1]]
      }
      

      #5. CREATE DRILL DOWN TABLE ----
        
      # subset the records to the row that was clicked
      drilldata <- reactive({
        shiny::validate(
          need(length(input$summary_rows_selected) > 0, "")
        )    
          
        # subset the summary table and extract the column to subset on
        # if you have more than one column, consider a merge instead
        # NOTE: the selected row indices will be character type so they
        #   must be converted to numeric or integer before subsetting
        values$selectedOrder <- values$summary_orders[as.integer(input$summary_rows_selected), ]$OrderNumber
          
        #values$open_orders_from_sql[values$open_orders_from_sql$OrderNumber %in% input$summary_rows_selected, ]
        values$open_orders_from_sql[values$open_orders_from_sql$OrderNumber %in% values$selectedOrder, ]
          
      })
        
      # display the subsetted data
      output$drilldown <- DT::renderDataTable(drilldata(), rownames = FALSE)
      
      #6. build 'order completed' button
      
      observeEvent(input$CollectComp, {
        
        cn <- dbConnect(drv      = RMySQL::MySQL(),
                        username = options()$mysql$user,
                        password = options()$mysql$password,
                        host     = options()$mysql$host,
                        port     = options()$mysql$port,
                        dbname = values$db)
        
        query2 <- sqlInterpolate(cn, "UPDATE ?tbl SET OrderStatus = 'Closed' WHERE OrderNumber = ?num", tbl = SQL(values$tbl_pub), num = SQL(values$selectedOrder))
        close_order_from_sql <- dbGetQuery(cn, query2)
        
        query3 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus = 'Open'", tbl = SQL(values$tbl_pub))
        values$open_orders_from_sql <- dbGetQuery(cn, query3)

        dbDisconnect(cn)

        # summarise table

        values$summary_orders <- group_by(values$open_orders_from_sql, OrderNumber) %>%
         summarise(Count = n())
        
        # display the data that is available to be drilled down
        output$summary <- DT::renderDataTable(values$summary_orders, selection = 'single', rownames = FALSE)
        
        values$num_orders <- dim(values$open_orders_from_sql)[[1]]
        print(values$num_orders)
        
        #session$reload()
        print(input$summary_rows_selected)
      })
      
      #7. build 'refresh orders list' button
      
      observeEvent(input$refreshOrders, {
        
        cn <- dbConnect(drv      = RMySQL::MySQL(),
                        username = options()$mysql$user,
                        password = options()$mysql$password,
                        host     = options()$mysql$host,
                        port     = options()$mysql$port,
                        dbname = values$db)
        
        query4 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus = 'Open'", tbl = SQL(values$tbl_pub))
        values$open_orders_from_sql <- dbGetQuery(cn, query4)
        
        dbDisconnect(cn)
        
        # summarise table
        
        values$summary_orders <- group_by(values$open_orders_from_sql, OrderNumber) %>%
          summarise(Count = n())
        
        # display the data that is available to be drilled down
        output$summary <- DT::renderDataTable(values$summary_orders, selection = 'single', rownames = FALSE)
        
        values$num_orders <- dim(values$open_orders_from_sql)[[1]]
        
        #session$reload()
        #print(input$summary_rows_selected)
      })
      
      #4. IF NO ORDERS IN LIST, REFRESH THE SUMMARY TABLE EVERY 5 SECONDS ----
      print(values$num_orders)
      
      # Anything that calls autoInvalidate will automatically invalidate
      # every 5 seconds.
      autoInvalidate <- reactiveTimer(5000)
        
      observeEvent(
        # Invalidate and re-execute this reactive expression every time the
        # timer fires.
        autoInvalidate(), {
          print(is.null(values$selectedOrder))
          if (is.null(values$selectedOrder) == TRUE){
            # Do something each time this is invalidated.
            # The isolate() makes this observer _not_ get invalidated and re-executed
            # when input$n changes.
            values$db <- "BAR"
            cn <- dbConnect(drv      = RMySQL::MySQL(),
                            username = options()$mysql$user,
                            password = options()$mysql$password,
                            host     = options()$mysql$host,
                            port     = options()$mysql$port,
                            dbname = values$db
            )
            values$query5 <- sqlInterpolate(cn, "SELECT * FROM ?tbl WHERE OrderStatus = 'Open'", tbl = SQL(values$tbl_pub))
            values$open_orders_from_sql <- dbGetQuery(cn, values$query5)
            
            cons<-dbListConnections(MySQL())
            for(con in cons) dbDisconnect(con)
            
            # summarise table
            
            values$summary_orders <- group_by(values$open_orders_from_sql, OrderNumber) %>%
              summarise(Count = n())
            
            values$num_orders <- dim(values$open_orders_from_sql)[[1]]
            
          }
          })

          
        
      
      #5. Delete rows from database for order completed and write back completed records
  })

} 

shinyApp(ui = shinyUI, server = shinyServer)


