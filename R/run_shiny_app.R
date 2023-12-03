#' @title Runs the shiny app
#' @description starts the shiny app
#' @return None.
#' @importFrom utils head
run_shiny_app <- function() {
  #library(shiny)
  #library(duckdb)
  #library(dplyr)
  #library(DBI)
  #library(DT)
  #library(ggplot2)
  #library(ctrialsgov)
  #library(tidyr)
  #library(forcats)

  ctgov_get_latest_snapshot(db_path = "ctgov.duckdb",
                            db_derived_path = "ctgov-derived.duckdb")

  con = dbConnect(
    duckdb(
      file.path("ctgov.duckdb"),
      read_only = TRUE
    )
  )

  dbListTables(con)
  studies = tbl(con, "studies")
  conditions = tbl(con, "conditions")
  countries = tbl(con, "countries")
  reported_events = tbl(con, "reported_events")
  designs = tbl(con, "designs")

  ctgov_load_duckdb_file(file.path("ctgov-derived.duckdb"))
  endpoints = ctgov_query_endpoint()
  max_num_studies = 1000

  # functions for the app
  query_kwds <- function(tbl, kwds, column, ignore_case = TRUE, match_all = FALSE) {
    kwds <- paste0("%", kwds, "%")
    kwds <- gsub("'", "''", x = kwds)
    if (ignore_case) {
      like <- " ilike "
    } else{
      like <- " like "
    }
    query <- paste(
      paste0(column, like, "'", kwds, "'"),
      collapse = ifelse(match_all, " AND ", " OR ")
    )

    dplyr::filter(tbl, dplyr::sql(query))
  }

  create_phase_histogram = function(d, studies) {
    d$phase[is.na(d$phase)] = "NA"

    # save all possible phases in a vector
    sorted_phases <- (studies |> collect())$phase |>
      unique() |>
      append("NA") |>
      sort()

    d$newphase <- factor(d$phase, levels=sorted_phases)

    d <- d |>
      select(newphase) |>
      group_by(newphase) |>
      summarize(n = n()) |>
      complete(newphase)

    ggplot(d, aes(x = newphase, y = n)) +
      geom_col() +
      theme_bw() +
      xlab("Phase") +
      ylab("Count")
  }

  create_endpoint_histogram = function(d, endpoints) {
    em = d |>
      select(nct_id) |>
      left_join(endpoints, by = "nct_id") |>
      group_by(endpoint_met) |>
      summarize(n = n())

    # multiply counts by 10 and divide them by 10 when scaling to make counts of 1 visible
    ggplot(em, aes(x = endpoint_met, y = n * 10)) +
      geom_col() +
      scale_y_log10(labels = function(x) x/10) +
      labs(x = "Endpoint Met", y = "Count") +
      theme_bw()
  }

  create_conditions_histogram = function(d, conditions) {
    em = d |>
      select(nct_id) |>
      left_join(conditions |> collect(), by = "nct_id")

    # lump together conditions that aren't considered the top 15 most frequent
    em <- em |>
      mutate(name = fct_lump_n(name, 15)) |>
      group_by(name) |>
      summarize(n = n()) |>
      arrange(desc(n)) |>
      mutate(conditionsordered = factor(name, levels = name))
    # multiply counts by 10 and divide them by 10 when scaling to make counts of 1 visible
    ggplot(em, aes(x = conditionsordered, y = n * 10)) +
      geom_col() +
      scale_y_log10(labels = function(x) x/10) +
      labs(x = "Condition Name", y = "Count") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  # the rest are my added functions
  create_adverse_events_histogram = function(d, con) {
    # have to use SQL to help us merge tables as dplyr's left_join is computationally
    # expensive for large datasets
    dbExecute(con, paste("DROP TABLE IF EXISTS", "studies_merge"))  # drop table if it exists
    copy_to(con, d, name = "studies_merge", temporary = TRUE)  # copies to connection
    # so we can sql query
    query <- "SELECT adverse_event_term
            FROM studies_merge
            LEFT JOIN reported_events
            ON studies_merge.nct_id = reported_events.nct_id"
    merged_ae <- dbGetQuery(con, query)
    #d$merged_ae[is.na(d$merged_ae)] = "NA"
    merged_ae <- merged_ae |>
      mutate(adverse_event_term = fct_lump_n(adverse_event_term, 10)) |>  # keeps
      # the top 10 most frequent adverse events
      group_by(adverse_event_term) |>
      summarize(n = n()) |>
      filter(adverse_event_term != "Other")  # gets rid of the "Other" column, contains
    # too many adverse events

    ggplot(merged_ae, aes(x = adverse_event_term, y = n)) +
      geom_col() +
      theme_bw() +
      coord_flip() +
      xlab("Adverse Events") +
      ylab("Count")
  }

  create_intervention_histogram = function(d, designs) {
    merged <- left_join(d, collect(designs), by = "nct_id")
    merged_interv <- merged |>
      select(intervention_model) |>
      filter(!is.na(intervention_model)) |>
      group_by(intervention_model) |>
      summarize(n = n())
    sum_interv <- sum(merged_interv$n)

    ggplot(merged_interv, aes(x = intervention_model, y = n)) +
      geom_col() +
      theme_bw() +
      labs(title = paste("Total Amount: ", sum_interv)) +
      coord_flip() +
      xlab("Intervention Type") +
      ylab("Count")
  }

  create_observational_histogram = function(d, designs) {
    merged <- left_join(d, collect(designs), by = "nct_id")
    merged_observ <- merged |>
      select(observational_model) |>
      filter(!is.na(observational_model)) |>
      group_by(observational_model) |>
      summarize(n = n())
    sum_observ <- sum(merged_observ$n)  # this is for the title to give a total count

    ggplot(merged_observ, aes(x = observational_model, y = n)) +
      geom_col() +
      theme_bw() +
      labs(title = paste("Total Amount: ", sum_observ)) +
      coord_flip() +
      xlab("Observational Type") +
      ylab("Count")
  }

  # Define UI
  ui <- fluidPage(

    # Application title
    titlePanel("Clinical Trials Query"),

    # Sidebar with search and dropdown menu of agency class; default shows all studies
    sidebarLayout(
      sidebarPanel(
        textInput("brief_title_kw", "Brief title keywords"),
        selectInput("source_class",
                    label = h3("Sponsor Type"),
                    choices = list("All" = "",
                                   "Ambiguous" = "NA",
                                   "Federal" = "FED",
                                   "Individual" = "INDIV",
                                   "Industry" = "INDUSTRY",
                                   "Network" = "NETWORK",
                                   "NIH" = "NIH",
                                   "Other" = "OTHER",
                                   "Other gov" = "OTHER_GOV",
                                   "Unknown" = "UNKNOWN"),
                    selected = "")
      ),

      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Phases Plot", plotOutput("distPlot")),
          tabPanel("Endpoint Met", plotOutput("endpointPlot")),
          tabPanel("Conditions Examined", plotOutput("conditionsPlot")),
          tabPanel("Adverse Events", plotOutput("adverseEventsPlot")),
          tabPanel("Research Model Designs",
                   fluidRow(
                     column(6, plotOutput("interventionPlot")),
                     column(6, plotOutput("observationalPlot"))
                   ))
        ),
        dataTableOutput("trial_table")
      )
    )
  )

  # Define server logic
  server <- function(input, output) {

    get_studies = reactive({
      if (input$brief_title_kw != "") {
        si = input$brief_title_kw |>
          strsplit(",") |>
          unlist() |>
          trimws()
        ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
      } else {
        ret = studies
      }
      if (input$source_class == "NA") {
        ret <- ret |>
          filter(is.na(source_class))
      }
      else if (input$source_class != "") {  # filter by agency class if one is selected
        ret <- ret |>
          filter(source_class == !!input$source_class)
      }
      ret |> head(max_num_studies)
    }
    )

    output$distPlot <- renderPlot({
      get_studies() |>
        collect() |>
        create_phase_histogram(studies)
    })

    output$endpointPlot <- renderPlot({
      get_studies() |>
        collect() |>
        create_endpoint_histogram(endpoints)
    })

    output$conditionsPlot <- renderPlot({
      get_studies() |>
        collect() |>
        create_conditions_histogram(conditions)
    })

    output$adverseEventsPlot <- renderPlot({
      get_studies() |>  # does not need collect(), function needs a duckdb object
        create_adverse_events_histogram(con)
    })

    output$interventionPlot <- renderPlot({
      get_studies() |>
        collect() |>
        create_intervention_histogram(designs)
    })

    output$observationalPlot <- renderPlot({
      get_studies() |>
        collect() |>
        create_observational_histogram(designs)
    })

    output$trial_table = renderDataTable({
      get_studies() |>
        collect() |>
        select(nct_id, brief_title, start_date, completion_date) |>
        rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
               `Start Date` = start_date, `Completion Date` = completion_date)
    })

  }

  # Run the application
  shinyApp(ui = ui, server = server)
}
