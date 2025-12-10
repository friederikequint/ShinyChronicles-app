# app.R
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

#### SETTINGS ####
PASSWORD   <- "SanityPhDGang26"
owner_file <- "mood_log_owner.csv"
guest_file <- "mood_log_guests.csv"

#### UI ####
ui <- fluidPage(
  # Button styles
  tags$head(
    tags$style(HTML("
      .mood-btn {
        width: 85%;
        padding: 25px;
        font-size: 22px;
        color: white;
        margin-bottom: 12px;
        border-radius: 12px;
        border: none;
      }
      #very_bad  { background-color: #e53935; }
      #bad       { background-color: #fb8c00; }
      #ok        { background-color: #fdd835; color: #000000; }
      #good      { background-color: #43a047; }
      #very_good { background-color: #1b5e20; }
    "))
  ),
  
  uiOutput("app_ui")
)

#### SERVER ####
server <- function(input, output, session) {
  
  # "owner", "guest" or NULL
  mode <- reactiveVal(NULL)
  
  #### helper: read log ####
  read_log <- function(file_to_use) {
    if (!file.exists(file_to_use)) {
      return(
        data.frame(
          date  = as.Date(character()),
          time  = character(),
          mood  = integer(),
          label = character(),
          stringsAsFactors = FALSE
        )
      )
    }
    
    df <- tryCatch(
      read.csv(file_to_use, stringsAsFactors = FALSE),
      error = function(e) {
        data.frame(
          date  = as.Date(character()),
          time  = character(),
          mood  = integer(),
          label = character(),
          stringsAsFactors = FALSE
        )
      }
    )
    
    if ("date" %in% names(df)) {
      df$date <- as.Date(df$date)
    }
    
    df
  }
  
  #### TOP-LEVEL UI: login vs main ####
  output$app_ui <- renderUI({
    
    # 1) login screen
    if (is.null(mode())) {
      return(
        fluidPage(
          titlePanel("Sanity Chronicles – How's the mood?"),
          br(),
          fluidRow(
            column(
              width = 4, offset = 4,
              h4("Choose how to use the app"),
              tags$hr(),
              h5("1) Log in as owner (your personal PhD sanity log)"),
              passwordInput("pw", "Owner password"),
              actionButton("login", "Log in as owner", class = "btn-primary"),
              br(), br(),
              h5("2) Continue as guest (shared public demo)"),
              actionButton("guest", "Continue as guest"),
              br(), br(),
              textOutput("login_status")
            )
          )
        )
      )
    }
    
    # 2) main page with buttons + plots + calendar
    fluidPage(
      titlePanel("Sanity Chronicles – How's the mood?"),
      
      h4(textOutput("today_text")),
      textOutput("mode_text"),
      p("Tap one button once per day, ideally in the evening."),
      
      actionButton("very_bad",  "Very bad",  class = "mood-btn"),
      actionButton("bad",       "Bad",       class = "mood-btn"),
      actionButton("ok",        "Okay",      class = "mood-btn"),
      actionButton("good",      "Good",      class = "mood-btn"),
      actionButton("very_good", "Very good", class = "mood-btn"),
      
      br(),
      strong(textOutput("status_text")),
      br(), br(),
      
      h4("Mood history for this mode"),
      p("Each point is the average mood for a day (1 = very bad, 5 = very good)."),
      plotOutput("mood_plot", height = "300px"),
      
      br(),
      h4("2025 calendar view"),
      p("Each cell is a day in 2025 for the current mode (owner or guest). Filled with your mood color; empty days are grey."),
      plotOutput("calendar_plot", height = "600px"),
      
      br(),
      h5("Last entries in the log:"),
      tableOutput("log_table")
    )
  })
  
  #### LOGIN / MODE ####
  observeEvent(input$login, {
    if (is.null(input$pw) || input$pw == "") {
      output$login_status <- renderText("Please enter a password.")
    } else if (identical(input$pw, PASSWORD)) {
      mode("owner")
      output$login_status <- renderText("")
    } else {
      output$login_status <- renderText("Wrong password.")
    }
  })
  
  observeEvent(input$guest, {
    mode("guest")
    output$login_status <- renderText("")
  })
  
  #### TEXT OUTPUTS ####
  output$today_text <- renderText({
    req(mode())
    paste("Today is", format(Sys.Date(), "%A, %d %B %Y"))
  })
  
  output$mode_text <- renderText({
    if (mode() == "owner") {
      "Mode: Owner – logging to your personal PhD mood file."
    } else if (mode() == "guest") {
      "Mode: Guest – logging to the shared public file."
    } else {
      ""
    }
  })
  
  #### SAVE MOOD – only once per day ####
  save_mood <- function(score, label) {
    req(mode())
    
    now   <- Sys.time()
    today <- as.Date(now)
    
    file_to_use <- if (mode() == "owner") owner_file else guest_file
    
    df <- read_log(file_to_use)
    
    # block second answer per day
    if (any(df$date == today)) {
      output$status_text <- renderText(
        paste("You already answered for today (", format(today, "%Y-%m-%d"),
              "). Come back tomorrow!", sep = "")
      )
      return(invisible(NULL))
    }
    
    new_row <- data.frame(
      date  = today,
      time  = format(now, "%H:%M:%S"),
      mood  = score,
      label = label,
      stringsAsFactors = FALSE
    )
    
    if (nrow(df) == 0) {
      df_new <- new_row
    } else {
      df_new <- bind_rows(df, new_row)
    }
    
    write.csv(df_new, file_to_use, row.names = FALSE)
    
    output$status_text <- renderText({
      paste("Thanks for the answer! Saved:", label, "for", format(today, "%Y-%m-%d"))
    })
  }
  
  observeEvent(input$very_bad,  { save_mood(1, "very_bad") })
  observeEvent(input$bad,       { save_mood(2, "bad") })
  observeEvent(input$ok,        { save_mood(3, "ok") })
  observeEvent(input$good,      { save_mood(4, "good") })
  observeEvent(input$very_good, { save_mood(5, "very_good") })
  
  #### LINE PLOT (history) ####
  output$mood_plot <- renderPlot({
    req(mode())
    
    file_to_use <- if (mode() == "owner") owner_file else guest_file
    
    df <- read_log(file_to_use)
    if (nrow(df) == 0) return(NULL)
    
    daily <- df %>%
      group_by(date) %>%
      summarise(
        avg_mood = mean(mood),
        .groups  = "drop"
      ) %>%
      arrange(date)
    
    plot(
      daily$date, daily$avg_mood,
      type = "b",
      xlab = "Date",
      ylab = "Mood (1 = very bad, 5 = very good)",
      ylim = c(1, 5)
    )
  })
  
  #### CALENDAR PLOT FOR FULL YEAR 2025 (month-style grid) ####
  output$calendar_plot <- renderPlot({
    req(mode())
    
    file_to_use <- if (mode() == "owner") owner_file else guest_file
    df <- read_log(file_to_use)
    
    # aggregate mood per day
    df_day <- df %>%
      group_by(date) %>%
      summarise(mood = mean(mood), .groups = "drop")
    
    year <- 2025
    all_dates <- seq(as.Date(paste0(year, "-01-01")),
                     as.Date(paste0(year, "-12-31")),
                     by = "day")
    
    cal <- tibble::tibble(date = all_dates) %>%
      left_join(df_day, by = "date") %>%
      mutate(
        month     = month(date, label = TRUE, abbr = TRUE),
        month_num = month(date),
        dom       = mday(date)           # day of month
      )
    
    # semi-transparent red → green palette
    base_pal   <- c("#e53935", "#fb8c00", "#fdd835", "#43a047", "#1b5e20")
    pal_alpha  <- grDevices::adjustcolor(base_pal, alpha.f = 0.5)
    
    par(mfrow = c(4, 3), mar = c(1, 1, 2, 1))
    for (m in sort(unique(cal$month_num))) {
      mdat <- cal %>% filter(month_num == m)
      
      # weekday of first day of this month (1 = Mon)
      first_day <- mdat$date[1]
      w1 <- wday(first_day, week_start = 1)
      
      # weekday (column 1..7) for each date
      mdat$dow <- wday(mdat$date, week_start = 1)
      # week-of-month (row 1..6)
      mdat$wom <- ((mdat$dom + w1 - 2) %/% 7) + 1
      
      # empty panel for month
      plot(1, type = "n",
           xlim = c(0.5, 7.5),
           ylim = c(6.5, 0.5),
           xaxt = "n", yaxt = "n",
           xlab = "", ylab = "", bty = "n")
      
      title(toupper(as.character(unique(mdat$month))), line = 0.5)
      
      # draw cells
      for (i in seq_len(nrow(mdat))) {
        d <- mdat[i, ]
        x <- d$dow
        y <- d$wom
        
        # background
        if (!is.na(d$mood)) {
          idx <- min(max(round(d$mood), 1), 5)
          rect(x - 0.5, y - 0.5, x + 0.5, y + 0.5,
               col = pal_alpha[idx], border = "grey80")
        } else {
          rect(x - 0.5, y - 0.5, x + 0.5, y + 0.5,
               col = "grey95", border = "grey90")
        }
        
        # day number (always visible)
        text(x, y, format(d$date, "%d"), cex = 0.75)
      }
    }
  })
  
  #### TABLE (last entries) ####
  output$log_table <- renderTable({
    req(mode())
    
    file_to_use <- if (mode() == "owner") owner_file else guest_file
    
    df <- read_log(file_to_use)
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      arrange(desc(date), desc(time)) %>%
      head(10)
  })
}

shinyApp(ui, server)
