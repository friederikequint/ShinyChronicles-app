# app.R
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(jsonlite)
library(DBI)
library(RPostgres)
library(pool)

#### SUPABASE CONNECTION ####
use_db <- nzchar(Sys.getenv("SUPABASE_HOST"))

pg_pool <- NULL
if (use_db) {
  pg_pool <- pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT", "5432")),
    dbname   = Sys.getenv("SUPABASE_DB", "postgres"),
    user     = Sys.getenv("SUPABASE_USER", "postgres"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    sslmode  = "require"
  )
  onStop(function() pool::poolClose(pg_pool))
}

#### SETTINGS ####
password_file <- ".secret_owner_password.r"

get_owner_password <- function(path = password_file) {
  pw_env <- Sys.getenv("OWNER_PASSWORD", unset = "")
  if (nzchar(pw_env)) return(trimws(pw_env))
  
  if (!file.exists(path)) {
    stop(paste0(
      "Owner password not found.\n",
      "Set OWNER_PASSWORD env var (recommended for hosting), or create file: ",
      path,
      " with your password as a single line."
    ))
  }
  pw <- trimws(paste(readLines(path, warn = FALSE), collapse = "\n"))
  if (identical(pw, "")) stop("Password file is empty.")
  pw
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

#### UI ####
ui <- fluidPage(
  tags$head(
    # Roboto + mobile viewport
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700;900&display=swap"
    ),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1"),
    
    # ✅ Guest user_id generation (localStorage) + KEEP SENDING UNTIL SHINY IS READY
    # This fixes the "user_id NULL" insert problem.
    tags$script(HTML("
      (function() {
        const key = 'sanity_guest_id';

        function makeId() {
          return 'guest_' + Math.random().toString(16).slice(2) + '_' + Date.now().toString(16);
        }

        function ensureGuestId() {
          let id = null;
          try { id = localStorage.getItem(key); } catch(e) {}
          if (!id) {
            id = makeId();
            try { localStorage.setItem(key, id); } catch(e) {}
          }
          return id;
        }

        function pushToShiny() {
          const id = ensureGuestId();
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('guest_id', id, {priority: 'event'});
            return true;
          }
          return false;
        }

        function boot() {
          const ok = pushToShiny();
          if (!ok) setTimeout(boot, 200);
        }

        document.addEventListener('DOMContentLoaded', boot);
        document.addEventListener('shiny:connected', boot);

        // Mobile browsers sometimes suspend JS; re-send when returning
        document.addEventListener('visibilitychange', function() {
          if (!document.hidden) pushToShiny();
        });
        window.addEventListener('focus', function() { pushToShiny(); });
      })();
    ")),
    
    # ✅ Client time zone detection + send to Shiny as input$client_tz
    tags$script(HTML("
      (function() {
        function sendTimezoneToShiny() {
          const tz = Intl.DateTimeFormat().resolvedOptions().timeZone || 'UTC';
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('client_tz', tz, {priority: 'event'});
          }
        }

        function bootTZ() {
          sendTimezoneToShiny();
          if (!(window.Shiny && Shiny.setInputValue)) setTimeout(bootTZ, 200);
        }

        document.addEventListener('DOMContentLoaded', bootTZ);
        document.addEventListener('shiny:connected', bootTZ);
      })();
    ")),
    
    # CSS
    tags$style(HTML("
      body, button, input, textarea, select {
        font-family: 'Roboto', system-ui, -apple-system, Segoe UI, Arial, sans-serif !important;
      }

      body {
        min-height: 100vh;
        background:
          radial-gradient(1200px 800px at 10% 10%, rgba(255, 193, 7, 0.35), transparent 60%),
          radial-gradient(900px 700px at 90% 20%, rgba(46, 125, 50, 0.28), transparent 55%),
          radial-gradient(900px 700px at 30% 95%, rgba(211, 47, 47, 0.22), transparent 55%),
          radial-gradient(900px 700px at 85% 85%, rgba(245, 124, 0, 0.22), transparent 55%),
          linear-gradient(135deg, #101827 0%, #0b1220 50%, #0f172a 100%);
        background-attachment: fixed;
      }

      .app-shell {
        max-width: 860px;
        margin: 18px auto 0 auto;
        padding: 64px 16px 22px 16px;
        background: rgba(255,255,255,0.86);
        border: 1px solid rgba(255,255,255,0.55);
        border-radius: 20px;
        box-shadow: 0 18px 50px rgba(0,0,0,0.22);
        backdrop-filter: blur(10px);
        -webkit-backdrop-filter: blur(10px);
        position: relative;
      }

      .app-subtitle {
        font-size: 16px;
        font-weight: 500;
        margin-top: -8px;
        margin-bottom: 14px;
        opacity: 0.85;
      }

      .app-instruction {
        font-size: 18px;
        font-weight: 900;
        margin: 10px 0 14px 0;
      }

      .answer-center {
        display: flex;
        justify-content: center;
        gap: 10px;
        flex-wrap: wrap;
        margin-bottom: 6px;
      }

      .btn {
        border-radius: 14px !important;
        padding: 12px 14px !important;
        font-weight: 900 !important;
        font-size: 16px !important;
        min-width: 120px;
        border: none !important;
        box-shadow: 0 8px 16px rgba(0,0,0,0.12);
        transition: transform 120ms ease, opacity 120ms ease, filter 120ms ease;
      }

      .btn-very-bad   { background: #d32f2f; color: white; }
      .btn-bad        { background: #f57c00; color: white; }
      .btn-ok         { background: #fbc02d; color: white; }
      .btn-good       { background: #7cb342; color: white; }
      .btn-very-good  { background: #2e7d32; color: white; }

      .btn-selected {
        outline: 4px solid rgba(0,0,0,0.18) !important;
        transform: translateY(-1px);
        opacity: 1 !important;
        filter: saturate(1) !important;
      }

      .btn-dim {
        opacity: 0.55 !important;
        filter: saturate(0.65) !important;
      }

      .note-label { font-weight: 900; margin-top: 12px; margin-bottom: 6px; }
      .note-help { opacity: 0.75; margin-bottom: 6px; }
      .note-area textarea {
        border-radius: 14px !important;
        padding: 12px !important;
        font-size: 15px !important;
        box-shadow: 0 8px 16px rgba(0,0,0,0.08);
        border: 1px solid rgba(0,0,0,0.10);
      }

      .top-right-icon { position: absolute; top: 18px; right: 18px; z-index: 10; }
      .top-left-icon  { position: absolute; top: 18px; left: 18px; z-index: 10; }

      .icon-btn {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 44px;
        height: 44px;
        border-radius: 14px;
        background: rgba(255,255,255,0.92);
        border: 1px solid rgba(0,0,0,0.08);
        box-shadow: 0 10px 18px rgba(0,0,0,0.10);
        cursor: pointer;
        text-decoration: none !important;
      }
      .icon-btn i { font-size: 20px; color: rgba(0,0,0,0.72); }

      .stress-slider { padding: 4px 0 8px 0; }
      .stress-slider .irs { font-size: 16px !important; }

      .stress-slider .irs-line {
        height: 16px !important;
        border-radius: 999px !important;
        background: linear-gradient(90deg, rgba(46,125,50,0.20), rgba(251,192,45,0.20), rgba(211,47,47,0.20)) !important;
        border: 1px solid rgba(0,0,0,0.06) !important;
      }
      .stress-slider .irs-bar {
        height: 16px !important;
        border-radius: 999px !important;
        background: rgba(15, 23, 42, 0.22) !important;
      }

      .stress-slider .irs-handle {
        width: 20px !important;
        height: 20px !important;
        top: 20px !important;
      }
      .stress-slider .irs-handle > i:first-child {
        width: 20px !important;
        height: 20px !important;
        border-radius: 999px !important;
        background: rgba(255,255,255,0.95) !important;
        border: 1px solid rgba(0,0,0,0.12) !important;
        box-shadow: 0 10px 18px rgba(0,0,0,0.16) !important;
      }

      .stress-slider .irs-single {
        font-size: 16px !important;
        font-weight: 900 !important;
        padding: 4px 10px !important;
        border-radius: 999px !important;
        top: -15px;
      }

      .stress-slider .irs-min,
      .stress-slider .irs-max {
        top: -36px !important;
        padding: 2px 6px !important;
        border-radius: 10px !important;
        background: rgba(15, 23, 42, 0.10) !important;
        border: 1px solid rgba(0,0,0,0.08) !important;
        font-size: 13px !important;
        font-weight: 800 !important;
        color: rgba(0,0,0,0.78) !important;
        opacity: 0.98 !important;
      }

      .section-title { font-size: 18px; font-weight: 900; margin: 6px 0 10px 0; }

      .app-footer {
        margin: 18px auto 24px auto;
        max-width: 860px;
        padding: 12px 16px;
        background: rgba(255,255,255,0.80);
        border: 1px solid rgba(255,255,255,0.55);
        border-radius: 16px;
        box-shadow: 0 14px 38px rgba(0,0,0,0.18);
        backdrop-filter: blur(10px);
        -webkit-backdrop-filter: blur(10px);
        font-size: 12px;
        opacity: 0.82;
        display: flex;
        flex-direction: column;
        gap: 4px;
      }
    ")),
    
    # JS to replace min/max label text with full phrases
    tags$script(HTML("
      function setStressEndLabels() {
        var root = document.querySelector('#stress');
        if (!root) return;

        var wrap = root.closest('.form-group');
        if (!wrap) return;

        var irs = wrap.querySelector('.irs');
        if (!irs) return;

        var minEl = irs.querySelector('.irs-min');
        var maxEl = irs.querySelector('.irs-max');

        if (minEl) minEl.textContent = '0 = Not stressed at all';
        if (maxEl) maxEl.textContent = '10 = Very stressed';
      }

      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(setStressEndLabels, 50);
      });

      if (window.Shiny) {
        Shiny.addCustomMessageHandler('refreshStressLabels', function(x) {
          setTimeout(setStressEndLabels, 50);
        });
      }
    "))
  ),
  
  uiOutput("app_ui"),
  
  div(
    class = "app-footer",
    tags$span("Purpose: private mood tracking app (owner) + optional public guest demo"),
    tags$span(paste0("Data backend: ", if (use_db) "Supabase (Postgres)" else "Local CSV (fallback - currently disabled in code)")),
    tags$span("Download: CSV/JSON exports available on the Insights page (guest demo data is public)"),
    tags$span(textOutput("user_id_footer")),
    tags$span("Last updated: 2026-01-07")
  )
)

#### SERVER ####
server <- function(input, output, session) {
  
  mode <- reactiveVal(NULL)
  page <- reactiveVal("questions") # "questions" or "insights"
  
  blocked_date  <- reactiveVal(NULL)
  status_msg_ui <- reactiveVal(NULL)
  
  selected_mood_score <- reactiveVal(NULL)
  selected_mood_label <- reactiveVal(NULL)
  
  # Current user_id logic
  current_user_id <- reactive({
    req(mode())
    
    if (mode() == "owner") return("owner")
    
    gid <- input$guest_id %||% ""
    if (!nzchar(gid)) return(NA_character_)
    as.character(gid)
  })
  
  output$user_id_footer <- renderText({
    if (is.null(mode())) return("User ID: (not selected yet)")
    uid <- current_user_id()
    if (is.na(uid) || !nzchar(uid)) return("User ID: (initializing...)")
    paste0("User ID: ", uid)
  })
  
  # Reset back to mode selection
  observeEvent(input$go_back_from_questions, {
    mode(NULL)
    page("questions")
    selected_mood_score(NULL)
    selected_mood_label(NULL)
    blocked_date(NULL)
    status_msg_ui(NULL)
  })
  
  # Re-apply slider labels whenever we switch pages (UI re-renders)
  observeEvent(page(), {
    session$sendCustomMessage("refreshStressLabels", list())
  })
  
  #### SUPABASE DB FUNCTIONS ####
  read_log_db <- function(user_id) {
    if (!use_db) stop("Supabase not configured. Set SUPABASE_HOST etc. (see README).")
    if (is.na(user_id) || !nzchar(user_id)) stop("User ID not initialized yet.")
    
    df <- DBI::dbGetQuery(
      pg_pool,
      "select entry_date as date,
              to_char(entry_time, 'HH24:MI:SS') as time,
              mood, label, stress, note
       from public.mood_logs
       where user_id = $1
       order by entry_date, entry_time",
      params = list(user_id)
    )
    df$date <- as.Date(df$date)
    df
  }
  
  insert_log_db <- function(user_id, mode, entry_date, entry_time, mood, label, stress, note) {
    if (!use_db) stop("Supabase not configured. Set SUPABASE_HOST etc. (see README).")
    if (is.na(user_id) || !nzchar(user_id)) stop("User ID not initialized yet.")
    
    DBI::dbExecute(
      pg_pool,
      "insert into public.mood_logs (user_id, mode, entry_date, entry_time, mood, label, stress, note)
       values ($1,$2,$3,$4::time,$5,$6,$7,$8)",
      params = list(user_id, mode, entry_date, entry_time, mood, label, stress, note)
    )
  }
  
  delete_log_db_for_date <- function(user_id, entry_date) {
    if (!use_db) stop("Supabase not configured. Set SUPABASE_HOST etc. (see README).")
    if (is.na(user_id) || !nzchar(user_id)) stop("User ID not initialized yet.")
    
    DBI::dbExecute(
      pg_pool,
      "delete from public.mood_logs
       where user_id = $1 and entry_date = $2",
      params = list(user_id, entry_date)
    )
  }
  
  read_log_any <- function() {
    uid <- current_user_id()
    if (is.na(uid) || !nzchar(uid)) {
      return(data.frame(
        date = as.Date(character()),
        time = character(),
        mood = integer(),
        label = character(),
        stress = integer(),
        note = character(),
        stringsAsFactors = FALSE
      ))
    }
    read_log_db(uid)
  }
  
  #### TIME WINDOW (CLIENT TZ) ####
  client_tz <- reactive({
    tz <- input$client_tz %||% ""
    if (!nzchar(tz)) tz <- Sys.timezone() %||% "UTC"
    tz
  })
  
  now_local <- reactive({
    lubridate::with_tz(Sys.time(), tzone = client_tz())
  })
  
  in_time_window <- reactive({
    hr <- lubridate::hour(now_local())
    hr >= 18 && hr <= 23
  })
  
  #### MAIN UI ####
  output$app_ui <- renderUI({
    if (is.null(mode())) {
      return(
        div(
          class = "app-shell",
          titlePanel("Sanity Chronicles – How's your mood?"),
          br(),
          h4("Choose how to use the app"),
          tags$hr(),
          h5("1) Log in as owner (your personal PhD sanity log)"),
          passwordInput("pw", "Owner password"),
          actionButton("login", "Log in as owner", class = "btn-primary"),
          br(), br(),
          h5("2) Continue as guest (shared public demo)"),
          actionButton("guest", "Continue as guest"),
          br(), br(),
          uiOutput("login_status_ui")
        )
      )
    }
    
    if (identical(page(), "questions")) {
      return(
        div(
          class = "app-shell",
          
          div(class = "top-left-icon",
              actionLink("go_back_from_questions", label = NULL, class = "icon-btn", icon("arrow-left"))),
          
          div(class = "top-right-icon",
              actionLink("open_insights_icon", label = NULL, class = "icon-btn", icon("calendar"))),
          
          titlePanel("Sanity Chronicles – How has your day been?"),
          div(class = "app-subtitle", paste0("Log your daily mood - ", format(as.Date(now_local()), "%d-%m-%Y"))),
          
          div(
            class = "app-instruction",
            "Choose your mood, then stress, then hit Save (once per day between 6pm and 11.59pm)."
          ),
          
          div(class = "answer-center", uiOutput("mood_buttons_ui")),
          br(),
          
          div(
            class = "app-instruction",
            style = "margin-bottom: 35px;",
            "How stressed did you feel today? (0–10)"
          ),
          
          div(
            class = "stress-slider",
            style = "width: min(96vw, 760px); margin: 0 auto 6px auto;",
            sliderInput("stress", label = NULL, min = 0, max = 10, value = 5, step = 1, ticks = TRUE)
          ),
          
          div(
            style = "
              text-align: left;
              font-size: 13px;
              font-style: italic;
              opacity: 0.75;
              margin-bottom: 18px;
            ",
            "(0 = Not at all stressed – 10 = Extremely stressed)"
          ),
          
          br(),
          div(class = "note-label", "Note (optional)"),
          div(class = "note-help", "Saved in the log as a separate column."),
          div(class = "note-area",
              textAreaInput("note", NULL, value = "", width = "100%", height = "90px")),
          
          br(),
          actionButton("save_answers", "Save", class = "btn btn-good"),
          br(), br(),
          strong(uiOutput("status_ui"))
        )
      )
    }
    
    # Insights page
    div(
      class = "app-shell",
      
      div(class = "top-left-icon",
          actionLink("back_to_questions_arrow", label = NULL, class = "icon-btn", icon("arrow-left"))),
      
      titlePanel("Sanity Chronicles – Insights"),
      
      div(
        style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:8px;",
        actionButton("back_to_questions", "Back to questions", class = "btn btn-ok"),
        downloadButton("download_csv", "Download CSV"),
        downloadButton("download_json", "Download JSON")
      ),
      
      br(),
      div(class = "section-title", "Trend"),
      p("Each point is the average mood for a day (1 = very bad, 5 = very good)."),
      plotOutput("mood_plot", height = "280px"),
      
      br(),
      div(class = "section-title", "Calendar view: 2026"),
      p("Each cell is a day, filled with your mood color. Empty days are grey."),
      plotOutput("calendar_plot", height = "700px"),
      
      br(),
      strong(uiOutput("status_ui"))
    )
  })
  
  #### LOGIN ####
  output$login_status_ui <- renderUI({ NULL })
  
  observeEvent(input$login, {
    pw_ok <- FALSE
    try({ pw_ok <- identical(input$pw %||% "", get_owner_password()) }, silent = TRUE)
    
    if (pw_ok) {
      mode("owner")
      page("questions")
      output$login_status_ui <- renderUI(NULL)
    } else {
      output$login_status_ui <- renderUI(div(style = "color:#b00020;font-weight:900;", "Wrong password. Try again."))
    }
  })
  
  observeEvent(input$guest, {
    mode("guest")
    page("questions")
    output$login_status_ui <- renderUI(NULL)
  })
  
  #### MOOD BUTTONS ####
  output$mood_buttons_ui <- renderUI({
    sel <- selected_mood_label()
    btn_class <- function(base, lbl) {
      if (!is.null(sel)) {
        if (identical(sel, lbl)) return(paste(base, "btn-selected"))
        return(paste(base, "btn-dim"))
      }
      base
    }
    
    tagList(
      actionButton("very_bad",  "Very bad",  class = btn_class("btn btn-very-bad", "very_bad")),
      actionButton("bad",       "Bad",       class = btn_class("btn btn-bad", "bad")),
      actionButton("ok",        "OK",        class = btn_class("btn btn-ok", "ok")),
      actionButton("good",      "Good",      class = btn_class("btn btn-good", "good")),
      actionButton("very_good", "Very good", class = btn_class("btn btn-very-good", "very_good"))
    )
  })
  
  observeEvent(input$very_bad,  { selected_mood_score(1); selected_mood_label("very_bad") })
  observeEvent(input$bad,       { selected_mood_score(2); selected_mood_label("bad") })
  observeEvent(input$ok,        { selected_mood_score(3); selected_mood_label("ok") })
  observeEvent(input$good,      { selected_mood_score(4); selected_mood_label("good") })
  observeEvent(input$very_good, { selected_mood_score(5); selected_mood_label("very_good") })
  
  #### STATUS ####
  output$status_ui <- renderUI({ status_msg_ui() %||% "" })
  
  #### NAVIGATION ####
  observeEvent(input$open_insights_icon, { page("insights") })
  observeEvent(input$back_to_questions, { page("questions") })
  observeEvent(input$back_to_questions_arrow, { page("questions") })
  
  #### SAVE ####
  observeEvent(input$save_answers, {
    req(mode())
    
    if (is.null(selected_mood_score()) || is.null(selected_mood_label())) {
      status_msg_ui(div(style = "color:#b00020;font-weight:900;", "Please select a mood first."))
      return(invisible(NULL))
    }
    
    # ✅ Ensure guest_id exists before doing anything DB-related
    uid <- current_user_id()
    if (is.na(uid) || !nzchar(uid)) {
      status_msg_ui(div(
        style = "color:#b00020;font-weight:900;",
        "Initializing guest session… please wait 1 second and click Save again."
      ))
      return(invisible(NULL))
    }
    
    if (!in_time_window()) {
      status_msg_ui("There's still some time until the day ends. Come back later between 6 p.m. and 23.59 p.m.")
      return(invisible(NULL))
    }
    
    now   <- now_local()
    today <- as.Date(now)
    
    df <- tryCatch(read_log_db(uid), error = function(e) {
      status_msg_ui(paste("DB read failed:", conditionMessage(e)))
      return(NULL)
    })
    if (is.null(df)) return(invisible(NULL))
    
    if (any(df$date == today)) {
      blocked_date(today)
      status_msg_ui(
        tagList(
          paste0("You already answered for today (", format(today, "%d-%m-%Y"), "). Come back tomorrow! If this was a mistake, click "),
          actionLink("revert_answer", "here"),
          "."
        )
      )
      return(invisible(NULL))
    }
    
    tryCatch({
      insert_log_db(
        user_id     = uid,
        mode        = mode(),
        entry_date  = today,
        entry_time  = format(now, "%H:%M:%S"),
        mood        = as.integer(selected_mood_score()),
        label       = as.character(selected_mood_label()),
        stress      = as.integer(input$stress %||% 5),
        note        = as.character(input$note %||% "")
      )
      
      blocked_date(NULL)
      status_msg_ui(paste("Saved for", format(today, "%d-%m-%Y"), "✅"))
      page("insights")
    }, error = function(e) {
      status_msg_ui(paste("Could not save:", conditionMessage(e)))
    })
  })
  
  #### REVERT ####
  observeEvent(input$revert_answer, {
    req(mode())
    
    uid <- current_user_id()
    if (is.na(uid) || !nzchar(uid)) {
      status_msg_ui(div(
        style = "color:#b00020;font-weight:900;",
        "Guest session not ready yet. Please refresh the page and try again."
      ))
      return(invisible(NULL))
    }
    
    bd <- blocked_date() %||% as.Date(now_local())
    
    tryCatch({
      delete_log_db_for_date(uid, bd)
      selected_mood_score(NULL)
      selected_mood_label(NULL)
      
      status_msg_ui(paste0("Reverted today's answer for ", format(bd, "%d-%m-%Y"), ". You can answer again now."))
      blocked_date(NULL)
      page("questions")
    }, error = function(e) {
      status_msg_ui(paste("Could not revert:", conditionMessage(e)))
    })
  })
  
  #### TREND ####
  output$mood_plot <- renderPlot({
    req(mode())
    df <- read_log_any()
    if (nrow(df) == 0) return(NULL)
    
    df_daily <- df %>%
      group_by(date) %>%
      summarise(avg_mood = mean(mood, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_daily, aes(date, avg_mood)) +
      geom_point(size = 3) +
      geom_line() +
      scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
      labs(x = NULL, y = "Average mood") +
      theme_minimal(base_size = 14)
  })
  
  #### CALENDAR (with weekdays) ####
  output$calendar_plot <- renderPlot({
    req(mode())
    df <- read_log_any()
    
    df_daily <- df %>%
      group_by(date) %>%
      summarise(avg_mood = mean(mood, na.rm = TRUE), .groups = "drop")
    
    get_col <- function(m) {
      if (is.na(m)) return("grey95")
      if (m <= 1.5) return("#d32f2f")
      if (m <= 2.5) return("#f57c00")
      if (m <= 3.5) return("#fbc02d")
      if (m <= 4.5) return("#7cb342")
      return("#2e7d32")
    }
    
    draw_month <- function(year, month) {
      first_day <- as.Date(sprintf("%04d-%02d-01", year, month))
      last_day  <- ceiling_date(first_day, "month") - days(1)
      days_seq  <- seq(first_day, last_day, by = "day")
      
      wd <- wday(first_day, week_start = 1)
      n_days <- length(days_seq)
      n_cells <- (wd - 1) + n_days
      n_rows <- ceiling(n_cells / 7)
      
      plot.new()
      plot.window(xlim = c(0.5, 7.5), ylim = c(0.5, n_rows + 1.6))
      title(main = paste(month.name[month], year), cex.main = 1.25)
      
      weekday_labels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      for (c in 1:7) {
        text(c, n_rows + 1.2, weekday_labels[c], cex = 1.25, font = 2)
      }
      
      for (i in seq_len(n_days)) {
        day_date <- days_seq[i]
        cell_index <- (wd - 1) + i
        row <- n_rows - floor((cell_index - 1) / 7)
        col <- ((cell_index - 1) %% 7) + 1
        
        val <- df_daily$avg_mood[df_daily$date == day_date]
        val <- if (length(val) == 0) NA_real_ else val[1]
        col_fill <- get_col(val)
        
        rect(col - 0.5, row - 0.5, col + 0.5, row + 0.5, col = col_fill, border = "grey85")
        text(col, row, format(day_date, "%d"), cex = 1.25)
      }
    }
    
    par(mfrow = c(4, 3), mar = c(1, 1, 2, 1))
    for (m in 1:12) draw_month(2026, m)
  })
  
  #### DOWNLOADS ####
  output$download_csv <- downloadHandler(
    filename = function() paste0("sanity-log-", if (mode() == "owner") "owner" else "guest", ".csv"),
    contentType = "text/csv",
    content = function(file) {
      df <- read_log_any() %>%
        arrange(date, time) %>%
        mutate(user_id = current_user_id()) %>%
        select(user_id, everything())
      
      write.csv(df, file, row.names = FALSE, na = "")
    }
  )
  
  output$download_json <- downloadHandler(
    filename = function() paste0("sanity-log-", if (mode() == "owner") "owner" else "guest", ".json"),
    contentType = "application/json",
    content = function(file) {
      df <- read_log_any() %>% arrange(date, time)
      
      out <- df %>%
        transmute(
          user_id = current_user_id(),
          date    = format(date, "%d-%m-%Y"),
          time    = time,
          mood    = mood,
          label   = label,
          stress  = stress,
          note    = note
        )
      
      jsonlite::write_json(out, file, pretty = TRUE, auto_unbox = TRUE, na = "null")
    }
  )
}

shinyApp(ui, server)


