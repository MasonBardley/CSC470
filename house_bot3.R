library(shiny)
library(shinychat)
library(stringr)
library(rvest)
library(tidytext)
library(dplyr)
library(purrr)
library(bslib)

Sys.setenv(ANTHROPIC_API_KEY = readLines("api-key.txt"))

data("stop_words")

# NOTE: THIS VERSION WAS MADE AND EDITED ENTIRELY BY POSIT ASSISTANT
# I'm currently uploading this for others to study. 

# ============================================================
# DEVELOPER FLAG: set to TRUE to enable Anthropic's extended
# thinking feature. Requires a model that supports it (see
# EXTENDED_THINKING_MODEL below). When FALSE, the bot streams
# responses normally using the default ellmer model.
# ============================================================
USE_EXTENDED_THINKING <- TRUE

# Model used only when USE_EXTENDED_THINKING = TRUE.
# claude-sonnet-4-5 is current (May 2026) and supports extended
# thinking with type = "enabled" and a token budget.
EXTENDED_THINKING_MODEL  <- "claude-sonnet-4-5-20250929"
EXTENDED_THINKING_BUDGET <- 10000  # thinking tokens; must be < max_tokens

### Token Budget Code

check_token_budget <- function(prompt_text, max_tokens = 300) {
  word_count <- str_count(prompt_text, "\\w+")
  estimated_tokens <- word_count * 1.3
  if (estimated_tokens > max_tokens) {
    warning("Prompt exceeds budget! Truncating...")
    return(substr(prompt_text, 1, max_tokens * 3))
  }
  return(prompt_text)
}

### Prompt Keyword Cleaner

clean_keywords <- function(user_input) {
  input_df <- data.frame(text = user_input)
  keywords <- input_df |>
    unnest_tokens(word, text) |>
    anti_join(stop_words) |>
    count(word, sort = TRUE)
  return(keywords$word)
}

### Code for Wiki Context

urls <- paste0("https://en.wikipedia.org/wiki/House_season_", 1:8)
all_paragraphs <- map(urls, \(url) {
  read_html(url) |>
    html_nodes("p") |>
    html_text()
})
context_text <- unlist(all_paragraphs) |>
  paste(collapse = "\n")
paragraphs <- unlist(str_split(context_text, "\n+"))

### Keyword/Context function
# Returns a list so the caller can log keywords and context separately.

get_relevant_context <- function(user_prompt, wiki_text, top_n = 5) {
  useful_keywords <- clean_keywords(user_prompt)
  scores <- sapply(wiki_text, function(p) {
    sum(str_detect(str_to_lower(p), useful_keywords))
  })
  relevant_chunks <- wiki_text[order(scores, decreasing = TRUE)][1:top_n]
  list(
    keywords = useful_keywords,
    context  = paste(relevant_chunks, collapse = "\n\n")
  )
}

### Helper: render one thinking-log entry as an HTML card

build_thinking_card <- function(entry) {
  tags$div(
    class = "thinking-entry border rounded p-3 mb-3 bg-light",
    style = "font-size: 0.82rem; font-family: monospace;",
    tags$p(tags$strong("Turn:"), entry$turn),
    tags$p(tags$strong("User input:"), entry$user_input),
    if (!is.null(entry$keywords))
      tags$p(tags$strong("Keywords extracted:"), entry$keywords),
    if (!is.null(entry$context_preview))
      tags$details(
        tags$summary(tags$strong("Context retrieved (first 400 chars)")),
        tags$pre(entry$context_preview)
      ),
    tags$details(
      tags$summary(tags$strong("Full prompt sent to API")),
      tags$pre(entry$prompt_sent)
    ),
    if (!is.null(entry$model_thinking))
      tags$details(
        tags$summary(tags$strong("Model extended thinking")),
        tags$pre(style = "white-space: pre-wrap;", entry$model_thinking)
      )
  )
}


## UI

ui <- bslib::page_fillable(
  uiOutput("main_screen"),
  fillable_mobile = TRUE
)

## Server

server <- function(input, output, session) {
  rv <- reactiveValues(
    mode         = NULL,
    secret_case  = NULL,
    chat         = NULL,
    thinking_log = list(),
    turn_counter = 0L
  )
  selected_case <- sample(paragraphs, 1)
  
  
  
  # ---- Main UI ----
  output$main_screen <- renderUI({
    if (is.null(rv$mode)) {
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Patient Clinic"),
          p("Talk to House about your symptoms."),
          actionButton("btn_patient", "I am a Patient", class = "btn-primary")
        ),
        card(
          card_header("Diagnosis Game"),
          p("Try to solve a case while House mocks you."),
          actionButton("btn_doctor", "I am a Doctor", class = "btn-danger")
        )
      )
    } else {
      layout_sidebar(
        sidebar = sidebar(
          title    = "Developer Thinking Log",
          width    = 420,
          position = "right",
          open     = TRUE,
          uiOutput("thinking_log_ui")
        ),
        chat_ui(
          id = "chat",
          messages = if (rv$mode == "patient") {
            "**Hello.** My name is Dr. House. What brings you to the clinic today?"
          } else {
            "My name is Dr. House. I see we're letting anybody become a doctor these days. **Why don't you start by asking for the patient's symptoms?**"
          }
        )
      )
    }
  })
  
  # ---- Thinking log panel ----
  output$thinking_log_ui <- renderUI({
    if (length(rv$thinking_log) == 0) {
      return(p(class = "text-muted", "No turns yet. Send a message to see reasoning steps here."))
    }
    # Most recent turn first
    tagList(rev(rv$thinking_log) |> map(build_thinking_card))
  })
  
  # ---- Mode buttons ----
  observeEvent(input$btn_patient, {
    rv$mode <- "patient"
    if (USE_EXTENDED_THINKING) {
      rv$chat <- ellmer::chat_anthropic(
        model      = EXTENDED_THINKING_MODEL,
        # max_tokens must exceed budget_tokens to leave room for the actual reply.
        # The thinking tokens are drawn from this same pool.
        api_args   = list(
          max_tokens = EXTENDED_THINKING_BUDGET + 4000,
          thinking   = list(type = "enabled", budget_tokens = EXTENDED_THINKING_BUDGET)
        ),
        system_prompt = readLines("house_system_prompt.Rmd")
      )
    } else {
      rv$chat <- ellmer::chat_anthropic(
        system_prompt = readLines("house_system_prompt.Rmd")
      )
    }
  })
  
  observeEvent(input$btn_doctor, {
    rv$mode <- "game"
    rv$secret_case <- selected_case
    rv$chat <- ellmer::chat_anthropic(
      system_prompt = readLines("game_system_prompt.Rmd")
    )
    cat("---DEBUG: SECRET_CASE ---\n", selected_case, "\n---\n")
  })
  
  # ---- Handle chat messages ----
  observeEvent(input$chat_user_input, {
    rv$turn_counter <- rv$turn_counter + 1L
    turn <- rv$turn_counter
    
    if (rv$mode == "patient") {
      result          <- get_relevant_context(input$chat_user_input, paragraphs)
      keywords        <- result$keywords
      context_data    <- result$context
      enriched_prompt <- paste0(
        "Context:\n", context_data,
        "\n\nPatient message: ", input$chat_user_input,
        if (USE_EXTENDED_THINKING) paste0(
          "\n\n[INTERNAL REASONING DIRECTIVE — do not include this analysis in your reply]",
          "\nBefore writing your response, work through the following in your thinking:",
          "\n1. List every plausible diagnosis consistent with the symptoms, from mundane to rare.",
          "\n2. Note what additional information would confirm or rule out each one.",
          "\n3. Identify any red-flag symptoms that would warrant urgent care.",
          "\n4. Decide which diagnosis to anchor on for your House-style response, and why.",
          "\nOnly after completing this reasoning should you write your in-character reply."
        ) else ""
      )
      
      # Store the pipeline's reasoning before sending to the API
      rv$thinking_log <- c(rv$thinking_log, list(list(
        turn            = turn,
        user_input      = input$chat_user_input,
        keywords        = paste(keywords, collapse = ", "),
        context_preview = substr(context_data, 1, 400),
        prompt_sent     = enriched_prompt,
        model_thinking  = NULL
      )))
      
      if (USE_EXTENDED_THINKING) {
        # ---- Extended thinking path ----
        # Use $chat() (synchronous, non-streaming) because streaming +
        # extended thinking has limited ellmer support.
        #
        # $chat() returns the assistant's text reply. Internally, ellmer
        # stores the full Turn object (including thinking blocks) so we
        # can retrieve it immediately via $last_turn().
        response_text <- rv$chat$chat(enriched_prompt)
        
        # last_turn() returns an ellmer Turn object. Its @content slot is
        # a list of Content objects. When extended thinking is on, the API
        # adds a ContentThinking block BEFORE the normal ContentText block.
        # We filter for any block whose class is "ContentThinking" and pull
        # its @thinking character field — that is the model's raw scratchpad.
        raw_turn <- rv$chat$last_turn()
        thinking_text <- tryCatch({
          # @contents (plural) is the correct S7 property name on AssistantTurn.
          # S7_inherits() is required for S7 class checking — base inherits() won't work.
          blocks          <- raw_turn@contents
          thinking_blocks <- Filter(\(b) S7::S7_inherits(b, ellmer::ContentThinking), blocks)
          if (length(thinking_blocks) > 0) thinking_blocks[[1]]@thinking else NULL
        }, error = \(e) NULL)
        
        # Update the log entry we created above with the model's thinking
        idx <- length(rv$thinking_log)
        rv$thinking_log[[idx]]$model_thinking <- thinking_text
        
        # Send the plain text reply into the chat UI
        chat_append("chat", response_text)
        
      } else {
        # ---- Streaming path (default) ----
        stream <- rv$chat$stream_async(enriched_prompt)
        chat_append("chat", stream)
      }
      
    } else {
      # Doctor / game mode — no RAG, always stream
      enriched_prompt <- paste0(
        "Selected Case: ", selected_case,
        "\n\nPrompt: ", input$chat_user_input
      )
      rv$thinking_log <- c(rv$thinking_log, list(list(
        turn            = turn,
        user_input      = input$chat_user_input,
        keywords        = NULL,
        context_preview = NULL,
        prompt_sent     = enriched_prompt,
        model_thinking  = NULL
      )))
      stream <- rv$chat$stream_async(enriched_prompt)
      chat_append("chat", stream)
    }
  })
}

shinyApp(ui, server)


## AI Conversations

### 3/2 --- https://gemini.google.com/share/80cc7190e5a4

## 4/2 --- https://gemini.google.com/share/ef99fbb25b52

## 5/3 --- https://gemini.google.com/share/37cbf7a0b641
### Help understanding Posit Assistant's changes with extended thinking

## Sample Prompts

### I've been experiencing leg pain for a couple of months now, and none of the painkillers that I've taken have helped.
### One of the people on your staff, I believe Dr. Foreman, broke my wrist in a fist fight yesterday.
### I smell burnt toast and my left arm has been numb for the last ten minutes.
