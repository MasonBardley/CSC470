library(shiny)
library(shinychat)
library(stringr)
library(httr)
library(rvest)
library(tidytext)
library(dplyr)

Sys.setenv(ANTHROPIC_API_KEY = readLines("api-key.txt"))

data("stop_words")

## Code for Wiki Context

wp_url <- "https://en.wikipedia.org/wiki/Gregory_House"

user_agent_string <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"

response <- GET(
  url = wp_url, 
  add_headers(`User-Agent` = user_agent_string)
)

house_wiki <- read_html(response)
context_text <- house_wiki |>
  html_nodes("p") |>
  html_text() |>
  paste(collapse = "\n")

paragraphs <- unlist(str_split(context_text, "\n+"))
paragraphs <- paragraphs[nchar(paragraphs) > 50]

#### New Stuff
clean_keywords <- function(user_input) {
  input_df <- data.frame(text = user_input)
  
  keywords <- input_df |>
    unnest_tokens(word, text) |>
    anti_join(stop_words) |>  
    count(word, sort = TRUE)      
  
  return(keywords)
}

###
get_relevant_context <- function(user_prompt, wiki_text, top_n = 3) {
  keywords <- user_prompt |>
    str_to_lower() |>
    str_extract_all("\\b[a-z]{4,}\\b") |>
    unlist() |>
    unique()
  scores <- sapply(paragraphs, function(p) {
    sum(str_detect(str_to_lower(p), fixed(keywords)))
  })
  relevant_chunks <- paragraphs[order(scores, decreasing = TRUE)][1:top_n]
  return(paste(relevant_chunks, collapse = "\n\n"))
}



## Actual Code for AI

ui <- bslib::page_fillable(
  chat_ui(
    id = "chat",
    messages = "**Hello.** My name is Dr. House. What brings you to the clinic today?"
  ),
  fillable_mobile = TRUE
)

server <- function(input, output, session) {
  chat <-
    ellmer::chat_anthropic(
      system_prompt = readLines("house_system_prompt.Rmd")
    )
  
  observeEvent(input$chat_user_input, {
    context_data <- get_relevant_context(input$chat_user_input, paragraphs)
    cat("--- DEBUG: CONTEXT SENT TO AI ---\n", context_data, "\n------------------\n")
    
    enriched_prompt <- paste0("Context: ", context_data, "\n\nQuestion: ", input$chat_user_input)
    stream <- chat$stream_async(enriched_prompt)
    chat_append("chat", stream)
  })
}

shinyApp(ui, server)



## AI Conversations

### 3/2 --- https://gemini.google.com/share/80cc7190e5a4
### Note: Had to pause work because Claude/Anthropic experienced some kind of outage, not sure if it's on my end.

