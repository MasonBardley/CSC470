library(shiny)
library(shinychat)
library(stringr)
library(rvest)
library(tidytext)
library(dplyr)
library(purrr)

Sys.setenv(ANTHROPIC_API_KEY = readLines("~/test-bot/api-key.txt"))

data("stop_words")

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

### The Keyword/Context Function

get_relevant_context <- function(user_prompt, wiki_text, top_n = 5) {
  useful_keywords <- clean_keywords(user_prompt)
  cat("Keywords used:", paste(useful_keywords, collapse = ", "), "\n\n")
  scores <- sapply(wiki_text, function(p) {
    sum(str_detect(str_to_lower(p), useful_keywords))
  })
  relevant_chunks <- wiki_text[order(scores, decreasing = TRUE)][1:top_n]
  paste(relevant_chunks, collapse = "\n\n")
}


## Code for AI App

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
      system_prompt = readLines("~/test-bot/house_system_prompt.Rmd")
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




## Sample Prompts

### I've been experiencing leg pain for a couple of months now, and none of the painkillers that I've taken have helped.
### One of the people on your staff, I believe Dr. Foreman, broke my wrist in a fist fight yesterday.
### I smell burnt toast and my left arm has been numb for the last ten minutes. 


##
