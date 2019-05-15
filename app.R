library(shiny)
library(feather)
library(dplyr)

if (file.exists("rings.fth")) {
  rings <- read_feather("rings.fth")
} else {
  
  library(tidyverse)
  
  files <- list.files("rings")
  rings <- tibble(files) %>% 
    mutate(path = "./rings")
  
  files_specials <- list.files("specials")
  specials <- tibble(files = files_specials) %>% 
    mutate(path = "./specials")
  
  rings <- bind_rows(rings, specials)
  
  rings <- rings %>% 
    mutate(name = str_sub(files, start = 1, end = -5)) %>% 
    separate(name, into = c("name", "variant"), sep = "\\(") %>% 
    mutate(name = str_trim(name),
           variant = variant %>% str_remove("\\)") %>% as.integer(),
           id = row_number())
  rings %>% write_feather("rings.fth")
}

nrings <- nrow(rings)

randomid <- function(n, easteregg_prob = 0.00) {
  if (runif(1) < easteregg_prob) {
    r <- c(n-1, n)
  } else {
    r <- sample(1:(n-2), 2)
  }
  return(r)
}

sessionfile <- paste0("session", as.integer(Sys.time()), ".csv")
# sessionfile <- "session_responses.csv"

tblheader <- matrix(c("round", "id", "name", "file", "choice", "tdiff"), nrow = 1) #%>% as_tibble(rownames = NA)

write.table(tblheader, file = sessionfile, sep = ",", col.names = F)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Dancing+Script');
      
      h1 {
        font-family: 'Dancing Script', cursive;
        font-weight: 500;
        line-height: 5;
        color: #000000;
      }
    "))
  ),
  
  fluidRow(column(12, h1("If you had to choose one...")), align = "center"),
  br(), 
  fluidRow(
    column(6, class = "col-xs-6  col-md-6 col-lg-6", 
           imageOutput("image1", click = "first"), 
           actionButton("superfirst", "superlike", icon = icon("grin-stars"),
                        style="color: #fff; background-color: #5f9fff"),
           align = "right"),
    column(6, class = "col-xs-6 col-md-6 col-lg-6",
           imageOutput("image2", click = "second"),
           actionButton("supersecond", "superlike", icon = icon("grin-stars"),
                        style="color: #fff; background-color: #5f9fff"),
           align = "left")
  ),

  br(), br(),
  fluidRow(
    column(10, actionButton("none", "none of these please", icon = icon("child")), align = "right")
  )
)

server <- function(input, output, session) {
  
  
  # counter for knowing in which round we are, i.e. which images were displayed together
  counter <- reactiveValues(count = 1)
  rimage <- reactiveValues(r = sample(1:(nrings-2), 2))
  choice <- reactiveValues(c = c(0, 0))
  t0 <- reactiveValues(t = Sys.time())
  
  # normal likes
  observeEvent(input$first, {
    
    choice$c <- c(1, -1)
    
    temptbl <- tibble(round = rep(counter$count, 2),
                      id = rimage$r,
                      image = rings[rimage$r, "name"] %>% unlist(),
                      file = rings[rimage$r, "files"] %>% unlist(),
                      choice = choice$c,
                      tdiff = as.integer(difftime(Sys.time(), t0$t, units = "secs")))
    # print(temptbl)
    write.table(temptbl, sessionfile, sep = ",", col.names = F, append = T)
    
    counter$count <- counter$count + 1
    # rimage$r <- sample(1:nrings, 2)
    rimage$r <- randomid(nrings)
    t0$t <- Sys.time()
  })
  
  observeEvent(input$second, {
    
    choice$c <- c(-1, 1)
    
    temptbl <- tibble(round = rep(counter$count, 2),
                      id = rimage$r,
                      image = rings[rimage$r, "name"] %>% unlist(),
                      file = rings[rimage$r, "files"] %>% unlist(),
                      choice = choice$c,
                      tdiff = as.integer(difftime(Sys.time(), t0$t, units = "secs")))
    # print(temptbl)
    write.table(temptbl, sessionfile, sep = ",", col.names = F, append = T)
    
    counter$count <- counter$count + 1
    rimage$r <- randomid(nrings)
    t0$t <- Sys.time()
  })
  
  # super likes
  observeEvent(input$superfirst, {
    
    choice$c <- c(2, 0)
    
    temptbl <- tibble(round = rep(counter$count, 2),
                      id = rimage$r,
                      image = rings[rimage$r, "name"] %>% unlist(),
                      file = rings[rimage$r, "files"] %>% unlist(),
                      choice = choice$c,
                      tdiff = as.integer(difftime(Sys.time(), t0$t, units = "secs")))
    # print(temptbl)
    write.table(temptbl, sessionfile, sep = ",", col.names = F, append = T)
    
    counter$count <- counter$count + 1
    rimage$r <- randomid(nrings)
    t0$t <- Sys.time()
  })
  
  observeEvent(input$supersecond, {
    
    choice$c <- c(0, 2)
    
    temptbl <- tibble(round = rep(counter$count, 2),
                      id = rimage$r,
                      image = rings[rimage$r, "name"] %>% unlist(),
                      file = rings[rimage$r, "files"] %>% unlist(),
                      choice = choice$c,
                      tdiff = as.integer(difftime(Sys.time(), t0$t, units = "secs")))
    # print(temptbl)
    write.table(temptbl, sessionfile, sep = ",", col.names = F, append = T)
    
    counter$count <- counter$count + 1
    rimage$r <- randomid(nrings)
    t0$t <- Sys.time()
  })
  
  observeEvent(input$none, {
    
    choice$c <- c(-2, -2)

    temptbl <- tibble(round = rep(counter$count, 2),
                      id = rimage$r,
                      image = rings[rimage$r, "name"] %>% unlist(),
                      file = rings[rimage$r, "files"] %>% unlist(),
                      choice = choice$c,
                      tdiff = as.integer(difftime(Sys.time(), t0$t, units = "secs")))
    # print(temptbl)
    write.table(temptbl, sessionfile, sep = ",", col.names = F, append = T)

    counter$count <- counter$count + 1
    rimage$r <- randomid(nrings)
    t0$t <- Sys.time()
  })

  
  output$image1 <- renderImage({
    
    # path1 <- normalizePath(file.path('./rings', rings[rimage$r[1], "files"]))
    path1 <- normalizePath(file.path(rings[rimage$r[1], "path"], 
                                     rings[rimage$r[1], "files"]))
    # path1 <- rings[rimage$r[1], "path"]
    
    # Return a list containing the filename and alt text
    list(src = path1,
         alt = "alt")
    
  }, deleteFile = FALSE)
  
  
  output$image2 <- renderImage({
    
    path2 <- normalizePath(file.path(rings[rimage$r[2], "path"], 
                                     rings[rimage$r[2], "files"]))

    list(src = path2,
         alt = "alt")

  }, deleteFile = FALSE)
}


shinyApp(ui, server)