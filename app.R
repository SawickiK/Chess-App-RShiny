library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(shinycssloaders)
library(shiny)
library(shiny.semantic)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringr)
library(forcats)
library(gridExtra)
library(maps)
library(sysfonts)
library(countrycode)

# options(semantic.themes = TRUE)
# options(shiny.custom.semantic = "www/")

font_add_google("Roboto")

CREATORS <- c(
  "Krzysztof Sawicki",
  "Jakub Grzywaczewski",
  "Kacper Wnek"
)

dfMoveQuality <- read.csv("./resources/MoveQuality.csv")
mapdata <- read.csv("./resources/WorldStats.csv")
dfWinRate <- read.csv("./resources/WinRate.csv")
dfGamesData <- read.csv("./resources/GamesData.csv")
dfFIDEData <- read.csv("./resources/players_in_chess.csv")

dfGamesData <- dfGamesData %>%
  mutate(date = as.Date(date)) %>%
  mutate(endHour = substring(endHour, 1, nchar(endHour) - 3)) %>%
  mutate(endHour = as.numeric(gsub(":", "\\.", endHour)))

dfFIDE <- dfFIDEData %>%
  group_by(country) %>%
  summarise(
    average_rating = mean(rating, na.rm = TRUE),
    max_rating = max(rating, na.rm = TRUE),
    number_of_fide_players = n()
  ) %>%
  left_join(dfFIDEData[c("name", "country", "rating")], by = c("country" = "country", "max_rating" = "rating")) %>%
  mutate(
    full_country_name = countrycode(country, origin = 'iso3c', destination = 'country.name'),
    .keep = "all"
  )


# HEADER
header <- dashboardHeader(
  includeCSS("./www/header.css"),
  class = "dsHeader",
  logo_path = "logo.png",
  logo_align = "center",
  title = "Chess ExploRer",
  right = div(
    id = "creator-container",
    textOutput("creator")
  )
)

# SIDEBAR
sidebar <- dashboardSidebar(
  size = "thin",
  color = "brown",
  sidebarMenu(
    includeCSS("./www/sidebar.css"),
    menuItem(text = "Home",
             tabName = "home",
             icon = icon("home")),
    menuItem(text = "Competition",
             tabName = "comp",
             icon = icon("trophy")),
    menuItem(text = "Map",
             tabName = "map",
             icon = icon("map")),
    menuItem(text = "Games",
             tabName = "games",
             icon = icon("chess board",
                         lib = "font-awesome")),
    menuItem(text = "Reporitory",
             href = "https://github.com/KacWNK/TWD-ChessProject",
             icon = icon("github"))
  )
)

###### HOME TAB ###### TODO

### FAQ
faq <- list(
  list(
    title = "Why was this project created?",
    content = p(class = "faq-content",
                "There once was a assigment worth 1/4 of your grade so we decided to make it awesome"
    )
  ),
  list(
    title = "Where did we get the data?",
    content = p("Data was collected or scarped from www.chess.com and www.kaggle.com")
  ),
  list(
    title = "What was our inspiration?",
    content = p("To create a progress tracker for our chess journey")
  ),
  list(
    title = "How long it took?",
    content = p("About 30 work hours spend on research and app development")
  )
)

homeTab <- semanticPage(
  title = "Start page",
  div(
    class = "ui grid full-span",
    div(class = "row h-70",
        div(class = "four wide column",
            div(class = "ui statistics",
                div(class = "statistic",
                    div(class = "value text-orange", 2),
                    div(class = "label text-white", "Player game")
                ),
                div(class = "statistic",
                    div(class = "value text-orange", 20),
                    div(class = "label text-white", "Starting moves")
                ),
                div(class = "statistic",
                    div(class = "value text-orange", 32),
                    div(class = "label text-white", "Chess pieces")
                ),
                div(class = "statistic",
                    div(class = "value text-orange", "10 Milion Milion"),
                    div(class = "label text-white", "Games Played")
                ),
                div(class = "statistic",
                    div(class = "value text-orange", span(
                      "Around 10", tags$sup("111")
                    )),
                    div(class = "label text-white", "Possible Chess Positions")
                )
            )
        ),
        div(class = "twelve wide column center",
            shinycssloaders::withSpinner(
              plotlyOutput(outputId = "mapFIDE"),
              type = 4,
              color = "#f9a03f"
            )
        ),
    ),
    div(class = "row h-30",
        div(class = "faq",
            accordion(faq,
                      fluid = TRUE,
                      styled = FALSE
            )
        )
    )
  )
)


###### MAP TAB ######
mapTab <- semanticPage(
  title = "Map",
  div(class = "ui grid full-span",
      div(class = "row",
          multiple_radio(
            "fill_var", "Select type: ",
            choices = c("Win Ratio", "Average Accuracy"),
            choices_value = c("WinP", "Accuracy"),
            position = "inline"
          )
      ),
      div(class = "two column row",
          div(
            class = "column",
            shinycssloaders::withSpinner(
              plotlyOutput(outputId = "mapKacper"),
              type = 4,
              color = "#f9a03f"
            )
          ),
          div(class = "column",
              shinycssloaders::withSpinner(
                plotlyOutput(outputId = "mapKrzysiek"),
                type = 4,
                color = "#f9a03f"
              ),
          )
      )
  )
)

###### GAMES TAB #######
gamesTab <- semanticPage(
  tilte = "Games",
  div(class = "ui grid",
      div(class = "row",
          selectInput(
            "gif", "Select a gif:",
            choices = c(
              "Best game- Kacper(white)" = "./resources/KW_immortalGame.gif",
              "First game- Kacper(white)" = "./resources/KacperPierwszaPartia.gif",
              "Best game- Krzysiek(white)" = "./resources/Kristof_Immortal.gif",
              "First game- Krzysiek(white)" = "./resources/Krzysiu_pierwszaPartia.gif"
            ))
      ),
      div(class = "row",
          shinycssloaders::withSpinner(
            imageOutput("selected_gif"),
            type = 4,
            color = "#f9a03f"
          )
      )
  )
)

###### COMPETITION TAB #######
panel_style <- "20px; border: solid 2px #dfdede; border-radius: 10px;"
### MAIN PLOT 2 - MOVE QUALITY
compTabRow2 <- div(
  class = "row clear-bg",
  div(class = "ten wide column",
      sidebar_layout(
        sidebar_panel(
          multiple_radio(
            "colorMoveQuality",
            "Choose color of pieces",
            selected = unique(dfMoveQuality$Color)[1],
            choices = c("White", "Black", "White and black"),
            choices_value = unique(dfMoveQuality$Color)
          ),
          width = 2
        ),
        main_panel(
          shinycssloaders::withSpinner(
            plotlyOutput("moveQualityPlot"),
            type = 4,
            color = "#f9a03f"
          ),
          width = 10
        ),
        container_style = panel_style
      )
  ),
  div(class = "four wide column",
      div(id = "winRate",
          style = panel_style,
          shinycssloaders::withSpinner(
            plotlyOutput("winRatePlot"),
            type = 4,
            color = "#f9a03f"
          )
      )
  )
)

### MAIN PLOT 3 - ELO
compTabRow3 <- div(
  class = "row clear-bg",
  sidebar_layout(
    sidebar_panel(
      uiOutput("timeLagElo"),
      width = 2
    ),
    main_panel(
      shinycssloaders::withSpinner(
        plotOutput("eloPlot"),
        type = 4,
        color = "#f9a03f"
      ),
      width = 10
    ),
    container_style = panel_style
  )
)

### MAIN PLOTS COMBINED
compTab <- semanticPage(
  title = "Competition page",
  div(class = "ui grid",
      div(class = "row",
          div(class = "four wide column",
              multiple_radio(
                "playerComp",
                "Select a player:",
                selected = unique(dfMoveQuality$Player)[1],
                choices = unique(dfMoveQuality$Player),
                position = "inline"
              ),
          ),
          div(class = "four wide column",
              multiple_radio(
                "timeControlComp",
                "Choose time control:",
                selected = unique(dfMoveQuality$Type)[1],
                choices = c("Bullet", "Blitz", "Rapid"),
                choices_value = unique(dfMoveQuality$Type),
                position = "inline"
              )
          )
      ),
      compTabRow3,
      compTabRow2,
      div(class = "row",
          style = panel_style,
          plotOutput("densPlot")
      )
  )
)

# BODY
body <- dashboardBody(class = "dsBody", tabItems(
  includeCSS("./www/body.css"),
  tabItem(tabName = "home", homeTab),
  tabItem(tabName = "comp", compTab),
  tabItem(tabName = "map", mapTab),
  tabItem(tabName = "games", gamesTab)
))
# RUN APP
shinyUI <-(dashboardPage(
  title = "ChessExploRer",
  header, sidebar, body,
  theme = "slate",
  class = "dsBodyOuter",
  sidebar_and_body_container_class = "dsPage"
))






shinyServer <- (function(input, output) {
  # Change creator
  output$creator <- renderText(CREATORS[index])
  update_creator <- function(index, interval = 2) {
    index <- index %% 3 + 1
    output$creator <- renderText(CREATORS[index])
    later::later(function() update_creator(index), interval)
  }
  update_creator(1)
  
  ## Home side map
  output$mapFIDE <- renderPlotly({
    fig <- plot_geo(dfFIDE) %>%
      add_trace(
        z = ~number_of_fide_players,
        color = ~number_of_fide_players,
        colors = "Oranges",
        text = paste(
          "Country:", dfFIDE$full_country_name,
          "<br>Best Player:", dfFIDE$name,
          "<br>Max FIDE Rating:", dfFIDE$max_rating,
          "<br>Average Country Rating:", round(dfFIDE$average_rating, digits = 2)
        ),
        locations = ~country
      ) %>%
      colorbar(
        title = list(
          text = "Number of FIDE players",
          font = list(color = "white")
        ),
        tickfont = list(size = 14, color = "white")
      ) %>%
      layout(
        height = 800,
        paper_bgcolor = "rgba(0,0,0,0)",
        title = list(
          y = 0.99,
          text = "Number of FIDE players per country",
          font = list(
            color = "white",
            family = "Roboto",
            size = 30
          )
        ),
        legend = list(
          font = list(
            color = "white",
            family = "Roboto",
            size = 14
          )
        ),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          showland = TRUE,
          landcolor = "#312e2b",
          showocean = FALSE,
          projection = list(type = "Mercator"),
          bgcolor = "transparent"
        )
      )
    ggplotly(fig)
  })
  
  
  ## Map for kacper
  output$mapKacper <- renderPlotly({
    fig <- mapdata[mapdata$Player == "Kacper", ] %>%
      ggplot(aes_string(x = "long", y = "lat", group = "group", fill = input$fill_var)) +
      geom_polygon(
        color = "white",
        size = 0.2,
        aes(tooltip = Country)
      ) +
      scale_fill_gradient(
        limits = c(0, 100),
        name = ifelse(input$fill_var == "WinP", "Win Ratio (%)", "Average Accuracy"),
        low = ifelse(input$fill_var == "WinP", "#7ed036", "#532808"),
        high = ifelse(input$fill_var == "WinP", "#00441b", "#f3cf30"),
        na.value = ifelse(input$fill_var == "WinP", "white", "white")
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 14, colour = "white"),
        legend.title = element_text(size = 12, colour = "white"),
        legend.text = element_text(size = 10, colour = "white"),
        rect = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)
      ) +
      labs(title = paste("Player Kacper vs World"))
    ggplotly(fig) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  ## Map for krzysiek
  output$mapKrzysiek <- renderPlotly({
    fig <- mapdata[mapdata$Player == "Krzysiek", ] %>%
      ggplot(aes_string(x = "long", y = "lat", group = "group", fill = input$fill_var)) +
      geom_polygon(
        color = "white",
        size = 0.2,
        aes(tooltip = Country)
      ) +
      scale_fill_gradient(
        limits = c(0, 100),
        name = ifelse(input$fill_var == "WinP", "Win Ratio (%)", "Average Accuracy"),
        low = ifelse(input$fill_var == "WinP", "#7ed036", "#532808"),
        high = ifelse(input$fill_var == "WinP", "#00441b", "#f3cf30"),
        na.value = ifelse(input$fill_var == "WinP", "white", "white")
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 14, colour = "white"),
        legend.title = element_text(size = 12, colour = "white"),
        legend.text = element_text(size = 10, colour = "white"),
        rect = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
      labs(title = "Player Krzysiek vs World")
    ggplotly(fig) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  
  # ELo change plot
  output$eloPlot <- renderPlot({
    df2 <- dfGamesData %>% filter(
      date >= input$date_from,
      timeControl %in% str_to_title(input$timeControlComp),
      date <= input$date_to,
      player %in% input$playerComp
    )
    ggplot(data = df2, aes(x = date, y = yourElo)) +
      geom_line(
        stat = "smooth",
        color = "#f0a95e",
        method = "loess",
        se = TRUE,
        alpha = 0.3,
        linewidth = 10
      ) +
      geom_line(
        color = "#1bada6",
        size = 1.2
      ) +
      labs(
        x = "Date",
        y = "Rating points",
        title = "ELO change in time"
      ) +
      theme(
        title = element_text(size = 14, colour = "#dfdede"),
        axis.title.x = element_text(size = 14, colour = "white"),
        axis.title.y = element_text(size = 14, colour = "white"),
        axis.text.x = element_text(size = 8, colour = "#dfdede"),
        axis.text.y = element_text(size = 8, colour = "#dfdede"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)
      )
  }, bg = "transparent")
  
  
  # Move quality bar plot
  output$moveQualityPlot <- renderPlotly({
    dfMoveQuality %>%
      filter(
        Type %in% input$timeControlComp,
        Color %in% input$colorMoveQuality,
        Player %in% input$playerComp
      ) %>%
      mutate(
        Move = fct_reorder(Move, Procent, .desc = FALSE)
      ) -> dfMoveQualityPlot
    
    ggplot(data = dfMoveQualityPlot, aes(x = Procent, y = Move, fill = MoveColor)) +
      geom_col() +
      labs(
        x = "Percent",
        y = "Move type",
        title = "Moves quality distribution"
      ) +
      scale_fill_manual(values = unique(dfMoveQualityPlot$MoveColor)) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, 35)
      ) +
      theme(legend.position = "none") +
      theme(
        title = element_text(size = 14, colour = "#dfdede"),
        axis.title.x = element_text(size = 14, colour = "white"),
        axis.title.y = element_text(size = 14, colour = "white"),
        axis.text.x = element_text(size = 10, colour = "#dfdede"),
        axis.text.y = element_text(size = 10, colour = "#dfdede"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)
      )
  })
  output$winRatePlot <- renderPlotly({
    dfWinRate %>%
      filter(
        Type %in% input$timeControlComp,
        Player %in% input$playerComp
      ) -> dfWinRatePlot
    
    fig <- plot_ly(
      dfWinRatePlot,
      x = ~Player,
      y = ~Matches,
      color = ~Result
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        title = list(
          y = 1,
          text = "Win Rate",
          font = list(
            color = "white",
            size = 20
          )
        ),
        legend = list(
          font = list(
            color = "white",
            size = 12
          )
        ),
        font = list(color = "white"),
        yaxis = list(
          title = paste(
            "# of games played in",
            str_to_title(input$timeControlComp)
          )
        ),
        xaxis = list(title = paste("Player")),
        barmode = "stack"
      )
    ggplotly(fig)
  })
  
  
  output$timeLagElo <- renderUI({
    dfGamesData %>% filter(
      player %in% input$playerComp,
      timeControl %in% str_to_title(input$timeControlComp)
    ) -> df3
    tagList(
      tags$div(
        tags$div(HTML("From")),
        date_input(
          "date_from",
          value = min(df3$date, na.rm = TRUE),
          min = min(df3$date, na.rm = TRUE),
          max = max(df3$date, na.rm = TRUE)
        )
      ),
      br(),
      tags$div(
        tags$div(HTML("To")),
        date_input(
          "date_to",
          value = max(df3$date, na.rm = TRUE),
          min = min(df3$date, na.rm = TRUE),
          max = max(df3$date, na.rm = TRUE))
      )
    )
  })
  
  output$densPlot <- renderPlot({
    ggplot(data = dfGamesData %>% filter(player == "Kacper"), aes(x = endHour)) +
      geom_density(fill = "#96bc4b") +
      labs(x = "Hours of a day", y = "Density", title = "Kacper - usual playing hours") +
      scale_x_continuous(limits = c(0, 24)) +
      scale_y_continuous(limits = c(0, 0.1)) +
      theme(
        title = element_text(size = 20, colour = "#dfdede"),
        axis.title.x = element_text(size = 14, colour = "white"),
        axis.title.y = element_text(size = 14, colour = "white"),
        axis.text.x = element_text(size = 10, colour = "white"),
        axis.text.y = element_text(size = 10, colour = "white"),
        legend.title = element_text(size = 14, colour = "white"),
        plot.title = element_text(size = 14, colour = "white"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)
      ) -> p1
    ggplot(data = dfGamesData %>% filter(player == "Krzysiek"), aes(x = endHour)) +
      geom_density(fill = "#96af8b") +
      labs(x = "Hours of a day", y = "Density", title = "Krzysiek - usual playing hours") +
      scale_x_continuous(limits = c(0, 24)) +
      scale_y_continuous(limits = c(0, 0.1)) +
      theme(
        title = element_text(size = 20, colour = "#dfdede"),
        axis.title.x = element_text(size = 14, colour = "white"),
        axis.title.y = element_text(size = 14, colour = "white"),
        axis.text.x = element_text(size = 10, colour = "white"),
        axis.text.y = element_text(size = 10, colour = "white"),
        plot.title = element_text(size = 14, colour = "white"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)
      ) -> p2
    grid.arrange(p1, p2, ncol = 2) + theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
  }, bg = "transparent")
  
  # Generating gifs
  output$selected_gif <- renderImage({
    list(src = input$gif)
  },
  deleteFile = FALSE
  )
})

# App
shinyApp(shinyUI,shinyServer)
