# library(jsonlite)
library(tidyverse)
library(ggrepel)
library(colorspace)
library(patchwork)
library(shiny)

homegame <- read_csv("games_old.csv")
talent2019 <- read_csv("talent2019.csv")
homegame <- homegame[,-1]
talent2019 <- talent2019[,-1]

conf <- unique(talent2019$conference)
wintype.lab <- c("Total", "Home", "Away")
names(wintype.lab) <- c("wrate_tol", "wrate_home", "wrate_away")

homegame$win_type <- factor(homegame$win_type, levels = c("wrate_tol", "wrate_home", 
                                                          "wrate_away"))

ui <- navbarPage(
  
    "College Football",
        
    tabPanel("How to Use",
             headerPanel(includeMarkdown("README.md")),
    ),
    tabPanel("Win Rate",
             
       sidebarPanel(
         sliderInput(inputId = "teams",
                     label = "Select top rankings to display",
                     min = 1, 
                     max = 5,
                     value = 5),
         checkboxInput(inputId = "flip",
                       label = "Flip Plot",
                       value = FALSE),
         helpText("Rankings are based on win rates over 2015 - 2019 season.",
                  "There may be ties in this data, so multiple columns will be displayed.",
                  "Blank columns indicate that a team was not in the top rankings for that win rate."),
         helpText("Dotted line represents the second highest team for that win rate.")
         # sliderInput("range", "Years to Examine", min = 2015, step = 1, value = c(2015, 2019), sep = "")
       ),
        mainPanel(
          # plotOutput(outputId = "plot", height = "700px", width = "700px"),
          plotOutput(outputId = "plot", height = "900px")
        )
    ),
    tabPanel("Pythagorean Wins",
         helpText("Pythagorean formula of wins calculates the approximate wins for a team given the points they scored and surrendered."),
         sliderInput(inputId = "pytha_teams",
                     label = "Select top rankings to display",
                     min = 1, 
                     max = 10,
                     value = 5),
         checkboxInput(inputId = "pytha_flip",
                       label = "Flip Plot",
                       value = FALSE),
         mainPanel(
          # plotOutput(outputId = "plot.pytha", height = "700px", width = "700px")
          plotOutput(outputId = "plot.pytha", height = "900px")
         )
    ),
    tabPanel("Talent vs. Wins",
             
      sidebarPanel(
        helpText("Explore how talent changes between different conferences in 2019",
                 "and how talent affects winning percentage."),
        checkboxGroupInput(inputId = "conferences",
                           label = "Select conferences",
                           choices = conf,
                           selected = c("SEC", "Big Ten", "Big 12", "Pac-12", "ACC")),
        checkboxInput(inputId = "facet",
                      label = "Separate conferences",
                      value = FALSE)
      ),
      mainPanel(
        # plotOutput(outputId = "talent.plot", height = "700px", width = "700px"),
        plotOutput(outputId = "talent.plot", height = "900px")
      )
    )
)

server <- function(input, output, session) {
  
  plot.teams <- reactive({
    homegame %>% 
      select(team, win_type, win_rate) %>% 
      group_by(win_type) %>% 
      top_n(n = input$teams, wt = win_rate) %>% 
      ungroup() %>% 
      select(team)
  })
  
  pytha.teams <- reactive({
    homegame %>% 
      select(team, pyhta_rate) %>% 
      unique() %>% 
      top_n(n = input$pytha_teams, wt = pyhta_rate) %>% 
      select(team)
  })
  
  output$plot <- renderPlot({
    
    if (input$flip) {
      
      q <- homegame %>%
           filter(team %in% as.vector(plot.teams()$team)) %>% 
           ggplot() +
           geom_bar(mapping = aes(x = reorder(team, -win_rate), y = win_rate, fill = team), stat = "identity") +
           geom_text(mapping = aes(x = team, y = win_rate, label = 100*round(win_rate, 3))) +
           coord_cartesian(ylim = c(0.5, 1)) +
           facet_grid(.~ win_type, scales = "free_x",
                      labeller = labeller(win_type = wintype.lab)) + 
           # geom_hline(data = ver_line, aes(yintercept = val), linetype = "dashed",
           #            colour = "red4") +
           coord_flip() +
           theme_minimal() +
           labs(title = "Overall Win Rates") +
           theme(
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "NULL",
             axis.text.x = element_text(angle = 90, size = 10),
             axis.text.y = element_text(family = "serif"),
             plot.title = element_text(size = 12, face = "bold"),
             plot.subtitle = element_text(size = 10, face = "italic")
           )
      
      q
      
    } else {
      
      q <- homegame %>%
           filter(team %in% as.vector(plot.teams()$team)) %>% 
           ggplot() +
           geom_bar(mapping = aes(x = reorder(team, -win_rate), y = win_rate, fill = team), stat = "identity") +
           geom_text(mapping = aes(x = team, y = win_rate, label = 100*round(win_rate, 3))) +
           coord_cartesian(ylim = c(0.5, 1)) +
           facet_grid(.~ win_type, scales = "free_x",
                      labeller = labeller(win_type = wintype.lab)) + 
           # geom_hline(data = ver_line, aes(yintercept = val), linetype = "dashed",
           #            colour = "red4") +
           theme_minimal() +
           labs(title = "Overall Win Rates") +
           theme(
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "NULL",
             axis.text.x = element_text(angle = 90, size = 10),
             axis.text.y = element_text(family = "serif"),
             plot.title = element_text(size = 12, face = "bold"),
             plot.subtitle = element_text(size = 10, face = "italic")
           )
      
      q
    }
  })
  
  output$plot.pytha <- renderPlot({
    
    if (input$pytha_flip) {
      homegame %>%
        filter(team %in% as.vector(pytha.teams()$team)) %>% 
        ggplot(aes(x = reorder(team, -diff))) +
        geom_bar(mapping = aes(y = win_rate,
                               fill = win_type),
                 stat = "identity",
                 position = "dodge",
                 alpha = .2) +
        geom_bar(mapping = aes(y = pyhta_rate,
                               color= win_type),
                 stat = "identity",
                 position = "dodge",
                 fill = NA) +
        coord_flip() +
        labs(
          x = "Team",
          y= "Win Rate",
          title = "Pythagorean Win Rate vs. Actual Win Rate",
          fill = "Actual Win Type",
          color = "Pythagorean Win Type"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, size = 10),
          plot.title = element_text(size = 12, face = "bold")
        ) +
        scale_fill_discrete(labels = c("Away", "Home", "Total")) +
        scale_color_discrete(labels = c("Away", "Home", "Total"))-> q; q
      
    } else {
      
      homegame %>%
        filter(team %in% as.vector(pytha.teams()$team)) %>% 
        ggplot(aes(x = reorder(team, -diff))) +
        geom_bar(mapping = aes(y = win_rate,
                               fill = win_type),
                 stat = "identity",
                 position = "dodge",
                 alpha = .2) +
        geom_bar(mapping = aes(y = pyhta_rate,
                               color= win_type),
                 stat = "identity",
                 position = "dodge",
                 fill = NA) +
        labs(
          x = "Team",
          y= "Win Rate",
          title = "Pythagorean Win Rate vs. Actual Win Rate",
          fill = "Win Type",
          color = "Pythagorean Win Type"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 90, size = 10),
          plot.title = element_text(size = 12, face = "bold")
        ) +
        scale_fill_discrete(labels = c("Away", "Home", "Total")) +
        scale_color_discrete(labels = c("Away", "Home", "Total"))-> q; q
      
    }
  })
  
  output$talent.plot <- renderPlot({
    
    if (input$facet) {
      
      talent2019 %>% 
        filter(conference %in% input$conferences) %>% 
        ggplot(.,
             mapping = aes(y = win_rate,
                           x = as.numeric(talent),
                           label = school)) +
        geom_point(mapping = aes(color = conference)) +
        geom_text_repel(size = 3) +
        labs(
          title = "Talent vs Winning Rate",
          x = "Talent Score",
          y = "Winning Rate"
        ) +
        theme_bw()+
        theme(
          legend.position = "none",
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10, face = "italic")
        ) +
        facet_grid(rows = vars(conference)) +
        xlim(0, 1000) -> q; q
      
    } else {
      
      talent2019 %>% 
        filter(conference %in% input$conferences) %>% 
        ggplot(.,
             mapping = aes(y = win_rate,
                           x = as.numeric(talent),
                           label = school)) +
        geom_point(mapping = aes(color = conference)) +
        geom_text_repel(size = 3) +
        labs(
          title = "Talent vs Winning Rate (2019)",
          x = "Talent Score",
          y = "Winning Rate"
        ) +
        theme_bw()+
        theme(
          legend.position = "top",
          plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10, face = "italic")
        ) +
        xlim(0, 1000) -> q; q
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
  
# homegame %>%
#   select(team, win_type, win_rate) %>%
#   group_by(win_type) %>%
#   top_n(n = 3, wt = win_rate) %>%
#   ungroup() %>%
#   select(team) -> plot.teams





# library(rsconnect)
# rsconnect::deployApp()











