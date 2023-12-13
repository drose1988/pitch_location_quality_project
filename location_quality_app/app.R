
library(tidyverse)
library(shiny)
library(shinydashboard)
library(rsconnect)

text <- read.csv("text.csv")
bar <- read.csv("bar.csv")
loli <- read.csv("loli.csv")
pie <- read.csv("pie.csv")

loli$pitch_name <- factor(loli$pitch_name, levels = c("Sweeper","Slider","Curveball","Changeup","Splitter","Cutter","Sinker","Fastball"))
pie$pitch_name <- factor(pie$pitch_name, levels = c("Fastball","Sinker","Cutter","Splitter","Changeup","Curveball","Slider","Sweeper"))

ui <- dashboardPage(
  dashboardHeader(title = "Pitch Location"),
  
  dashboardSidebar(
    selectInput("v_pitchers", "Pitchers", choices = text %>% 
                  select(player_name, lp2) %>%
                  group_by(player_name) %>%
                  summarise(lp2=mean(lp2))%>%
                  arrange(desc(lp2)) %>%
                  select(player_name))
  ),
  dashboardBody(
    fluidRow(
      box(title = "Average Quality of Pitch", width = 4, plotOutput("text")),
      box(title = "Quality of Pitch Distribution", width = 8, plotOutput("bar"))),
    fluidRow(
      box(title = "Quality of Pitch per Pitch Type", plotOutput("loli")),
      box(title = "Pitch Type Usage", plotOutput("pie")))
  )
)

server <- function(input, output) { 
  
  output$text <- renderPlot({
    
    text %>%
      filter(player_name == input$v_pitchers) %>%
      ggplot(aes(x = input$v_pitchers, y = lp2, fill=lp2)) +
      geom_rect(xmin=0, xmax=2, ymin=0, ymax=1, show.legend = FALSE)+
      geom_text(aes(label = lp2), size = 50, color="white")+
      scale_fill_gradient(low = "#C6DBEF", high = "#08519C", limits = c(0.6,0.7))+
      theme_void()
    
  })
  
  output$loli <- renderPlot({
    
    loli %>%
      filter(player_name == input$v_pitchers) %>%
      ggplot(aes(x = pitch_name, y = lp2, fill = pitch_name), show.legend = FALSE)+
      geom_segment(aes(x = pitch_name, xend = pitch_name, y = 0.45, yend = lp2),
                   color = "gray", lwd = 2) +
      geom_point(size = 24, pch = 21, col = "white", show.legend = FALSE) +
      geom_text(aes(label = lp2), color = "grey30", size = 5, fontface = "bold") +
      scale_fill_manual(values = c("Fastball" = "#FBB4AE",
                                   "Curveball" = "#B3CDE3",
                                   "Slider" = "#CCEBC5",
                                   "Sinker" = "#DECBE4",
                                   "Changeup" = "#FED9A6",
                                   "Sweeper" = "#FFFFCC",
                                   "Cutter" = "#E5D8BD",
                                   "Splitter" = "#FDDAEC")) +
      coord_flip() +
      ylim(0.45, 0.75) +
      theme_void()+
      theme(axis.text.y = element_text(size=20, color = "grey30"))
  })
  
  output$bar <- renderPlot({
    
    bar %>%
      filter(player_name == input$v_pitchers) %>%
      ggplot(aes(x = lp2)) +
      geom_histogram(aes(y = after_stat(count / sum(count)), fill = after_stat(count / sum(count))), bins=50, show.legend = FALSE) +
      scale_fill_gradient(low = "#C6DBEF", high = "#08519C", na.value = NA)+
      xlim(0,1)+
      ylim(0, 0.055)+
      theme_classic()+
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            #axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })
  
  output$pie <- renderPlot({
    
    pie %>%
      filter(player_name == input$v_pitchers) %>%
      ggplot(aes(x="", y=p, fill=pitch_name))+
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      geom_text(aes(label = paste0(p, "%")), position = position_stack(vjust=0.5), size = 5) +
      labs(x = NULL, y = NULL) +
      theme_void() +
      scale_fill_manual(values = c("Fastball" = "#FBB4AE",
                                   "Curveball" = "#B3CDE3",
                                   "Slider" = "#CCEBC5",
                                   "Sinker" = "#DECBE4",
                                   "Changeup" = "#FED9A6",
                                   "Sweeper" = "#FFFFCC",
                                   "Cutter" = "#E5D8BD",
                                   "Splitter" = "#FDDAEC")) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            legend.key.size = unit(1, 'cm'),
            legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(size=14))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
