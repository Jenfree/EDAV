library(shiny)
library(plotly)
library(ggplot2)
df <- read.csv(file="new_df.csv", header=TRUE)
colnames(df)[colnames(df) == 'percent'] <- 'fruit_percent'
df$fruit_percent<-format(round(df$fruit_percent, 2), nsmall = 2)
df$heavy_drinker_percent<-format(round(df$heavy_drinker_percent, 2), nsmall = 2)
df$smoker_percent<-format(round(df$smoker_percent, 2), nsmall = 2)
df$hover <- with(df, paste(state, '', '<br>', "Heavy drinker rate: ", heavy_drinker_percent, '<br>', "Smoker rate: ", smoker_percent,'<br>', "Fruit hater rate: ", fruit_percent))
# specify some map projection/options

ui <- fluidPage(
  titlePanel("Health factors that are associated with blood cholesterol in USA "),
  br(),
  tags$blockquote("Instruction: mouse hover the state to view the info and click the state to view the bar chart of health factors"),
  mainPanel(
    column(12,   plotlyOutput("plot", width = "100%",height = "350px")),
    column(12,    plotlyOutput("plot2", width = "100%", height = "300px"))
    ))


server <- function(input, output) {
  output$plot <- renderPlotly({
   
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_ly(z = df$high_chlo_percent,locations = df$abbr, colors = "Reds", text = df$hover, 
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(
        title = 'Percent of people who has high blood cholesterol level by state in 2015',
        geo = g)
  })

  
  output$plot2 <- renderPlotly({
    d <- event_data("plotly_click")
    if(is.null(d) == T) return(NULL)
    rownumber <- d$pointNumber + 1
    smoke_p <- df[rownumber,'smoker_percent']
    fruit_p  <- df[rownumber,'fruit_percent']
    drink_p  <- df[rownumber,'heavy_drinker_percent']
    chlo_p <-df[rownumber,'high_chlo_percent']
    type <- c('smoker','heavy drinker','fruit hater','high blood cholesterol')
    percent <- as.numeric(c(smoke_p, drink_p,fruit_p,chlo_p))
    subset <- data.frame(type, percent)
    subset$type = factor(subset$type, levels = c('high blood cholesterol','fruit hater','smoker','heavy drinker'))
    st <- df[rownumber,'state']
    ggplot(subset, aes(x = type, y = percent, fill = type)) +
      geom_bar(position = "dodge", stat = "identity")+
      ylim(0, 50)+
      scale_fill_manual(values =c("#F8766D", "#7CAE00","#C77CFF","#01BFC4")) +
      ggtitle(paste("Different health factors that are associated with high blood cholesterol level in", st)) +
      labs(y = "Percent", x= "Type of health factors", caption = "Source: CDC 2015 BRFSS data") +
      theme(plot.title = element_text(size=12))+
      theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
      theme(plot.caption = element_text(color = "grey68"))+
      theme(legend.position="bottom") +
      theme(legend.title=element_blank())
    
    
  })
  
  
}

shinyApp(ui, server)