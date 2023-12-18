library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(stringr)
library(shinythemes)
library(DT)

data <- read.csv("Data.csv")
choice_year = as.integer(sort(unique(data$year)))
choice_country=sort(unique(data$country))
choice_age=unique(data$age)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Global Suicide Trends and Analysis", 
             
             tabPanel("Introduction", 
                      textOutput("introduction_title"),
                      textOutput("introduction"), 
                      textOutput("Tabular_title"),
                      textOutput("Tabular"),
                      textOutput("chart_title"),
                      textOutput("chart"),
                      textOutput("trend_title"),
                      textOutput("trend"),
                      textOutput("correlation_title"),
                      textOutput("correlation"),
                      textOutput("conclusion_title"),
                      textOutput("conclusion"),
                      
                      tags$head(tags$style("#introduction_title{ color:#1c1b1b; font-size: 24px; text-align: left;}
                                            #Tabular_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                            #chart_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                            #trend_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                            #correlation_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                            #conclusion_title{ color:#1c1b1b; font-size: 19px; text-align: left;}
                                           #introduction{text-align: justify;font-size: 14px;}
                                           #Tabular{text-align: justify;font-size: 12px;}
                                           #chart{text-align: justify;font-size: 12px;}
                                           #correlation{text-align: justify;font-size: 12px;}
                                           #conclusion{text-align: justify;font-size: 12px;}
                                           #trend{text-align: justify;font-size: 12px;}"))
             ),
             
             tabPanel("Tabular",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("input_year", "Year", value = 1985, min = 1985, max = 2016, sep=""),
                          selectInput("input_country","Country", choices = choice_country),
                          radioButtons("input_gender","Sex", choices = c("male" , "female", "Both"),selected="Both"),
                          selectInput("input_age","Age Group", choices = choice_age)
                        ), mainPanel(
                          
                          DTOutput("table"),
                          
                        )
                        
                      )),
             
             tabPanel("Charts",
                      sidebarLayout(
                        sidebarPanel(selectInput("vis_year", "Year", choices = choice_year),
                                     selectInput("vis_country","Country", choices = choice_country),
                                     radioButtons("vis_gender","Sex", choices = c("male" , "female", "Both"),selected = "Both")),
                        mainPanel(
                          textOutput("bar_title"),
                          plotOutput("barg"),
                          textOutput("bar_explanation"),
                          tags$head(tags$style("#bar_title{color: #000000; font-size: 20px}"))
                        )
                      )
             ),
             tabPanel("Trend plot ", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country_for_range", "Country", choices = choice_country),
                          sliderInput("input_range", "Year Range to be Displayed", 
                                      min = 1985, max = 2016, value = c(1996, 2005), sep = "")),
                        mainPanel(
                          textOutput("lineg_title"),
                          textOutput("analysis1"),
                          plotOutput("lineg"),
                          tags$head(tags$style("#lineg_title{ color: #000000; font-size: 20px}"))
                        )
                      )
             ),
             tabPanel("Correlation",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("country_for_analysis", "Country", choices = choice_country),
                          sliderInput("input_range_analysis", "Year Range", 
                                      min = 1985, max = 2016, value = c(1996, 2005), sep = "")
                        ), mainPanel(
                          
                          textOutput("analysis_introduction"),
                          textOutput("analysis"),
                          textOutput("analysis_gdp_explanation"),
                          splitLayout(cellWidths = c("50%","50%"), plotOutput("analysis_gdp"), plotOutput("analysis_suicides")),
                        )
                      )
             ),tabPanel("Conclusion",
                        textOutput("project_descr_title"),
                        textOutput("project_description"),
                        textOutput("Source_Title"),
                        uiOutput("Source_link"),
                        uiOutput("Source1"),
                        uiOutput("Source2"),
                        uiOutput("Source3"),
                        
                        tags$head(tags$style("#project_descr_title{color: 	#000000;font-size: 20px}
                                         #audience_title{color:	#000000; font-size: 20px}
                                         #takeaway_title{color: 	#000000; font-size: 20px}
                                         #group{ color: #000000; font-size: 20px}
                                         #kevin_title{font-size: 20px}
                                         #lakshay_title{font-size: 20px}
                                         #ethan_title{ font-size: 20px}
                                         #Source_Title{ color:#000000;font-size: 20px; text-align: left;}
                                         #Source_link{ text-align: justify; font-size: 15px;}
                                         #code{ color : #000000; font-size: 20px}"))
                        
             )
  )
) 


server <- function(input, output){
  
  output$introduction_title <- renderText({
    intro <- paste0("About the Project")
  })
  output$introduction <- renderText({
    text <- paste("Hello, My name is Bhuvnesh Sahu, Student of Chennai Mathematical Institute.The 'Global Suicide Trends and Analysis' app is a comprehensive and interactive tool built using R programming language and the Shiny framework. This app aims to provide insightful analysis and visualization of global suicide data collected between 1985 and 2016. It serves as a platform for users to explore and understand various aspects of suicide rates across different demographics and geographical locations.
    Suicide is a serious problem that affects people all around the world, no matter where they live or who they
are. Our project focuses on studying trends related to suicide on a global scale. We aim to analyze and
understand the patterns and changes in suicide rates over time and across different regions. By examining
these trends, we can gain valuable insights into how suicide rates are evolving worldwide.
Our goal is not to provide solutions but rather to provide a clear picture of how suicide rates have been
changing and if there are any noticeable patterns or shifts.")
  })
  output$Tabular_title <- renderText({
    text <- paste("1. Tabular view")})
  output$Tabular <- renderText({
    text <- paste("          This offers a detailed table showcasing suicide-related data, allowing users to filter information by year, country, gender, and age group. The table presents statistics such as the number of suicides, population, suicides per 100k population, and generation.")})
  output$chart_title <- renderText({
    text <- paste("2. Charts Section")})
  output$chart <- renderText({
    text <- paste("          This enables users to visualize the relationship between gender and age groups concerning the number of suicides through interactive bar graphs. Users can explore data across different years and countries to observe patterns and comparisons.")})
  output$trend_title <- renderText({
    text <- paste("3. Trend Plot Section")})
  output$trend <- renderText({
    text <- paste("          This presents a line graph illustrating the trends in suicide rates over a selected range of years for a chosen country. This feature allows users to track changes in suicide rates over time.")})
  output$correlation_title <- renderText({
    text <- paste("4. Correlation Analysis")})
  output$correlation <- renderText({
    text <- paste("          This provides insights into the correlation between GDP per capita and suicide rates. The section includes line graphs that show the relationship between GDP and the total number of suicides in a selected country over a specified time range.")})
  output$conclusion_title <- renderText({
    text <- paste("5. Conclusion")})
  output$conclusion <- renderText({
    text <- paste('          This summarizes key findings from the data analysis. It highlights trends such as the age groups most affected, regional disparities, correlations with GDP, and gender-based differences in suicide rates. This Includes references to the sources used for the dataset, providing users with reliable links to the original data on platforms.')
  })
  #table
  output$table <- renderDataTable({
    validate(
      need(input$input_country, message = "Please select country."),
    ) 
    if(input$input_gender == "Both") {
      desired_df <-  data %>%
        filter(input$input_country == country,input$input_age == age,input$input_year == year) %>% select(country,year,sex,age,suicides_no,population,suicides.100k.pop,generation)
    } else {
      desired_df <- data %>%
        filter(input$input_country == country,input$input_gender == sex,input$input_age == age,input$input_year == year) %>% select(country,year,sex,age,suicides_no,population,suicides.100k.pop,generation)
    }
  })
  #chart
  
  output$bar_title <- renderText({
    explanation <- paste0("Which gender has higher suicide rate ")
  })
  output$barg <- renderPlot({
    
    if ( input$vis_gender == "Both") {
      both_case <- data %>% filter(input$vis_year == year, input$vis_country == country)
      
      validate(
        need(nrow(both_case) > 1, message = "Data not available")
      )
      
      output <- ggplot(both_case, aes(both_case$age, both_case$suicides_no, fill = both_case$sex)) + geom_bar(stat="identity", position ="dodge") +
        scale_fill_manual(values=c('#DF536B','#12a4d9')) + labs(fill = "Gender") + 
        xlab("Age Groups") + ylab("Number of Suicides") 
      output
      
    } else {
      data_filter_df <- data %>% 
        filter(input$vis_year == year, input$vis_country == country, input$vis_gender == sex)
      
      validate(
        need(input$vis_year, message = "Please select a year"),
        need(input$vis_country, message = "Please choose a country."),
        need(nrow(data_filter_df) > 1, message = "Data not available")
      )
      output <- barplot(data_filter_df$suicides_no, names.arg = as.character(sort(data_filter_df$age)), main = "Number of Suicides in each Age Group",
                        xlab = "Age Groups", ylab = "Suicides Recorded", col = "#12a4d9", border = "black")
    }})
  #trend
  output$lineg_title <- renderText({
    explanation <- paste0("Suicide rates over period of time")
  })
  output$analysis1 <- renderText({
    analysis1_data <- data %>% filter(input$country_for_range == country) %>% group_by(year) %>% summarize(suicides = sum(suicides_no)) %>% 
      filter( input$input_range[1] <= year & input$input_range[2] >= year ) %>% select(suicides)
    
    validate(
      need(nrow(analysis1_data) > 1,"Select a Country")  
    )
    
    old <- as.integer(analysis1_data[1,1])
    new <- as.integer(analysis1_data[nrow(analysis1_data),1])
    change <- new - old
    if(old != 0){
      perc_inc <- round((change/old)*100,2)
    } else {
      perc_inc <- 0
    }
    if(change > 0 & nrow(analysis1_data) > 1) {
      text <- paste0("In ", input$country_for_range," there was a ", perc_inc,"% increase in suicide rates.")
    } else if(change <= 0  & nrow(analysis1_data) > 1){
      text <- paste0("In ", input$country_for_range, " there was a ", abs(perc_inc),"% decrease in suicide rates.")
    }  else {
      text <- paste0("Data is not available")
    }
    text
  })
  output$lineg <- renderPlot({
    line_data <- data %>% 
      filter(input$country_for_range == country) %>% group_by(year) %>% summarize(suicides = sum(suicides_no)) %>% 
      filter( input$input_range[1] <= year & input$input_range[2] >= year )
    
    validate(
      need(nrow(line_data) > 1 , "")
    )
    plot_view <- ggplot(data = line_data, aes(x = line_data$year, y = line_data$suicides)) + geom_line(col = "#DF536B", size = 1) +geom_point(col = "#12a4d9", size = 2) +
      labs(title='')+ xlab("Year") + ylab("Number of Suicides")
    plot_view
    
  })
  
  
  #correlation
  output$analysis <- renderText({
    analysis_data <- data %>% filter(input$country_for_analysis == country) %>% group_by(year) %>% summarize(suicides = sum(suicides_no)) %>% 
      filter( input$input_range_analysis[1] <= year & input$input_range_analysis[2] >= year ) %>% select(suicides)
    
    validate(
      need(nrow(analysis_data) > 1, message = "Please select a country to analyze and look at.")  
    )
    
    old <- as.integer(analysis_data[1,1])
    new <- as.integer(analysis_data[nrow(analysis_data),1])
    change <- new - old
    if(old != 0){
      perc_inc <- round((change/old)*100,2)
    } else {
      perc_inc <- 0
    }
    if(change > 0 & nrow(analysis_data) > 1) {
      text <- paste0("In ", input$country_for_analysis," there was a ", perc_inc,"% increase in suicide rates.")
    } else if(change <= 0  & nrow(analysis_data) > 1){
      text <- paste0("In ", input$country_for_analysis, " there was a ", abs(perc_inc),"% decrease in suicide rates.")
    }  else {
      text <- paste0("Data is not available")
    }
    text
  })
  
  output$analysis_gdp <- renderPlot({
    gdp <- data %>% 
      filter(input$country_for_analysis == country) %>% select(country, year, suicides_no, gdp_per_capita....) %>% 
      group_by(country, year, gdp_per_capita....) %>% summarize(suicides = sum(suicides_no)) %>% 
      filter( input$input_range_analysis[1] <= year & input$input_range_analysis[2] >= year )
    
    validate(
      need(nrow(gdp) > 1, "")
    )
    
    plot <- ggplot(gdp, aes(gdp$year, gdp$gdp_per_capita....)) + geom_line(col = "#DF536B", size = 1) +geom_point(col = "#12a4d9", size = 2) +  labs(title = "GDP-per-capita per year") +
      xlab("Year") + ylab("GDP-per-capita")
    plot
  })
  
  output$analysis_suicides <- renderPlot({
    test <- data %>% filter(input$country_for_analysis == country) %>% group_by(year) %>% summarize(suicides = sum(suicides_no)) %>% 
      filter( input$input_range_analysis[1] <= year & input$input_range_analysis[2] >= year )
    
    validate(
      need(nrow(test) > 1 , "")
    )
    
    testing <- ggplot(test, aes(test$year, test$suicides)) + geom_line(col = "#DF536B", size = 1) +geom_point(col = "#12a4d9", size = 2) +  labs(title = "total suicide numbers for each year") +
      xlab("Year") + ylab("Total number of suicides")
    testing
  })
  
  
  
  #conclusion
  output$project_descr_title <- renderText({
    title <- paste("Conclusion")
  })
  output$project_description <- renderText({
    project_description <- paste("After a comprehensive analysis of the data and all graphs, key trends have emerged. Suicide rates exhibited
an upward trajectory until 1995, followed by a decline after reaching a peak in that year. Age consistently
correlated with higher suicide rates across all countries. European men faced the highest risk, necessitating
region-specific mental health initiatives. While GDP per capita displayed a weak positive correlation with
suicide rates. Most concerning was the persistent overrepresentation of men in suicide deaths, with global
male rates roughly 3.5 times higher than those of women.There is less representation of african countries.
Suicide rates are highest for the age group 75+. Overall, our analysis provides a comprehensive view of
global suicide trends, guiding evidence-based strategies for suicide prevention and mental health support
worldwide.")
  })
  
  
  output$Source_Title <- renderText({
    text <- paste0("Sources Used: ")
  })
  
  output$Source_link <- renderUI({
    url <- a("Data", href ="https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016")
    tagList(url)
  })
  output$Source1 <- renderText({
    title <- paste("Google")
  })
  output$Source2 <- renderText({
    title <- paste("Wikipedia")
  })
  output$Source3 <- renderText({
    title <- paste("Stack Overflow")
  })
  output
  
}

shinyApp(ui = ui, server = server)
