# To start necessary packages are loaded
library("MetBrewer")
library("ggplot2")
library("ggstatsplot")
library("tidyverse")
library("rgl")
library("RColorBrewer")
library("hexbin")
library("corrplot")
library("dplyr")
library("plotly")
library("viridis")
library("hrbrthemes")
library("DT")
library("bslib")
library("ggbeeswarm")
library("shiny")
library('ggradar')
library('palmerpenguins')
library('scales')
library('showtext')

#setwd("~/School/HOWEST/R/assignment")

#data file is imported
data <- read.csv(file="data.csv",
                 header=TRUE,
                 sep=",")

# the division column is converted to a factor 
levels(data$Division)
data$Division <- as.factor(data$Division)
levels(data$Division)

# a color paletted is assigned to the division column
# this variable is used for several plots
colorblind.friendly("OKeeffe1")
division_color <- met.brewer("OKeeffe1",
                             length(unique(data$Division)))

ui <- navbarPage(
  title = "Triathlon Dataset Analyzer",
  theme = bs_theme(bootswatch = "spacelab"),
  
  #Designing the home page
  tabPanel("Introduction", 
           fluidPage(
             titlePanel("Welcome to the Triathlon Dataset Analyzer"),
             mainPanel(
               HTML("
             <hr>
             <div class='container'>
               <div class='row'>
                 <div class='col-md-8'>
                   <p>The used dataset contains the results of the female competitors on the
                   2022 Lake Placid Ironman.<br>
                   The actual dataset can be found in the 'Dataset' pane.</p>

                   <p>The dataset contains several interesting categorical and numeric variables.</p>

                   <h4>Categorical Variables</h4>
                   <ul>
                     <li>Athletes Country</li>
                     <li>Athletes Division</li>
                   </ul>

                   <h4>Numeric Variables</h4>
                   <ul>
                     <li>Overall time and ranking</li>
                     <li>Swim time and ranking</li>
                     <li>Bike time and ranking</li>
                     <li>Run time and ranking</li>
                   </ul>

                   <p>The video serves as illustration and shows the race of the professional
                   athletes in the 2022 edition.</p>
                 </div>

                 <div class='col-md-4'>
                   <iframe width='220%' height='400' 
                           src='https://www.youtube.com/embed/OquSjgz_qRE' 
                           title='2022 Lake Placid Ironman' 
                           frameborder='0' 
                           allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' 
                           allowfullscreen>
                   </iframe>
                 </div>
               </div>
             </div>
             ")
             )
           )
  ),
  
  # 2nd tab containing interactive dataset table
  tabPanel("Dataset", 
           fluidPage(
             titlePanel("Dataset overview"),
             p("Here you can view and explore the dataset."),
             mainPanel(
               DTOutput("dataset_table")
             )
            )
  ),
  
  #3th tab visualizing distribution categigorical variabales (country, division) with barplot
  #swarmplot
  tabPanel("Data Distribution", 
            fluidPage(
              h2("Data Distribution"),
              p("Here you can visualize the distribution of key variables."),
              
              sidebarLayout(
                sidebarPanel( 
                  p("The division variable consist of different age groups that participants 
                can compete in, additionally there is a pro division."),
                  p("The country variable consist of the countries participants
                    originate from."),
                  selectInput(
                    inputId = "y_bar", 
                    label = "Y-axis", 
                    choices = c("Division", "Country")  # Use column names, not data values
                   ),
                  p("We see that most participants are between 40 and 44 years old, 
                    14 professional athletes competed and 1 participant was over 70 years old.
                    The majority of participants is from the USA.")
                  ),
                  mainPanel(
                    plotOutput("barplot", height = "550px", width = "1000px")
                  )
                )
              )
  ),
  #4th tab looking at differences between division with swarmplot, boxplot and violin plot
  tabPanel("Division Comparison", 
           fluidPage(
             h2("Division Comparison"),
             p("Compare the performance of different divisions using different plot types."),
             sidebarLayout(
               sidebarPanel(
                 p(" The swarm plot shows how data is distributed"),
                 p("A boxplot also shows some statistical parameters.
                 The box of the plot spans from the 1st till the 3th quartile and 
                 also shows the median value.
                 The whiskers repesent 1.5 times the inter quartile range outside
                 the 1st and 3th quartile.
                 The dots represent outliers."),
                 p("The violin plot which shows the distribution of a variable together with its density."),
                 selectInput(
                   inputId = "y_division", 
                   label = "Y-axis", 
                   choices = c("Overall.Time", "Swim.Time", "Bike.Time", "Run.Time")
                 ),
                selectInput(
                   inputId = "plot_type", 
                   label = "Select Plot Type", 
                   choices = c("Swarm Plot", "Violin Plot", "Box Plot")
                ),
                 p("It seems that the professionals outperform the other categories 
                   and that the oldest competitor performs the worst."),
                p("What is maybe a bit surprising is that the youngsters of 18 up
                  to 24 years old (except one outlier) performed worse than the older categories.
                  Even the mean performance in the 65-69 category was better than
                  the performance of the entire 18-24 category.")
               )
              ,
               mainPanel(
                 plotOutput("division_plot", height = "550px", width = "1000px")
               )
          )
  )  
  ),
  #5th tab analyzing performance metrics (times in disciplines) seperatly per division with regression
  # correlation plot to find most influential discipline
  tabPanel("Performance Analysis",
           fluidPage(
             h2("Performance Analysis"),
             p("Looking at the relationship between performance in the different disciplines
      and final performance"),
             sidebarLayout(
               sidebarPanel(
                 p("In this section you can find linear regression plots which look at the 
                    relationship between variables."),
                 p("You can look at the relationship between performances on the different disciplines.
                    Additionally, you can also investigate how they relate to the final time and ranking."),
                 p("These relationships are shown in a facet plot so you can investigate them for
                    every division separately."),
                 selectInput(
                   inputId = "y_performance",
                   label = "Y-axis",
                   choices = c( "Overall.Rank", "Swim.Time", "Bike.Time", "Run.Time", "Overall.Time")
                 ),
                 selectInput(
                   inputId = "x_performance",
                   label = "X-axis",
                   choices = c("Swim.Time", "Bike.Time", "Run.Time", "Overall.Time") 
                 ),
                 p("We clearly see linear relationships with the final ranking for every discipline. 
                    However, it is hard to determine which discipline has the highest influence 
                    on the final ranking."),
                 p("To solve this problem, the correlation plot was used."),
                 p("The correlation plot shows there is a strong positive relationship 
                    between each of the disciplines and the final result."),
                 p("Additionally, we can clearly see that both the biking and running part 
                    have a much bigger influence on the final result than the swimming part.")
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Regression Plots", 
                            div(
                              h3("Relationships between performance metrics", style = "text-align: center; margin-bottom: 50px;"),
                              plotOutput("performance_plot", height = "550px", width = "1000px"),
                            )
                   ),
                   tabPanel("Correlation Plot", 
                            div(
                              h3("Correlation between discipline performances and final rank", style = "text-align: center;"),
                              plotOutput("correlation_plot", height = "600px", width = "1000px")
                            )
                   )
                 )
               )
             )
           )
  ),
  #6th tab interactive plots: 3D & bubble chart --> relationship between performance variables per division
  #radar plot: simple way and visually clear way to compare performances between disciplines
  tabPanel("Overview Plots", 
           fluidPage(
             h2("Overview plots"),
             p("The interactive bubble chart and 3D plot give the relationships 
             between the 5 most important variables."),
             p("The radar plot shows the performance on the different disciplines per 
             division and enables easy inter division comparison."),
             sidebarLayout(
               sidebarPanel(
                 p("This plot might not really add any extra informational value.
                   However, it is a nice display of the plotting possibilities 
                    one has in R."),
                 p("The bubble chart enables us to compare four variables in one plot."),
                 p("In this plot x and y axes are the swimming and running time.
                 The dot size represents the bike time and the dot color represents 
                 the total time spent on the entire triathlon."),
                 p("By hovering over the bubbles one can inspect the actual values 
                   of every individual data point."),
                 p("The 3D plot visualizes the relationship between the time spent 
                 on each of the disciplines and the final result."),
                 p("The bubble color represents the division of the participant
                   and the bubble size represents the final ranking. The smaller the
                   bubble the better the final rank."),
                 p("In both plots we clearly see that athletes who are fast in one of the disciplines you 
                    tend to be also fast in the other disciplines. These faster times
                   also result in better final results."),
                 p("The radar plot gives the average performance time per discipline 
                   per division. Where 100% is equal to the highest measured average 
                   and 0% is equal to the lowest measured average."),
                 p("This means that a lower score equals a better performance."),
                 p("With the radarplot division performance can easily be compared. 
                   We clearly see that the professionals outperform the other categories")
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Bubble Chart",
                      plotlyOutput("bubble_plot", height = "550px", width = "1000px")
                   ),
                   tabPanel("3D Plot",
                      plotlyOutput("D3_plot", height = "550px", width = "1000px")
                   ),
                   tabPanel("Radar Plot",
                            fluidRow(
                              column(2,  # Sidebar for checkboxes (adjust width if needed)
                                     checkboxGroupInput(
                                       inputId = "division_select",
                                       label = "Select Divisions:",
                                       choices = unique(data$Division),
                                       selected = unique(data$Division)
                                     )
                              ),
                              column(10,  # Main area for the plot
                                     plotOutput("spider_plot", height = "500px")
                              )
                            )
                   )
                 )
               )
             )
           )
          ),
  #7th tab: statistical calculations +  statistical plot
  tabPanel("Statistics",
           fluidPage(
             h2("Statistical tests"),
             p("On this page we search for statistically significant differences between divisions"),
             sidebarLayout(
               sidebarPanel(
                 p("In this tab we ran statistical analysis to find between which 
                   divisions there are statistically significant differences in performance"),
                 p("For this I used the one-way-analysis of variance (ANOVA)."),
                 p("The analysis showed that there was a significant difference in total 
                    time between at least 2 of the divisions."),
                 p("After the ANOVA a Tukey-Kramer Test can be used to see between which 
                   groups the difference is located."),
                 p("For ease of analysis only significant results are displayed."),
                 p("The main conclusion of the Tukey-kramer Test is that the professional 
                 division significantly outperformed all other divisions. Additionally 
                 the 50-54 division performed significantly worse than all ages between 24 and 49."),
                 p("The statistical findings are also summarised in a graphic."),
                 p("The graphic gives a combination of a violin plot  with integrated boxplots,
                   jittered dots (which show how individual data points are spread) 
                   and statistical information."),
                 p("So basically it is a combination of the boxplot and swarmplot 
                   that also shows the density and extra statistical parameters."),
                 p("From the graphic statistics we can derive the group averages 
                 (and how they differ of the mean). Additionally we can see which 
                 groups differ significantly (p-value < 0.05)."),
                 p("From this plot we can make the same conclusions as from the 
                    Tukey-Kramer test confirming there supremacy of the professionals."),
                 p("However, he graphic does not detect all differnces shown in the Tukey test
                   for the 50-54 division, probably because a slightly different test was used.")
                  
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("ANOVA",
                            tableOutput("anova")),
                   tabPanel("Tukey-Kramer",
                            tableOutput("tukeykramer")
                            ),
                   tabPanel("Graphic",
                            plotOutput("statistic_plot", height = "600px", width = "1000px")
                            )
                 )
               )
             )
           )
           )
  )


server <- function(input, output) {
  
  #returning the interactive table
  output$dataset_table <- renderDT({
    datatable(data, options = list(
      pageLength = 10,  
      autoWidth = TRUE,
      filter = "top"
    ))
  })
  
  #returning the barplot
  output$barplot <- renderPlot({
    req(input$y_bar)
    
    ggplot(data, aes(x = .data[[input$y_bar]], fill = as.factor(.data[[input$y_bar]]))) +
      geom_bar() +
      # print the value next to the bar in the same color
      geom_text(stat = "count", aes(label = ..count.., color = as.factor(.data[[input$y_bar]])), hjust = -0.2, size = 6, fontface = "bold") + 
      coord_flip() +  
      scale_fill_manual(values = division_color) +
      scale_color_manual(values = division_color) +
      labs(title = paste("Number of competitors per", input$y_bar),
           x = input$y_bar,
           y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size=25),
        axis.title=element_text(size=18,face="bold"),
        axis.text.y = element_text(color = division_color, size = 14, face = "bold"), # Match x-axis tick labels
        axis.text.x = element_text(size = 14),
        legend.position = "none"
      )
  })
  
  #returning the plots of the 4th tab
  output$division_plot <- renderPlot({
    req(input$y_division, input$plot_type)  # Ensure inputs are selected
    
    plot <- ggplot(data, aes(y = Division, x = .data[[input$y_division]], color = Division)) +
      scale_color_manual(values = division_color) +
      labs(title = "Performance comparison by division",
           x = input$y_division,
           y = "Division") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size=25),
        axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
    
    # select swarm, box or violin plot based on user input
    if (input$plot_type == "Swarm Plot") {
      plot <- plot + geom_beeswarm(size = 4, shape=15)
    } else if (input$plot_type == "Violin Plot") {
      plot <- plot + geom_violin()
    } else if (input$plot_type == "Box Plot") {
      plot <- plot + geom_boxplot()
    }
    plot
  })
  
  #returning the regression plots 
  output$performance_plot <- renderPlot({
    req(input$x_performance, input$y_performance)
    
    plot <- ggplot(data, aes(x = .data[[input$x_performance]], y = .data[[input$y_performance]], color = Division)) +
      scale_color_manual(values = division_color) +
      #ggtitle("Relationship between performance metrics") +
      xlab(input$x_performance) +
      ylab(input$y_performance) +
      geom_point(size = 3) +  # Points plotted here
      facet_wrap(~Division, ncol = 4, drop = FALSE) +  # Faceting
      theme(
        panel.grid.major = element_line(color = "grey80"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size=25),
        axis.title=element_text(size=18,face="bold"),
        axis.text = element_text(size = 12),
        legend.position = "none", # don't give the default legend
        strip.text = element_text(size = 16, face = "bold")
      )
    plot
  })
  
  #returning the correlation plot
  output$correlation_plot <- renderPlot({

    # creating the matrix used for plot input
    corr_data <- data.frame(
      data$Swim.Time, 
      data$Bike.Time, 
      data$Run.Time,
      data$Overall.Rank
    )
    
    corr_matrix <- cor(corr_data)
    colnames(corr_matrix) <- c("Swim Time", "Bike Time", "Run Time", "Overall Ranking")
    rownames(corr_matrix) <- c("Swim Time", "Bike Time", "Run Time", "Overall Ranking")
    
    plot <- corrplot(corr_matrix, 
                     method = "circle", 
                     #main = "Correlation between individual disciplines and final rank",  
                     sub = "Correlations between Swim, Bike, Run, and Overall Times",  
                     xlab = "Activity Types",  
                     ylab = "Activity Types",  
                     tl.col = "black",  
                     tl.cex = 1.4, 
    )
  })
  
  #returning bubble plt
  #renderPlotly for interactive plot
  output$bubble_plot <- renderPlotly({
    text <- paste("Swim Time(min): ", data$Swim.Time, 
                  "\nRun Time(min): ", data$Run.Time, 
                  "\nBike Time(min): ", data$Bike.Time, 
                  "\nOverall Rank: ", data$Overall.Rank, sep="")
    
    plot <- ggplot(data, aes(x=Swim.Time, y=Run.Time, size=Bike.Time, color=Overall.Time, text=text)) +
      geom_point(alpha=0.7) +
      labs(
          #title = "Bubble plot of achieved times",
           x = "Swim Time (min)", y = "Run Time (min)") +
      scale_size(range = c(0.5, 5), name="Bike Time (min)") +
      scale_color_viridis_c(name="Overall Time (min)") +
      theme_minimal() +
      theme(legend.position="right",
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.title.x = element_text(hjust = 0.5, size = 14),
            axis.title.y = element_text(hjust = 0.5, size = 14)) +
      guides(
        size = guide_legend(title = "Bike Time (min)"),
        color = guide_legend(title = "Overall Rank (min)")
      )
    
    plot <- ggplotly(plot, tooltip="text")

  })
  
  #returning 3D plt
  #renderPlotly for interactive plot
  output$D3_plot <- renderPlotly({
    division_color2 <- as.character(met.brewer("OKeeffe1", length(unique(data$Division))))
    
    fig <- plot_ly(data, x = ~Swim.Time, y = ~Run.Time, z = ~Bike.Time, color = ~Division, 
                   size = ~Overall.Rank, colors = division_color2,
                   marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 30),
                   text = ~paste('Swim Time:', Swim.Time, '<br>Run Time:', Run.Time,
                                 '<br>Bike Time:', Bike.Time,
                                 '<br>Division:', Division,
                                 '<br>Overall Ranking:', Overall.Rank))
    fig <- fig %>% layout(
                          #title = 'Relationship between the different performance metrics',
                          scene = list(xaxis = list(title = 'Swim Time (min)',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    gridwidth = 2),
                                       yaxis = list(title = 'Run Time (min)',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    gridwith = 2),
                                       zaxis = list(title = 'Bike Time (min)',
                                                    gridcolor = 'rgb(255, 255, 255)',
                                                    zerolinewidth = 1,
                                                    ticklen = 5,
                                                    gridwith = 2)),
                          #paper_bgcolor = 'rgb(243, 243, 243)',
                          plot_bgcolor = 'rgb(200, 200, 200)')
    
    fig
  })
  
  #returning radar plot 
  # %>% is apiping operator
  output$spider_plot <- renderPlot({
    req(input$division_select)
    
    radar <- data %>%
      drop_na() %>%
      filter(Division %in% input$division_select) %>%
      group_by(Division) %>%
      summarise(
        swim_time = mean(Swim.Time),
        bike_time = mean(Bike.Time),
        run_time = mean(Run.Time),
        overall_time = mean(Overall.Time)
      ) %>%
      ungroup() %>%
      mutate_at(vars(-Division), rescale)
    
    if (nrow(radar) == 0) return(NULL)  # Avoid plotting errors if no data remains
    
    # Ensure division_color is a named vector with correct colors
    color_map <- division_color[radar$Division]
    
    plt <- radar %>%
      ggradar(
        font.radar = "roboto",
        grid.label.size = 10,  # Affects the grid annotations (0%, 50%, etc.)
        axis.label.size = 6, # Affects the names of the variables
        group.point.size = 3,   # Simply the size of the points
        group.colours = color_map # Assign correct colors
      )+
      theme(legend.position = "right")
    
    plt
  })
  
  #returning the anova results
  output$anova <- renderTable({
    time_model <- lm(Overall.Time ~ Division, data=data)
    #summary(time_model)
    anova(time_model)
    #confint(time_model)
  })
  
  #returning the Tukey-Kramer results
  output$tukeykramer <- renderTable({
    time.aov <- aov(Overall.Time ~ Division, data = data)
    #summary(time.aov)
    Tresults <- TukeyHSD(time.aov, "Division")
    
    # only keep the significant results
    significant_results <- Tresults$Division[Tresults$Division[,"p adj"] <= 0.05,]
    significant_results
  })
  
  # return the statistics plot
  output$statistic_plot <- renderPlot({
    plt <- ggbetweenstats(
      data = data,
      x = Division,
      y = Overall.Time,
    )
    
    plt <- plt + 
      coord_flip() + 
      labs(
        x = NULL,
        y = "Overall Time (min)",
        title = "Overall triathlon time across different Divisions",
      ) + 
      scale_color_manual(values = division_color) +
      theme(
        text = element_text(family = "Roboto", size = 13, color = "black"),
        plot.title = element_text(
          family = "Lobster Two", 
          size = 15,
          face = "bold",
          color = "#2a475e",
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          family = "Roboto", 
          size = 10, 
          face = "bold",
          color="#1b2838"
        ),
        plot.title.position = "plot", # slightly different from default
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12),
        plot.margin = margin(t = 10, r = 0, b = 0, l = 0)
      )
    
    plt
  })
}


shinyApp(ui = ui, server = server)
