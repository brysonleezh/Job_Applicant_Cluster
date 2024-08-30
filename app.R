library(shiny)
library(shinythemes)
library(cluster)
library(ggplot2)
library(factoextra)
library(stringr)
library(dplyr)
library(FactoMineR)
library(DT)
library(plotly)
library(shinyWidgets)

stackoverflow_full <- read.csv("stackoverflow_full.csv")

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Job Applicant Clustering Dashboard"),
  sidebarLayout(
    sidebarPanel(
      numericInput("clusters", label = h4("Select Number of Clusters"), value = 4, min = 2, max = 8),
      h4("Filter Applicants"),
      pickerInput("ageInput", "Age:", choices = sort(unique(stackoverflow_full$Age)), multiple = TRUE, selected= c(">35","<35")),
      pickerInput("edLevelInput", "Education Level:", choices = sort(unique(stackoverflow_full$EdLevel)), multiple = TRUE, selected= c("Master","Undergraduate","PhD")),
      pickerInput("genderInput", "Gender:", choices = sort(unique(stackoverflow_full$Gender)), multiple = TRUE, selected = c("Man","Woman","NonBinary")),
      pickerInput("mentalHealthInput", "Mental Health:", choices = sort(unique(stackoverflow_full$MentalHealth)), multiple = TRUE, selected=c("Yes","No")),
      pickerInput("mainBranchInput", "Main Branch:", choices = sort(unique(stackoverflow_full$MainBranch)), multiple = TRUE, selected = "Dev"),
      selectInput("programmingSkillsInput", "Select Programming Skills:", 
                  choices = unique(unlist(str_split(stackoverflow_full$HaveWorkedWith, ";"))), 
                  selected = c("Python","C++","PostgreSQL","HTML/CSS","JavaScript","Node.js","SQL","Git","Express","React.js","C","Java","Perl","AWS","Vue.js", "Bash/Shell"),
                  multiple = TRUE),
      sliderInput("yearsCodeInput", "Years Coding Experience:", min = 0, max = 50, value = c(2, 8)),
      sliderInput("yearsCodeProInput", "Years Professional Coding Experience:", min = 0, max = 50, value = c(2, 8)),
      sliderInput("previousSalaryInput", "Current Salary:", min = 0, max = 250000, value = c(50000, 150000)),
      actionButton("loadButton", "Load")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cluster Plot", plotlyOutput("clusterPlot", height = "500px")),
        tabPanel("Applicant Table", DTOutput("applicantTable")),
        tabPanel("Cluster Distribution", plotOutput("clusterDistribution")),
        tabPanel("Numerical Attirbutes",
                 fluidRow(
                   column(4, plotOutput("clusterYearCode")),
                   column(4, plotOutput("clusterSalary")),
                   column(4, plotOutput("clusterComputerSkill"))                 
                   )),
        tabPanel("Categorical Attirbutes",
                 fluidRow(
                   column(4, plotOutput("clusterEdLevel")),
                   column(4, plotOutput("clusterGender")),
                   column(4, plotOutput("clusterMentalHealth"))                 
                 )),
        tabPanel("Cluster Profile", plotlyOutput("clusterProfile", height = "700px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$loadButton,{
      filtered_data <- reactive({
         df_filtered <- stackoverflow_full %>%
           filter(
              Age %in% input$ageInput,  # Allows for multiple age selections
              EdLevel %in% input$edLevelInput,  # Allows for multiple education level selections
              Gender %in% input$genderInput ,  # Mapping numeric to string
              MentalHealth %in% input$mentalHealthInput,  # Mapping numeric to string
              MainBranch %in% input$mainBranchInput,  # Allows for multiple main branch selections
              # Country %in% input$countryInput,  # Allows for multiple country selections
              YearsCode >= input$yearsCodeInput[1] & YearsCode <= input$yearsCodeInput[2],  # Filters based on coding experience range
              YearsCodePro >= input$yearsCodeProInput[1] & YearsCodePro <= input$yearsCodeProInput[2],  # Filters based on professional coding experience range
              PreviousSalary >= input$previousSalaryInput[1] & PreviousSalary <= input$previousSalaryInput[2]  # Filters based on salary range
          )

        if (!is.null(input$computerSkillsInput)) {
          for (skill in input$computerSkillsInput) {
            df_filtered <- df_filtered %>% filter(str_detect(ComputerSkills, skill))
          }
        }
        set.seed(123)
        df_filtered <- df_filtered %>% sample_n(min(6000, nrow(df_filtered)))
        return(df_filtered)
      })
      
      # Preprocess data after filtering
      preprocessed_data <- reactive({
        df <- filtered_data() %>%
          mutate(Age = ifelse(Age == "<35", 0, 1),
                 Accessibility = ifelse(Accessibility == "Yes", 1, 0),
                 MentalHealth = ifelse(MentalHealth == "Yes", 1, 0),
                 Gender = ifelse(Gender == "Man", 1, 0)) %>%
          mutate(across(c(EdLevel, MainBranch, Country), as.factor)) %>%
          mutate(across(c(EdLevel, MainBranch, Country), ~ as.numeric(as.factor(.))))
        
        # Handle skills columns (e.g., HaveWorkedWith and ComputerSkills)
        skills_columns <- c("HaveWorkedWith", "ComputerSkills")
        unique_skills <- unique(unlist(str_split(ifelse(str_detect(df$HaveWorkedWith, ";"), df$HaveWorkedWith, paste0(df$HaveWorkedWith, ";")), ";")))
        unique_skills <- unique_skills[unique_skills != ""]
        
        for (skill in unique_skills) {
          df[[skill]] <- ifelse(str_detect(df$HaveWorkedWith, skill), 1, 0)
        }
        
        df <- df %>% select(-c(HaveWorkedWith, ComputerSkills, Country))
        
        num_cols <- c("YearsCode", "YearsCodePro", "PreviousSalary")
        df[num_cols] <- scale(df[num_cols])
        print(df)
        return(df)
      })
      
      # Perform PCA and reduce dimensions
      reduced_df <- reactive({
        validate(need(nrow(preprocessed_data()) > 0, "Data is empty or not properly loaded"))
        pca_result <- PCA(preprocessed_data() %>% select(-Applicant_ID), ncp = 3, graph = FALSE)
        cbind(pca_result$ind$coord, ID = preprocessed_data()$Applicant_ID)  # Combine with IDs
      })
      
      # Calculate Gower distance on reduced data
      gower_dist <- reactive({
        req(reduced_df())
        daisy(reduced_df()[, -ncol(reduced_df())], metric = "gower")  # Exclude ID column
      })
      
      # Perform hierarchical clustering
      hc <- reactive({
        req(gower_dist())
        hclust(gower_dist(), method = "complete")
      })
      
      # Get cluster assignments
      cluster_assignments <- reactive({
        req(hc(), input$clusters, reduced_df())
        cutree(hc(), k = input$clusters)
      })
      
      # Merge cluster assignments back to the original dataset using Applicant_ID
      df_with_clusters <- reactive({
        cluster_df <- data.frame(ID = preprocessed_data()$Applicant_ID, Cluster = cluster_assignments())
        merged_df <- merge(stackoverflow_full, cluster_df, by.x = "Applicant_ID", by.y = "ID", all = FALSE)
        merged_df
      })
      
      # Plot the clusters using factoextra
      output$clusterPlot <- renderPlotly({
        req(hc(), input$clusters, reduced_df())
        df_with_clusters <- data.frame(reduced_df(), Cluster = factor(cluster_assignments()))
        
        # Plot using ggplot and convert to plotly for interactivity
        p <- ggplot(df_with_clusters, aes(x = Dim.1, y = Dim.2, color = Cluster, text = ID)) +
          geom_point() +
          labs(title = "Cluster Plot with Hoverable IDs", x = "Dim 1", y = "Dim 2") +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")  # Add tooltip to display IDs
      })
      
      # Render applicant information table
      output$applicantTable <- renderDT({
        req(df_with_clusters())
        datatable(df_with_clusters())
      })
      
      # Render cluster distribution
      output$clusterDistribution <- renderPlot({
        req(df_with_clusters())
        ggplot(df_with_clusters(), aes(x = factor(Cluster), fill = factor(Cluster))) +
          geom_bar() +
          geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +  # Add text labels on top of the bars
          labs(title = "Distribution of Different Clusters", x = "Cluster", y = "Count", fill = "Cluster") +
          scale_fill_brewer(palette = "Set3") + 
          theme_minimal() +
          theme(
            legend.position = "right",  # Position the legend on the right
            legend.title = element_text(size = 12),  # Set the legend title size
            legend.text = element_text(size = 10)   # Set the legend text size
          )
      })
      
      # Numerical attributes:
      output$clusterYearCode <- renderPlot({
        req(df_with_clusters())
        ggplot(df_with_clusters(), aes(x = factor(Cluster), y = YearsCodePro, fill = factor(Cluster))) +
          geom_boxplot() +
          labs(title = "Coding Experience by Cluster", x = "Cluster", y = "Years Coding Experience") +
          theme_minimal()
      })
      output$clusterSalary <- renderPlot({
        req(df_with_clusters())
        ggplot(df_with_clusters(), aes(x = factor(Cluster), y = PreviousSalary, fill = factor(Cluster))) +
          geom_boxplot() +
          labs(title = "Salary by Cluster", x = "Cluster", y = "Current Salary") +
          theme_minimal()
      })
      output$clusterComputerSkill <- renderPlot({
        req(df_with_clusters())
        ggplot(df_with_clusters(), aes(x = factor(Cluster), y = ComputerSkills, fill = factor(Cluster))) +
          geom_boxplot() +
          labs(title = "Computer Skill by Cluster", x = "Cluster", y = "Number of Computer Skill") +
          theme_minimal()
      })
      
      # Categorical attributes
      output$clusterEdLevel <- renderPlot({
        req(df_with_clusters())
        df_with_clusters() %>%
          group_by(Cluster, EdLevel) %>%
          summarise(Count = n()) %>%
          group_by(Cluster) %>%
          mutate(Proportion = Count / sum(Count)) %>%
          ggplot(aes(x = factor(Cluster), y = Proportion, fill = EdLevel)) +
          geom_bar(stat = "identity", position = "fill") +
          geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
                    position = position_fill(vjust = 0.5), size = 3, color = "black", fontface = "bold") + # Increased size and set color
          labs(title = "Education Levels by Cluster", x = "Cluster", y = "Proportion") +
          scale_fill_brewer(palette = "Set3") +
          theme_minimal()
      })
      output$clusterGender <- renderPlot({
        req(df_with_clusters())
        df_with_clusters() %>%
          group_by(Cluster, Gender) %>%
          summarise(Count = n()) %>%
          group_by(Cluster) %>%
          mutate(Proportion = Count / sum(Count)) %>%
          ggplot(aes(x = factor(Cluster), y = Proportion, fill = Gender)) +
          geom_bar(stat = "identity", position = "fill") +
          geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
                    position = position_fill(vjust = 0.5), size = 3, color = "black", fontface = "bold") +
          labs(title = "Gender Level by Cluster", x = "Cluster", y = "Count") +
          scale_fill_brewer(palette = "Set2") +
          theme_minimal()
      })
      output$clusterMentalHealth <- renderPlot({
        req(df_with_clusters())
        df_with_clusters() %>%
          group_by(Cluster, MentalHealth) %>%
          summarise(Count = n()) %>%
          group_by(Cluster) %>%
          mutate(Proportion = Count / sum(Count)) %>%
          ggplot(aes(x = factor(Cluster), y = Proportion, fill = MentalHealth)) +
          geom_bar(stat = "identity", position = "fill") +
          geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)),
                    position = position_fill(vjust = 0.5), size = 3, color = "black", fontface = "bold") +
          labs(title = "MentalHealth Level by Cluster", x = "Cluster", y = "Count") +
          scale_fill_brewer(palette = "Set2") +
          theme_minimal()
      })
      
      
      # Render cluster profile scatter plot
      output$clusterProfile <- renderPlotly({
        req(df_with_clusters())
        plot_ly(data = df_with_clusters(), 
                x = ~YearsCodePro, 
                y = ~ComputerSkills, 
                z = ~PreviousSalary, 
                color = ~Cluster, 
                type = 'scatter3d', 
                mode = 'markers' ,
                hoverinfo = "text",
                text = ~paste("Applicant ID:", Applicant_ID, 
                              "<br>Years of Professional Coding:", YearsCodePro, 
                              "<br>Computer Skills:", ComputerSkills, 
                              "<br>Previous Salary:", PreviousSalary, 
                              "<br>Cluster:", Cluster)) %>%
          layout(title = "3D Cluster Plot with Current Salary",
                 scene = list(xaxis = list(title = "YearsCodePro"),
                              yaxis = list(title = "ComputerSkills"),
                              zaxis = list(title = "Current Salary")),
                 legend = list(title = list(text = 'Cluster')))
      })
      
  })
  
  
}

shinyApp(ui = ui, server = server)
