library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(usmap)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(httr)

# Load and preprocess data
df <- read.csv("Software Engineer Salaries.csv")
df <- df[ , -5]
df$Salary <- gsub("\\s*\\([^\\)]+\\)", "", df$Salary)
df$Company.Score[is.na(df$Company.Score)] <- 3.89

df <- df %>%
  mutate(Salary = str_replace_all(Salary, "\\$", "")) %>%
  separate(Salary, into = c("Lower_range", "Upper_range"), sep = " - ") %>%
  mutate(Lower_range = str_replace(Lower_range, "K", "000"),
         Upper_range = str_replace(Upper_range, "K", "000"))

df <- df %>%
  mutate(Lower_range = ifelse(str_detect(Lower_range, "Per Hour"),
                              as.numeric(str_replace(Lower_range, " Per Hour", "")) * 2080,
                              as.numeric(str_replace(Lower_range, "K", "000"))),
         Upper_range = ifelse(str_detect(Upper_range, "Per Hour"),
                              as.numeric(str_replace(Upper_range, " Per Hour", "")),
                              Upper_range),
         Upper_range = str_replace_all(Upper_range, "[^0-9]", ""),  
         Upper_range = as.integer(Upper_range))

df$Upper_range[is.na(df$Upper_range)] <- 153000
df$Lower_range[is.na(df$Lower_range)] <- 100000
mean_lower <- mean(df$Lower_range, na.rm = TRUE)
mean_upper <- mean(df$Upper_range, na.rm = TRUE)

df <- df %>%
  mutate(
    lower_range = ifelse(Lower_range < 1000, mean_lower, Lower_range),
    upper_range = ifelse(Upper_range < 1000, mean_upper, Upper_range),
    Salary = (lower_range + upper_range) / 2,
    Salary = as.integer(Salary)
  ) %>%
  select(-Lower_range, -Upper_range)

df <- df %>%
  mutate(location = Location) %>%
  separate(Location, into = c("City", "State"), sep = ",", extra = "merge") %>%
  mutate(State = trimws(State), City = trimws(City))




# Google OAuth credentials
google_client_id <- "695491232934-2qj7ofvi87ggm6dmcst86q3ublnplmpj.apps.googleusercontent.com"

google_client_secret <- "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"

# Define the OAuth scopes
scopes <- c("https://www.googleapis.com/auth/userinfo.profile",
            "https://www.googleapis.com/auth/userinfo.email")




# UI with Dashboard Layout and Custom Styling
ui <- dashboardPage(
  dashboardHeader(
    title = "Company Data Dashboard",
    # Adding Logout Button as an Action Button inside the header
    tags$li(
      class = "dropdown",
      uiOutput("logout_button_ui")
    )
  ),
  
  # Correctly defined dashboardSidebar using dashboardSidebar()
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Prediction", tabName = "prediction", icon = icon("calculator")),
      menuItem("Login", tabName = "login", icon = icon("google"))
    )
  ),
  
  dashboardBody(
    # Apply shinytheme for UI styling
    theme = shinytheme("flatly"),
    
    # Custom CSS for the Home Page and other components
    tags$style(HTML("
      /* Custom CSS for Card-like Styling */
      .home-box {
        background-color: #ffffff;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        padding: 20px;
        margin-bottom: 30px;
        border-radius: 10px;
        border: 1px solid #ddd;
      }
      .home-header {
        font-size: 36px;
        font-weight: bold;
        color: #2980b9;
        padding-bottom: 20px;
        text-align: center;
        margin-top: 30px;
      }
      .home-text {
        font-size: 20px;
        color: #34495e;
        text-align: center;
        margin-bottom: 20px;
      }
      .box-header {
        background-color: #3498db;
        color: white;
        font-size: 22px;
        font-weight: bold;
        padding: 15px;
        border-radius: 10px 10px 0 0;
      }
      .box-body {
        font-size: 16px;
        color: #7f8c8d;
        text-align: left;
        line-height: 1.6;
      }
     
      /* Customize the Back Button */
      .btn-primary {
        background-color: #2980b9;
        color: white;
        border-radius: 5px;
        padding: 10px;
        font-size: 16px;
      }
      .btn-primary:hover {
        background-color: #3498db;
      }
    ")),
    
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, title = "Welcome to the Dashboard", status = "primary", solidHeader = TRUE,
                    class = "home-box",
                    p(class = "home-header", "Explore Company Data Insights"),
                    p(class = "home-text", "This dashboard provides insights into various company attributes, salary trends, and predictive analytics. Use the dashboard features to explore the data and make informed decisions.")
                )
              ),
              fluidRow(
                box(width = 6, status = "info", solidHeader = TRUE, class = "home-box",
                    title = "About the Project",
                    p("This project aims to predict software engineering salaries based on various factors such as company score, location, and other company attributes."),
                    p("The dashboard provides detailed visualizations for salary trends, company score analysis, geographical distribution of companies, and much more.")
                ),
                box(width = 6, status = "warning", solidHeader = TRUE, class = "home-box",
                    title = "Features",
                    p("Key features include:"),
                    tags$ul(
                      tags$li("Salary distribution and trend analysis"),
                      tags$li("Company Score vs Salary correlation"),
                      tags$li("State-wise average salary heatmap"),
                      tags$li("Geographical distribution of companies")
                    ),
                    p("The app also evaluates prediction model performance with key metrics such as MAE, RMSE, and R-Squared.")
                )
              )
      ),
      
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                # Value Boxes for Key Metrics
                valueBoxOutput("avg_salary"),
                valueBoxOutput("avg_company_score"),
                valueBoxOutput("num_companies")
              ),
              fluidRow(
                # Filters and Inputs
                box(width = 4,
                    sliderInput("salaryRange", "Salary Range:",
                                min = min(df$Salary, na.rm = TRUE),
                                max = max(df$Salary, na.rm = TRUE),
                                value = c(min(df$Salary, na.rm = TRUE), max(df$Salary, na.rm = TRUE)),
                                step = 5000)),
                box(width = 4,
                    selectInput("company", "Select Company:",
                                choices = c("All", unique(df$Company)))),
                box(width = 4,
                    selectInput("state", "Select State:",
                                choices = c("All", unique(df$State)))),
                box(width = 4,
                    selectInput("companyScore", "Select Company Score:",
                                choices = c("All", unique(df$Company.Score)))),
                box(width = 4,
                    actionButton("refresh", "Refresh Data", icon = icon("sync"), class = "btn-danger"))
              ),
              fluidRow(
                # Visualizations and Data Outputs
                box(width = 12, title = "Top Companies", status = "primary", solidHeader = TRUE,
                    plotOutput("top_companies")),
                box(width = 12, title = "Salary Trend", status = "primary", solidHeader = TRUE,
                    plotOutput("salary_trend")),
                box(width = 12, title = "Company Score vs Salary", status = "primary", solidHeader = TRUE,
                    plotOutput("score_vs_salary")),
                box(width = 12, title = "Salary Distribution", status = "primary", solidHeader = TRUE,
                    plotOutput("salary_distribution")),
                box(width = 12, title = "Salary Density Plot", status = "primary", solidHeader = TRUE,
                    plotOutput("salary_density")),
                box(width = 12, title = "Company Locations Heatmap", status = "primary", solidHeader = TRUE,
                    plotOutput("company_heatmap")),
                box(width = 12, title = "Average Salary by State", status = "primary", solidHeader = TRUE,
                    plotOutput("salary_choropleth"))
                
              )
      ),
      
      # Prediction Tab
      tabItem(tabName = "prediction",
              fluidRow(
                box(width = 12, title = "Prediction Settings", status = "primary", solidHeader = TRUE,
                    selectInput("pred_company", "Select Company:", choices = c("All", unique(df$Company))),
                    sliderInput("pred_score", "Select Company Score Range:", min = 1, max = 5, value = c(2, 4), step = 0.1),
                    selectInput("pred_state", "Select State for Prediction:", choices = c("All", unique(df$State))),
                    actionButton("predict_btn", "Make Prediction", icon = icon("play-circle"), class = "btn-success")
                )
              ),
              fluidRow(
                box(width = 12, title = "Predicted Salary", status = "success", solidHeader = TRUE,
                    textOutput("predicted_salary")),
                box(width = 12, title = "Predicted Companies", status = "success", solidHeader = TRUE,
                    tableOutput("predicted_companies")),
                box(width = 12, title = "Prediction Visualization", status = "primary", solidHeader = TRUE,
                    plotOutput("prediction_plot"))
              )
      ),
      
      # Google Authentication Tab
      tabItem(tabName = "login",
              fluidRow(
                box(width = 12, title = "Login with Google", status = "primary", solidHeader = TRUE,
                    actionButton("login_btn", "Login with Google", icon = icon("google"))
                ),
                box(width = 12, title = "User Info", status = "info", solidHeader = TRUE,
                    textOutput("user_info")
                )
              )
      )
      
      
    )
  )
)




# Server Function
server <- function(input, output, session) {
  
  
  
  # Google OAuth authentication
  observeEvent(input$login_btn, {
    req(input$login_btn)
    
    # Initiate OAuth 2.0 authentication
    oauth_endpoints("google")
    myapp <- oauth_app("google", key = google_client_id, secret = google_client_secret)
    goog_auth <- oauth2.0_token(oauth_endpoints("google"), myapp, scope = scopes, cache = FALSE)
    
    # Get user information
    user_info <- GET("https://www.googleapis.com/oauth2/v2/userinfo", config(token = goog_auth))
    user_info_data <- content(user_info)
    
    output$user_info <- renderText({
      paste("Hello,", user_info_data$name)
    })
    
    
    user_logged_in(TRUE)
    shinyjs::runjs('window.location.href = "http://localhost:8000/dashboard";')
  })
  
  user_logged_in <- reactiveVal(FALSE)
  # Render the logout button UI conditionally based on user login status
  output$logout_button_ui <- renderUI({
    if (user_logged_in()) {
      # If user is logged in, show the logout button
      actionButton("logout_btn", "Logout", class = "btn-danger")
    }
  })
  
  
  
  # Logout logic: when user clicks logout button
  observeEvent(input$logout_btn, {
    showModal(
      modalDialog(
        title = "Confirm Logout",
        "Are you sure you want to log out?",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_logout", "Logout", class = "btn-danger")
        )
      )
    )
  })
  
  
  observeEvent(input$confirm_logout, {
    removeModal()  # Close the confirmation modal
    
    
    user_logged_in(FALSE)
    
    # Invalidate the token and clear session variables
    session$reload()
  })
  
  
  
  # Data filtering, plotting, etc.
  filtered_data <- reactive({
    df %>%
      filter(
        Salary >= input$salaryRange[1] & Salary <= input$salaryRange[2],
        (Company == input$company | input$company == "All"),
        (State == input$state | input$state == "All"),
        (Company.Score == input$companyScore | input$companyScore == "All")
      )
  })
  
  # Outputs for value boxes
  output$avg_salary <- renderValueBox({
    valueBox(
      formatC(mean(df$Salary, na.rm = TRUE), format = "d", big.mark = ","),
      "Average Salary", icon = icon("dollar-sign"), color = "green"
    )
  })
  
  output$avg_company_score <- renderValueBox({
    valueBox(
      round(mean(df$Company.Score, na.rm = TRUE), 2),
      "Average Company Score", icon = icon("star"), color = "blue"
    )
  })
  
  output$num_companies <- renderValueBox({
    valueBox(
      n_distinct(df$Company),
      "Number of Companies", icon = icon("building"), color = "purple"
    )
  })
  
  # Output for model evaluation metrics
  output$mae_value <- renderText({
    paste("MAE:", round(mean(abs(predictions_lm - test_df_filtered$Salary)), 2))
  })
  output$rmse_value <- renderText({
    paste("RMSE:", round(sqrt(mean((predictions_lm - test_df_filtered$Salary)^2)), 2))
  })
  output$rsq_value <- renderText({
    paste("R-Squared:", round(summary(model_lm)$r.squared, 2))
  })
  
  # Plot Outputs
  output$top_companies <- renderPlot({
    top_companies <- filtered_data() %>%
      count(Company) %>%
      arrange(desc(n)) %>%
      top_n(20, n)
    
    ggplot(top_companies, aes(x = reorder(Company, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black") +  
      labs(title = "Top 20 Companies by Frequency", x = "Company", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$salary_trend <- renderPlot({
    ggplot(filtered_data(), aes(x = Company.Score, y = Salary)) +
      geom_line(color = "green", size = 1) +
      labs(title = "Salary Trend by Company Score", x = "Company Score", y = "Salary") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
  })
  
  output$score_vs_salary <- renderPlot({
    ggplot(filtered_data(), aes(x = Company.Score, y = Salary)) +
      geom_point(color = "orange", size = 2) +
      labs(title = "Company Score vs Salary", x = "Company Score", y = "Salary") +
      theme_minimal()
  })
  
  output$salary_distribution <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(Company.Score), y = Salary)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = "Salary Distribution by Company Score", x = "Company Score", y = "Salary") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
  })
  
  output$salary_density <- renderPlot({
    ggplot(filtered_data(), aes(x = Salary)) +
      geom_density(fill = "purple", alpha = 0.5) +
      labs(title = "Salary Density Plot", x = "Salary", y = "Density") +
      scale_x_continuous(labels = comma) +
      theme_minimal()
  })
  
  output$company_heatmap <- renderPlot({
    heatmap_data <- filtered_data() %>%
      mutate(Count = 1) %>%
      group_by(State) %>%
      summarise(Count = sum(Count), .groups = 'drop') %>%
      rename(state = State)
    
    plot_usmap(data = heatmap_data, values = "Count", regions = "states") +
      scale_fill_continuous(name = "Number of Companies", low = "lightblue", high = "darkblue") +
      labs(title = "Heatmap of Company Locations in the US") +
      theme_minimal()
  })
  
  output$salary_choropleth <- renderPlot({
    salary_data <- filtered_data() %>%
      group_by(State) %>%
      summarise(Average_Salary = mean(Salary, na.rm = TRUE)) %>%
      rename(state = State)
    
    plot_usmap(data = salary_data, values = "Average_Salary", regions = "states") +
      scale_fill_continuous(name = "Average Salary", low = "yellow", high = "red") +
      labs(title = "Average Salary by State in the US") +
      theme_minimal()
  })
  
  # Prediction Logic for the "Prediction" tab
  observeEvent(input$predict_btn, {
    # Implement prediction logic here
    selected_data <- df %>%
      filter(
        (Company == input$pred_company | input$pred_company == "All"),
        (State == input$pred_state | input$pred_state == "All"),
        (Company.Score >= input$pred_score[1] & Company.Score <= input$pred_score[2])
      )
    
    predicted_salary <- mean(selected_data$Salary, na.rm = TRUE)
    
    output$predicted_salary <- renderText({
      paste("Predicted Salary:", formatC(predicted_salary, format = "d", big.mark = ","))
    })
    
    output$predicted_companies <- renderTable({
      selected_data %>%
        select(Company, State, Salary) %>%
        distinct() %>%
        head(10)  # Show top 10 companies
    })
    
    output$prediction_plot <- renderPlot({
      ggplot(selected_data, aes(x = Company.Score, y = Salary)) +
        geom_point(color = "red", size = 3) +
        geom_smooth(method = "lm", color = "blue") +
        labs(title = "Prediction Trend for Salary vs Company Score", x = "Company Score", y = "Salary") +
        theme_minimal()
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server, options = list(port = 8000))
