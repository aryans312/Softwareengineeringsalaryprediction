Project Overview
This repository hosts an innovative project aimed at analyzing and predicting software engineering salaries using an interactive R Shiny application and an accompanying Power BI dashboard. The project leverages advanced data preprocessing, dynamic visualizations, and predictive analytics to uncover insights into salary trends, company attributes, and geographical variations. By combining intuitive dashboards with robust predictive modeling, this application empowers users to make informed decisions based on real-world data.

At the outset, we would like to express our heartfelt gratitude to Mr. Sumit Kumar, Assistant Professor in the Department of Computer Science and Engineering at SKIT Jaipur. His expert guidance, constructive feedback, and unwavering support were instrumental in the successful completion of this project.

Features
Interactive Dashboard:

Explore salary trends, analyze company attributes, and examine geographical salary distributions.
Utilize heatmaps and choropleths to visualize state-wise average salaries and company density.
Customize data exploration with dynamic filters for salary ranges, company names, states, and company scores.
Advanced Predictive Analytics:

Predict software engineering salaries based on inputs such as company score, location, and other key attributes.
Evaluate prediction accuracy using metrics such as Mean Absolute Error (MAE), Root Mean Squared Error (RMSE), and R-Squared.
Google Authentication:

Secure and user-friendly login with Google OAuth2 ensures personalized access to the dashboard.
Enhanced User Interface:

Elegant design powered by CSS and Shiny themes for a seamless and visually appealing experience.
Prerequisites and Installation
To use this application, ensure the following setup:

Required R Packages: The application relies on these libraries:

shiny, shinydashboard, shinythemes, dplyr, ggplot2, usmap, shinyjs, scales, httr, tidyr, and stringr.
Install these by running:


install.packages(c("shiny", "shinydashboard", "shinythemes", "dplyr", "ggplot2", "usmap", "shinyjs", "scales", "httr", "tidyr", "stringr"))
Dataset: Place the Software Engineer Salaries.csv file in the root directory of the project.

Run the App: Clone the repository to your local machine:


git clone https://github.com/username/software-salary-dashboard.git
cd software-salary-dashboard
Launch the application with the following command in R:


shiny::runApp()
Open your browser and navigate to http://localhost:8000 to access the dashboard.

Usage
Navigate through the following sections of the dashboard for a comprehensive analysis:

Home: Learn about the project and its features.
Dashboard: Access dynamic visualizations for salary trends, company attributes, and geographical insights.
Prediction: Input specific attributes to predict software engineering salaries.
Login: Securely log in using your Google credentials for a personalized experience.
Screenshots
For a detailed view of the application's features, refer to the Project Screenshots.pptx file included in the repository. It showcases various dashboards and visualizations, including state-wise heatmaps and salary distributions.

Contributors
This project was developed by:

Aryan Sharma (22ESKCX015)  

Dhruv Gupta (22ESKCX028)

We extend our deepest thanks to Mr. Sumit Kumar, Assistant Professor, SKIT Jaipur, for his mentorship and insightful suggestions throughout the project.

License
This project is distributed under the MIT License. For more details, refer to the LICENSE file included in the repository.













