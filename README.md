# R-Shiny Data Visualization Web App For Data Visualization
 
This R Shiny web application provides interactive visualizations for the dataset `data.csv`. It allows users to explore various aspects of the data through different chart types and interactive elements.

## Key Features:

1.  **Interactive Bubble Chart with K-Means Clustering:**
    * Visualizes data points as bubbles on a 2D plane.
    * Allows users to select variables for the X and Y axes.
    * Implements K-means clustering to group data points based on their values.
    * The number of clusters is user-defined.
    * Bubble colors correspond to the assigned clusters.

2.  **World Map Visualization:**
    * Displays a world map (specifically focused on Europe based on the dataset).
    * Colors countries on the map according to the clusters identified in the bubble chart.
    * Provides a geographical context for the clustered data.

3.  **Bar Plots Based on Overall Mean:**
    * Generates bar plots comparing the characteristics of selected countries against the overall mean values of those characteristics across the entire dataset.
    * Uses color-coding (Green, Red, Blue) to indicate whether a country's value for a specific characteristic is positive, negative, or not comparable relative to the mean.
    * Offers single country comparisons as well as grouped comparisons for multiple selected countries.

4.  **Interactive Grouped Bar Plots:**
    * Allows users to select multiple countries.
    * Displays a grouped bar plot showing the proportion of each selected country's characteristics relative to the European mean.
    * Facilitates direct comparison of multiple countries across different variables.

5.  **Interactive 2D and 3D Scatter Plots with Unidirectional Interaction:**
    * **2D Scatter Plot:**
        * Allows users to choose variables for the X and Y axes.
        * Displays data points with labels (country names).
        * Includes a linear regression smooth line.
        * Provides hover information for individual data points.
    * **3D Scatter Plot:**
        * Allows users to select variables for the X, Y, and Z axes.
        * **Unidirectional Interaction:** Points selected in the 2D scatter plot are highlighted in the 3D scatter plot.
        * Color of the points in the 3D plot represents the 'Area' of the corresponding country.
        * Provides interactive rotation and zooming.

6.  **Interactive Data Table with Dynamic Filtering:**
    * Presents the underlying data in an interactive table.
    * Includes slider inputs for various numerical columns (Area, GDP, Inflation, Military, Life Expectancy, Population Growth, Unemployment).
    * The data table automatically updates and filters rows based on the selected ranges in the sliders, allowing for dynamic data exploration.

## Technologies Used:

* **R:** The primary programming language.
* **Shiny:** For building the interactive web application.
* **shinydashboard:** For a structured and visually appealing user interface.
* **googleCharts:** For creating interactive Google Charts visualizations (bubble chart).
* **ggplot2:** For generating static and customizable plots (bar plots, 2D scatter plot).
* **plotly:** For creating interactive 3D scatter plots.
* **dplyr:** For data manipulation and transformation.
* **tidyr:** For data reshaping (long to wide format and vice versa).
* **countrycode:** For converting country names to ISO codes for map visualization.
* **rworldmap:** For plotting world map data.
* **ggrepel:** For preventing text labels from overlapping in the scatter plots.
* **DT:** For creating interactive data tables.
* **reshape2:** For data reshaping (melt function used for bar plots).

## Data Source:

The application uses a CSV file named `data.csv`. Ensure this file is present in the same directory as the Shiny app files or provide the correct path in the code. The `data.csv` file is expected to have the following columns (with the 5th and 7th columns being renamed):

| Column 1 | Column 2 | Column 3 | Column 4 | Life expectancy (originally Column 5) | Column 6 | Population growth (originally Column 7) | Column 8 | ... |
| :------- | :------- | :------- | :------- | :------------------------------------- | :------- | :----------------------------------------- | :------- | :-- |
| ...      | ...      | ...      | ...      | ...                                    | ...      | ...                                        | ...      | ... |

## Setup and Running the App:

1.  **Install Required Packages:**
    ```R
    install.packages(c("shiny", "shinydashboard", "googleCharts", "maps", "mapproj", "dplyr", "ggplot2", "tidyr", "gridExtra", "countrycode", "rworldmap", "ggrepel", "leaflet", "plotly", "tidyverse", "gghighlight", "reshape2", "DT"))
    # For googleCharts, you might need to install from GitHub:
    # if(!requireNamespace("devtools", quietly = TRUE)){
    #     install.packages("devtools")
    # }
    # devtools::install_github("jcheng5/googleCharts")
    # If you encounter issues with googleCharts on Linux, try:
    # sudo apt-get update
    # sudo apt-get install libcurl4-openssl-dev
    # sudo apt-get install libssl-dev
    ```

2.  **Save the Code:** Save the provided R code as a single file (e.g., `app.R`).

3.  **Place `data.csv`:** Ensure that the `data.csv` file is in the same directory as the `app.R` file.

4.  **Run the App:** Open R or RStudio, navigate to the directory where you saved the files, and run the following command:
    ```R
    shiny::runApp()
    ```

This will launch the Shiny web application in your default web browser.

## Further Development/Potential Improvements:

* Add tooltips or more detailed information on hover for all chart types.
* Implement brushing and linking between more chart types for enhanced interactive exploration.
* Allow users to download the filtered data from the data table.
* Incorporate more advanced statistical analysis or modeling features.
* Improve the visual aesthetics and user interface based on user feedback.
* Add error handling for cases where the `data.csv` file is missing or improperly formatted.
* Make the map more interactive (e.g., zoom, pan, display country-specific information on click).
* Explore different clustering algorithms or allow users to customize clustering parameters.
