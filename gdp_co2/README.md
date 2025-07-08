# 🌍 Sustainable Growth Dashboard

An interactive Shiny dashboard to explore the relationship between economic growth and greenhouse gas (GHG) emissions. Built to help identify countries that are reducing CO₂ emissions without sacrificing economic performance.

## 🔎 Features

- **Bubble Plot**  
  Explore cross-sectional comparisons between key indicators like GDP Growth, CO₂ Growth, and population size, with filters for top/bottom performers.

- **Line Chart**  
  Visualize trends over time across countries, continents, or regions for metrics like GDP per CO₂, CO₂ emissions, and more.

- **Interactive Data Table**  
  Filter, summarize, and export the raw data. Group by country, region, or continent. Choose the columns you want to view.

## 📁 Project Structure

```
gdp_co2/
├── global.R                # Data loading and preprocessing
├── ui.R                   # UI layout and theming
├── server.R               # Server logic and module handling
├── data/
│   └── data.csv           # Main dataset
├── modules/
│   ├── bubble_plot.R
│   ├── bubble_plot_server.R
│   ├── line_chart.R
│   └── line_chart_server.R
└── .Rprofile              # Deployment config
```

## 🚀 Deployment (shinyapps.io)

To deploy this app to [shinyapps.io](https://www.shinyapps.io/):

1. Install the **rsconnect** package:
   ```r
   install.packages("rsconnect")
   ```

2. Set your account info (replace with your credentials):
   ```r
   rsconnect::setAccountInfo(name='yourname', token='yourtoken', secret='yoursecret')
   ```

3. Deploy the app:
   ```r
   rsconnect::deployApp("path/to/gdp_co2")
   ```

> _Tip:_ You can also deploy from RStudio via **Tools → Publish to Server...**

## 📦 Requirements

- R (≥ 4.0)
- R packages:
  - `shiny`, `shinydashboard`, `tidyverse`, `plotly`, `ggiraph`, `bslib`, `countrycode`, `shinyWidgets`, `shinycssloaders`, `dashboardthemes`, `DT`, `patchwork`, `hrbrthemes`, `gganimate`

Install all dependencies using:
```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "ggiraph", "bslib",
                   "countrycode", "shinyWidgets", "shinycssloaders", "dashboardthemes",
                   "DT", "patchwork", "hrbrthemes", "gganimate"))
```

## 📊 Data Source

Global Data on Sustainable Energy (2000–2020):  
[Kaggle Dataset](https://www.kaggle.com/datasets/anshtanwar/global-data-on-sustainable-energy)

---

Built with ❤️ using R and Shiny.