
# Texas real estate market analysis

This project is my solution to a descriptive statistics task using a CSV dataset of the Texas real estate market. I used R, RStudio, and relevant R packages to extract core business insights from the data. Developed for a first-level postgraduate master’s program at ProfessionAi.


## Overview and goals

For this task i was provided with a 240 sample .CSV dataset concerning four Texas cities and a real estate company housing market set of variables. Although the attached dataset is raw and untouched by myself, the goal of this descriptive statistics project is not to teach efficient data preprocessing, therefore the dataset is ready for EDA and doesn't require further attention under that departament.

Regarding the company/project goals i was assigned with the following tasks:

- Variables assessment and description, with specific attention to variables with a time-series nature;
- Calculate measures of shape, central tendency and variability indexes;
- Identify the variables which show the highest level of skewness and kurtosis;
- Select a quantitative variable and split it by classes, calculate the Gini index;
- Calculate the probabilities of a single randomly extracted row to pertain to the city of "Beaumont", also calculate the probability that it shows "july" as a month or "December 2012" specifically;
- Create the "mean price" and "listings efficiency" variables starting from available data;
- Use "dplyr" to analyze the base statistics conditioned by city, year and month, use summaries to calculate mean and standard deviation and plot charts;
- Use ggplot 2 to plot visually appealing charts.


## Content

The following content is in this repo:

- main_english.R: RStudio main worksheet for this project;

- realestate_texas.CSV: The original raw dataset the assignment was based on, no preprocessing needed on this one if your goal is a descriptive statistics EDA.

- renv.lock: needed to recover the project dependencies used in RStudio;

- project_paper_knit R HTML file: The knit file required to generate either the PDF or HTML output through Latex contaning the whole assignment.

- project_paper_knit HTML: The completed assignment knitted in HTML. You may open it with any browser and evaluate my solution to the project, it also uses the "city_probability_animation.gif" and "months_probability_animation.gif" image files in the folder

- renv folder with settings.json: needed for renv.lock extraction and renv operations to restore the dependencies.
## RStudio Package Requirements

Although learning how to properly use R and Rstudio were among the main goals of this task, "ggplot2" and "dplyr" were also required for some of its assignments.

To further underline the results I also used other RStudio packages which will be mandatory to open this repository in RStudio and run R. 

To properly list all of the packages required for this repo, i used renv to create a renv.lock file the user can load into RStudio in order to install all of the required packages.

Follow these instructions:

- clone this github repo

```
 git clone https://github.com/Sandbox1115/texas-real-estate-market-analysis.git
```

- Open RStudio;

- Set the repo directory contaning the renv.lock file as working directory in RStudio;

- If you never installed the renv package, then do it now:

```
install.packages("renv")
```
- When renv is installed proceed to restore the packages by calling the follwoing command:

```
renv::restore()
```

renv should now reinstall all the required packages listed in the lock file to their original version.

## Credits and Contributions

This project was developed by Sandbox1115.

If you have any question, advice, or want to share your own findings and solution to this project raw dataset please get in touch, i'd be glad to evaluate your work and grow up professionally toghether.

### License

You are allowed to share and adapt this project by copying, redistributing, remixing, transforming or building on the original content made by me through the **Creative Commons Attribution-NonCommercial 4.0 International CC BY-NC 4.0** License. Please do not forget attrbituion the the original content creator. 
