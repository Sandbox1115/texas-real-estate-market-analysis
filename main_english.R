#Worksheet .CSV dataset import
realestate_stats= read.csv("realestate_texas.csv", sep=",")

#Let's take a look at datasets' head to check on elements and variables.

head(realestate_stats)
dim(realestate_stats)

#Dataset variables identification

str(realestate_stats)

#Proceeding to calculate mode for "city" variable
#Since it is a nominal qualitative variable, we can only consider
#this measure of central tendency

#R does not have a built-in function for calculating mode 
#therefore we will develop one for this purpose

mode = function(x){
  u = unique(x)
  tab = tabulate(match(x, u))
  u[tab == max(tab)]
}

city_mode = c(realestate_stats$city)
mode(city_mode)

#Using the table function to verify whether variable is quadrimodal
table(realestate_stats$city)

#year variable min and max functions
min(realestate_stats$year)
max(realestate_stats$year)

#frequency distribution for "year" variable.
cytable = addmargins(table(realestate_stats$city, realestate_stats$year), 2)
print(cytable)

#month variable range
range(realestate_stats$month)

#A final check for the presence of all 12 months in each year
#can be performed by taking the first 12 rows of the month variable from the dataset
print(realestate_stats$month[1:12])

#sales variable. min/max values calculation
minsales = min(realestate_stats$sales)
maxsales = max(realestate_stats$sales)
sprintf("sales variable minimum is: %i", minsales)
sprintf("sales variable maximum is: %i", maxsales)

#Let's calculate the median
mediansales = median(realestate_stats$sales)
sprintf("sales variable median is: %.2f", mediansales)

#sales variable mean calculation
meansales = mean(realestate_stats$sales)
sprintf("The mean of the quantities sold across all cities and all the years considered is: %.2f", meansales)

#sales quartiles calculation
quantile_sales = quantile(realestate_stats$sales, probs= c(.25,.75))
print(quantile_sales)

#calculating sales range:
varinterval = (maxsales-minsales)
sprintf("range for sales variable is: %i", varinterval)

#let's move on by calculating sales interquartile range:
intersales = IQR(realestate_stats$sales)
sprintf("sales variable interquartile range is: %i", intersales)

#sales variable variance calculation:
salesvar = var(realestate_stats$sales)
sprintf("Sales variable has a variance of: %.1f", salesvar)

#Let's move on to calculate standard deviation by using the dedicated function:
stand_dev_sales = sd(realestate_stats$sales)
sprintf("Sales standard deviation is: %.3f", stand_dev_sales)

##Let's write the coefficient of variation function.
coeff_var = function(x){
  return(sd(x)/mean(x)*100)
}
#Sales variable coefficient of variation calculation
salescv = coeff_var(realestate_stats$sales)
sprintf("sales variable coefficient of variation is: %.3f", salescv)

#Let's proceed to calculate, for this first time, the skewness index by hand
salesn = length(realestate_stats$sales)
salesm3 = sum((realestate_stats$sales-meansales)^3)/salesn

asimmetry_sales_index = salesm3/stand_dev_sales^3
plot(density(realestate_stats$sales))
abline(v=meansales, col="red")
sprintf("sales skewness index is: %.3f", asimmetry_sales_index)

#Manual Kurtosis calculation
salesm4 = sum((realestate_stats$sales-meansales)^4)/salesn
kurtosis_sale_index = salesm4/stand_dev_sales^4 -3 
sprintf("Sales Kurtosis index is: %.3f", kurtosis_sale_index)

#Let's move to volume variable. Measures of central tendency.
minvolume = min(realestate_stats$volume)
maxvolume = max(realestate_stats$volume)
meanvolume = mean(realestate_stats$volume)
medianvolume = median(realestate_stats$volume)
modevolume = mode(realestate_stats$volume)
quantile_volume = quantile(realestate_stats$volume, probs = c(.25, .75))
sprintf("Volume variable has a min/max of: %.3f %.3f", minvolume, maxvolume)
sprintf("Volume variable mean is: %.3f", meanvolume)
sprintf("Volume variable meadian is: %.3f", medianvolume)
sprintf("Volume variable is trimodal: %.3f", modevolume)
print(quantile_volume)

#Volume measures of dispersion
volumevarinterval = (maxvolume-minvolume)
interquartile_volume = IQR(realestate_stats$volume)
volumevar = var(realestate_stats$volume)
volumesd = sd(realestate_stats$volume)
volume_coeff_var = coeff_var(realestate_stats$volume)
sprintf("Volume range is: %.3f", volumevarinterval)
sprintf("Volume interquartile range is: %.3f", interquartile_volume)
sprintf("Volume variance is: %.3f", volumevar)
sprintf("Volume standard deviation is: %.3f", volumesd)
sprintf("Volume coefficient of variation is: %.3f", volume_coeff_var)

#volume, shape indices
library("moments")
asimmetry_volume_index = skewness(realestate_stats$volume)
kurtosis_volume_index = kurtosis(realestate_stats$volume)-3
plot(density(realestate_stats$volume))
abline(v=meanvolume, col="red")
sprintf("Volume skewness is: %.3f", asimmetry_volume_index)
sprintf("Volume kurtosis is: %.3f", kurtosis_volume_index)

#median_price, measures of central tendency
minmp = min(realestate_stats$median_price)
maxmp = max(realestate_stats$median_price)
meanmp = mean(realestate_stats$median_price)
medianmp = median(realestate_stats$median_price)
quantile_mp = quantile(realestate_stats$median_price, probs = c(.25, .75))
sprintf("median_price variable has a min/max of: %i %i", minmp, maxmp)
sprintf("median_price variable mean is: %.3f", meanmp)
sprintf("median_price variable median is: %i", medianmp)
sprintf("median_price variable is unimodal: %i", modemp)
print(quantile_mp)

#median_price, mesures of dispersion.
mpvarinterval = (maxmp-minmp)
interquartile_mp = IQR(realestate_stats$median_price)
mpvar = var(realestate_stats$median_price)
mpsd = sd(realestate_stats$median_price)
mp_coeff_var = coeff_var(realestate_stats$median_price)
sprintf("median_price variable range is: %i", mpvarinterval)
sprintf("median_prince interquartile range: %i", interquartile_mp)
sprintf("mediane_prince variance is: %.3f", mpvar)
sprintf("median_prince standard deviation is: %.3f", mpsd)
sprintf("median_prince coefficient of variation is: %.3f", mp_coeff_var)

#median_price, shape indices
library("ggplot2")
asimmetry_mp_index = skewness(realestate_stats$median_price)
kurtosis_mp_index = kurtosis(realestate_stats$median_price)-3

ggplot()+
  geom_density(aes(x=realestate_stats$median_price), col="black", fill="palegreen")+
  geom_vline(xintercept =meanmp, linewidth=1.5, color = "lightcoral")+
  labs(x="Median Price", y="Density")+
  geom_text(aes(x = meanmp + 20000, y = 1.8e-05, label = sprintf("Mean\n %.3f", meanmp)), inherit.aes = FALSE) +
  ylim(0, 2e-05)

sprintf("median_price skewness is: %.3f", asimmetry_mp_index)
sprintf("median_prince kurtosis is: %.3f", kurtosis_mp_index)

#listings variable, measures of central tendency
minlist = min(realestate_stats$listings)
maxlist = max(realestate_stats$listings)
meanlist = mean(realestate_stats$listings)
medianlist = median(realestate_stats$listings)
quantile_list = quantile(realestate_stats$listings, probs = c(.25, .75))
sprintf("listings variable has a min/max of: %i %i", minlist, maxlist)
sprintf("listings variable mean is: %.3f", meanlist)
sprintf("listings variable median is: %.2f", medianlist)
sprintf("listings variable is unimodal: %i", modelist)
print(quantile_list)

#listings variable, measures of dispersion
listvarinterval = (maxlist-minlist)
interquartile_list = IQR(realestate_stats$listings)
listvar = var(realestate_stats$listings)
listsd = sd(realestate_stats$listings)
list_coeff_var = coeff_var(realestate_stats$listings)
sprintf("listings variable range is: %i", listvarinterval)
sprintf("listings variable interquartile range is: %.2f", interquartile_list)
sprintf("listings variance is: %.3f", listvar)
sprintf("listings variable standard deviation is: %.3f", listsd)
sprintf("listings variable coefficient of variation is: %.3f", list_coeff_var)

#To determine the y-scale more precisely
#by which to place the label "mean", I used the density function
#applied to the listings variable. The max index of the listings density on y was used
#to correctly position the "mean" label in the plot
density_data = density(realestate_stats$listings)
max_listdensity = max(density_data$y)

#listings variable, shape indices
library("ggplot2")
asimmetry_lists_index = skewness(realestate_stats$listings)
kurtosis_lists_index = kurtosis(realestate_stats$listings)-3

ggplot()+
  geom_density(aes(x=realestate_stats$listings), col="black", fill="palegreen")+
  geom_vline(xintercept = meanlist, linewidth=1.5, color = "lightcoral")+
  labs(x="Active listings", y="Density")+
  geom_text(aes(x = meanlist+500, y = max_listdensity, label = sprintf("Mean\n %.3f", meanlist)), inherit.aes = FALSE) +
  xlim(0, 5000)

sprintf("listings variable skewness is: %.3f", asimmetry_lists_index)
sprintf("listings variable kurtosis is: %.3f", kurtosis_lists_index)

#months_inventory variable, measures of central tendency
minmonths = min(realestate_stats$months_inventory)
maxmonths = max(realestate_stats$months_inventory)
meanmonths = mean(realestate_stats$months_inventory)
medianmonths = median(realestate_stats$months_inventory)
modemonths = mode(realestate_stats$months_inventory)
quantile_months = quantile(realestate_stats$months_inventory, probs = c(.25, .75))
sprintf("months_inventory variable has a min/max of: %.2f %.2f", minmonths, maxmonths)
sprintf("month_inventory mean is: %.3f", meanmonths)
sprintf("months_inventory median is: %.2f", medianmonths)
sprintf("months_inventory variable is unimodal: %.2f", modemonths)
print(quantile_months)

#months_inventory variable, measures of dispersion
monthsvarinterval = (maxmonths-minmonths)
interquartile_months = IQR(realestate_stats$months_inventory)
monthsvar = var(realestate_stats$months_inventory)
monthssd = sd(realestate_stats$months_inventory)
months_coeff_var = coeff_var(realestate_stats$months_inventory)
sprintf("months_inventory variable range is: %.2f", monthsvarinterval)
sprintf("months_inventory variable interquartile range is: %.2f", interquartile_months)
sprintf("months_inventory variance is: %.3f", monthsvar)
sprintf("months_inventory variable standard deviation is: %.3f", monthssd)
sprintf("months_inventory variable coefficient of variance is: %.3f", months_coeff_var)

#months_inventory variable, shape indices
library("ggplot2")
asimmetry_months_index = skewness(realestate_stats$months_inventory)
kurtosis_months_index = kurtosis(realestate_stats$months_inventory)-3

invendensity = density(realestate_stats$months_inventory)
invenmaxdensity = max(invendensity$y)

ggplot()+
  geom_density(aes(x=realestate_stats$months_inventory), col="black", fill="palegreen")+
  geom_vline(xintercept = meanmonths, linewidth=1.5, color = "lightcoral")+
  labs(x="Remaining time for\n active listings depletion", y="Density")+
  geom_text(aes(x = meanmonths+2, y = invenmaxdensity, label = sprintf("Mean\n %.3f", meanmonths)), inherit.aes = FALSE)+
  xlim(0,20)

sprintf("months_inventory skewness is: %.3f", asimmetry_months_index)
sprintf("months_inventory kurtosis is: %.3f", kurtosis_months_index)

#Let's make the initial comparisons on variance and skewness for the available data
#we use ggplot2 and a lollipop chart to highlight the differences

#Comparison of variability

library("ggplot2")

cvarcomparison = data.frame(
  x = c(salescv, volume_coeff_var, mp_coeff_var, list_coeff_var, months_coeff_var),
  y = factor(c("Sales", "Total Sales Volume", "Median Price", "Listings", "Months per Inventory"),
             levels = c("Sales", "Total Sales Volume", "Median Price", "Listings", "Months per Inventory"))
)

# plotting a lollipop chart with ggplot2
ggplot(cvarcomparison, aes(x = x, y = y)) +
  geom_segment(aes(x = 0, xend = x, y = y, yend = y), color = "grey") +
  geom_point(aes(x = x), color = "red", fill=alpha("lightcoral", 0.3), alpha=0.7, shape=21, stroke=1.5, size = 5) +
  geom_text(aes(label=round(x, 3)), hjust = -0.3, color= "black")+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title.position = "plot"
  ) +
  xlab("Coefficients of variation") +
  ylab("")+
  labs(title = "Coefficients of variation comparison",
       subtitle = "Using the realestate_texas.csv dataset tuned accordingly")+
  coord_flip()

#Confronto di asimmetria

asimmetrycomparison = data.frame(
  x = c(asimmetry_sales_index, asimmetry_volume_index, asimmetry_mp_index, asimmetry_lists_index, asimmetry_months_index),
  y = factor(c("Vendite", "Volume di Vendita", "Prezzo Mediano", "Inserzioni", "Mesi allo Smaltimento"),
             levels = c("Vendite", "Volume di Vendita", "Prezzo Mediano", "Inserzioni", "Mesi allo Smaltimento"))
)

#Skewness comparison

asimmetrycomparison = data.frame(
  x = c(asimmetry_sales_index, asimmetry_volume_index, asimmetry_mp_index, asimmetry_lists_index, asimmetry_months_index),
  y = factor(c("Sales", "Total Sales Volume", "Median Price", "Listings", "Months per inventory"),
             levels = c("Sales", "Total Sales Volume", "Median Price", "Listings", "Months per inventory"))
)

# lollipop chart plotting through ggplot2
ggplot(asimmetrycomparison, aes(x = x, y = y)) +
  geom_segment(aes(x = 0, xend = x, y = y, yend = y), color = "grey") +
  geom_point(aes(x = x), color = "blue", fill=alpha("lightblue", 0.3), alpha=0.7, shape=21, stroke=1.5, size = 5) +
  geom_text(aes(label=round(x, 3)), hjust = -0.3, color= "black")+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title.position = "plot"
  ) +
  xlab("Fisher's Skewness") +
  ylab("")+
  labs(title = "Fisher Skewness comparison",
       subtitle = "Using the realestate_texas.csv dataset tuned accordingly")+
  coord_flip()

#We select the sales variable to perform a class division
#We develop its frequency distribution and display it in a bar chart
#We will then proceed to calculate the Gini index of the chosen variable

#sales, class division
salesclass = cut(realestate_stats$sales, breaks = seq(50,450, by=50), right = FALSE)

#calculating frequencies and organizing the distribution

sales_ndistribution = table(salesclass)
sales_frtable = sales_ndistribution/sum(sales_ndistribution)
sales_cumsum = cumsum(sales_ndistribution)
sales_cumrel = sales_cumsum/sum(sales_ndistribution)

finaltable = cbind(ni=sales_ndistribution, fi=sales_frtable, Ni=sales_cumsum, Fi=sales_cumrel)
colnames(finaltable) = c("Abs. Frequencies", "Rel. Frequencies", "Cumulated abs. frequenceis", "Cumulated rel. frequencies")
print(finaltable)

#Using the distribution with a ggplot2 chart.

#dataframe to use with the chart
saled_distribution_data = data.frame(
  salesclass = as.factor(names(sales_ndistribution)),
  n = as.vector(sales_ndistribution)
)

ggplot(saled_distribution_data, aes(x=salesclass, y = n, fill = salesclass))+
  geom_bar(stat = "identity", fill="grey")+
  scale_x_discrete(limits = levels(salesclass))+
  labs(x = "variable classes", y = "n observations")+
  theme(legend.position = "none",
        plot.title.position = "plot")+
  labs(title = "Sales variable frequency distributions",
       subtitle = "n observations separated into classes")


#Sales Gini heterogeneity index calculation
#let's build the function accordingly
gini.index = function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  J=length(table(x))
  
  gini= 1-sum(fi2)
  gini.normalizzato = gini/((J-1)/J)
  
  return (gini.normalizzato)
}

#proceeding to calculation
salesgini = gini.index(salesclass)
sprintf("sales variable frequency distribution has a Gini geterogeneity index of: %.3f", salesgini)

#Probability calculations

#We consider the probability that, with a sufficient number of trials, the city of Beaumont will be drawn from the dataset.
#Building the related chart.

library("ggplot2")
library("gganimate")
library("dplyr")
library("gifski")
library("knitr")

frames = lapply(1:10, function(i){
  data.frame(city = sample(realestate_stats$city, 1000000, replace = TRUE),
             frame = paste("Frame", i))
})

framedata = do.call(rbind, frames)

framedata_summary = framedata %>%
  group_by(city, frame) %>%
  summarise(count = n(), .groups = "drop")

animated_plot = ggplot(framedata_summary, aes(x = city, y = count, fill = city)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = "City", y = "Number of extractions") +
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('linear')+
  labs(title = "Random extraction probability for one of city variables modes",
       subtitle = "Frame: {closest_state}")+
  theme(legend.position = "none")

cityanimation = animate(animated_plot, fps = 15, duration = 5, nframes = 30, renderer = gifski_renderer())
anim_save("city_probability_animation.gif", animation = cityanimation)
knitr::include_graphics("city_probability_animation.gif")

#Classic probability for city variable.

city_classic_prob = (60/240)
city_relative_prob = (city_classic_prob*100)
sprintf("The plot visually represents what the classical probability calculation tells us about the city variable. With a sufficient number of extraction attempts, the city variable will yield a probability of %.1f%% obtaining the city of Beaumont", city_relative_prob)

#Let's consider the month variable for July
#We also build the related chart

library("ggplot2")
library("gganimate")
library("dplyr")
library("gifski")
library("knitr")

frames_month = lapply(1:10, function(i){
  data.frame(months = sample(realestate_stats$month, 1000000, replace = TRUE),
             frame = paste("Frame", i))
})

frame_month_data = do.call(rbind, frames_month)

framedatamonth_summary = frame_month_data %>%
  group_by(months, frame) %>%
  summarise(count = n(), .groups = "drop")

animated_plot_months = ggplot(framedatamonth_summary, aes(x = months, y = count, fill = months)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = "Mesi", y = "Number of extractions") +
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('linear')+
  labs(title = "Casual extraction probability for one of months variable modes",
       subtitle = "Frame: {closest_state}")+
  theme(legend.position = "none")

monthsanimation = animate(animated_plot_months, fps = 15, duration = 5, nframes = 30, renderer = gifski_renderer())
anim_save("months_probability_animation.gif", animation = monthsanimation)
knitr::include_graphics("months_probability_animation.gif")

#Classic probability for months variable

month_classic_prob = (20/240)
month_relative_prob = (month_classic_prob*100)
sprintf("The plot visually represents what the classical probability calculation tells us about the month variable. With a sufficient number of extraction attempts, the month variable will yield a probability of %.1f%% obtaining the month of July", month_relative_prob)

#We evaluate the probability that December 2012 is drawn from the dataset

#plotting the chart
library("ggplot2")
library("gganimate")
library("dplyr")
library("gifski")
library("knitr")


#probability calculation
twentyt_classic_prob = (4/240)
twentyt_relative_prob = (twentyt_classic_prob*100)

sprintf("The plot visually represents what the classical probability calculation tells us. With a sufficient number of extraction attempts, the December 2012 category will yield a probability of %.1f%% of being drawn.", twentyt_relative_prob)

set.seed(42) 
months_full_dataset = c(rep(1:59, each = 4), rep(60, times = 4))

frames_twentyt = lapply(1:10, function(i) {
  data.frame(
    month_category = sample(months_full_dataset, 1000000, replace = TRUE),
    frame = paste("Frame", i)
  )
})

frame_twentyt_data = do.call(rbind, frames_twentyt)

framedatatwentyt_summary = frame_twentyt_data %>%
  group_by(month_category, frame) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(color_group = ifelse(month_category == 60, "December", "Other months"))

animated_plot_twentyt = ggplot(framedatatwentyt_summary, aes(x = as.factor(month_category), y = count, fill = color_group)) +
  geom_bar(stat = 'identity', position = "dodge", width = 0.8) +
  theme_minimal() +
  labs(x = "Month category", y = "Number of extractions") +
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('linear')+
  labs(title = "Random extraction probability for December 2012",
       subtitle = "Frame: {closest_state}")+
  scale_fill_manual(values = c("December" = "lightcoral", "Other months" = "grey"))+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.margin = margin(1, 1, 1, 1, "cm"))

twentytanimation = animate(animated_plot_twentyt, 
                           fps = 15, 
                           duration = 5, 
                           nframes = 30, 
                           renderer = gifski_renderer()
)
anim_save("twentyt_probability_animation.gif", animation = twentytanimation)
knitr::include_graphics("twentyt_probability_animation.gif")

#Let's add a column to the dataset with the average property price

realestate_stats$mean_price = (realestate_stats$volume/realestate_stats$sales)*1000000

#To evaluate listings effectiveness, we relate monthly sales to active listings of the same month

realestate_stats$listings_efficiency = realestate_stats$sales/realestate_stats$listings

#before plotting, we need the means of the listings_efficiency variable to better represent effectiveness by city.

realestate_listingse_summary = realestate_stats %>%
  group_by(city) %>%
  summarise(listings_efficiency = mean(listings_efficiency))

#Let's build the chart now

ggplot(realestate_listingse_summary, aes(x=listings_efficiency, y=city)) +
  geom_segment(aes(x=0, xend=listings_efficiency, y=city, yend=city), color="grey", size = 1) +
  geom_point(aes(x= listings_efficiency, y = city), color="lightcoral", size=5) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title.position = "plot"
  ) +
  labs(x="Listings effectiveness", y="City", 
       title = "Grafico di efficacia delle inserzioni",
       subtitle = "Representation of the mean ratio of completed sales to total listings for each month")

realestate_listingse_summary

realestate_listingse_summary$percent = realestate_listingse_summary$listings_efficiency*100
print(realestate_listingse_summary)

#Further dataset considerations

#We create a time series of sales for the four cities in the dataset, comparing them in a single plot

#First, we create a group with the dataset's cities

sales_group_data = realestate_stats %>%
  group_by(city, year, month) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

#Plotting chart

ggplot(sales_group_data, aes(x = date, y = total_sales, group = city, color = city))+
  geom_line(size = 1.2)+
  theme_minimal()+
  labs(title = "Comparative sales time series for the dataset's cities",
       x = "Year",
       y = "Number of Sales",
       color = "City")+
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5))+
  facet_wrap(~ city, scales="free_y")

#We compare the distribution of median house prices across the cities
#using 4 separate boxplots

#plotting chart
ggplot(realestate_stats, aes(x = city, y = median_price))+
  geom_boxplot(
    color = "indianred4",
    fill = "indianred3",
    alpha = 0.2,
    outlier.colour = "mediumvioletred",
    outlier.fill = "mediumvioletred",
    outlier.size = 3,
  )+
  theme_minimal()+
  labs(title = "Median price distribution for all four cities",
       x = "City",
       y = "Median Price")+
  theme(plot.title = element_text(hjust = 0.5))

#We now consider the total sales value broken down by city and year

#We use the hrbhthemes library and then aggregate the data.
library("hrbrthemes")

volume_group = realestate_stats %>%
  group_by(year, city) %>%
  summarise(total_volume_data = sum(volume, na.rm = TRUE)) %>%
  ungroup()

#Plotting chart

ggplot(volume_group, aes(x = city, y = total_volume_data, fill = factor(year)))+
  geom_bar(stat="Identity", color="#e9ecef", position=position_dodge(), alpha=0.6)+
  scale_fill_manual(values = c("#69b3a2", "#404080", "#FF6347", "#4682B4", "#FFA500"))+
  theme_ipsum()+
  labs(fill = "Years", title = "Total volume of sales distribution",
       subtitle = "Data broken down by year and city",
       x= "City", y ="Total sales volume in mil$")+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        plot.title.position = "plot")

#stacked bar chart with total sales by city (monthly)

sales_grouping = realestate_stats %>%
  group_by(city, month) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  ungroup()

ggplot(sales_grouping, aes(x = as.factor(month), y = total_sales, fill = city)) +
  geom_bar(stat = "identity", position = "Stack") +
  scale_fill_hue(c = 40)+
  theme_minimal()+
  labs(title = "Total monthly sales by city",
       x = "Month",
       y = "Total sales",
       fill = "City")+
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#Let's also consider the normalized version for this chart.

ggplot(sales_grouping, aes(x = as.factor(month), y = total_sales, fill = city)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_hue(c = 40)+
  theme_minimal()+
  labs(title = "Normalized distribution of monthly sales",
       x = "Month",
       y = "Total sales",
       fill = "City")+
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )+
  scale_y_continuous(labels = scales::percent)

#Let's try to also include the year breakdown by creating separate stacked bar charts.

sales_grouping = realestate_stats %>%
  group_by(city, year, month) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  ungroup()

ggplot(sales_grouping, aes(x = as.factor(month), y = total_sales, fill = city)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_hue(c = 40) +
  theme_minimal() +
  labs(title = "Total monthly sales for each city",
       x = "Month",
       y = "Total Sales",
       fill = "City") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  facet_wrap(~ year, ncol = 1)

#We use the same data for a heatmap.

library(ggplot2)
library(dplyr)

monthly_sales_grouping = realestate_stats %>%
  group_by(city, year, month) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  ungroup()

heatmap_plot = ggplot(monthly_sales_grouping, aes(x = factor(month, levels = 1:12), y = city, fill = total_sales)) +
  geom_tile(color = "white") +
  facet_wrap(~ year, ncol = 1) +
  scale_fill_gradient(low = "white", high = "lightcoral") +
  labs(title = "Monthly sales by city and year heatmap",
       x = "Month",
       y = "City",
       fill = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

print(heatmap_plot)

#Let's build a time series of the average price
mean_group_data = realestate_stats %>%
  group_by(city, year, month) %>%
  summarise(total_mean_price = sum(mean_price, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

#Plotting the chart

ggplot(mean_group_data, aes(x = date, y = total_mean_price, group = city, fill = city))+
  geom_area(alpha = 0.6, color = "black", size = 0.5)+
  theme_minimal()+
  labs(title = "Mean price by city time series",
       x = "Year",
       y = "Mean Price",
       fill = "City")+
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5))+
  facet_wrap(~ city, scales = "free_y")+
  scale_y_continuous(labels = scales::comma)

#using renv to generate renv.lock with dependencies
lock <- renv::dependencies()
req <- paste(lock$Package, lock$Version, "==", sep = "")
renv::snapshot()