library(tidyverse)
### Summary of the data contained in the mpg dataset
mpg # Provides a summary of the contents of the tibble
?mpg # Putting a "?" before a function name gives info about its contents
### Explore the contents of the mpg dataset
mpg$manufacturer # List items in the first column using "$" to choose column
mpg[[1]] # Can also list the contents of the first column by its location
mpg[ ,1] # Subsets the first column of the data frame (notice the ",")
# Listing the contents of a column is not the same as subsetting
class(mpg[[1]])
class(mpg[ ,1])
summary(mpg)
mpg[1, ] # Subsets the first row from the tibble
mpg[1:5, ] # Subsets the first five columns of the tibble
head(mpg) # Same as mpg[1:5, ]
### Make a sting, list and data frame using “<-”
some_text <- "Easter" # Makes a sting
some_text
a_list <- seq(1:3) # Makes a list of number from 1 to 3
a_list
(b_list <- c("Bunny", "Eggs", some_text)) # Makes a list of characters
a_data_frame <- data.frame(a_list, b_list) # Combines list into data frame
b_data_frame <- data.frame(num_list = a_list, b_list) # “=“ =/= “<-”
mpg_df <- as.data.frame(mpg) # Converts tibble to data frame
mpg_df
nrow(mpg_df) # Gives the number of rows in the data frame 
ncol(mpg_df) # Gives the columns of rows in the data frame 
head(mpg_df)
mpg
ggplot(data = mpg, aes(x = class, y = hwy)) + geom_boxplot() # class vs highway
mpg
### Plotting the mpg data (drive train/year and hwy) as a boxplot
ggplot(mpg, aes(drv, hwy)) + geom_boxplot() # plots drv (drive train) vs hwy
ggplot(mpg, aes(year, hwy)) + geom_boxplot() # plots year vs hwy
mpg
ggplot(mpg, aes(as.character(year), hwy)) + geom_boxplot() # plots year vs hwy
### loads rabbit egg size data (from: Al-Mufti et al. 1988) into R
setwd("Desktop/Rdata") # Establishes the working directory where file is located
rabbit_eggs_df <- read_csv("rabbit_eggs.csv") # Reads the file into a tibble
### Make sure that the data was imported correctly
rabbit_eggs_df # Shows a summary of the data in the tibble
nrow(rabbit_eggs_df) # lists the number of rows
unique(rabbit_eggs_df$Follicle) # lists the stages of follicular development
max(rabbit_eggs_df$Diameter_um) # Max value in the dataset
rabbit_eggs_plot <- ggplot(rabbit_eggs_df, aes(Follicle, Diameter_um))
rabbit_eggs_plot + geom_boxplot()
### Reorder the x axis values by converting them to factors
rabbit_eggs_df$Follicle <- factor(rabbit_eggs_df$Follicle, 
levels = unique(rabbit_eggs_df$Follicle)) # converts column to factors 
rabbit_eggs_df
rabbit_eggs_plot <- ggplot(rabbit_eggs_df, aes(Follicle, Diameter_um))
rabbit_eggs_plot + geom_boxplot() # Replot the data
### Correct for over plotting using geom_jitter()
rabbit_eggs_plot + geom_point()
rabbit_eggs_plot + geom_jitter(width = 0.15)
### Plots the oocyte data as boxplot while showing the raw data
rabbit_eggs_plot + geom_boxplot() + geom_jitter(width = 0.15)
### Plots the oocyte data as a violin plot
rabbit_eggs_plot + geom_violin()
rabbit_eggs_plot + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
### Colours the violin plot by the follicle factors
rabbit_eggs_plot + geom_violin(draw_quantiles = 0.5, aes(fill = Follicle))
rabbit_eggs_plot + geom_violin(draw_quantiles = 0.5, aes(fill = Follicle)) + 
scale_fill_grey() # grey scale
rabbit_eggs_plot + geom_violin(draw_quantiles = 0.5, aes(fill = Follicle)) + 
scale_fill_brewer(palette="Set3") # qualitative colour set
rabbit_eggs_plot + geom_violin(draw_quantiles = 0.5, aes(fill = Follicle)) + 
scale_fill_brewer(palette="OrRd") # scale from oranges to reds
# Can manually set the colours using HEX codes
rabbit_eggs_plot + geom_violin(draw_quantiles = 0.5, aes(fill = Follicle)) +
scale_fill_manual(values=c(
"#E0F0EA", "#C4E4D9", "#A9D8CA", "#8DCDBB", "#6FC3AC"))
### Changing the theme and axis labels
rabbit_eggs_plot2 <- rabbit_eggs_plot +
geom_violin(draw_quantiles = 0.5, aes(fill = Follicle), show.legend = FALSE) +
scale_fill_manual(values=c(
"#E0F0EA", "#C4E4D9", "#A9D8CA", "#8DCDBB", "#6FC3AC")) + 
theme_classic() + ylab("Diameter μm") + xlab("Follicular stage")
### Change the number of tick marks
rabbit_eggs_plot2 + coord_cartesian(ylim = c(30,110)) 
rabbit_eggs_plot2 + scale_y_continuous(breaks = c(seq(0, 110, by = 10)), 
limits = c(0, 110)) + 
theme_classic() + ylab("Diameter μm") + xlab("Follicular stage")
seq(0, 100, by = 10)
### Export the plot as a pdf
ggsave("Rabbit_eggs_plot.pdf", width = 10, height = 20, units = "cm")
warnings()


### Get median values and the SD for each follicular state
eggs_med <- aggregate(rabbit_eggs_df$Diameter_um~rabbit_eggs_df$Follicle,
FUN=median)
eggs_sd <- aggregate(rabbit_eggs_df$Diameter_um~rabbit_eggs_df$Follicle,
FUN=sd)
rabbit_eggs_df2 <- eggs_med # Makes a new dataset, copying the median data 
rabbit_eggs_df2$sd <- eggs_sd [ ,2] # combines median data with SD
colnames(rabbit_eggs_df2) <- c("Follicle", "Diameter_um", "SD")
rabbit_eggs_df2$Days <- c(150, 270, 335, 345, 355)
rabbit_eggs_df2
### Plot using geom_point showing increase in size
ggplot(rabbit_eggs_df2, aes(Days, Diameter_um)) + 
geom_point(aes(size = Diameter_um/10), show.legend = FALSE) + 
scale_size_identity() + theme_classic() +
coord_cartesian(ylim = c(30,100))
ggsave("Egg_development_plot1.pdf", width = 10, height = 20, units = "cm") # saves the image
### Plot the development of the oocyte over time using geom_line
ggplot(rabbit_eggs_df2, aes(Days, Diameter_um)) + geom_line() + 
geom_ribbon(aes(ymin = Diameter_um - SD, ymax = Diameter_um + SD),
alpha = 0.2) + coord_cartesian(ylim = c(30,100)) 
ggsave("Egg_development_plot2.pdf", width = 10, height = 20, units = "cm") # saves the image



### Combining a dataset of mammal eggs with our rabbit egg data
mammal_eggs_df <- read_csv("mammal_eggs.csv")
mammal_eggs_df
# Add the data for the rabbit preovulatory oocyte
mammal_eggs_df <- add_row(mammal_eggs_df, Species = "Rabbit", 
Diameter_um = rabbit_eggs_df2$Diameter_um[5])
mammal_eggs_df2 <- mammal_eggs_df # Makes a copy of the dataset for later
### Load the package containing the mammal mass data and build a tibble of it
library(MASS)
head(mammals)
(mammal_size_df <- as_tibble(mammals, rownames = "Species"))
### Combine data for mass, with the egg size data frame for matching species
mammal_eggs_df <- left_join(mammal_eggs_df, mammal_size_df, by = "Species")
mammal_eggs_df
# replace outdated species name 
mammal_eggs_df2$Species <- str_replace(mammal_eggs_df2$Species, "Man", "Human")
mammal_eggs_df2 <- left_join(mammal_eggs_df2, mammal_size_df, by = "Species")
### Plot the data and remove outliers
ggplot(mammal_eggs_df2, aes(body, Diameter_um)) + 
geom_point(aes(colour = Species))
# removes monotremes
monotremes <- tibble(Species = c("Echidna", "Platypus")) # monotreme species
mammal_eggs_df3 <- anti_join(mammal_eggs_df2, monotremes, by = "Species")
ggplot(mammal_eggs_df3, aes(body, Diameter_um)) + 
  geom_point(aes(colour = Species))
### Convert to a log scale and makes new named columns
mammal_eggs_df3$log_body <- log(mammal_eggs_df3$body)
mammal_eggs_df3$log_diameter <- log(mammal_eggs_df3$Diameter_um)
### Fits a linear model to the data
r_line <-lm(log_diameter ~ log_body, data = mammal_eggs_df3)
summary(r_line) # summarizes the results of the line fit
ggplot(mammal_eggs_df3, aes(log_body, log_diameter)) +
geom_point(aes(colour = Species)) + 
geom_abline(intercept = r_line$coefficients[1],
slope = r_line$coefficients[2])
library(RColorBrewer) # Loads a package for the colour scale
ggplot(mammal_eggs_df3, aes(log_body, log_diameter)) +
geom_point(aes(fill = fct_reorder2(Species, log_diameter, log_body), 
size = Diameter_um/10), shape = 21) +  scale_size_identity()+ 
scale_fill_manual(name = "Species",
values = rev(colorRampPalette(brewer.pal(8, "YlGnBu"))
(nrow(mammal_eggs_df3)))) +
theme_classic() + ylab("log diameter (μm)") + xlab("log mass (kg)") + 
theme(legend.position="bottom") +
guides(fill=guide_legend(nrow=4, reverse = T)) + 
geom_abline(intercept = r_line$coefficients[1],
slope = r_line$coefficients[2])
ggsave("Mammal_eggvsbody_plot.pdf", width = 30, height = 30, units = "cm") # saves the image
write.table(mammal_eggs_df3, "Mammal_egg_data.csv",
row.names=FALSE,col.names=TRUE, sep=",")



### Reload the mammal egg dataset from the csv file you saved
mammal_eggs_df_final <- read_csv("Mammal_egg_data.csv")
mammal_eggs_df_final
library(MASS)
mammal_size_df <- as_tibble(mammals, rownames = "Species")
# Catagorize which species appear in the mammalian oocyte size dataset
mammal_size_df$Sampled <- ifelse(mammal_size_df$Species 
%in% mammal_eggs_df_final$Species, "yes", "no")
mammal_size_df[order(mammal_size_df$body),] # Arranges small to large 
mammal_size_df[order(-mammal_size_df$body),] # Arranges large to small
### Plot the data as a stacked histogram 
ggplot(mammal_size_df, aes(body, fill = Sampled)) + 
geom_histogram(binwidth = 50)
ggplot(mammal_size_df, aes(log(body), fill = Sampled)) + 
geom_histogram(binwidth = 0.5)
### Plots the data as a violin plot
ggplot(mammal_size_df, aes(Sampled, body, fill = Sampled)) + 
geom_violin(scale = "count") + geom_point()
ggplot(mammal_size_df, aes(Sampled, log(body), fill = Sampled)) + 
geom_violin(scale = "count") + geom_point()
### Read data in for bird egg dataset and convert units to match mammals
bird_eggs_df <- read_csv("bird_eggs.csv")
bird_eggs_df$Area_um <- bird_eggs_df$Area_cm * 100000000
bird_eggs_df$Mass_kg <- bird_eggs_df$Mass_g / 1000
mammal_eggs_df_final$Area_um <- (mammal_eggs_df_final$Diameter_um/2)^2 * pi
colnames(mammal_eggs_df_final)[3] <- "Mass_kg"
### Combine the desired rows for the two data frames
mammal_eggs_df_final$Class <- "Mammals"
bird_eggs_df$Class <- "Birds" # so we can distinguish both later
cols <- c("Species", "Area_um", "Mass_kg", "Class")
bird_eggs_df$Area_um <- bird_eggs_df$Area_cm * 100000000
vertebrate_eggs_df <- bind_rows(mammal_eggs_df_final[cols],
bird_eggs_df[cols])
ggplot(vertebrate_eggs_df, aes(log(Mass_kg), log(Area_um))) + 
geom_point(aes(fill = Class), shape = 21) + 
labs(fill = "Class") + theme_classic() + 
ylab("log area (μm)") + xlab("log mass (kg)") + 
coord_cartesian(ylim = c(5, 25)) + 
scale_fill_manual(values = c("White",  "Black"))
ggsave("MammalvsBird_plot.pdf", width = 9, height = 6, units = "cm") # saves the image


