# --------------- 1. imported data and saved as "MoviesDb" ---------------

# ---------------2. install & import libraries : "tidyverse", "ggplot2" ------------

install.packages("tidyverse")
install.packages("gridExtra")  


library(tidyverse)
library(ggplot2)
library(dplyr)  # library for renaming multiple columns
library(gridExtra)


#  ----------------------- 3. check data table info ---------------------------------------------------

str(MoviesDb)  # check data type

dim(MoviesDb) # check dimension - row & COl count = 74 , 8


# ---------------------- Extra : rename columns using dplyr library -------------------------------

MoviesDb <- MoviesDb %>% 
  rename("Lead_Studio" = "Lead.Studio",
         "Audience_score" = "Audience..score..",
         "Score_RottenTomatoes" = "Rotten.Tomatoes..",
         "Worldwide_Gross" = "Worldwide.Gross")


head(MoviesDb) # check the heading after rename
View(MoviesDb) # check data


# ------------------------ 4. check missing values --------------------------------------------

sum(is.na(MoviesDb)) # check total missing values = 5

colSums(is.na(MoviesDb))    # check missing values against each feature. Audience_score = 1, profitability = 3 , Score_RottenTomatoes = 1






# ------------------------ 5. drop missing values ---------------------------------------------

# -- as none of the columns contain missing value > 50% ; so we dont delete any columns

MoviesDb <- na.omit(MoviesDb)   # delete rows with missing value
dim(MoviesDb) # Recheck: 4 rows which had missing values have been deleted. new row & col count = [70 , 8]


MoviesDb <- MoviesDb%>%  filter(!row_number() %in% c(38)) # remove empty rows for row#38
View(MoviesDb)

dim(MoviesDb) # re-check. [row,col = [69, 8]]

# ------------------------ 6. Check for duplicates & remove them ----------------------------------------

# --- check duplicate film name
 n_occur <- data.frame(table(MoviesDb$Film))
 n_occur[n_occur$Freq > 1,]  # count of duplicate value. There are no duplicate film name

# --- delete the duplicate rows
 MoviesDb <-MoviesDb %>% distinct(Film, .keep_all = TRUE)   # way-1
# MoviesDb <- MoviesDb[!duplicated(MoviesDb$id), ] #way-2

 dim(MoviesDb) # Recheck
 
 
 
#  ------------------------ 7. Round off values to 2 places ----------------------------------------

 # MoviesDb$growth <- round(MoviesDb$growth ,digit=2) # way-1 to round 1 column

MoviesDb <- MoviesDb %>% mutate(across(c('Profitability', 'Worldwide_Gross'), round, 2)) # way-2: round multiple columns
head(MoviesDb) # reCheck 

  

#  ------------------------ 8. Outlier removal ----------------------------------------

# ********** Find outliers

# -- way-1 boxplot : result= 4 outliers
ggplot(MoviesDb, aes(x=Profitability, y=Worldwide_Gross)) + geom_boxplot(outlier.colour= "red", outlier.shape = 1) 
    + scale_x_continuous(labels = scales::comma) + coord_cartesian(ylim = c(0, 1000))



# -- way-2 : using individual columns

 OutVals_profit = boxplot(MoviesDb$Profitability, plot=FALSE)$out
 View(OutVals_profit) # result: 11.1,  14.2,  22.9,  66.9 

 OutVals_gross = boxplot(MoviesDb$Worldwide_Gross, plot=FALSE)$out
 View(OutVals_gross)  # result: 521 , 609 , 702 , 710


# -- way-4 : using individual columns with boxplot

OutVals_profit_box = boxplot(MoviesDb$Profitability)$out
#which(MoviesDb$Profitability %in% OutVals_profit_box)

OutVals_gross_box = boxplot(MoviesDb$Worldwide_Gross)$out
#which(MoviesDb$Worldwide_Gross %in% OutVals_gross_box)

# -- way-5 : using histogram
hist(MoviesDb$Profitability, main = "Histogram")
hist(MoviesDb$Worldwide_Gross, main = "Histogram")

# -- way-6 : using Normal QQ plot
qqnorm(MoviesDb$Profitability, main = "Normal Q-Q plot")
qqnorm(MoviesDb$Worldwide_Gross, main = "Normal Q-Q plot")



# ********** Remove outliers

# for Profitablility column
Q1_profit <- quantile(MoviesDb$Profitability, .25)
Q3_profit <- quantile(MoviesDb$Profitability, .75)
IQR_profit <- IQR(MoviesDb$Profitability)
no_outliers_profit <- subset(MoviesDb, MoviesDb$Profitability> (Q1_profit - 1.5*IQR_profit) & MoviesDb$Profitability< (Q3_profit + 1.5*IQR_profit))

dim(no_outliers_profit)  # new row & col count = [64 , 8]. so 5 outliers removed


# for Worldwide_Gross column
Q1_gross <- quantile(no_outliers_profit$Worldwide_Gross, .25)
Q3_gross <- quantile(no_outliers_profit$Worldwide_Gross, .75)
IQR_gross <- IQR(no_outliers_profit$Worldwide_Gross)
no_outliers <- subset(no_outliers_profit, no_outliers_profit$Worldwide_Gross> (Q1_gross - 1.5*IQR_gross) & no_outliers_profit$Worldwide_Gross< (Q3_gross + 1.5*IQR_gross))

dim(no_outliers)  # new row & col count = [60 , 8]. so 5 outliers removed
View(no_outliers)
head(no_outliers)


# ------------ extra check plots before & after outlier removal

ggp1 <- ggplot(MoviesDb, aes(x=Profitability, y=Worldwide_Gross)) + geom_boxplot(outlier.colour= "red", outlier.shape = 1) + scale_x_continuous(labels = scales::comma) + coord_cartesian(ylim = c(0, 1000)) + ggtitle("Original")
ggp2 <- ggplot(no_outliers, aes(x=Profitability, y=Worldwide_Gross)) + geom_boxplot(outlier.colour= "red", outlier.shape = 1) + scale_x_continuous(labels = scales::comma) + coord_cartesian(ylim = c(0, 1000)) + ggtitle("Outlier removed")
grid.arrange(ggp1, ggp2, ncol = 2) 


ggp1 <- ggplot(MoviesDb, aes(x=Profitability, y=Worldwide_Gross)) + geom_boxplot(outlier.colour= "red", outlier.shape = 1) + scale_x_continuous(labels = scales::comma) + coord_cartesian(ylim = c(0, 1000)) + ggtitle("Original")
ggp2 <- ggplot(no_outliers, aes(x=Profitability, y=Worldwide_Gross)) + geom_boxplot(outlier.colour= "red", outlier.shape = 1) + scale_x_continuous(labels = scales::comma) + coord_cartesian(ylim = c(0, 1000)) + ggtitle("Outlier removed")
grid.arrange(ggp1, ggp2, ncol = 2) 

#  ------------------------ 9. Exploratory Data Analysis ----------------------------------------

# --bivariate analysis
# scatter plot
ggplot(no_outliers, aes(x=Lead_Studio , y=Score_RottenTomatoes)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

# bar chart
ggplot(MoviesDb, aes(x=Year)) + geom_bar()


#  ------------------------ 10. Export Data ----------------------------------------

write.csv(no_outliers, "E:/clean_df_MoviesDb.csv" , row.names=FALSE)

