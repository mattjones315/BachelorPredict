# analyzing linear relationships in bachelor data 
# bmi 209 
# may 2018 
# author <christa.caggiano@ucsf.edu> 


setwd("~/Documents/UCSF_year1/deep_learning/final_project/")
rm(list=ls())

############################### manually one-hot encoding categorical variables ##############################

bachelors = read.csv("bachelors.csv", header=T)

for(unique_value in unique(bachelors$Hometown.Region)){
  bachelors[paste("Hometown.Region", unique_value, sep = ".")] <- ifelse(bachelors$Hometown.Region == unique_value, 1, 0)
}

for(unique_value in unique(bachelors$Occupation.Category)){
  bachelors[paste("Occupation.Category", unique_value, sep = ".")] <- ifelse(bachelors$Occupation.Category == unique_value, 1, 0)
}

for(unique_value in unique(bachelors$employment.amount)){
  bachelors[paste("employment.amount", unique_value, sep = ".")] <- ifelse(bachelors$employment.amount == unique_value, 1, 0)
}

for(unique_value in unique(bachelors$Occupation.Education)){
  bachelors[paste("Occupation.Education", unique_value, sep = ".")] <- ifelse(bachelors$Occupation.Education == unique_value, 1, 0)
}

for(unique_value in unique(bachelors$Hair.color)){
  bachelors[paste("Hair.color", unique_value, sep = ".")] <- ifelse(bachelors$Hair.color == unique_value, 1, 0)
}

for(unique_value in unique(bachelors$Hair.wavy)){
  bachelors[paste("Hair.wavy", unique_value, sep = ".")] <- ifelse(bachelors$Hair.wavy == unique_value, 1, 0)
}

for(unique_value in unique(bachelors$ethnicity)){
  bachelors[paste("ethnicity", unique_value, sep = ".")] <- ifelse(bachelors$ethnicity == unique_value, 1, 0)
}

write.csv(bachelors, "bachelor_encoded.csv", row.names = F) # write csv to be used later 

# for contestants 
b = read.csv("one_hot_encoded.csv", header=T)

for(unique_value in unique(b$Hometown.Region)){
  b[paste("Hometown.Region", unique_value, sep = ".")] <- ifelse(b$Hometown.Region == unique_value, 1, 0)
}

for(unique_value in unique(b$Occupation.Category)){
  b[paste("Occupation.Category", unique_value, sep = ".")] <- ifelse(b$Occupation.Category == unique_value, 1, 0)
}

for(unique_value in unique(b$employment.amount)){
  b[paste("employment.amount", unique_value, sep = ".")] <- ifelse(b$employment.amount == unique_value, 1, 0)
}

for(unique_value in unique(b$Occupation.Education)){
  b[paste("Occupation.Education", unique_value, sep = ".")] <- ifelse(b$Occupation.Education == unique_value, 1, 0)
}

for(unique_value in unique(b$hair_color)){
  b[paste("hair_color", unique_value, sep = ".")] <- ifelse(b$hair_color == unique_value, 1, 0)
}

for(unique_value in unique(b$Hair.wavy)){
  b[paste("Hair.wavy", unique_value, sep = ".")] <- ifelse(b$Hair.wavy == unique_value, 1, 0)
}

for(unique_value in unique(b$ethnicity)){
  b[paste("ethnicity", unique_value, sep = ".")] <- ifelse(b$ethnicity == unique_value, 1, 0)
}

encoded = write.csv(b, "encoded.csv", row.names = F)

##################################### simple PCA analysis ############################################


row.has.na = apply(encoded, 1, function(x){any(is.na(x))})
final.encoded = encoded[!row.has.na,]
final.encoded = final.encoded[, !names(final.encoded) %in% c("ElimWeek", "Season")]
final.bach_cat = b[!row.has.na,]

final.bach_cat$Age = as.factor(final.bach_cat$Age)
final.bach_cat$ElimWeek = as.factor(final.bach_cat$ElimWeek)
final.bach_cat$Season = as.factor(final.bach_cat$Season)

autoplot(prcomp(final.encoded, center=TRUE))
autoplot(prcomp(final.encoded, center=TRUE), data=final.bach_cat, colour="ElimWeek")

##################################### simple cluster analysis ############################################

library(cluster)

autoplot(clara(final.encoded, 2))
model <- lfda(final.encoded[-1], final.encoded[,1], r = 2, metric="plain")
autoplot(model, data = final.bach_cat, frame = TRUE, frame.colour = 'ElimWeek')

##################################### multi-dimensional analysis ############################################

library("RColorBrewer")
library("gplots")

freq = as.data.frame(table(final.bach_cat$ElimWeek, final.bach_cat$intro_order))
names(freq) <- c('elim_week', 'intro_order', 'Frequency')

freq = as.data.frame(table(final.bach_cat$ElimWeek, final.bach_cat$Age))
names(freq) <- c('elim_week', 'age', 'Frequency')

ggplot(freq, aes(elim_week, age)) + geom_tile(aes(fill = Frequency)) + scale_fill_gradient(low="white", high="red")

final.bach_cat$Age = as.factor(final.bach_cat$Age)
barplot(table(final.bach_cat$Age))


############################# correlations with one-hot encoded  datasets made earlier ####################################
rm(list=ls()) # start clean 

bach = read.csv("bachelor_encoded.csv", header=T) 
bach=bach[-3]  # remove height column, missing too much data 

cont = read.csv("encoded.csv", header=T)
cont=cont[,-5] # remove height
cont=cont[,-2] # season 

# make sure all data is numeric 
for (i in names(bach)){
  bach[[i]] <- as.numeric(bach[[i]])
}

for (i in names(cont)){
  cont[[i]] <- as.numeric(cont[[i]])
}

# remove women with missing data 
row.has.na = apply(cont, 1, function(x){any(is.na(x))})
cont = cont[!row.has.na,]

# heat map by age
freq_t = as.data.frame(table(cont$ElimWeek, cont$Age))
names(freq_t) <- c('elim_week', 'age', 'Frequency')
ggplot(freq, aes(elim_week, age)) + geom_tile(aes(fill = Frequency)) + scale_fill_gradient(low="white", high="blue")


race_elim = cont[, names(cont) %in% c("ethnicity", "ElimWeek")]
race_elim_agg = aggregate(Ethnicity ~ ElimWeek, data=age_elim, mean)

# clean up double caucasian entries
myData = race_elim[race_elim$ethnicity != "Caucasian",]
myData = myData[myData$ethnicity != "NA",]
myData = subset(myData, myData$ElimWeek>8)
race = as.matrix.data.frame(table(age_elim))
names(race) = c("elim", "race", "freq")
ggplot(data=myData, aes(x=ethnicity)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="purple") + theme_minimal()

############################# calc pearson correlation  ####################################

calculate_correlation = function(season, bach, cont) {
  cont_subset = subset(cont, cont$Season==season)
  bach_subset = subset(bach, bach$Season==season)
  corr = c()
  for (row in 1:nrow(cont_subset)) {
    correlation_value = cor(t(cont_subset[row, -1:-2]), t(bach_subset[,-1]), method="kendall")
    corr = c(corr, correlation_value)
  }
  
  return(corr)
}

############################# calc euclidian distance  ####################################

calc_distance = function(season, bach, cont) {
  cont_subset = subset(cont, cont$Season==season)
  bach_subset = subset(bach, bach$Season==season)
  dist = c()
  for (row in 1:nrow(cont_subset)) {
    dist = c(dist, sqrt(sum((cont_subset[row, -1:-2] - bach_subset[,-1]) ^ 2)))
  }
  
  return(dist)
}


# do correlation and distance for seasons with data 

elim_week = c()
m_dist = c()
m_corr = c()
for(i in 10:22){ 
  cont_subset = subset(cont, cont$Season==i)
  elim_week = c(elim_week, cont_subset$ElimWeek)
  m_corr = c(m_corr, calculate_correlation(i, bach, cont))
  m_dist = c(m_dist, calc_distance(i, bach, cont) )
  }

# round to something reasonable for plotting 
m_dist = round(m_dist, digits = 2)
m_corr = round(m_corr, digits = 1)

# make into a complete dataframe 
df = do.call(rbind, Map(data.frame, A=elim_week, B=m_corr, C=m_dist))
names(df) = c("elim", "corr", "dist")

# linear models
summary(lm(corr~dist, data=df)) 
summary(lm(elim~corr, data=df))

# box plot for looking at interactions 
boxplot(dist~elim, data=df, col="red")
ggplot(df, aes(elim, dist)) + 
  geom_boxplot()

# frequency table for heatmap 
d = as.data.frame(table(df$elim, df$corr))
names(d) = c("elim", "corr", "freq")

ggplot(d, aes(elim, corr)) + geom_tile(aes(fill = freq)) + scale_fill_gradient(low="white", high="red")

