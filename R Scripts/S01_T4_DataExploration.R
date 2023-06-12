# Packages needed ====
library(data.table)
library(ggplot2)
library(caTools)
library(dplyr)
library(caret)
library(randomForest)
library(ModelMetrics)
library(rpart)
library(rpart.plot)
library(ggcorrplot)
library(DescTools)
library(car)
library(gridExtra)
library(tibble)
library(scales)

# Data Handling ====
data <- fread("S01_T4_Tiktok dataset.csv", na.strings = c("NA", "missing", "N/A", "", "na", "M", "."))

# Check for NA values
sum(is.na(data)) #0

# Removing duplicates
dim(data) # 968 observations 
summary(duplicated(data)) #1 duplicate
data <- data[!duplicated(data), ] #Drop duplicates
dim(data) #967 unique obs

# Function to convert numerical values to millions or thousands
convert_numerical <- function(x) {
  if(grepl("M videos", x)) {
    x <- gsub("M videos", "", x)
    x <- as.numeric(x) * 1000000
  } else if (grepl("K videos", x)) {
    x <- gsub("K videos", "", x)
    x <- as.numeric(x) * 1000
  } else if (grepl(" videos", x)) {
    x <- gsub(" videos", "", x)
  }
  return(x)
}
data$Videos_created <- sapply(data$Videos_created, convert_numerical)

# Removing NA values and changing datatype to num
data$Videos_created <- as.numeric(data$Videos_created)
summary(data$Videos_created)
data <- na.omit(data)
sum(is.na(data)) #0

# Summary of data
summary(data)

# Create a new column virality 
# Find mean of Videos_created
treshold <- mean(data$Videos_created)
# Create a new column and initialize it with "No"
data$virality <- "No"
# Replace the values in the "New_Column" column with "Yes" where "Videos_created" is greater than or equal to treshold
data$virality[data$Videos_created >= treshold] <- "Yes"
# Change class
data$virality <- as.factor(data$virality)

# Changing data types for X variables
data$key <- as.factor(data$key)
data$mode <- as.factor(data$mode)
data$time_signature <- as.factor(data$time_signature)

# Dropping irrelevant columns
data <- subset(data, select=-c(Year_of_release, track_name, artist_name, album, Videos_created, artist_pop, track_pop, time_signature.1, time_signature.2, duration_ms.1, duration_ms.2))

# Final dataset
dim(data) #964 rows, 14 cols

#Export to CSV
write.csv(data, "Tiktok DE Virality.csv", row.names = FALSE)
# Data exploration ====

## Box Plots on Cont Variables ====

# danceability 
ggplot(data = data, aes(x = danceability)) +
  geom_boxplot()+
  labs(title = "Box Plot of danceability")
  
# energy 
ggplot(data = data, aes(x = energy)) +
  geom_boxplot()+
  labs(title = "Box Plot of energy")
  
# loudness 
ggplot(data = data, aes(x = loudness)) +
  geom_boxplot()+
  labs(title = "Box Plot of loudness")

# speechiness
ggplot(data = data, aes(x = speechiness)) +
  geom_boxplot()+
  labs(title = "Box Plot of speechiness") 

# acousticness 
ggplot(data = data, aes(x = acousticness)) +
  geom_boxplot()+
  labs(title = "Box Plot of acousticness")

# instrumentalness
ggplot(data = data, aes(x = instrumentalness)) +
  geom_boxplot()+
  labs(title = "Box Plot of instrumentalness") 

# liveness
ggplot(data = data, aes(x = liveness)) +
  geom_boxplot()+
  labs(title = "Box Plot of liveness") 

# valence
ggplot(data = data, aes(x = valence)) +
  geom_boxplot()+
  labs(title = "Box Plot of valence") 

# tempo 
ggplot(data = data, aes(x = tempo)) +
  geom_boxplot()+
  labs(title = "Box Plot of tempo") 

# duration_ms
ggplot(data = data, aes(x = duration_ms)) +
  geom_boxplot()+
  labs(title = "Box Plot of duration_ms") 


## Deal with the Outliers (replace with median) ==== 

# danceability
boxplot.stats(data$danceability)$out
x <- boxplot.stats(data$danceability)$out
x
for(i in 1:nrow(as.matrix(x))){
  data[data$danceability == x[i], "danceability"] = median(data$danceability)
}

# energy
boxplot.stats(data$energy)$out
x <- boxplot.stats(data$energy)$out
x
for(i in 1:nrow(as.matrix(x))){
  data[data$energy == x[i], "energy"] = median(data$energy)
}

# loudness
boxplot.stats(data$loudness)$out
x <- boxplot.stats(data$loudness)$out
x
for(i in 1:nrow(as.matrix(x))){
  data[data$loudness == x[i], "loudness"] = median(data$loudness)
}

# acousticness
boxplot.stats(data$acousticness)$out
x <- boxplot.stats(data$acousticness)$out
x
for(i in 1:nrow(as.matrix(x))){
  data[data$acousticness == x[i], "acousticness"] = median(data$acousticness)
}

# tempo
boxplot.stats(data$tempo)$out
x <- boxplot.stats(data$tempo)$out
x
for(i in 1:nrow(as.matrix(x))){
  data[data$tempo == x[i], "tempo"] = median(data$tempo)
}

# duration_ms
boxplot.stats(data$duration_ms)$out
x <- boxplot.stats(data$duration_ms)$out
x
for(i in 1:nrow(as.matrix(x))){
  data[data$duration_ms == x[i], "duration_ms"] = median(data$duration_ms)
}
x


## Bar Plots on Cat Variables ====

# key
ggplot(data = data, aes(x = key, fill = key)) +
  geom_bar() +
  labs(title = "Distribution of Key among Tracks", x = "key", y = "Count") 

# mode
ggplot(data = data, aes(x = mode, fill = mode)) +
  geom_bar() +
  labs(title = "Distribution of Mode among Tracks", x = "mode", y = "Count") 

# time_signature
ggplot(data = data, aes(x = time_signature, fill = time_signature)) +
  geom_bar() +
  labs(title = "Distribution of Time Signature among Tracks", x = "time_signature", y = "Count") 

# virality
ggplot(data = data, aes(x = virality, fill = virality)) +
  geom_bar() +
  labs(title = "Distribution of Virality among Tracks", x = "virality", y = "Count")

## Histograms on Cont Variables ====

# danceability 
ggplot(data = data, aes(x = danceability)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of danceability ", x = "danceability", y = "Count") 

# energy 
ggplot(data = data, aes(x = energy)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of energy ", x = "energy", y = "Count") 

# loudness 
ggplot(data = data, aes(x = loudness)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of loudness ", x = "loudness", y = "Count")

# speechiness
ggplot(data = data, aes(x = speechiness)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of speechiness ", x = "speechiness", y = "Count") 

# acousticness 
ggplot(data = data, aes(x = acousticness)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of acousticness ", x = "acousticness", y = "Count") 

# instrumentalness
ggplot(data = data, aes(x = instrumentalness)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of instrumentalness ", x = "instrumentalness", y = "Count") 

# liveness
ggplot(data = data, aes(x = liveness)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of liveness ", x = "liveness", y = "Count") 

# valence
ggplot(data = data, aes(x = valence)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of valence ", x = "valence", y = "Count") 

# tempo 
ggplot(data = data, aes(x = tempo)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of tempo ", x = "tempo", y = "Count") 

# duration_ms
ggplot(data = data, aes(x = duration_ms)) +
  geom_histogram(color = "darkblue", fill = "lightblue", bins = 30) +
  labs(title = "Histogram Plot of duration_ms ", x = "duration_ms", y = "Count") 


# Further data exploration & visualisation ====

# Categorical Variables (Stacked bar plot, Facet-ted Bar Plot, Jitter Plot) 
# key and virality

prop_data <- data %>%
  group_by(key, virality) %>%
  summarize(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(prop_data, aes(x = key, y = percentage, fill = virality)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Key", y = "Percentage", title = "Percentage Bar Plot by Key and Virality") +
  scale_fill_manual(values = c("lightpink", "lightblue"), name = "Virality", labels = c("No", "Yes")) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))

ggplot(data) + aes(x = key, fill = virality) + 
  geom_bar(position = 'stack') + 
  facet_wrap(.~virality) +
  labs( x = "Key", y = "Count", 
        title ="Distribution of Key and Virality") 

ggplot(data) + aes(key, virality , color =  key) +
  geom_jitter() + labs(title ="Distribution of Key and Virality") 

# mode and virality 
prop_data <- data %>%
  group_by(mode, virality) %>%
  summarize(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(prop_data, aes(x = mode, y = percentage, fill = virality)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Mode", y = "Percentage", title = "Percentage Bar Plot by Mode and Virality") +
  scale_fill_manual(values = c("lightpink", "lightblue"), name = "Virality", labels = c("No", "Yes")) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))


ggplot(data) + aes(x = mode, fill = virality) + 
  geom_bar(position = 'stack') + 
  facet_wrap(.~virality) +
  labs( x = "Mode", y = "Count", 
        title ="Distribution of Mode and Virality") 

ggplot(data) + aes(mode, virality, color =  mode) +
  geom_jitter() + labs(title ="Distribution of Mode and Virality") 

# time_signature and virality 
prop_data <- data %>%
  group_by(time_signature, virality) %>%
  summarize(n = n()) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(prop_data, aes(x = time_signature, y = percentage, fill = virality)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Time Signature", y = "Percentage", title = "Percentage Bar Plot by Time Signature and Virality") +
  scale_fill_manual(values = c("lightpink", "lightblue"), name = "Virality", labels = c("No", "Yes")) +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))


ggplot(data) + aes(x = time_signature, fill = virality) + 
  geom_bar(position = 'stack') + 
  facet_wrap(.~virality) +
  labs( x = "time_signature", y = "Count", 
        title ="Distribution of Time Signature and Virality") 

ggplot(data) + aes(time_signature, virality, color =  time_signature) +
  geom_jitter() + labs(title ="Distribution of Time Signature and Virality") 


# Continuous Variables (Correlation Plot, Bar Plot, Jitter Plot, Box Plot, Density Plot)
# Correlation Plot
str(data)
corr_data <-subset(data, select=-c(key, mode,time_signature,virality ))
cor_matrix <- cor(corr_data)
ggcorrplot(cor_matrix, type = "lower", outline.col = "white", colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE)

# danceability
ggplot(data) + aes(x = danceability, fill = virality ) +
  geom_bar(position = 'dodge') +
  facet_wrap(.~virality ) +
  labs(x = "Danceability", y = "Count",
       title ="Distribution of Danceability and Virality ") 

ggplot(data) + aes(danceability, virality , color = danceability) +
  geom_jitter() +
  labs(title ="Distribution of Danceability and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(virality , danceability, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Danceability",
        title ="Distribution of Danceability and Virality ")

ggplot(data, aes(x = danceability, fill = virality )) + 
  geom_density(alpha = 0.5) +
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Danceability and Virality ", x = "Danceability", y = "Density") 

# energy
ggplot(data) + aes(x = energy, fill = virality ) +
  geom_bar(position = 'dodge') +
  facet_wrap(.~virality ) +
  labs(x = "Energy", y = "Count",
       title ="Distribution of Energy and Virality ") 

ggplot(data) + aes(energy, virality , color = energy) +
  geom_jitter() +
  labs(title ="Distribution of Energy and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(virality , energy, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Energy",
        title ="Distribution of Energy and Virality ")

ggplot(data, aes(x = energy, fill = virality )) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Energy and Virality ", x = "Energy", y = "Density") 

# loudness
ggplot(data) + aes(loudness, virality , color = loudness) +
  geom_jitter() +
  labs(title ="Distribution of Loudness and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(virality , loudness, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Loudness",
        title ="Distribution of Loudness and Virality ")

ggplot(data, aes(x = loudness, fill = virality )) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Loudness and Virality ", x = "Loudness", y = "Density") 

# speechiness
ggplot(data) + aes(speechiness, virality , color = speechiness) +
  geom_jitter() +
  labs(title ="Distribution of Speechiness and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(virality , speechiness, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Speechiness",
        title ="Distribution of Speechiness and Virality ")

ggplot(data, aes(x = speechiness, fill = virality )) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Speechiness and Virality ", x = "Speechiness", y = "Density") 

# acousticness
ggplot(data) + aes(acousticness, virality , color = acousticness) +
  geom_jitter() +
  labs(title ="Distribution of Acousticness and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(virality , acousticness, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Acousticness",
        title ="Distribution of Acousticness and Virality ")

ggplot(data, aes(x = acousticness, fill = virality )) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Acousticness and Virality ", x = "Acousticness", y = "Density") 

# instrumentalness 
ggplot(data) + aes(instrumentalness, virality , color = instrumentalness) +
  geom_jitter() +
  labs(title ="Distribution of Instrumentalness and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

# liveness
ggplot(data) + aes(liveness, virality , color = liveness) +
  geom_jitter() +
  labs(title ="Distribution of Liveness and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(virality , liveness, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Liveness",
        title ="Distribution of Liveness and Virality ")

ggplot(data, aes(x = liveness, fill = virality )) + 
  geom_density(alpha = 0.5) +
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Liveness and Virality ", x = "Liveness", y = "Density") 

# valence
ggplot(data) + aes(x = valence , fill = virality ) +
  geom_bar(position = 'dodge') +
  facet_wrap(.~virality ) +
  labs(x = "Valence ", y = "Count",
       title ="Distribution of Valence and Virality ") 

ggplot(data) + aes(valence, virality , color = valence) +
  geom_jitter() +
  labs(title ="Distribution of valence and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(virality , valence, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Valence",
        title ="Distribution of Valence and Virality ")

ggplot(data, aes(x = valence, fill = virality)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Valence and Virality ", x = "Valence", y = "Density")

# tempo
ggplot(data) + aes(tempo, virality , color = tempo) +
  geom_jitter() +
  labs(title ="Distribution of Tempo and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(virality , tempo, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Tempo",
        title ="Distribution of Tempo and Virality ")

ggplot(data, aes(x = tempo, fill = virality )) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Tempo and Virality ", x = "Tempo", y = "Density") 

#duration_ms
ggplot(data) + aes(duration_ms, virality , color = duration_ms) +
  geom_jitter() +
  labs(title ="Distribution of Duration in Milliseconds and Virality ") +
  stat_smooth(method = "lm", col = "red", se = FALSE) 

ggplot(data) + aes(duration_ms, tempo, color = virality ) + 
  geom_boxplot() +
  labs( x = "Virality ", y = "Duration in Milliseconds",
        title ="Distribution of Duration in Milliseconds and Virality ")

ggplot(data, aes(x = duration_ms, fill = virality )) + 
  geom_density(alpha = 0.5) +
  facet_wrap(.~virality ) +
  labs(title = "Distribution of Duration in Milliseconds and Virality ", x = "Duration in Milliseconds", y = "Density") 

# energy, loudness and virality  
ggplot(data) + aes(energy, loudness ) +
  geom_point() +  geom_smooth(method = "lm",col = "darkblue", se = FALSE) + 
  facet_wrap(.~virality )
  labs(title ="Relationship Between Energy and Loudness") 
  
# energy, loudness and virality  
ggplot(data) + aes(valence, danceability ) +
geom_point() +  geom_smooth(method = "lm",col = "darkblue", se = FALSE) + 
facet_wrap(.~virality )+ labs(title ="Relationship Between Valence and Danceability") 

# key, mode and virality 
ggplot(data) + aes(x = key, fill = mode) +
  geom_bar(position = 'dodge') +
  facet_wrap(.~virality ) +
  labs(title = "Relationship Between Key, Mode and Virality ")

# mode, time_signature and virality 
ggplot(data) + aes(x = mode, fill = time_signature) +
  geom_bar(position = 'dodge') +
  facet_wrap(.~virality ) +
  labs(title = "Relationship Between Mode, Time Signature and Virality ")

