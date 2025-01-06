# Load necessary libraries
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("magrittr")) install.packages("magrittr", dependencies = TRUE)
if (!require("stringr")) install.packages("stringr", dependencies = TRUE)
if (!require("skimr")) install.packages("skimr", dependencies = TRUE)
if (!require("visdat")) install.packages("visdat", dependencies = TRUE)
if (!require("DataExplorer")) install.packages("DataExplorer", dependencies = TRUE)

library(dplyr)
library(magrittr)
library(stringr)
library(skimr)
library(visdat)
library(DataExplorer)

# Set working directory
setwd("C:\\Users\\User\\Desktop\\Assignment_R")

# Define folder with CSV files
myfolder <- "csv_folder"
allfiles <- list.files(path = myfolder, pattern = "*.csv", full.names = TRUE)

# Function to process each CSV file
addFileNameColumn <- function(file) {
  data <- read.csv(file) %>%
    mutate(
      date = str_remove_all(basename(file), "principal_offence_category_|\\.csv"),
      year = substr(date, nchar(date) - 3, nchar(date)),
      month = word(date, 1, sep = "_")
    )
  
  # Convert all columns to character for consistency
  data <- data %>%
    mutate(across(everything(), as.character))
  
  # Save the modified data
  write.csv(data, file, row.names = FALSE)
  return(data)
}

# Apply processing to all CSV files and combine
modified_data <- lapply(allfiles, addFileNameColumn)
combined_data <- bind_rows(modified_data)

# Store the combined data
mydata <- combined_data

# ---------------------------------------------------------------------
# Data Cleaning and Exploration
# ---------------------------------------------------------------------

# Initial Data Exploration
str(mydata)
summary(mydata)
skim(mydata)
vis_dat(mydata)
create_report(mydata)

# Replace "-" with NA
mydata[mydata == "-"] <- NA
create_report(mydata)

# ---------------------------------------------------------------------
# Change Data Types
# ---------------------------------------------------------------------

# Convert specific columns
mydata[, 53] <- as.numeric(mydata[, 53]) # Column 53 to numeric
for (i in seq(2, 51, by = 2)) mydata[[i]] <- as.integer(mydata[[i]]) # Columns 2-51 (even) to integer
for (i in seq(3, 51, by = 2)) mydata[, i] <- as.numeric(sub("%", "", mydata[, i])) / 100 # Columns 3-51 (odd) to float

# ---------------------------------------------------------------------
# Rename Columns
# ---------------------------------------------------------------------

colnames(mydata)[1] <- "Area" # Rename first column
columns_to_modify <- names(mydata)[2:ncol(mydata)]
new_column_names <- sub("^Number.of.", "", columns_to_modify)
colnames(mydata)[2:ncol(mydata)] <- new_column_names
colnames(mydata)[-1] <- str_replace_all(colnames(mydata)[-1], "Percentage.of.", "(%)")

# ---------------------------------------------------------------------
# Handle Missing Values
# ---------------------------------------------------------------------

# Replace missing numeric values with column mean
numeric_columns <- sapply(mydata, is.integer)
mean_values <- colMeans(mydata[, numeric_columns], na.rm = TRUE)
for (col in names(mean_values)) {
  mydata[is.na(mydata[[col]]), col] <- mean_values[col]
}

# ---------------------------------------------------------------------
# Fix String Inconsistencies
# ---------------------------------------------------------------------

colnames(mydata) <- gsub("\\.", " ", colnames(mydata))
colnames(mydata)[52:54] <- c("Date", "Year", "Month")

# ---------------------------------------------------------------------
# Reorganize Columns
# ---------------------------------------------------------------------

# Create lookup table for month numbers
month_lookup <- data.frame(
  Month_Name = tolower(month.name),
  Month_Number = sprintf("%02d", 1:12)
)

# Add month numbers
mydata <- mydata %>%
  left_join(month_lookup, by = c("Month" = "Month_Name")) %>%
  select(-Month) %>%
  rename(Month = Month_Number)

# Create a new column "Time"
mydata <- mydata %>%
  mutate(Time = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d"))

# Remove rows with Area = "National"
mydata <- mydata %>%
  filter(Area != "National")

# ---------------------------------------------------------------------
# Create Additional Data Frames
# ---------------------------------------------------------------------

# Data Frame 2: Exclude unsuccessful columns and percentages
mydata2 <- mydata %>%
  select(-matches("\\(%)"), -contains("unsuccessful"))

# Data Frame 3: Only unsuccessful columns
mydata3 <- mydata %>%
  select(Area, Year, Month, Time, contains("unsuccessful"), -matches("\\(%)")) %>%
  mutate(across(contains("unsuccessful"), as.integer))

# ---------------------------------------------------------------------
# Final Check
# ---------------------------------------------------------------------

str(mydata)
str(mydata2)
str(mydata3)


# 3. Descriptive Analysis

# 3.1 Summary Statistics

# Function to calculate summary statistics
calculate_summary <- function(var) {
  stats <- c(
    Minimum = min(var, na.rm = TRUE),
    Maximum = max(var, na.rm = TRUE),
    Mean = mean(var, na.rm = TRUE),
    Median = median(var, na.rm = TRUE),
    SD = sd(var, na.rm = TRUE)
  )
  return(stats)
}

# Summary statistics for convictions (successful)
selected_vars_successful <- c(
  "Homicide Convictions", "Offences Against The Person Convictions", 
  "Sexual Offences Convictions", "Burglary Convictions", 
  "Robbery Convictions", "Theft And Handling Convictions", 
  "Fraud And Forgery Convictions", "Criminal Damage Convictions", 
  "Drugs Offences Convictions", "Public Order Offences Convictions", 
  "Motoring Offences Convictions"
)

summary_table_successful <- t(sapply(mydata2[, selected_vars_successful], calculate_summary))
summary_table_successful <- as.data.frame(summary_table_successful)
View(summary_table_successful)

# Summary statistics for convictions (unsuccessful)
selected_vars_unsuccessful <- c(
  "Homicide Unsuccessful", "Offences Against The Person Unsuccessful", 
  "Sexual Offences Unsuccessful", "Burglary Unsuccessful", 
  "Robbery Unsuccessful", "Theft And Handling Unsuccessful", 
  "Fraud And Forgery Unsuccessful", "Criminal Damage Unsuccessful", 
  "Drugs Offences Unsuccessful", "Public Order Offences Unsuccessful", 
  "Motoring Offences Unsuccessful"
)

summary_table_unsuccessful <- t(sapply(mydata3[, selected_vars_unsuccessful], calculate_summary))
summary_table_unsuccessful <- as.data.frame(summary_table_unsuccessful)
View(summary_table_unsuccessful)

# Summarize total convictions by area (successful)
area_summary_successful <- mydata2 %>%
  group_by(Area) %>%
  summarise(across(all_of(selected_vars_successful), sum, na.rm = TRUE))
View(area_summary_successful)
write.csv(area_summary_successful, "area_summary_successful.csv", row.names = FALSE)

# Summarize total convictions by area (unsuccessful)
area_summary_unsuccessful <- mydata3 %>%
  group_by(Area) %>%
  summarise(across(all_of(selected_vars_unsuccessful), sum, na.rm = TRUE))
View(area_summary_unsuccessful)
write.csv(area_summary_unsuccessful, "area_summary_unsuccessful.csv", row.names = FALSE)

# 3.2 Visualization

# Distribution of observations per year
library(ggplot2)

ggplot(mydata, aes(x = Year)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Observations per Year",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal()

# Types of convictions across years (National level)
convictions_data <- mydata[mydata$Area == "National", c("Year", selected_vars_successful)]
convictions_data_long <- tidyr::gather(convictions_data, key = "Conviction_Type", value = "Count", -Year)

ggplot(convictions_data_long, aes(x = Year, y = Count, fill = Conviction_Type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "white") +
  labs(
    title = "Conviction Types Across Time (National)",
    x = "Year",
    y = "Count"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Counties with highest and lowest crime rates
total_crimes <- area_summary_successful %>%
  rowwise() %>%
  mutate(Total = sum(c_across(-Area)))

# Top 10 areas with highest total crimes
top_10_crimes <- total_crimes %>%
  arrange(desc(Total)) %>%
  head(10)

ggplot(top_10_crimes, aes(x = reorder(Area, -Total), y = Total, fill = Area)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Areas with Highest Total Crimes",
    x = "Area",
    y = "Total Crimes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bottom 10 areas with lowest total crimes
lowest_10_crimes <- total_crimes %>%
  arrange(Total) %>%
  head(10)

ggplot(lowest_10_crimes, aes(x = reorder(Area, Total), y = Total, fill = Area)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Bottom 10 Areas with Lowest Total Crimes",
    x = "Area",
    y = "Total Crimes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Libraries
library(ggplot2)
library(dplyr)

# 1. Homicide Convictions
#########################################
# Homicide Convictions vs Average All Other Convictions Across Time
ggplot(mydata4, aes(x = Time)) +
  geom_line(aes(y = Homicide_Convictions, color = "Homicide Convictions"), size = 1) +
  geom_line(aes(y = Average_All_Other_Convictions, color = "Average All Other Convictions"), size = 1, linetype = "dashed") +
  labs(
    title = "Homicide Convictions vs Average All Other Convictions Across Time",
    x = "Time", y = "Count"
  ) +
  scale_color_manual(values = c("Homicide Convictions" = "blue", "Average All Other Convictions" = "red")) +
  theme_minimal()

# Homicide Unsuccessful vs Average All Other Unsuccessful
ggplot(mydata5, aes(x = Time)) +
  geom_line(aes(y = Homicide_Unsuccessful, color = "Homicide Unsuccessful"), size = 1) +
  geom_line(aes(y = Average_All_Other_Unsuccessful, color = "Average All Other Unsuccessful"), size = 1, linetype = "dashed") +
  labs(
    title = "Homicide_Unsuccessful vs Average_All_Other_Unsuccessful Across Time",
    x = "Time", y = "Count"
  ) +
  scale_color_manual(values = c("Homicide Unsuccessful" = "blue", "Average All Other Unsuccessful" = "red")) +
  theme_minimal()

# Homicide Convictions Across Areas
my_plot_homicide_convictions <- ggplot(mydata2, aes(x = Area, y = `Homicide Convictions`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Homicide Convictions Across Areas (Successful)",
    x = "Area", y = "Homicide Convictions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("homicide_convictions.png", plot = my_plot_homicide_convictions, width = 10, height = 6)
print(my_plot_homicide_convictions)

# 2. Offences Against the Person
#########################################
# Offences Against the Person Convictions Over Time
ggplot(mydata4, aes(x = Time, y = Offences_Against_Person_Convictions, fill = "Convictions")) +
  geom_area(color = "blue", alpha = 0.5) +
  labs(
    title = "Offences Against Person Convictions Over Time",
    x = "Time", y = "Count"
  ) +
  scale_fill_manual(values = "blue") +
  theme_minimal()

# Offences Against the Person Across Areas
my_plot_offences_convictions <- ggplot(mydata2, aes(x = Area, y = `Offences Against The Person Convictions`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Offences Against the Person Convictions Across Areas",
    x = "Area", y = "Convictions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("offences_against_person.png", plot = my_plot_offences_convictions, width = 10, height = 6)
print(my_plot_offences_convictions)

# 3. Sexual Offences
#########################################
# Sexual Offences Convictions vs Average All Other Convictions
ggplot(mydata4, aes(x = Time)) +
  geom_line(aes(y = Sexual_Offences_Convictions, color = "Sexual Offences Convictions"), size = 1) +
  geom_line(aes(y = Average_All_Other_Convictions, color = "Average All Other Convictions"), size = 1, linetype = "dashed") +
  labs(
    title = "Sexual Offences Convictions vs Average All Other Convictions",
    x = "Time", y = "Count"
  ) +
  scale_color_manual(values = c("Sexual Offences Convictions" = "blue", "Average All Other Convictions" = "red")) +
  theme_minimal()

# Sexual Offences Across Areas
my_plot_sexual_offences <- ggplot(mydata2, aes(x = Area, y = `Sexual Offences Convictions`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Sexual Offences Convictions Across Areas",
    x = "Area", y = "Convictions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("sexual_offences.png", plot = my_plot_sexual_offences, width = 10, height = 6)
print(my_plot_sexual_offences)

# 4. Burglary
#########################################
# Burglary Convictions vs Average All Other Convictions
ggplot(mydata4, aes(x = Time)) +
  geom_line(aes(y = Burglary_Convictions, color = "Burglary Convictions"), size = 1) +
  geom_line(aes(y = Average_All_Other_Convictions, color = "Average All Other Convictions"), size = 1, linetype = "dashed") +
  labs(
    title = "Burglary Convictions vs Average All Other Convictions",
    x = "Time", y = "Count"
  ) +
  scale_color_manual(values = c("Burglary Convictions" = "blue", "Average All Other Convictions" = "red")) +
  theme_minimal()

# Burglary Across Areas
my_plot_burglary <- ggplot(mydata2, aes(x = Area, y = `Burglary Convictions`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Burglary Convictions Across Areas (Successful)",
    x = "Area", y = "Convictions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("burglary_convictions.png", plot = my_plot_burglary, width = 10, height = 6)
print(my_plot_burglary)




###########################################5.Robbery ########################################################

# Robbery Convictions vs Average All Other Convictions Across Time"
ggplot(mydata4, aes(x = Time)) +
  geom_line(aes(y = Robbery_Convictions, color = "Robbery Convictions"), size = 1) +
  geom_line(aes(y = Average_All_Other_Convictions, color = "Average All Other Convictions"), size = 1, linetype = "dashed") +
  labs(title = "Robbery Convictions vs Average All Other Convictions Across Time",
       x = "Time",
       y = "Count") +
  scale_color_manual(values = c("Robbery Convictions" = "blue", "Average All Other Convictions" = "red")) +
  theme_minimal()


# "Robbery_Unsuccessful vs Average_All_Other_Unsuccessful Across Time"
ggplot(mydata5, aes(x = Time)) +
  geom_line(aes(y = Robbery_Unsuccessful, color = "Robbery Unsuccessful"), size = 1) +
  geom_line(aes(y = Average_All_Other_Unsuccessful, color = "Average All Other Unsuccessful"), size = 1, linetype = "dashed") +
  labs(title = "Robbery_Unsuccessful vs Average_All_Other_Unsuccessful Across Time",
       x = "Time",
       y = "Count") +
  scale_color_manual(values = c("Robbery Unsuccessful" = "blue", "Average All Other Unsuccessful" = "red")) +
  theme_minimal()




# Robbery Convictions across different areas
my_plot_robbery_convictions <- ggplot(mydata2, aes(x = Area, y = `Robbery Convictions`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Robbery Convictions Across Areas(Successful)",
       x = "Area",
       y = "Robbery Convictions") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot with specific dimensions
ggsave("my_plot_robbery_convictions.png", plot = my_plot_robbery_convictions, width = 10, height = 6, units = "in")
print(my_plot_robbery_convictions)



# Robbery Unsuccessful across different areas
my_plot_robbery_unsuccessful <- ggplot(mydata3, aes(x = Area, y = `Robbery Unsuccessful`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Robbery Unsuccessful Across Areas",+
         x = "Area",
       y = "Robbery Unsuccessful") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot with specific dimensions
ggsave("my_plot_robbery_unsuccessful.png", plot = my_plot_robbery_unsuccessful, width = 10, height = 6, units = "in")

# Explicitly print the plot
print(my_plot_robbery_unsuccessful)

# Select "Robbery Unsuccessful" in the dataset
selected_data_robbery_unsuccessful <- mydata3 %>%
  select(Area, `Robbery Unsuccessful`)




##########################################6.Theft And Handling #######################################################

# Theft And Handling Convictions vs Average All Other Convictions Across Time"
ggplot(mydata4, aes(x = Time)) +
  geom_line(aes(y = Theft_And_Handling_Convictions, color = "Theft And Handling Convictions"), size = 1) +
  geom_line(aes(y = Average_All_Other_Convictions, color = "Average All Other Convictions"), size = 1, linetype = "dashed") +
  labs(title = "Theft And Handling Convictions vs Average All Other Convictions Across Time",
       x = "Time",
       y = "Count") +
  scale_color_manual(values = c("Theft And Handling Convictions" = "blue", "Average All Other Convictions" = "red")) +
  theme_minimal()



# "Theft_And_Handling_Unsuccessful vs Average_All_Other_Unsuccessful Across Time"
ggplot(mydata5, aes(x = Time)) +
  geom_line(aes(y = Theft_And_Handling_Unsuccessful, color = "Theft And Handling Unsuccessful"), size = 1) +
  geom_line(aes(y = Average_All_Other_Unsuccessful, color = "Average All Other Unsuccessful"), size = 1, linetype = "dashed") +
  labs(title = "Theft_And_Handling_Unsuccessful vs Average_All_Other_Unsuccessful Across Time",
       x = "Time",
       y = "Count") +
  scale_color_manual(values = c("Theft And Handling Unsuccessful" = "blue", "Average All Other Unsuccessful" = "red")) +
  theme_minimal()





# Theft And Handling Convictions across different areas
my_plot_theft_handling_convictions <- ggplot(mydata2, aes(x = Area, y = `Theft And Handling Convictions`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Theft And Handling Convictions Across Areas(Successful)",
       x = "Area",
       y = "Theft And Handling Convictions") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot with specific dimensions
ggsave("my_plot_theft_handling_convictions.png", plot = my_plot_theft_handling_convictions, width = 10, height = 6, units = "in")
print(my_plot_theft_handling_convictions)




# Theft And Handling Unsuccessful across different areas
my_plot_theft_handling_unsuccessful <- ggplot(mydata3, aes(x = Area, y = `Theft And Handling Unsuccessful`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Theft And Handling Unsuccessful Across Areas",
       x = "Area",
       y = "Theft And Handling Unsuccessful") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot with specific dimensions
ggsave("my_plot_theft_handling_unsuccessful.png", plot = my_plot_theft_handling_unsuccessful, width = 10, height = 6, units = "in")

# Explicitly print the plot
print(my_plot_theft_handling_unsuccessful)

# Select "Theft And Handling Unsuccessful" in the dataset
selected_data_theft_handling_unsuccessful <- mydata3 %>%
  select(Area, `Theft And Handling Unsuccessful`)






library(ggplot2)
# Theft And Handling Unsuccessful across different areas
my_plot_theft_handling_unsuccessful <- ggplot(mydata3, aes(x = Area, y = `Theft And Handling Unsuccessful`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Theft And Handling Unsuccessful Across Areas",
       x = "Area",
       y = "Theft And Handling Unsuccessful") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot with specific dimensions
ggsave("my_plot_theft_handling_unsuccessful.png", plot = my_plot_theft_handling_unsuccessful, width = 10, height = 6, units = "in")

# Explicitly print the plot
print(my_plot_theft_handling_unsuccessful)

# Select "Theft And Handling Unsuccessful" in the dataset
selected_data_theft_handling_unsuccessful <- mydata3 %>%
  select(Area, `Theft And Handling Unsuccessful`)






############################################# 10. Public Order Offences Convictions ##########################################

# Public Order Offences Convictions vs Average All Other Convictions Across Time
ggplot(mydata4, aes(x = Time)) +
  geom_line(aes(y = Public_Order_Offences_Convictions, color = "Public Order Offences Convictions"), size = 1) +
  geom_line(aes(y = Average_All_Other_Convictions, color = "Average All Other Convictions"), size = 1, linetype = "dashed") +
  labs(title = "Public Order Offences Convictions vs Average All Other Convictions Across Time",
       x = "Time", y = "Count") +
  scale_color_manual(values = c("Public Order Offences Convictions" = "blue", "Average All Other Convictions" = "red")) +
  theme_minimal()

# Public Order Offences Unsuccessful vs Average All Other Unsuccessful Across Time
ggplot(mydata5, aes(x = Time)) +
  geom_line(aes(y = Public_Order_Offences_Unsuccessful, color = "Public Order Offences Unsuccessful"), size = 1) +
  geom_line(aes(y = Average_All_Other_Unsuccessful, color = "Average All Other Unsuccessful"), size = 1, linetype = "dashed") +
  labs(title = "Public Order Offences Unsuccessful vs Average All Other Unsuccessful Across Time",
       x = "Time", y = "Count") +
  scale_color_manual(values = c("Public Order Offences Unsuccessful" = "blue", "Average All Other Unsuccessful" = "red")) +
  theme_minimal()

# Public Order Offences Convictions Across Different Areas
my_plot_public_order_convictions <- ggplot(mydata2, aes(x = Area, y = `Public Order Offences Convictions`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Public Order Offences Convictions Across Areas (Successful)",
       x = "Area", y = "Public Order Offences Convictions") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot
ggsave("my_plot_public_order_convictions.png", plot = my_plot_public_order_convictions, width = 10, height = 6, units = "in")
print(my_plot_public_order_convictions)

# Select "Public Order Offences Convictions" in the dataset
selected_data <- mydata2 %>%
  select(Area, `Public Order Offences Convictions`)

# Public Order Offences Unsuccessful Across Different Areas
my_plot_public_order_unsuccessful <- ggplot(mydata3, aes(x = Area, y = `Public Order Offences Unsuccessful`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Public Order Offences Unsuccessful Across Areas",
       x = "Area", y = "Public Order Offences Unsuccessful") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot
ggsave("my_plot_public_order_unsuccessful.png", plot = my_plot_public_order_unsuccessful, width = 10, height = 6, units = "in")
print(my_plot_public_order_unsuccessful)

# Select "Public Order Offences Unsuccessful" in the dataset
selected_data_public_order_unsuccessful <- mydata3 %>%
  select(Area, `Public Order Offences Unsuccessful`)

############################################ 11. Motoring Offences Convictions ############################################

# Motoring Offences Convictions vs Average All Other Convictions Across Time
ggplot(mydata4, aes(x = Time)) +
  geom_line(aes(y = Motoring_Offences_Convictions, color = "Motoring Offences Convictions"), size = 1) +
  geom_line(aes(y = Average_All_Other_Convictions, color = "Average All Other Convictions"), size = 1, linetype = "dashed") +
  labs(title = "Motoring Offences Convictions vs Average All Other Convictions Across Time",
       x = "Time", y = "Count") +
  scale_color_manual(values = c("Motoring Offences Convictions" = "blue", "Average All Other Convictions" = "red")) +
  theme_minimal()

# Motoring Offences Unsuccessful vs Average All Other Unsuccessful Across Time
ggplot(mydata5, aes(x = Time)) +
  geom_line(aes(y = Motoring_Offences_Unsuccessful, color = "Motoring Offences Unsuccessful"), size = 1) +
  geom_line(aes(y = Average_All_Other_Unsuccessful, color = "Average All Other Unsuccessful"), size = 1, linetype = "dashed") +
  labs(title = "Motoring Offences Unsuccessful vs Average All Other Unsuccessful Across Time",
       x = "Time", y = "Count") +
  scale_color_manual(values = c("Motoring Offences Unsuccessful" = "blue", "Average All Other Unsuccessful" = "red")) +
  theme_minimal()

# Motoring Offences Convictions Across Different Areas
my_plot_motoring_offences_convictions <- ggplot(mydata2, aes(x = Area, y = `Motoring Offences Convictions`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Motoring Offences Convictions Across Areas (Successful)",
       x = "Area", y = "Motoring Offences Convictions") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot
ggsave("my_plot_motoring_offences_convictions.png", plot = my_plot_motoring_offences_convictions, width = 10, height = 6, units = "in")
print(my_plot_motoring_offences_convictions)

# Select "Motoring Offences Convictions" in the dataset
selected_data_motoring_offences <- mydata2 %>%
  select(Area, `Motoring Offences Convictions`)

# Motoring Offences Unsuccessful Across Different Areas
my_plot_motoring_offences_unsuccessful <- ggplot(mydata3, aes(x = Area, y = `Motoring Offences Unsuccessful`, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Motoring Offences Unsuccessful Across Areas",
       x = "Area", y = "Motoring Offences Unsuccessful") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

# Save the plot
ggsave("my_plot_motoring_offences_unsuccessful.png", plot = my_plot_motoring_offences_unsuccessful, width = 10, height = 6, units = "in")
print(my_plot_motoring_offences_unsuccessful)

# Select "Motoring Offences Unsuccessful" in the dataset
selected_data_motoring_offences_unsuccessful <- mydata3 %>%
  select(Area, `Motoring Offences Unsuccessful`)

########################### Hypothesis Testing ##########################################

# Create dataframe mydata6 with selected variables
mydata6 <- mydata %>%
  select(-matches("\\(%)"), -contains("unsuccessful")) %>%
  mutate(Month = as.numeric(Month), Time = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
  filter(Area == "National") %>%
  arrange(Time) %>%
  group_by(Time) %>%
  select(-c("Date", "Year", "Month", "Time_numeric")) %>%
  mutate(across(c("Homicide Convictions", "Robbery Convictions"), as.numeric))

# Calculate sum and average of convictions
columns_to_sum <- c("Homicide Convictions", "Offences Against The Person Convictions", "Sexual Offences Convictions",
                    "Burglary Convictions", "Robbery Convictions", "Theft And Handling Convictions", "Fraud And Forgery Convictions",
                    "Criminal Damage Convictions", "Drugs Offences Convictions", "Public Order Offences Convictions",
                    "All Other Offences excluding Motoring Convictions", "Motoring Offences Convictions")

mydata6$Sum_Convictions <- rowSums(mydata6[columns_to_sum], na.rm = TRUE)
mydata6$Average_Convictions <- rowMeans(mydata6[columns_to_sum], na.rm = TRUE)

# View updated data
View(mydata6)

# Process unsuccessful data (mydata7)
mydata7 <- mydata3 %>%
  mutate(Time = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
  mutate_at(vars(contains("unsuccessful")), as.integer) %>%
  filter(Area == "National") %>%
  select(-c("Year", "Month")) %>%
  arrange(Time)

# Calculate sum and average of unsuccessful convictions
columns_to_sum_unsuccessful <- c("Homicide Unsuccessful", "Offences Against The Person Unsuccessful", "Sexual Offences Unsuccessful",
                                 "Burglary Unsuccessful", "Robbery Unsuccessful", "Theft And Handling Unsuccessful", "Fraud And Forgery Unsuccessful",
                                 "Criminal Damage Unsuccessful", "Drugs Offences Unsuccessful", "Public Order Offences Unsuccessful",
                                 "All Other Offences excluding Motoring Unsuccessful", "Motoring Offences Unsuccessful", "Admin Finalised Unsuccessful")

mydata7$Sum_Unsuccessful <- rowSums(mydata7[columns_to_sum_unsuccessful], na.rm = TRUE)
mydata7$Average_Unsuccessful <- rowMeans(mydata7[columns_to_sum_unsuccessful], na.rm = TRUE)

# View updated unsuccessful data
View(mydata7)

# Calculate total convictions and unsuccessful in area summary
area_summary$Total_Convictions <- rowSums(area_summary[, grepl("Convictions", names(area_summary))])
area_summary3$Total_Unsuccessful <- rowSums(area_summary3[, grepl("Unsuccessful", names(area_summary3))])

# Print area summary
print(area_summary)
print(area_summary3)

# Create Time_Numeric from Time
mydata4$Time_Numeric <- as.numeric(gsub("-", "", mydata4$Time))

# Convert all columns except "Time" to numeric
mydata4 <- mydata4 %>%
  mutate_if(!grepl("Time", names(.)), as.numeric)

# Check the structure of the modified data frame
str(mydata4)


# Loading necessary libraries
library(dplyr)
library(factoextra)
library(randomForest)
library(caret)

# Hypothesis Testing: Linear Regression Analysis for Time vs Convictions

# Hypothesis 1: Time vs Sum_Convictions (Successful)
model1 <- lm(Sum_Convictions ~ Time, data = mydata6)
summary(model1)
plot(model1)

# Hypothesis 2: Time vs Sum_Unsuccessful (Unsuccessful)
model2 <- lm(Sum_Unsuccessful ~ Time, data = mydata7)
summary(model2)
plot(model2)

# Hypothesis 3: Area vs Total_Convictions (ANOVA Test)
anova_result1 <- aov(Total_Convictions ~ Area, data = area_summary)
summary(anova_result1)

# Hypothesis 4: Area vs Total_Unsuccessful (ANOVA Test)
anova_result2 <- aov(Total_Unsuccessful ~ Area, data = area_summary3)
summary(anova_result2)

# Correlation Analysis: Time vs Convictions

# Correlation Matrix
correlation_matrix <- cor(mydata4[, c("Time_Numeric", "Homicide_Convictions", "Offences_Against_Person_Convictions", 
                                      "Sexual_Offences_Convictions", "Burglary_Convictions", "Robbery_Convictions", 
                                      "Theft_And_Handling_Convictions", "Fraud_And_Forgery_Convictions", 
                                      "Criminal_Damage_Convictions", "Drugs_Offences_Convictions", 
                                      "Public_Order_Offences_Convictions", "Motoring_Offences_Convictions", 
                                      "Average_All_Other_Convictions", "Sum_Convictions")])
print(correlation_matrix)

# Correlation Tests with Multiple Comparisons Adjustment
dependent_variables <- c("Homicide_Convictions", "Offences_Against_Person_Convictions", "Sexual_Offences_Convictions", 
                         "Burglary_Convictions", "Robbery_Convictions", "Theft_And_Handling_Convictions", 
                         "Fraud_And_Forgery_Convictions", "Criminal_Damage_Convictions", "Drugs_Offences_Convictions", 
                         "Public_Order_Offences_Convictions", "Motoring_Offences_Convictions")

cor_results <- lapply(dependent_variables, function(variable) {
  cor_test_result <- cor.test(mydata4$Time_Numeric, mydata4[[variable]])
  cat("Correlation Test for", variable, ":\n")
  print(cor_test_result)
  return(cor_test_result$p.value)
})

adjusted_p_values <- p.adjust(unlist(cor_results), method = "bonferroni")
cat("\nAdjusted p-values for multiple comparisons:\n")
print(adjusted_p_values)

# Prediction Modeling: Linear Regression for Time vs Sum_Convictions
model3 <- lm(Sum_Convictions ~ Time, data = mydata6)
summary(model3)

# Clustering: K-Means Clustering
mydata2_data <- mydata2[, c("Homicide_Convictions", "Offences_Against_Person_Convictions", "Sexual_Offences_Convictions", 
                            "Burglary_Convictions", "Robbery_Convictions", "Theft_And_Handling_Convictions", 
                            "Fraud_And_Forgery_Convictions", "Criminal_Damage_Convictions", "Drugs_Offences_Convictions", 
                            "Public_Order_Offences_Convictions", "Motoring_Offences_Convictions")]

mydata2_data_scale <- scale(mydata2_data)
mydata2_dist <- dist(mydata2_data_scale)

# Elbow Method for Optimal Number of Clusters
fviz_nbclust(mydata2_data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

# K-Means Clustering
km_out <- kmeans(mydata2_data_scale, centers = 4, nstart = 100)
print(km_out)

# Visualize Clustering
km_clusters <- km_out$cluster
rownames(mydata2_data_scale) <- paste(mydata2$Area, 1:nrow(mydata2), sep = "_")
fviz_cluster(list(data = mydata2_data_scale, cluster = km_clusters))

# Check Cluster Distribution
table(km_clusters, mydata2$Area)

# Classification: Random Forest Model

# Prepare Data for Classification
mydata9 <- mydata[, c("Area", "Homicide_Convictions", "Offences_Against_Person_Convictions", "Sexual_Offences_Convictions", 
                      "Burglary_Convictions", "Robbery_Convictions", "Theft_And_Handling_Convictions", 
                      "Fraud_And_Forgery_Convictions", "Criminal_Damage_Convictions", "Drugs_Offences_Convictions", 
                      "Public_Order_Offences_Convictions", "Motoring_Offences_Convictions")]

mydata9$Area <- as.factor(mydata9$Area)
mydata9$`Robbery_Convictions` <- as.numeric(mydata9$`Robbery_Convictions`)

# Replace spaces with underscores in column names
names(mydata9) <- gsub(" ", "_", names(mydata9))
str(mydata9)

# Train Random Forest Model
train_data <- mydata9[1:1000, ]
test_data <- mydata9[1001:1419, ]
rf_model <- randomForest(Area ~ ., data = train_data, ntree = 500)

# Make Predictions
predictions <- predict(rf_model, newdata = test_data)

# Evaluate Accuracy
accuracy <- sum(predictions == test_data$Area) / length(predictions)
cat("Accuracy:", accuracy, "\n")

# Confusion Matrix
confusion_matrix <- confusionMatrix(predictions, test_data$Area)
print("Confusion Matrix:")
print(confusion_matrix)

# Precision, Recall, F1-Score, and Accuracy
precision <- confusion_matrix$byClass["Precision"]
recall <- confusion_matrix$byClass["Recall"]
f1_score <- confusion_matrix$byClass["F1"]
accuracy <- confusion_matrix$overall["Accuracy"]

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n")
cat("Accuracy:", accuracy, "\n")

# Feature Importance
importance <- importance(rf_model)
par(mfrow = c(1, 2))
varImpPlot(rf_model, main = "Variable Importance (Blue)", col = "blue")
varImpPlot(rf_model, main = "Variable Importance (Red)", col = "red")











