1.Introduction
1.1 Background
The dataset, published by the Crown Prosecution Service encapsulates a monthly breakdown of criminal case outcomes in the United Kingdom across various primary offence categories and CPS Areas. The Crown Prosecution Service (CPS) was established in 1986. Which is a key component of the criminal justice system in England and Wales and operates independently to make decisions about criminal prosecutions. CPS’s primary role is to review evidence gathered by the police and other investigative authorities and determine whether there is enough evidence to pursue criminal charges against an individual (The Crown Prosecution Service, n.d.)
The original dataset includes 51 variables related to various criminal case outcomes that happened over 43 geographical locations across England and Wales. These variables include numerous criminal activities such as homicide, fraud and forgery, criminal damage, drug offences, theft and handling, offences against the person, robbery, sexual offences, burglary, public order offences, motoring offences, and other offences excluding motoring. Moreover, the dataset reports the number of convictions and unsuccessful outcomes for each criminal category for each CPS Area, presenting a detailed examination of the legal outcomes across different locations.
1.2 Objectives
The objective of this analysis is to provide valuable insights into the patterns, trends, and factors driving case outcomes inside the Crown Prosecution Service, Applying a combination of descriptive and predictive analytics tools. Such as the use of descriptive analytics to analyze the dataset, as well as the development of prediction models utilizing linear regression, clustering, and classification approaches.
2.Data Integration and Cleaning
2.1 Data Download and Integration
Data integration is the problem of merging data from several sources and delivering a consistent representation of these facts to the user (Lenzerin, 2002). The dataset download and integration process involved several key steps to assemble a comprehensive dataset for subsequent analysis. The dataset was sourced from the website called data.gov.uk and which covered the data on various principal offences, from the period of 2014 to 2018. The original dataset consists of five distinct folders for each year. In each folder, that represents a year, individual CSV files were allocated to represent monthly breakdowns of criminal case outcomes across various principal offence categories and CPS Areas in the United Kingdom
A subset of the dataset covering the years 2014 to 2016 was chosen, yielding 33 months of data, for this analysis. Remarkably, records for 2014 were available in full, for 11 months in 2015, and 10 months in 2016. To account for differences in data completeness throughout the designated years and cover a meaningful timeframe, this sample was carefully selected. After the selection procedure, the 33 separate files were combined into a special folder while keeping their original filenames. 
 
Figure 2.1 Data Download and Integration
The above code demonstrates a systematic approach to integrate and preprocess the Crown Prosecution Service (CPS) Case Outcomes dataset. After configuring the working directory to the relevant folder, using dplyr, magrittr and stringr packages, the following steps have been taken for further analysis. Firstly, a new column called "date” was added to every CSV file. The file name was used to create this new column by removing the ".csv" extension and the particular prefix "principal_offence_category_." The code then continues to extract and add further temporal data, the year and month, from the newly formed "date" column. After that, each column's data type was converted to a common data type for further calculations. Moreover, the updated dataset is saved back into the original CSV files, safeguarding the changes for later use. Then using lapply, the script
iterates through each CSV file, applying the custom function to add date-related columns to the data frames. The script uses the bind_rows function to combine the modified frames into a single dataset. The valuable information extracted from each monthly breakdown of criminal case outcomes across various principal offense categories and CPS Areas in the United Kingdom is combined in this final step to create a cohesive and enriched dataset.

2.2 Data Cleaning and Exploration
2.2.1 Initial data exploration
The process of finding and removing issues from a data warehouse is known as data cleaning. When gathering and merging data from multiple sources into a data warehouse, maintaining good data quality and consistency becomes critical (1Vaishali Chandrakant Wangikar and 2Ratnadeep R. Deshmukh, 2011). During initial data exploration, using tidyverse package, initial look of the data frame has been explored with the help of str() and summary() functions. With the help of Skimr package, skim() has been used to generate further details of summary statistics. Furthermore, using vis_dat() function, visual representation of missing values, summary statistics and distribution and correlation plots of the data set has been generated.  
 
Figure 2.2 Initial data exploration
Figure 2.2 show that the dataset has 76 626 of total observations with 54 columns and 1419 rows. 

2.2.2	Changing data types
The data type of “Year” has been converted to numeric. Also, from the 2nd variable to the 51st variable, the data type of every other variable was changed to as integer since the observations of particular variables consist of rounded values. Subsequently, from the 3rd variable to the 51st variable, the data type of every other variable was changed to numeric, since it contains percentage values
2.2.3	Changing the column names
With the help of tidyverse and dplyr packages, I have renamed the first variable as “Area”. After that every other column from the 2nd variable to the 51st variable which had “Number.of” as a prefix was removed. Subsequently, every variable which had “Percentage.of” as a prefix was replaced with “%” symbol for easy reference.
2.2.4	Inspect for mislabeled column names
After examining for mislabeled column names, it was discovered that there were no reports of columns with improper labels or mislabeling.
2.2.5	Fixing string inconsistencies 
Column names 52, 53, and 54 were changed to "Date," "Year," and "Month," accordingly. This ensures consistency and adheres to the naming practice in which variable names begin with capital letters. The names(mydata) function is used to display the modified column names. Moreover, using “gsub” dots (“.”) between column names were removed
 
Figure 2.3 Variable Names

2.2.6	Missing Values
Missing values can arise as a result of a variety of causes, including missing totally at random, missing at random, or missing not at random. All of this could be the consequence of a system failure during data collecting or human error during data pre-processing (Tlamelo Emmanuel, Thabiso Maupong, Dimane Mpoeleng, Thabo Semong, Banyatsang Mphago & Oteng Tabona , 2021). The missing values problem is common in all domains that deal with data and produces a variety of concerns such as performance degradation, data analysis issues, and biased results caused by disparities in missing and complete values (Ayilara OF, Zhang L, Sajobi TT, Sawatzky R, Bohm E, Lix LM, 2019). The package's skim_without_charts() function is then used to provide a summary of the data frame, including missing values, data kinds, and summary statistics. Figure 2.4 and Figure 2.5 shows the missing Values in the original data frame.
 
 Figure 2.4 Missing Values

 Figure 2.5 Missing Values


As the first step, the columns with integer data types were selected. After that missing values of each variable were replaced with the mean values of respective variables. When data for specific observations is missing, eliminating those rows may result in the loss of valuable information and a biased representation of the dataset. The report has maintained the original mean of the variable by imputing missing values with the mean, ensuring that the statistical summary of the dataset remains relatively unchanged. This method provides a decent estimate for missing values without significantly altering the overall properties of the data. Thereafter, missing values of percentage variables have been calculated by using the below formulas,

Ex: %Homicide Convictions (Success) = Homicide Convictions` / (`Homicide Convictions` +                                                                                                                             Homicide Unsuccessful)

Ex: %Homicide Convictions = `Homicide Unsuccessful` / (`Homicide Convictions` + `Homicide Unsuccessful`)

Below figure 2.6 and figure 2.7 shows summary of variables after removing missing values. 
  
 
Figure 2.6 Missing Values

