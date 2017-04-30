# Checkpoint 1:
# Load the companies and rounds2 data 
#(provided on the previous page) into two data frames and name them companies and rounds2 respectively.
# Puts a blank space if rows contain inequal no. of data compared to no of columns
# removes "EOF within quoted string" error

companies <- read.table('companies.txt', header = T, sep = '\t', fill = TRUE, quote=NULL, comment.char = "", stringsAsFactors = FALSE)
rounds2 <- read.csv('rounds2.csv', header = T, fill = TRUE, comment.char = "", stringsAsFactors = FALSE)

# replace blank space with na values

is.na(companies) <- companies ==''
is.na(rounds2) <- rounds2 ==''

# Load packages to work with the data
library(tidyr)
library(dplyr)
library(stringr)

# How many unique companies are present in rounds2? We can put a count() on top of distinct()


total_distnct_company_rounds2 <- count(distinct(rounds2, company_permalink))
# Ans 90247


# How many unique companies are present in companies?

total_distnct_company_companies <- count(distinct(companies, permalink))
# Ans 66368


# Note: if you use "name" instead of permalink you get less no. of obs.
# Does this mean two companies can have same name but different permalink's?
# If yes, this answers the next question!

# In the companies data frame, which column can be used 
# as the unique key for each company? Write the name of the column.


# Ans: permalink 

# Are there any companies in the rounds2 file which are not present in companies? Answer yes or no:
# Since no. of obs. in distinct_company_rounds2 > no. of obs. in distinct_company_companies,
# then considering each permalink denotes unique company, the answer should be 'Yes'

total_distnct_company_rounds2>total_distnct_company_companies
# Ans: Yes

#Merge the two data frames so that all variables (columns) in the companies frame are 
#added to the rounds2 data frame. Name the merged frame master_frame. How many observations 
#are present in master_frame?


data_round <- rounds2
data_company <- companies

# I found we cannot just use merge by permalink's as the case don't match in two columsns of the two data frames
# change permalink and company_permalink to uppercase so that they can be 
# used as a key to merge

data_round$company_permalink <- str_to_upper(data_round$company_permalink)
data_company$permalink <- str_to_upper(data_company$permalink)

# merging based on assuming now that permanlink and company_permalink are same keys
# May be we will need a left join here instead

master_frame <- merge(x=data_round,y=data_company, by.x = c("company_permalink"), by.y = c("permalink") ,x.all=True )

# note in the merged data frame there are 114948 obs. which is 1 less than 114949 obs. in rounds2. 
# I don't undertand where did 1 row go?


# Checkpoint: 2
# How many NA values are present in the column raised_amount_usd?

sum(is.na(master_frame$raised_amount_usd))

# Ans: 19990

# Replace all the NA values from the raised_amount_usd column of the master frame
# What do you replace NA values of raised_amount_usd with? Enter a numeric value. 
# Ans. 0

master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0 

# Checkpoint: 3

# Group by funding_round_type
master_frame_funding_round_grp <- group_by(master_frame, funding_round_type)
# Summarize the data by raised amount average
summary_by_funding_round <- summarise(master_frame_funding_round_grp,mean(raised_amount_usd, na.rm=T))

#filter the required types i.e."angel","venture","seed","private_equity"
filtered_summary <- filter(summary_by_funding_round,funding_round_type %in% c("angel","venture","seed","private_equity"))

View(filtered_summary)
# Average funding amount of venture type

# Ans: 10634054.4

# Average funding amount of angel type

# Ans: 764564.3

# Average funding amount of seed type

# Ans: 556623

# Average funding amount of private equity type

# Ans: 62111788.2

# Considering that Spark Funds wants to invest between 5 to 15 million USD 
# per investment round, which investment type is the most suitable for it?
# Ans: Venture


# Checkpoint: 4
# 1. Spark Funds wants to see the top nine countries which have received 
# the highest total funding (across ALL sectors for the chosen investment type)
# 2. For the chosen investment type, make a data frame named top9 with the top nine countries 
# (based on the total investment amount each country has received)
#
# remove NA values in country_code
#master_by_country <- master_frame[!is.na(master_frame$country_code),]
# First we group by countries
#master_by_country <- group_by(master_by_country, country_code) 
# Then sum the investments for each country
#master_by_country_invest <- summarise(master_by_country, tot_invest = sum(raised_amount_usd, na.rm = T))

# 1 & 2.

# Filter master_frame using funding round type "venture"
master_venture_invest <- filter(master_frame,funding_round_type == "venture")

# Fill missing country codes with new type "Other"

#master_venture_invest[is.na(master_venture_invest$country_code),]$country_code <- "Other"

# Removing rows with na values in country code column
master_venture_invest <- master_venture_invest[!is.na(master_venture_invest$country_code),]

# Group by country code
master_vent_invest_country <- group_by(master_venture_invest, country_code)
# Summarise by tot_investment 
master_vent_summary <- summarise(master_vent_invest_country, tot_invest = sum(raised_amount_usd, na.rm = T))

# arrange in descending order of investment
master_vent_invest_country <- arrange(master_vent_summary, desc(tot_invest))
# take top 9 rows
top9 <- master_vent_invest_country[1:9,]

# Table 4.1 


#  Top English-speaking country: USA
#  Second English-speaking country: GBR (Does GBR stands for Great Britain? )
#  Third English-speaking country	: IND

# Checkpoint 5

#Extract the primary sector of each category list from the category_list column
#str_split(master_frame$category_list,pattern= "\\|")

#pending

#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors (Note that 'Others' is also considered one of the main sectors)

#Read mapping.csv
primary_sector_mapping <- read.csv("mapping.csv",stringsAsFactors = FALSE)
#check names of columns
names(primary_sector_mapping)
# gather on primary_sector
category_primary_sector <- gather(primary_sector_mapping,Primary_Sector,value,`Automotive...Sports`:`Social..Finance..Analytics..Advertising`)

# remove unwanted gathered rows
category_primary_sector <- filter(category_primary_sector,value ==1)
#remove value column as it is constant
category_primary_sector <- category_primary_sector[,-3]

#left join to add Primary sector to merged master frame
merged_master_frame <- merge(x=master_frame,y=category_primary_sector,by.x=c("category_list"),by.y=c("category_list"),x.all=T)





# Checkpoint 6

#Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of funding type FT falling within the 5-15 million USD range. The three data frames should contain:
#All the columns of the master_frame along with the primary sector and the main sector
#The total number (or count) of investments for each main sector in a separate column
#The total amount invested in each main sector in a separate column
