
# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 1 - STATISTICAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------


# Load text file into local variable called 'data'
data = read.delim(file = 'purchases.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Display the data after transformation
head(data)
summary(data)

# Compute key marketing indicators using SQL language
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                  MIN(days_since) AS 'recency',
                  COUNT(*) AS 'frequency',
                  AVG(purchase_amount) AS 'amount'
                  FROM data GROUP BY 1")

# Explore the data
head(customers)
summary(customers)
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 100)


# --- PREPARING AND TRANSFORMING DATA ----------------------


# Copy customer data into new data frame
new_data = customers

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
new_data$amount = log(new_data$amount)
hist(new_data$amount)

# 1) the segmentation variable "frequency" is replaced by its log (before it is scaled),
new_data$frequency = log(new_data$frequency)

# Standardize variables
new_data = scale(new_data)
head(new_data)


# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------


# Compute distance metrics on standardized data
# This will likely generate an error on most machines
# d = dist(new_data)

# Take a 10% sample
sample = seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Compute distance metrics on standardized data
d = dist(new_data_sample)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method = "ward.D2")

# Plot de dendogram
plot(c)

# "2) you select a 5-segment solution instead of a 9-segment solution."
members = cutree(c, k = 5)

# Show 30 first customers, frequency table
members[1:30]
table(members)

# Show profile of each segment
segment_profiles = aggregate(customers_sample[, 2:4], by = list(members), mean)

# What is the average purchase amount of the segment which contains, on average, 
# the customers who have made their last purchase the most recently?
segment_profiles_final = 

# What is the size of the largest segment?
# Ugly code, but efficient (for me)
segment_size_1 = sum(members == 1)
segment_size_2 = sum(members == 2)
segment_size_3 = sum(members == 3)
segment_size_4 = sum(members == 4)
segment_size_5 = sum(members == 5)

segment_size = data.frame(segment_size_1, segment_size_2, segment_size_3,segment_size_4, segment_size_5)

colnames(segment_size) = c("Segment 1", "Segment 2", "Segment 3", "Segment 4", "Segment 5") 

segment_size = t(segment_size)

colnames(segment_size) = c("Size")

segment_size_final = as.data.frame(segment_size)

segment_size_final

# Does customer #260 belongs to the same segment than customer #5920?
customer260 = customers[customers$customer_id == "260", ]

customer5920 = customers[customers$customer_id == "5920", ]

customer_compared = rbind(customer260, customer5920)

customer_compared

