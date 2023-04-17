# ============================================================ 
# 	Mini-assignment # 8
# ============================================================ 

# load the data_pat.csv file as a data.table and perform the following EDA、
library(data.table)
library(readr)
pat <- as.data.table(read_csv("data_pat.csv"))

# (1) find how many patients have "zip_income" of 0

print(paste("the number of patients have 'zip_income' of 0 is:", pat[zip_income == 0, .N], ""))

# (2) find how many patients have missing "zip_state" and "zip_income" of 0

print(paste0("the number of patients have 'zip_income' of 0 and missing 'zip_state' is: ", pat[zip_income == 0 & is.na(zip_state), .N]))

# (3) show the histogram of zip_income

hist(pat$zip_income, main = "Histogram of Zip_income", xlab='Zip_income', col='lightblue', labels=TRUE)

# (4) calculate the number of patients from each state and their avergage zip_income (round by 0) -- order by states with highest zip_income

pat[, .(max_income = max(zip_income), ct = .N, avg_zip_income = round(mean(zip_income), 0)), by = zip_state][order(-max_income)]

# (5) run the same data exploration as #4, but censor all states with fewer than 50 patients

pat[, .(max_income = max(zip_income), ct = .N, avg_zip_income = round(mean(zip_income), 0)), by = zip_state][ct >= 50][order(-max_income)]

# (6) plot histograms of zip_income distribution of all patients residing in MD, VA and FL separately
# use the par(mfrow) command to plot all three histograms in one screen (3 rows and 1 column)

states <- c('MD', 'VA', 'FL')
par(mfrow = c(3, 1))
for (s in states) {
  
  hist(pat[zip_state == s, ]$zip_income, main = paste("Zip_income Distribution of Residents in State", s, "")
       , xlab = 'zip_income', col = 'lightblue', labels = TRUE)
  
}

# (7) quantitatively measure if zip_income is normally distributed in FL patients (use at least two different tests)
x = data_pat[zip_state == 'FL', ]$zip_income
shapiro.test(x)
ks.test(unique(x),y='pnorm',alternative='two.sided') 

