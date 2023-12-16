# Question 1
data = read.csv("compustat.csv");
data.2015 = data[data$YEAR == 2015,];
data.2016 = data[data$YEAR == 2016,];

uniqueCompanies = length(unique(data$CUSIP));
uniqueCompanies.2015 = length(unique(data.2015$CUSIP));
uniqueCompanies.2016 = length(unique(data.2016$CUSIP));

data.q4 = data[data$FQTR == 4,];
data.q4 = data.q4[!(is.na(data.q4$EPS)),];
data.q4 = data.q4[!(is.na(data.q4$Price)),];
data.q4 = data.q4[-which(data.q4$EPS == 0),];

data.q4$PE.ratio = data.q4$Price / data.q4$EPS;
data.2015.q4 = data.q4[data.q4$YEAR == 2015,];
data.2016.q4 = data.q4[data.q4$YEAR == 2016,];
data.both.q4 = data.2015.q4[data.2015.q4$CUSIP %in% data.2016.q4$CUSIP,];

print(sum(data.2015.q4$PE.ratio > 0) / nrow(data.2015.q4) * 100);
print(sum(data.2016.q4$PE.ratio < 0) / nrow(data.2016.q4) * 100);


print("TODO");