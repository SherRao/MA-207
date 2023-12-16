options(max.print=1000000000);

# Question 1
data = read.csv("institution_owner.csv");
data.2015 = data[startsWith(as.character(data$DATE), "2015"),];
data.2016 = data[startsWith(as.character(data$DATE), "2016"),];

uniqueCompanies = length(unique(data$CUSIP));
uniqueCompanies.2015 = length(unique(data.2015$CUSIP));
uniqueCompanies.2016 = length(unique(data.2016$CUSIP));

data = data[-(which(data$SHR_OUT == 0)),]
data$ioc.i = data$SHARES / data$SHR_OUT;