# Question 1
data = read.csv("institution_owner.csv");

# Question 1.1
print("Question 1.1");
data.2015 = data[startsWith(as.character(data$DATE), "2015"),];
data.2016 = data[startsWith(as.character(data$DATE), "2016"),];
print(length(unique(data$Institute_Name)));
print(length(unique(data.2015$Institute_Name)));
print(length(unique(data.2016$Institute_Name)));


data = data[-(which(data$SHR_OUT == 0)),];
data$ioc.i = data$SHARES / data$SHR_OUT;
data$ioc.i2 = data$ioc.i^2;

data.ioc = aggregate(ioc.i ~ CUSIP+DATE, data, sum);
data.ioc2 = aggregate(ioc.i2 ~ CUSIP+DATE, data, sum);
data.agg = merge(data.ioc, data.ioc2, by=c("CUSIP", "DATE"));
names(data.agg)[names(data.agg) == "ioc.i"] = "ioc";
names(data.agg)[names(data.agg) == "ioc.i2"] = "ioc2";

data.agg$hhi = data.agg$ioc2 / data.agg$ioc^2;
data.agg$YEAR = substr(data.agg$DATE, 1, 4);
print(data.agg[seq(1,10),])

# Question 1.2
data.agg.2015 = data.agg[data.agg$YEAR == "2015",];
data.agg.2016 = data.agg[data.agg$YEAR == "2016",];
data.agg.2015.lte20 = data.agg.2015[data.agg.2015$ioc <= 0.20,];
data.agg.2016.gte50 = data.agg.2016[data.agg.2016$ioc >= 0.50 & data.agg.2016$ioc <= 1.0,];

print("Question 1.2");
print(nrow(data.agg.2015.lte20) / nrow(data.agg.2015) * 100);
print(nrow(data.agg.2016.gte50) / nrow(data.agg.2016) * 100);

# Question 1.3
print("Question 1.3");
hhi.total.count = 0;
hhi.increase.count = 0;

for(i in 1:nrow(data.agg.2015)) {
  row2015 = data.agg.2015[i,];
  cusip = row2015[[1]];
  
  row2016 = data.agg.2016[data.agg.2016$CUSIP == cusip,];
  if(nrow(row2016) > 0) {
    hhi.total.count = hhi.total.count + 1;
    hhi2015 = row2015[[5]];
    hhi2016 = row2016[[5]];
    if(hhi2016 > hhi2015) {
      hhi.increase.count = hhi.increase.count + 1;
    }
  }
}
print(hhi.increase.count);
print(hhi.increase.count / hhi.total.count * 100);
