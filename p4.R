#CMPT 459 Data mining P4
#Shawn An

#install.packages("arules")
library(arules)
data(Groceries)

# P1 histogram of the number of categories per transaction
item <- summary(Groceries)@lengths
summary(Groceries)
item <- item/9835
barplot(item,xlab='size of category',ylab='frequency',main='histogram of size per transaction') 

# P2 frequent itemsets, closed frequent itemsets, and maximal frequent itemsets with minimum support = 0.001
# Use either apriori or fsets
#fsets <- eclat(Groceries, parameter = list(supp = 0.001))
fsets <- apriori(Groceries, parameter = list(supp = 0.001, target = "frequent itemsets"))
summary(fsets)
#inspect(fsets)

# closed frequent itemset
cfsets <- apriori(Groceries, parameter = list(supp = 0.001, target = "closed frequent itemsets"))
summary(cfsets)
#inspect(cfsets)

# maximal frequent itemset
mfsets <- apriori(Groceries, parameter = list(supp = 0.001, target = "maximally frequent itemsets"))
summary(mfsets)
#inspect(mfsets)

# P3 frequent itemsets, closed frequent itemsets, and maximal frequent itemsets with minimum support = 0.01
# Use either apriori or fsets
#fsets <- eclat(Groceries, parameter = list(supp = 0.01))
fsets2 <- apriori(Groceries, parameter = list(supp = 0.01, target = "frequent itemsets"))
summary(fsets2)
#inspect(fsets2)

# closed frequent itemset
cfsets2 <- apriori(Groceries, parameter = list(supp = 0.01, target = "closed frequent itemsets"))
summary(cfsets2)
#inspect(cfsets2)

# maximal frequent itemset
mfsets2 <- apriori(Groceries, parameter = list(supp = 0.01, target = "maximally frequent itemsets"))
summary(mfsets2)
#inspect(mfsets2)

# P4 top 10 itemset with high support
top10set <- sort(fsets2, by="support", decreasing=TRUE)
inspect(top5set[1:10])

# P5 
summary(mfsets2)

# P6 support = 0.01, confidence = 0.9
rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.9))
# no rules generated for the params, reducing conf = 0.
rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.51))
inspect(rules)

# P7 support = 0.01, confidence = 0.5 with "whole milk" on rhs 
rules2 <- apriori(data=Groceries, parameter=list(supp=0.01,conf = 0.50), appearance = list(default="lhs",rhs="whole milk"),control = list(verbose=F))
rules2 <- sort(rules2, decreasing=TRUE,by="lift")
inspect(rules2)

# P7 plotting the rules2
#install.packages("arulesViz")
library(arulesViz)
plot(rules2,method="graph",engine='interactive',shading=NA)








