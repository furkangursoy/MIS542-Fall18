
install.packages("arules")
library(arules)

#read transaction from file
tdata <- read.transactions("sampletransactions.txt", sep=",")
inspect(tdata)

#Groceries data from arules package
data(Groceries)
summary(Groceries)
inspect(head(Groceries))

#frequent items & frequent items data frame
frequentItems <- apriori(Groceries, parameter = list(supp = 0.01, maxlen = 15, minlen=2, target="frequent itemsets"))
frequentItemsDF <- inspect(frequentItems)
edit(frequentItemsDF)

frequentItems <- apriori(Groceries, parameter = list(supp = 0.1, maxlen = 15, minlen=1, target="frequent itemsets")) 
frequentItemsDF <- inspect(frequentItems)
edit(frequentItemsDF)


#rules
rules1 <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.3, minlen=2)) #identify strong rules
inspect(rules1)

rules2 <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.5, minlen=2))
inspect(rules2)

rules3 <- apriori(Groceries, parameter = list(supp = 0.005, conf = 0.5, minlen=4))
inspect(rules3)

#sort by lift
rules3_lift <- sort(rules3, by="lift", decreasing=TRUE)
inspect(rules3_lift)

#filter rules
rules_wholemilk_rhs <- apriori(data=Groceries, parameter=list(supp=0.05,conf = 0.1), appearance=list(rhs="whole milk")) #select rules where right hand side is 'whole milk' 
inspect(rules_wholemilk_rhs)

rules_wholemilk_lhs <- apriori(data=Groceries, parameter=list(supp=0.005,conf = 0.05), appearance=list(rhs="whole milk")) #select rules where left hand side is 'whole milk' 
inspect(rules_wholemilk_rhs)

rulesFiltered1 <-  apriori(data=Groceries, parameter=list(supp=0.005,conf = 0.1), appearance=list(lhs=c("yogurt","citrus fruit")))
inspect(rulesFiltered1) 

rulesFiltered2 <-  apriori(data=Groceries, parameter=list(supp=0.005,conf = 0.1), appearance=list(lhs=c("yogurt","citrus fruit"), rhs=c("rolls/buns", "whole milk")))
inspect(rulesFiltered2) 

####
# Assume that I am interested in rules where lhs contains yogurt. 
# If I just specify lhs="yogurt" or lhs=c("yogurt"), I get only the rules where lhs contains yogurt and nothing else. 
# I could not find an easy way to obtain rules where lhs contains yogurt and potentially other items.
# If you know/find an easy way or have written the code for it, please share. 
####


####
# The original arules package documentation states: "Apriori only creates rules with one item in the RHS (Consequent)!"
# If you know/find an way or have written the code for obtaining rules with more than one item on the rhs, please share.
####


####
# I've noticed that there were issues installing arules package on macbooks. I am not using a Mac, and unfortunately I could not replicate or solve the issue.
# this might (or might not) help: https://stackoverflow.com/questions/47352445/r-package-arules-installation-error-on-macos 
####



#### SEQUENTIAL PATTERN MINING ####

#install.packages("arulesSequences")

library(arulesSequences)

data(zaki)
zaki
summary(zaki)
inspect(zaki)
z <- as(zaki, "data.frame")
z
z[order(z$eventID),]
z[order(z$sequenceID),]

rules <- cspade(zaki, parameter = list(support = 0.50))
inspect(rules)

rules2 <- cspade(zaki, parameter = list(support = 0.25))
inspect(rules2)

rules3 <- cspade(zaki, parameter = list(support = 0.25, maxsize=1))
inspect(rules3)

rules4 <- cspade(zaki, parameter = list(support = 0.25, maxsize=1, maxlen=2))
inspect(rules4)


baskets <- read_baskets(con = file("test.txt"), info = c("sequenceID","eventID","SIZE"))
inspect(baskets)

rules5 <- cspade(baskets, parameter = list(support = 0.30))
inspect(rules5)

rules6 <- cspade(baskets, parameter = list(support = 0.30, mingap=3))
inspect(rules6)


rules7 <- cspade(baskets, parameter = list(support = 0.30, mingap= 3, maxgap=6))
inspect(rules7)


rules8 <- cspade(baskets, parameter = list(support = 0.30, maxwin=5)) #reydan says that maxwin is disabled in command line
inspect(rules8)
