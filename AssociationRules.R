install.packages('arules')
install.packages('arulesViz')
library(arules)
library(arulesViz)
data("Groceries")
Groceries
summary(Groceries)
Groceries@itemInfo[1:20,]
apply(Groceries@data[,10:20], 2, function(r) paste (Groceries@itemInfo[r,"labels"],
                                                    collapse = ", "))
itemset <- apriori(Groceries, parameter = list(minlen=1, maxlen=1,
                                               support=0.02,
                                               target="frequent itemsets"))
summary(itemset)

inspect(head(sort(itemset, by = "support"), 10))

itemsets <- apriori(Groceries, parameter = list(minlen=2, maxlen=2,
                                                support=0.02,
                                                target="frequent itemsets"))

summary(itemsets)

inspect(head(sort(itemsets, by= "support"), 10))
itemsets <- apriori(Groceries, parameter = list(minlen=3, maxlen=3,
                                                support=0.02,
                                                target="frequent itemsets"))
inspect(sort(itemsets, by= "support"))
itemsets <- apriori(Groceries, parameter = list(minlen=4, maxlen=4,
                                                support=0.02,
                                                target="frequent itemsets"))

itemsets <- apriori(Groceries, parameter = list(minlen=1,
                                                support=0.02,
                                                target="frequent itemsets"))

rules <- apriori(Groceries, parameter = list(support=0.001,
                                                confidence=0.6,
                                                target="rules"))
# Plot
itemFrequencyPlot(Groceries, topN = 10)

# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])
plot(rules, method = "graph", 
     measure = "confidence", shading = "lift")

plot(rules@quality)
plot(rules)

inspect(head(sort(rules, by='lift'), 10))

confidenceRules <- rules[quality(rules)$confidence > 0.9]
confidenceRules
plot(confidenceRules, method = 'matrix', measure = c('lift', 'confidence'),
     control = list(reorder='similarity'))

plot(confidenceRules, method = 'matrix', measure = c('lift', 'confidence'),
     control = list(reorder='support/confidence'))
highLiftRules <- head(sort(rules, by="lift"), 5)
plot(highLiftRules, method = "graph", control = list(type="items"))

