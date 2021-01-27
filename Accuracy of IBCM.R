#Evaluation of IBCF with

eval <- as(train, "realRatingMatrix")

scheme <- evaluationScheme(eval[1:700], method="cross", k=4, given=-1, goodRating=3, train = 0.8)

scheme

results <- evaluate(scheme, method="IBCF", type = "topNList",
                    n=c(1,3,5,10,15,20))

results

getConfusionMatrix(results)[[1]]

avg(results)

plot(results, annotate=TRUE)

plot(results, "prec/rec", annotate=TRUE)


#Evaluation of IBCF with clusters

eval_c <- as(train, "realRatingMatrix")

scheme_c <- evaluationScheme(train_c5[1:700], method="cross", k=4, given=-1, goodRating=3, train = 0.8)

scheme_c

results_c <- evaluate(scheme, method="IBCF", type = "topNList",
                    n=c(1,3,5,10,15,20))

results_c

getConfusionMatrix(results)[[1]]

avg(results)

plot(results, annotate=TRUE)

plot(results, "prec/rec", annotate=TRUE)
