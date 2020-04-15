#Help functions
to_numeric = function(x) {
    return(as.numeric(as.character(x)))
}
#Splits dataset into a train and test set with given percent split
train_test_split = function(data, train_size = 0.7) {
    set.seed(1)
    
    #train_idx = createDataPartition(data$target, p = train_size, list = FALSE, times = 1)
    train_idx = sample(1:nrow(data), train_size*nrow(data))
    
    tr = data[train_idx, ]
    te = data[-train_idx, ]
    
    return(list("train" = tr, "test" = te))
}

#Make logistic regression predictions, can be changed to return probabilities or it can be used to predict lasso and ridge regression
make_preds = function(model, data.test, threshold = 0.5, probs = FALSE, feature.selection = FALSE) {
    if(feature.selection == TRUE) {
        glm.probs = predict(model, newx = model.matrix(target ~ ., data = data.test)[, -1], type = "response")
    } else {
        glm.probs = predict(model, newdata = data.test, type = "response")   
    }
    
    if(probs == TRUE) {
        return(glm.probs)
    }
    
    glm.preds = ifelse(glm.probs > threshold, 1, 0)
    return(factor(glm.preds)) #deixa de ser preciso o y.test
}

#Predict for the lda model
lda_preds = function(model, x.test, probs = FALSE) {
    lda.pred = predict(model, x.test)
    
    if(probs == TRUE) {
        return(lda.pred$posterior[, 2]) #probs of being 1 (positive)
    }
    
    return(lda.pred$class)
}

#Evaluate the various models passed as parameters, returns the metrics (Should be able to receive a function like "make_preds" for each type of model)
#If feature.selection = TRUE, models.reduced contains the feature selection models
evaluate_models = function(models.complete, models.reduced, predict_function = make_preds, feature.selection = FALSE, plot.width = 24, plot.height = 12, x.models = c("Complete", "Complete_step", "Reduced", "Reduced_step")) {
    
    #train test spliting for both types of datasets
    ret = train_test_split(uci_heart, train_size = 0.7); train.complete = ret$train; test.complete = ret$test
    if(feature.selection == FALSE) {
        ret = train_test_split(df.reduced, train_size = 0.7); train.reduced = ret$train ;test.reduced = ret$test
    }
    #It is the same for both datasets
    y_train = train.complete$target; y_test = test.complete$target

    #Evaluate model and retrieve immportant metrics
    recalls = c(); precisions = c(); f1scores = c(); specificities = c(); accs = c(); AUCs = c(); type = c()

    i = 1
    for(model in models.complete){
        preds = predict_function(model, test.complete)
        confm = caret::confusionMatrix(preds, y_test, positive = "1")
        recalls[i] = confm$byClass["Recall"]; precisions[i] = confm$byClass["Precision"]; f1scores[i] = confm$byClass["F1"]; specificities[i] = confm$byClass["Specificity"]; accs[i] = confm$overall["Accuracy"];
        probs = predict_function(model, test.complete, probs = TRUE)
        AUCs[i] = calc_AUC(probs, y_test)$auc
        type[i] = "Complete"

        i = i + 1
    }

    for(model in models.reduced) {
        if(feature.selection == TRUE) {
            preds = predict_function(model, test.complete, feature.selection = TRUE)
            probs = predict_function(model, test.complete, feature.selection = TRUE, probs = TRUE)
            type[i] = "Feature Selection"
        } else {
            preds = predict_function(model, test.reduced)
            probs = predict_function(model, test.reduced, probs = TRUE)
            type[i] = "Reduced"
        }
        
        confm = caret::confusionMatrix(preds, y_test, positive = "1")
        recalls[i] = confm$byClass["Recall"]; precisions[i] = confm$byClass["Precision"]; f1scores[i] = confm$byClass["F1"]; specificities[i] = confm$byClass["Specificity"]; accs[i] = confm$overall["Accuracy"];
        AUCs[i] = calc_AUC(probs, y_test)$auc

        i = i + 1
    }

    metrics = data.frame(Recall = recalls, Precision = precisions, F1 = f1scores, Specificity = specificities, Accuracy = accs, AUC = AUCs, Type = type, Models = x.models, row.names = NULL)
    
    #visualization
    print(visualize_metrics(metrics, plot.width, plot.height))
    
    return(metrics)
}

#Method to Visualize all the metrics of a model, the metrics dataframe must have the structure [Metrics, Model Type, Model Name]
visualize_metrics = function(metrics, plot.width = 24, plot.height = 12) {
    options(repr.plot.width = plot.width, repr.plot.height = plot.height)
    #melt for facet wrap
    metrics.melt = melt(metrics, id.vars = c("Models", "Type"), value.name = "Count", variable.name = "Variable")

    p = ggplot(data = metrics.melt, aes(x = Models, y = Count)) + 
            geom_bar(aes(fill = Type), stat = "identity", width = 0.5, position = "dodge") + facet_wrap("Variable") +
            geom_text(aes(label = round(Count, 2)), position = position_dodge(width = 0.9), vjust = 1.25, size = 10) + 
            scale_y_continuous(limits = c(0.65, 0.95), oob=rescale_none) + 
            labs(title = "Different metrics for each model", y = "Metric Percentage") +
            theme(text = element_text(size = 20), plot.title = element_text(size = 30, face = "bold", hjust = 0.5), panel.grid.minor = element_blank(), 
                  panel.grid.major = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.75))
    
    return(p)
}

#Plots the ROC Curve of all the models in the parameters "models.complete" and "models.reduced". 
#"x.models" must have the same size as the number of models passed.
plot_ROCCurve = function(models.complete, models.reduced, predict_function = make_preds, x.models = c("Complete", "Complete_step", "Reduced", "Reduced_step"), plots.display = c(2,2), plot.width = 24, plot.height = 12) {
    options(repr.plot.width = plot.width, repr.plot.height = plot.height)
    par(cex.main = 1.75, cex.axis = 1.35, cex.lab = 1.5, lwd = 2.5, mfrow = plots.display)
    
    #train test spliting for both types of datasets
    ret = train_test_split(uci_heart, train_size = 0.7); train.complete = ret$train; test.complete = ret$test
    ret = train_test_split(df.reduced, train_size = 0.7); train.reduced = ret$train ;test.reduced = ret$test
    #It is the same for both datasets
    y_train = train.reduced$target; y_test = test.reduced$target
    
    i = 1
    for(model in models.complete) {
        ret = calc_AUC(predict_function(model, test.complete, probs = TRUE), y_test)
        
        perf <- performance(ret$pred,"tpr","fpr")
        plot(perf, colorize = TRUE, main = paste("ROC Curve for", x.models[i], "\nAUC =", ret$auc))
        i = i + 1
    }
    
    for(model in models.reduced) {
        ret = calc_AUC(predict_function(model, test.reduced, probs = TRUE), y_test)
        
        perf <- performance(ret$pred,"tpr","fpr")
        plot(perf, colorize = TRUE, main = paste("ROC Curve for", x.models[i], "\nAUC =", ret$auc))

        i = i + 1
    }
}

#Calculate the AUC using the ROCR package
calc_AUC = function(y.pred, y.test) {
    pred <- prediction(to_numeric(y.pred), y.test)
    auc.tmp = performance(pred, "auc")
    return(list("auc" = as.numeric(auc.tmp@y.values), "pred" = pred))
}

#Best prediction probabilities threhsoçld, according to the ROC curve cutoffs
roc_cutoff = function(predict_function = make_preds, model, x.test, y.test, tpr.threshold = 0.8, fpr.threshold = 0.2) {
    pred = calc_AUC(predict_function(model, x.test, probs = TRUE), y.test)$pred
    perf = performance(pred, "tpr", "fpr")
    
    cutoffs = data.frame(cutoff = perf@alpha.values[[1]], fpr = perf@x.values[[1]], tpr = perf@y.values[[1]]) #get the cutoff, tpr and fpr values
    cutoffs = cutoffs[order(cutoffs$tpr, decreasing = TRUE), ] #The best cutoff for the max tpr will be the first
    
    return(cutoffs[(cutoffs$tpr >= tpr.threshold) & (cutoffs$fpr <= fpr.threshold), ])
}

library(nbconvertR)
nbconvert(
    "Help_Functions.ipynb",
    fmt = "script",
    quiet = TRUE,
    )
