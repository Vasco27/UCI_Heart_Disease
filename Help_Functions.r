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

#Predict for the lda and qda models
lda_preds = function(model, x.test, threshold = 0.5, probs = FALSE) {
    lda.pred = predict(model, x.test)
    lda.probs = lda.pred$posterior[, 2] #probs of being 1 (positive)
    
    if(probs == TRUE) {
        return(lda.probs)
    }
    
    if(threshold == 0.5) {
        return(lda.pred$class)
    } else {
        return(factor(ifelse(lda.probs > threshold, 1, 0)))
    }    
}

#Evaluate the various models passed as parameters, returns the metrics (Should be able to receive a function like "make_preds" for each type of model)
#If feature.selection = TRUE, models.reduced contains the feature selection models
evaluate_models = function(models.complete, models.reduced, predict_function = make_preds, best.thresholds = FALSE, tpr.threshold = 0.8, fpr.threshold = 0.3,
                           feature.selection = FALSE, plot.width = 24, plot.height = 12, title = "Different metrics for each model", 
                           x.models = c("Complete", "Complete_step", "Reduced", "Reduced_step")) {
    
    #train test spliting for both types of datasets
    ret = train_test_split(uci_heart, train_size = 0.7); train.complete = ret$train; test.complete = ret$test
    if(feature.selection == FALSE) {
        ret = train_test_split(df.reduced, train_size = 0.7); train.reduced = ret$train ;test.reduced = ret$test
    }
    #It is the same for both datasets
    y_train = train.complete$target; y_test = test.complete$target
    
    #define thresholds
    if(best.thresholds == FALSE) {
        thresholds = c(0.5, 0.5, 0.5, 0.5)
    } else {
        thresholds = best_thresholds(models.complete, models.reduced, predict_function, test.complete, test.reduced, y_test, tpr.thresh = tpr.threshold, fpr.thresh = fpr.threshold)
    }

    #Evaluate model and retrieve immportant metrics
    recalls = c(); precisions = c(); f1scores = c(); specificities = c(); accs = c(); AUCs = c(); type = c(); i = 1
    for(model in models.complete){
        preds = predict_function(model, test.complete, threshold = thresholds[i])
        confm = caret::confusionMatrix(preds, y_test, positive = "1")
        recalls[i] = confm$byClass["Recall"]; precisions[i] = confm$byClass["Precision"]; f1scores[i] = confm$byClass["F1"]; specificities[i] = confm$byClass["Specificity"]; accs[i] = confm$overall["Accuracy"];
        probs = predict_function(model, test.complete, threshold = thresholds[i], probs = TRUE)
        AUCs[i] = calc_AUC(probs, y_test)$auc
        type[i] = "Complete"

        i = i + 1
    }

    for(model in models.reduced) {
        if(feature.selection == TRUE) {
            preds = predict_function(model, test.complete, threshold = thresholds[i], feature.selection = TRUE)
            probs = predict_function(model, test.complete, threshold = thresholds[i], feature.selection = TRUE, probs = TRUE)
            type[i] = "Feature Selection"
        } else {
            preds = predict_function(model, test.reduced, threshold = thresholds[i])
            probs = predict_function(model, test.reduced, threshold = thresholds[i], probs = TRUE)
            type[i] = "Reduced"
        }
        
        confm = caret::confusionMatrix(preds, y_test, positive = "1")
        recalls[i] = confm$byClass["Recall"]; precisions[i] = confm$byClass["Precision"]; f1scores[i] = confm$byClass["F1"]; specificities[i] = confm$byClass["Specificity"]; accs[i] = confm$overall["Accuracy"];
        AUCs[i] = calc_AUC(probs, y_test)$auc

        i = i + 1
    }

    metrics = data.frame(Recall = recalls, Precision = precisions, F1 = f1scores, Specificity = specificities, Accuracy = accs, AUC = AUCs, Type = type, Models = x.models, row.names = NULL)
    
    #visualization
    #Uses thresholds?
    if(thresholds[1] != 0.5) {
        subtitle = make_subtitle(thresholds, x.models)
        print(visualize_metrics(metrics, plot.width, plot.height, plot.title = title, plot.subtitle = subtitle))
    } else {
        print(visualize_metrics(metrics, plot.width, plot.height, plot.title = title))
    }
    
    return(metrics)
}

#Method to Visualize all the metrics of a model, the metrics dataframe must have the structure [Metrics, Model Type, Model Name]
visualize_metrics = function(metrics, plot.width = 24, plot.height = 12, plot.title = "Different metrics for each model", plot.subtitle = "Threshold = 0.5") {
    options(repr.plot.width = plot.width, repr.plot.height = plot.height)
    #melt for facet wrap
    metrics.melt = melt(metrics, id.vars = c("Models", "Type"), value.name = "Count", variable.name = "Variable")

    p = ggplot(data = metrics.melt, aes(x = Models, y = Count)) + 
            geom_bar(aes(fill = Type), stat = "identity", width = 0.5, position = "dodge") + facet_wrap("Variable") +
            geom_text(aes(label = round(Count, 2)), position = position_dodge(width = 0.9), vjust = 1.25, size = 10) + 
            scale_y_continuous(limits = c(0.55, 0.95), oob=rescale_none) + 
            labs(title = plot.title, subtitle = plot.subtitle, y = "Metric Percentage") +
            theme(text = element_text(size = 20), plot.title = element_text(size = 30, face = "bold", hjust = 0.5), panel.grid.minor = element_blank(), 
                  panel.grid.major = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.75))
    
    return(p)
}

#Make subtitle for the metrics plot, based on thresholds
make_subtitle = function(thresh, x.models) {
    s = ""
    for(i in 1:length(thresh)) {
        s = paste(s, x.models[i], "threshold =", thresh[i], "\n")
    }
    return(s)
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

#This method computes the best thresholds for a set of models, that improve the recall the most.
best_thresholds = function(models.complete, models.reduced, predict_function = make_preds, test.complete, test.reduced, y.test, tpr.thresh = 0.8, fpr.thresh = 0.3) {
    thresh = c(); i = 1

    for(model in models.complete) {
        temp = roc_cutoff(predict_function, model, test.complete, y.test, tpr.threshold = tpr.thresh, fpr.threshold = fpr.thresh) #chose the acceptable fpr
        thresh[i] = round(temp$cutoff[1], 2)
        i = i + 1
    }

    for(model in models.red) {
        temp = roc_cutoff(predict_function, model, test.reduced, y.test, tpr.threshold = tpr.thresh, fpr.threshold = fpr.thresh)
        thresh[i] = round(temp$cutoff[1], 2)
        i = i + 1
    }
    
    return(thresh)
}




#FOR THE KNN MODELS-----------------------------------------------------------------------------------------------------

#Evaluates KNN method according to the K parameter of the method. Returns the relevant metrics for the model
evaluate_knn = function(ks = c(1,3,5,7,9,11,15), x.models = c("Complete model", "Reduced model")) {
    #train test spliting for both types of datasets
    ret = train_test_split(uci_heart, train_size = 0.7); train.complete = ret$train; test.complete = ret$test
    ret = train_test_split(df.reduced, train_size = 0.7); train.reduced = ret$train ;test.reduced = ret$test
    
    #It is the same for both datasets
    y_train = train.complete$target; y_test = test.complete$target
    
    #Evaluate model and retrieve immportant metrics
    set.seed(1)
    recalls = c(); precisions = c(); f1scores = c(); specificities = c(); accs = c(); i = 1
    for(k in ks) {
        knn.preds = knn(train.complete, test.complete, cl = y_train, k = k)
        confm = caret::confusionMatrix(knn.preds, y_test, positive = "1")
            
        recalls[i] = confm$byClass["Recall"]; precisions[i] = confm$byClass["Precision"]; f1scores[i] = confm$byClass["F1"]; specificities[i] = confm$byClass["Specificity"]
        accs[i] = confm$overall["Accuracy"]
        i = i + 1
    }
    
    metrics.complete = data.frame(Recall = recalls, Precision = precisions, F1 = f1scores, Specificity = specificities, Accuracy = accs, k = ks, row.names = NULL)
    i = 1
    for(k in ks) {
        knn.preds = knn(train.reduced, test.reduced, cl = y_train, k = k)
        confm = caret::confusionMatrix(knn.preds, y_test, positive = "1")
            
        recalls[i] = confm$byClass["Recall"]; precisions[i] = confm$byClass["Precision"]; f1scores[i] = confm$byClass["F1"]; specificities[i] = confm$byClass["Specificity"]
        accs[i] = confm$overall["Accuracy"]
        i = i + 1
    }
    
    metrics.reduced = data.frame(Recall = recalls, Precision = precisions, F1 = f1scores, Specificity = specificities, Accuracy = accs, k = ks, row.names = NULL)
    
    #Visualization (arranged grid)
    p1 = visualize_knn(metrics.complete, plot.title = x.models[1])
    p2 = visualize_knn(metrics.reduced, plot.title = x.models[2])
    
    fig = ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right") #one common legend between the plots
    grid = annotate_figure(fig,
                        top = textGrob("Metric values for the KNN method", gp = gpar(fontsize = 40, fontface = "bold", col = "darkred"))) #add a title to the grid
    print(grid)
    
    return(list("complete" = metrics.complete, "reduced" = metrics.reduced))
}

#Transforms the metrics dataset into plotable data. Plots the evolution of the metrics as the parameter K increases.
visualize_knn = function(metrics, plot.title = "Metric values for KNN") {
    #melt the dataset to use with ggplot
    metrics.melt = melt(metrics, id.vars = c("k"), value.name = "Count", variable.name = "Variable")
    
    p = ggplot(data = metrics.melt, aes(x = k, y = Count)) + 
            geom_line(aes(color = Variable), stat = "identity", size = 1) +
            scale_x_discrete(limits = k, labels = k) +
            scale_y_continuous(limits = c(0.3, 1)) +
            scale_color_brewer(palette = "Set1", name = "Metrics") +
            labs(title = plot.title, y = "Metric Percentage") +
                    theme(text = element_text(size = 20), plot.title = element_text(size = 30, face = "bold", hjust = 0.5, color = "darkorange"), panel.grid.minor = element_blank(), 
                          panel.grid.major = element_blank())
    
    return(p)
}

#Visualization
visualize_best_k = function(metrics.list, best.ks = c(5,5), x.models = c("Complete", "Reduced")) {
    metrics.complete = metrics.list$complete
    metrics.reduced = metrics.list$reduced
    
    best.metrics.complete = metrics.complete[metrics.complete$k == best.ks[1], ]
    best.metrics.reduced = metrics.reduced[metrics.reduced$k == best.ks[2], ]

    best.metrics = rbind(best.metrics.complete, best.metrics.reduced)
    best.metrics$Type = x.models
    best.metrics$k = as.character(best.metrics$k) #Make it a discrete value

    best.metrics.melt = melt(best.metrics, id.vars = c("k", "Type"), value.name = "Count", variable.name = "Variable")

    p = ggplot(data = best.metrics.melt, aes(x = Type, y = Count)) +
            geom_bar(aes(fill = k), stat = "identity", position = "dodge", width = 0.5) + facet_wrap("Variable") +
            geom_text(aes(label = round(Count, 2)), position = position_dodge(width = 0.9), vjust = 1.25, size = 10) + 
            scale_y_continuous(limits = c(0.55, 0.95), oob=rescale_none) + 
            labs(title = "Best KNN metrics", y = "Metric Percentage") +
            theme(text = element_text(size = 20), plot.title = element_text(size = 30, face = "bold", hjust = 0.5), panel.grid.minor = element_blank(), 
                    panel.grid.major = element_blank())
    
    print(p)
    
    return(best.metrics.melt)
}

#Evaluating and plotting the train and test errors of the different models for the KNN method
evaluate_knn_traintest_errors = function(k, x.models = c("Complete model", "Reduced model")) {
    set.seed(1)
    knn.complete.trainerr = c(); knn.reduced.trainerr = c(); knn.complete.testerr = c(); knn.reduced.testerr = c(); i = 1
    for(param in k) {
        preds = knn(train.complete, train.complete, cl = y_train, k = param)
        knn.complete.trainerr[i] = 1 - mean(preds == y_train)

        preds = knn(train.complete, test.complete, cl = y_train, k = param)
        knn.complete.testerr[i] = 1 - mean(preds == y_test)

        preds = knn(train.reduced, train.reduced, cl = y_train, k = param)
        knn.reduced.trainerr[i] = 1 - mean(preds == y_train)

        preds = knn(train.reduced, test.reduced, cl = y_train, k = param)
        knn.reduced.testerr[i] = 1 - mean(preds == y_test)

        i = i + 1
    }

    knn.complete.err = data.frame(cbind(knn.complete.trainerr, knn.complete.testerr))
    knn.reduced.err = data.frame(cbind(knn.reduced.trainerr, knn.reduced.testerr))
    
    p1 = plot_knn_traintest_errors(knn.complete.err, k, x.models[1])
    p2 = plot_knn_traintest_errors(knn.reduced.err, k, x.models[2])
    
    fig = ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right") #one common legend between the plots
    grid = annotate_figure(fig,
                        top = textGrob("Train vs Test errors for the KNN method", gp = gpar(fontsize = 40, fontface = "bold", col = "darkred"))) #add a title to the grid
    print(grid)
    
    return(list("complete_err" = knn.complete.err, "reduced_err" = knn.reduced.err))
}

#Visualization of the train and test errors for the KNN method
plot_knn_traintest_errors = function(model.errors, ks, plot.title) {
    colnames(model.errors) = c("train error", "test error")
    model.errors$k = ks
    model.errors.melt = melt(model.errors, id.vars = "k", value.name = "Count", variable.name = "Variable")


    p = ggplot(data = model.errors.melt, aes(x = k, y = Count)) + 
            geom_line(aes(color = Variable), stat = "identity", size = 1) +
                    scale_x_discrete(limits = k, labels = k) +
                    scale_y_continuous(limits = c(0, 0.4)) +
                    scale_color_brewer(palette = "Set1", name = "Train/Test errors") +
                    labs(title = plot.title, y = "Error percentage") +
                        theme(text = element_text(size = 20), plot.title = element_text(size = 30, face = "bold", hjust = 0.5, color = "darkorange"), panel.grid.minor = element_blank(), 
                                  panel.grid.major = element_blank())
    
    return(p)
}


#HIERARCHICAL CLUSTERING--------------------------------------------------------------------

plot_dendograms = function(h1,h2,h3,h4,title) {
#     windows(width=20, height=15)
   
    par(mfrow=c(2,2),oma=c(0,0,2,0))
    plot(h1,main="Complete Linkage", cex =.6)
    plot(h2,main="Single Linkage", cex =.6) 
    plot(h3,main="Average Linkage", cex =.6)
    plot(h4,main="Ward Linkage", cex =.6) 
    mtext(title, outer = TRUE, cex = 1.5)
}

plot_metrics = function(data,metric,tick,title,metric_name){
    
    ggplot(data, aes(fill=dissimilarity_measure, y=metric, x=linkage_method)) + 
    geom_bar(position="dodge", stat="identity")+
    labs(y = metric_name) +
    scale_y_continuous(breaks = round(seq(0, max(metric), by = tick),2))+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
}
calculate_metrics_table = function(){

    c11 <- cutree(hclus11, k = 2)
    c12 <- cutree(hclus12, k = 2)
    c13 <- cutree(hclus13, k = 2)
    c14 <- cutree(hclus14, k = 2)

    c21 <- cutree(hclus21, k = 2)
    c22 <- cutree(hclus22, k = 2)
    c23 <- cutree(hclus23, k = 2)
    c24 <- cutree(hclus24, k = 2)

    c31 <- cutree(hclus31, k = 2)
    c32 <- cutree(hclus32, k = 2)
    c33 <- cutree(hclus33, k = 2)
    c34 <- cutree(hclus34, k = 2)

    c41 <- cutree(hclus41, k = 2)
    c42 <- cutree(hclus42, k = 2)
    c43 <- cutree(hclus43, k = 2)
    c44 <- cutree(hclus44, k = 2)

    c51 <- cutree(hclus51, k = 2)
    c52 <- cutree(hclus52, k = 2)
    c53 <- cutree(hclus53, k = 2)
    c54 <- cutree(hclus54, k = 2)

    agregation_coef <- c(coef(hclus11),coef(hclus12),coef(hclus13),coef(hclus14),
                               coef(hclus21),coef(hclus22),coef(hclus23),coef(hclus24),
                               coef(hclus31),coef(hclus32),coef(hclus33),coef(hclus34),
                               coef(hclus41),coef(hclus42),coef(hclus43),coef(hclus44),
                               coef(hclus51),coef(hclus52),coef(hclus53),coef(hclus54))
    trgt = as.numeric(target)
    rand_index <- c(adj.rand.index(c11,trgt),adj.rand.index(c12,trgt),adj.rand.index(c13,trgt),adj.rand.index(c14,trgt),
                    adj.rand.index(c21,trgt),adj.rand.index(c22,trgt),adj.rand.index(c23,trgt),adj.rand.index(c24,trgt),
                    adj.rand.index(c31,trgt),adj.rand.index(c32,trgt),adj.rand.index(c33,trgt),adj.rand.index(c34,trgt),
                    adj.rand.index(c41,trgt),adj.rand.index(c42,trgt),adj.rand.index(c43,trgt),adj.rand.index(c44,trgt),
                    adj.rand.index(c51,trgt),adj.rand.index(c52,trgt),adj.rand.index(c53,trgt),adj.rand.index(c54,trgt))

    dissimilarity_measure <- c("pearson","pearson","pearson","pearson",
                               "euclidean","euclidean","euclidean","euclidean",
                               "spearman","spearman","spearman","spearman",
                               "manhattan","manhattan","manhattan","manhattan",
                               "cosine","cosine","cosine","cosine")

    linkage_method <- c("complete","single","average","ward",
                        "complete","single","average","ward",
                        "complete","single","average","ward",
                        "complete","single","average","ward",
                        "complete","single","average","ward")

    trgt_fac = as.factor(trgt)

    precision <- c(caret::precision(trgt_fac,as.factor(c11)),caret::precision(trgt_fac,as.factor(c12)),caret::precision(trgt_fac,as.factor(c13)),caret::precision(trgt_fac,as.factor(c14)),
                   caret::precision(trgt_fac,as.factor(c21)),caret::precision(trgt_fac,as.factor(c22)),caret::precision(trgt_fac,as.factor(c23)),caret::precision(trgt_fac,as.factor(c24)),
                   caret::precision(trgt_fac,as.factor(c31)),caret::precision(trgt_fac,as.factor(c32)),caret::precision(trgt_fac,as.factor(c33)),caret::precision(trgt_fac,as.factor(c34)),
                   caret::precision(trgt_fac,as.factor(c41)),caret::precision(trgt_fac,as.factor(c42)),caret::precision(trgt_fac,as.factor(c43)),caret::precision(trgt_fac,as.factor(c44)),
                   caret::precision(trgt_fac,as.factor(c51)),caret::precision(trgt_fac,as.factor(c52)),caret::precision(trgt_fac,as.factor(c53)),caret::precision(trgt_fac,as.factor(c54)))

    recall <- c(caret::recall(trgt_fac,as.factor(c11)),caret::recall(trgt_fac,as.factor(c12)),caret::recall(trgt_fac,as.factor(c13)),caret::recall(trgt_fac,as.factor(c14)),
                caret::recall(trgt_fac,as.factor(c21)),caret::recall(trgt_fac,as.factor(c22)),caret::recall(trgt_fac,as.factor(c23)),caret::recall(trgt_fac,as.factor(c24)),
                caret::recall(trgt_fac,as.factor(c31)),caret::recall(trgt_fac,as.factor(c32)),caret::recall(trgt_fac,as.factor(c33)),caret::recall(trgt_fac,as.factor(c34)),
                caret::recall(trgt_fac,as.factor(c41)),caret::recall(trgt_fac,as.factor(c42)),caret::recall(trgt_fac,as.factor(c43)),caret::recall(trgt_fac,as.factor(c44)),
                caret::recall(trgt_fac,as.factor(c51)),caret::recall(trgt_fac,as.factor(c52)),caret::recall(trgt_fac,as.factor(c53)),caret::recall(trgt_fac,as.factor(c54)))


    f5 <- c(caret::F_meas(trgt_fac, as.factor(c11),beta=5),caret::F_meas(trgt_fac, as.factor(c12),beta=5),caret::F_meas(trgt_fac, as.factor(c13),beta=5),caret::F_meas(trgt_fac, as.factor(c14),beta=5),
            caret::F_meas(trgt_fac, as.factor(c21),beta=5),caret::F_meas(trgt_fac, as.factor(c22),beta=5),caret::F_meas(trgt_fac, as.factor(c23),beta=5),caret::F_meas(trgt_fac, as.factor(c24),beta=5),
            caret::F_meas(trgt_fac, as.factor(c31),beta=5),caret::F_meas(trgt_fac, as.factor(c32),beta=5),caret::F_meas(trgt_fac, as.factor(c33),beta=5),caret::F_meas(trgt_fac, as.factor(c34),beta=5),
            caret::F_meas(trgt_fac, as.factor(c41),beta=5),caret::F_meas(trgt_fac, as.factor(c42),beta=5),caret::F_meas(trgt_fac, as.factor(c43),beta=5),caret::F_meas(trgt_fac, as.factor(c44),beta=5),
            caret::F_meas(trgt_fac, as.factor(c51),beta=5),caret::F_meas(trgt_fac, as.factor(c52),beta=5),caret::F_meas(trgt_fac, as.factor(c53),beta=5),caret::F_meas(trgt_fac, as.factor(c54),beta=5))
    
    return(data.frame(dissimilarity_measure, linkage_method, agregation_coef,rand_index,precision,recall,f5))
}

library(nbconvertR)
nbconvert(
    "Help_Functions.ipynb",
    fmt = "script",
    quiet = TRUE,
    )
