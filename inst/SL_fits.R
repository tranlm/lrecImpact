# Description: SL library
# Author: Linh Tran
# Email: tranlm@berkeley.edu
# Coauthors: Maya Petersen, Mark van der Laan, Constantin Yiannoutsous, et. al.
###############################################################################

#############
## SL VARS ##
#############
.nfolds = 8
.SL.glmnet.nfolds = 4
.SL.glmnet.nlambda = 100
.SL.knn.k = 10
.SL.polymars.cv = 4
.SL.svm.gamma = 0.1
.SL.nnet.size = 2
.SL.nnet.maxit = 1000
.SL.gbm.trees = 2000
.SL.gbm.depth = 1
.SL.gbm.nfolds = 4


###############################################################################
########################## SUPERLEARNER INGREDIENTS ###########################
###############################################################################

###############################################################################
# Q-fit NP methods have three variations:
# 1) Use continuous and truncate
# 2) Use logit transformation, model continuously while truncating
# 3) Use logit transformation and stratify by whether outcome is {0,1} OR (0,1)
	# nb. This is only set up for situations where newX = X
###############################################################################

#################
## ACCESSORIES ##
#################
bound = function(x, range) {
	newX = x
	newX[newX<min(range)] = min(range)
	newX[newX>max(range)] = max(range)
	return(newX)
}
method.NNloglik.LT = function() {
	out = list(
			require = NULL,
			computeCoef = function(Z, Y, libraryNames, verbose, obsWeights, control, ...) {
				# compute cvRisk
				cvRisk = apply(Z, 2, function(x) { -mean(obsWeights * (Y*log(x) + (1-Y)*log(1-x))) } )
				names(cvRisk) = libraryNames
				# compute coef
				.NNloglik = function(x, y, wt, start = rep(0, ncol(x))) {
					# adapted from MASS pg 445
					fmin = function(beta, X, y, w) {
						p = plogis(crossprod(t(X), beta))
						-sum(2 * w * (y*log(p) + (1-y)*log(1-p)))
					}
					gmin = function(beta, X, y, w) {
						eta = X %*% beta
						p = plogis(eta)
						-2 * t(w * dlogis(eta) * (y/p + -1*(1-y)/(1-p))) %*% X
					}
					fit = optim(start, fmin, gmin, X = x, y = y, w = wt, method = "L-BFGS-B", lower = 0, control=list(parscale= rep(1/length(start), length(start))), ...)
					invisible(fit)
				}
				tempZ = trimLogit(Z, trim = control$trimLogit)
				fit.nnloglik = .NNloglik(x = tempZ, y = Y, wt = obsWeights)
				if(verbose) {
					message(paste("Non-Negative log-likelihood convergence: ", fit.nnloglik$convergence == 0))
				}
				initCoef = fit.nnloglik$par
				initCoef[initCoef < 0] = 0.0
				initCoef[is.na(initCoef)] = 0.0
				# normalize so sum(coef) = 1 if possible
				if(sum(initCoef) > 0) {
					coef = initCoef/sum(initCoef)
				} else {
					warning("All algorithms have zero weight", call. = FALSE)
					coef = initCoef
				}
				out = list(cvRisk = cvRisk, coef = coef)
				return(out)
			},
			computePred = function(predY, coef, control, ...) {
				out = plogis(crossprod(t(trimLogit(predY, trim = control$trimLogit)), coef))
				return(out)
			}
	)
	invisible(out)
}

###########
## LASSO ##
###########
SL.lasso.LT = function(Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = .SL.glmnet.nfolds, nlambda = .SL.glmnet.nlambda, useMin = TRUE, ...) {
	# X must be a matrix, should we use model.matrix or as.matrix
	if(!is.matrix(X)) {
		X = model.matrix(~ -1 + ., X)
		newX = model.matrix(~ -1 + ., newX)
	}
	# now use CV to find lambda
	Y.matrix = cbind(1-Y,Y)
	fitCV = cv.glmnet(x = X, y = Y.matrix, weights = obsWeights, lambda = NULL, type.measure = 'deviance', nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda)
	# two options for lambda, fitCV$lambda.min and fitCV$lambda.1se
	pred = predict(fitCV$glmnet.fit, newx = newX, s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se), type = 'response')
	fit = list(object = fitCV, useMin = useMin)
	class(fit) = 'SL.glmnet'
	out = list(pred = pred, fit = fit)
	return(out)
}

###########
## RIDGE ##
###########
SL.ridge.LT = function (Y, X, newX, family, obsWeights, id, alpha = 0, nfolds = .SL.glmnet.nfolds, nlambda = .SL.glmnet.nlambda, useMin = TRUE, ...) {
	# X must be a matrix, should we use model.matrix or as.matrix
	if(!is.matrix(X)) {
		X = model.matrix(~ -1 + ., X)
		newX = model.matrix(~ -1 + ., newX)
	}
	# now use CV to find lambda
	Y.matrix = cbind(1-Y,Y)
	fitCV <- cv.glmnet(x = X, y = Y.matrix, weights = obsWeights, lambda = NULL, type.measure = "deviance", nfolds = nfolds, family = family$family, alpha = alpha, nlambda = nlambda)
	pred <- predict(fitCV$glmnet.fit, newx = newX, s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se), type = "response")
	# two options for lambda, fitCV$lambda.min and fitCV$lambda.1se
	fit <- list(object = fitCV, useMin = useMin)
	class(fit) <- "SL.glmnet"
	out <- list(pred = pred, fit = fit)
	return(out)
}

#########
## KNN ##
#########
SL.knn.LT = function (Y, X, newX, family, k = .SL.knn.k, ...) {
	if (family$family == "binomial" & length(unique(Y))>2) {
		fit.knn <- knn.reg(train = X, test = newX, k = k, y = Y)
		pred = fit.knn$pred
		fit = list(k = k)
	}
	else if (family$family == "binomial") {
		fit.knn <- knn(train = X, test = newX, k = k, cl = Y, prob = TRUE)
		pred <- (as.numeric(fit.knn) - 1) * attr(fit.knn, "prob") + 
				(1 - (as.numeric(fit.knn) - 1)) * (1 - attr(fit.knn, "prob"))
		fit = list(k=k)
	}
	out <- list(pred = pred, fit = fit)
	class(out$fit) <- c("SL.knn")
	return(out)
}
SL.knn.5 = function(..., k = 5) SL.knn.LT(..., k = k)
SL.knn.15 = function(..., k = 15) SL.knn.LT(..., k = k)
SL.knn.20 = function(..., k = 20) SL.knn.LT(..., k = k)
SL.knn.25 = function(..., k = 25) SL.knn.LT(..., k = k)

##########
## MARS ##
##########
SL.polymars.LT.1 = function(Y, X, newX, family, obsWeights, cv=.SL.polymars.cv, ...){
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		fit.mars = polymars(Y, X, weights = obsWeights)
		pred = bound(predict(fit.mars, x = newX), c(0,1))
		fit = list(object = fit.mars)
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.mars = polyclass(newY, X, cv = cv, weight = obsWeights, seed=1000)
		pred = ppolyclass(cov = newX, fit = fit.mars)[, 2]
		fit = list(fit = fit.mars)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.polymars")
	return(out)
}
SL.polymars.LT.2 = function(Y, X, newX, family, obsWeights, cv=.SL.polymars.cv, ...){
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		newY = qlogis(bound(Y, .Qbounds))
		fit.mars = polymars(newY, X, weights = obsWeights)
		pred = plogis(predict(fit.mars, x = newX))
		fit = list(object = fit.mars)
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.mars = polyclass(newY, X, cv = cv, weight = obsWeights, seed=1000)
		pred = ppolyclass(cov = newX, fit = fit.mars)[, 2]
		fit = list(fit = fit.mars)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.polymars")
	return(out)
}
SL.polymars.LT.3 = function(Y, X, newX, family, obsWeights, cv=.SL.polymars.cv, ...){
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		binary = Y %in% c(0,1)
		# Binary model
		newY.b = Y[binary]; X.b = X[binary,]; newX.b = newX[binary,]; obsWeights.b = obsWeights[binary]
		fit.b = polyclass(newY.b, X.b, cv = cv, weight = obsWeights.b, seed=1000)
		pred.b = ppolyclass(cov = newX.b, fit = fit.b)[, 2]
		# Continuous model
		newY.c = qlogis(Y[!binary]); X.c = X[!binary,]; newX.c = newX[!binary,]; obsWeights.c = obsWeights[!binary]
		fit.c = polymars(newY.c, X.c, weights = obsWeights.c)
		pred.c = plogis(predict(fit.c, x = newX.c))
		# Output
		fit = list(object.b = fit.b, object.c = fit.c)
		pred = rep(NA, nrow(newX))
		pred[binary] = pred.b; pred[!binary] = pred.c
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.mars = polyclass(newY, X, cv = cv, weight = obsWeights, seed=1000)
		pred = ppolyclass(cov = newX, fit = fit.mars)[, 2]
		fit = list(fit = fit.mars)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.polymars")
	return(out)
}

#########
## SVM ##
#########
SL.svm.LT.1 = function (Y, X, newX, family, type.reg = "eps-regression", type.class = "C-classification", nu = 0.5, gamma = .SL.svm.gamma, ...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		fit.svm = svm(y = Y, x = X, nu = nu, type = type.reg, fitted = FALSE, gamma=gamma)
		pred = bound(predict(fit.svm, newdata = newX), c(0,1))
		fit = list(object = fit.svm)
	}
	else if (family$family == "binomial") {
		newY = as.factor(Y)
		fit.svm = svm(y = newY, x = X, nu = nu, type = type.class, fitted = FALSE, gamma=gamma, probability = TRUE)
		pred = attr(predict(fit.svm, newdata = newX, probability = TRUE), "prob")[, "1"]
		fit = list(object = fit.svm)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.svm")
	return(out)
}
SL.svm.LT.2 = function (Y, X, newX, family, type.reg = "eps-regression", type.class = "C-classification", nu = 0.5, gamma = .SL.svm.gamma, ...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		newY = qlogis(bound(Y, .Qbounds))
		fit.svm = svm(y = newY, x = X, nu = nu, type = type.reg, fitted = FALSE, gamma=gamma)
		pred = plogis(predict(fit.svm, newdata = newX))
		fit = list(object = fit.svm)
	}
	else if (family$family == "binomial") {
		newY = as.factor(Y)
		fit.svm = svm(y = newY, x = X, nu = nu, type = type.class, fitted = FALSE, gamma=gamma, probability = TRUE)
		pred = attr(predict(fit.svm, newdata = newX, probability = TRUE), "prob")[, "1"]
		fit = list(object = fit.svm)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.svm")
	return(out)
}
SL.svm.LT.3 = function (Y, X, newX, family, type.reg = "eps-regression", type.class = "C-classification", nu = 0.5, gamma = .SL.svm.gamma...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		binary = Y %in% c(0,1)
		# Binary model
		newY.b = as.factor(Y[binary]); X.b = X[binary,]; newX.b = newX[binary,]; obsWeights.b = obsWeights[binary]
		fit.b = svm(y = newY.b, x = X.b, nu = nu, type = type.class, fitted = FALSE, gamma=gamma, probability = TRUE)
		pred.b = attr(predict(fit.b, newdata = newX.b, probability = TRUE), "prob")[, "1"]
		# Continuous model
		newY.c = qlogis(Y[!binary]); X.c = X[!binary,]; newX.c = newX[!binary,]; obsWeights.c = obsWeights[!binary]
		fit.c = svm(y = newY.c, x = X.c, nu = nu, type = type.reg, fitted = FALSE, gamma=gamma)
		pred = plogis(predict(fit.c, newdata = newX.c))
		# Output
		fit = list(object.b = fit.b, object.c = fit.c)
		pred = rep(NA, nrow(newX))
		pred[binary] = pred.b; pred[!binary] = pred.c
	}
	else if (family$family == "binomial") {
		newY = as.factor(Y)
		fit.svm = svm(y = newY, x = X, nu = nu, type = type.class, fitted = FALSE, gamma=gamma, probability = TRUE)
		pred = attr(predict(fit.svm, newdata = newX, probability = TRUE), "prob")[, "1"]
		fit = list(object = fit.svm)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.svm")
	return(out)
}

##########
## NNET ##
##########
SL.nnet.LT.1 = function (Y, X, newX, family, obsWeights, size = .SL.nnet.size, maxit = .SL.nnet.maxit, ...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		fit.nnet = nnet(x = X, y = Y, size = size, trace = FALSE, maxit = maxit, linout = TRUE, weights = obsWeights)
		pred = bound(predict(fit.nnet, newdata = newX, type = "raw"), c(0,1))
		fit = list(object = fit.nnet)
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.nnet = nnet(x = X, y = newY, size = size, trace = FALSE, maxit = maxit, linout = FALSE, weights = obsWeights)
		pred = predict(fit.nnet, newdata = newX, type = "raw")
		fit = list(object = fit.nnet)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.nnet")
	return(out)
}
SL.nnet.LT.2 = function (Y, X, newX, family, obsWeights, size = .SL.nnet.size, maxit = .SL.nnet.maxit, ...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		newY = qlogis(bound(Y, .Qbounds))
		fit.nnet = nnet(x = X, y = newY, size = size, trace = FALSE, maxit = maxit, linout = TRUE, weights = obsWeights)
		pred = plogis(predict(fit.nnet, newdata = newX, type = "raw"))
		fit = list(object = fit.nnet)
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.nnet = nnet(x = X, y = newY, size = size, trace = FALSE, maxit = maxit, linout = FALSE, weights = obsWeights)
		pred = predict(fit.nnet, newdata = newX, type = "raw")
		fit = list(object = fit.nnet)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.nnet")
	return(out)
}
SL.nnet.LT.3 = function (Y, X, newX, family, obsWeights, size = .SL.nnet.size, maxit = .SL.nnet.maxit, ...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		binary = Y %in% c(0,1)
		# Binary model
		newY.b = Y[binary]; X.b = X[binary,]; newX.b = newX[binary,]; obsWeights.b = obsWeights[binary]
		fit.b = nnet(x = X.b, y = newY.b, size = size, trace = FALSE, maxit = maxit, linout = FALSE, weights = obsWeights.b)
		pred.b = predict(fit.b, newdata = newX.b, type = "raw")
		# Continuous model
		newY.c = qlogis(Y[!binary]); X.c = X[!binary,]; newX.c = newX[!binary,]; obsWeights.c = obsWeights[!binary]
		fit.c = nnet(x = X.c, y = newY.c, size = size, trace = FALSE, maxit = maxit, linout = TRUE, weights = obsWeights.c)
		pred.c = plogis(predict(fit.c, newdata = newX.c, type = "raw"))
		# Output
		fit = list(object.b = fit.b, object.c = fit.c)
		pred = rep(NA, nrow(newX))
		pred[binary] = pred.b; pred[!binary] = pred.c
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.nnet = nnet(x = X, y = newY, size = size, trace = FALSE, maxit = maxit, linout = FALSE, weights = obsWeights)
		pred = predict(fit.nnet, newdata = newX, type = "raw")
		fit = list(object = fit.nnet)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.nnet")
	return(out)
}
SL.nnet.3 = function(..., size = 3) SL.nnet.LT.1(..., size = size, )
SL.nnet.4 = function(..., size = 4) SL.nnet.LT.1(..., size = size, )
SL.nnet.5 = function(..., size = 5) SL.nnet.LT.1(..., size = size, )

#########
## GBM ## 
#########
SL.gbm.LT.1 = function (Y, X, newX, family, obsWeights, gbm.trees = .SL.gbm.trees, interaction.depth = .SL.gbm.depth, cv.folds = .SL.gbm.nfolds, ...) {
	gbm.model = as.formula(paste("Y ~", paste(colnames(X), collapse = "+")))
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		fit.gbm = gbm(formula = gbm.model, data = X, distribution = "gaussian", 
				n.trees = gbm.trees, interaction.depth = interaction.depth, 
				cv.folds = cv.folds, keep.data = TRUE, weights = obsWeights, 
				verbose = FALSE)
		best.iter = gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
		pred = bound(predict(fit.gbm, newdata = newX, best.iter, type = "response"), c(0,1))
		fit = list(object = fit.gbm, n.trees = best.iter)
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.gbm = gbm(formula = gbm.model, data = X, distribution = "bernoulli", 
				n.trees = gbm.trees, interaction.depth = interaction.depth, 
				cv.folds = cv.folds, keep.data = TRUE, verbose = FALSE, 
				weights = obsWeights)
		best.iter = gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
		pred = predict(fit.gbm, newdata = newX, best.iter, type = "response")
		fit = list(object = fit.gbm, n.trees = best.iter)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.gbm")
	return(out)
}
SL.gbm.LT.2 = function (Y, X, newX, family, obsWeights, gbm.trees = .SL.gbm.trees, interaction.depth = .SL.gbm.depth, cv.folds = .SL.gbm.nfolds, ...) {
	gbm.model = as.formula(paste("newY ~", paste(colnames(X), collapse = "+")))
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		newY = qlogis(bound(Y, .Qbounds))
		fit.gbm = gbm(formula = gbm.model, data = X, distribution = "gaussian", 
				n.trees = gbm.trees, interaction.depth = interaction.depth, 
				cv.folds = cv.folds, keep.data = TRUE, weights = obsWeights, 
				verbose = FALSE)
		best.iter = gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
		pred = plogis(predict(fit.gbm, newdata = newX, best.iter, type = "response"))
		fit = list(object = fit.gbm, n.trees = best.iter)
	}
	else if (family$family == "binomial") {
		newY = Y
		fit.gbm = gbm(formula = gbm.model, data = X, distribution = "bernoulli", 
				n.trees = gbm.trees, interaction.depth = interaction.depth, 
				cv.folds = cv.folds, keep.data = TRUE, verbose = FALSE, 
				weights = obsWeights)
		best.iter = gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
		pred = predict(fit.gbm, newdata = newX, best.iter, type = "response")
		fit = list(object = fit.gbm, n.trees = best.iter)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.gbm")
	return(out)
}
SL.gbm.LT.3 = function (Y, X, newX, family, obsWeights, gbm.trees = .SL.gbm.trees, interaction.depth = .SL.gbm.depth, cv.folds = .SL.gbm.nfolds, ...) {
	if (family$family == "binomial" & !all(Y %in% c(0,1))) {
		binary = Y %in% c(0,1)
		# Binary model
		gbm.model = as.formula(paste("newY.b ~", paste(colnames(X), collapse = "+")))
		newY.b = Y[binary]; X.b = X[binary,]; newX.b = newX[binary,]; obsWeights.b = obsWeights[binary]
		fit.b = gbm(formula = gbm.model, data = X.b, distribution = "bernoulli", 
				n.trees = gbm.trees, interaction.depth = interaction.depth, 
				cv.folds = cv.folds, keep.data = TRUE, weights = obsWeights.b, verbose = FALSE)
		best.iter.b = gbm.perf(fit.b, method = "cv", plot.it = FALSE)
		pred.b = predict(fit.b, newdata = newX.b, best.iter.b, type = "response")
		# Continuous model
		gbm.model = as.formula(paste("newY.c ~", paste(colnames(X), collapse = "+")))
		newY.c = qlogis(Y[!binary]); X.c = X[!binary,]; newX.c = newX[!binary,]; obsWeights.c = obsWeights[!binary]
		fit.c = gbm(formula = gbm.model, data = X.c, distribution = "gaussian", 
				n.trees = gbm.trees, interaction.depth = interaction.depth, 
				cv.folds = cv.folds, keep.data = TRUE, weights = obsWeights.c, verbose = FALSE)		
		best.iter.c = gbm.perf(fit.c, method = "cv", plot.it = FALSE)
		pred.c = plogis(predict(fit.c, newdata = newX.c, best.iter.c, type = "response"))
		# Output
		fit = list(object.b = fit.b, object.c = fit.c, n.trees.b = best.iter.b, n.trees.c = best.iter.c)
		pred = rep(NA, nrow(newX))
		pred[binary] = pred.b; pred[!binary] = pred.c
	}
	else if (family$family == "binomial") {
		newY = Y
		gbm.model = as.formula(paste("newY ~", paste(colnames(X), collapse = "+")))
		fit.gbm = gbm(formula = gbm.model, data = X, distribution = "bernoulli", 
				n.trees = gbm.trees, interaction.depth = interaction.depth, 
				cv.folds = cv.folds, keep.data = TRUE, verbose = FALSE, 
				weights = obsWeights)
		best.iter = gbm.perf(fit.gbm, method = "cv", plot.it = FALSE)
		pred = predict(fit.gbm, newdata = newX, best.iter, type = "response")
		fit = list(object = fit.gbm, n.trees = best.iter)
	}
	out = list(pred = pred, fit = fit)
	class(out$fit) = c("SL.gbm")
	return(out)
}
SL.gbm.2 = function(..., interaction.depth = 2) SL.gbm.LT.1(..., interaction.depth = interaction.depth)

#########
## GAM ##
#########
SL.gam.3 = function(..., deg.gam = 3) SL.gam (..., deg.gam = deg.gam)
SL.gam.4 = function(..., deg.gam = 4) SL.gam (..., deg.gam = deg.gam)
SL.gam.5 = function(..., deg.gam = 5) SL.gam (..., deg.gam = deg.gam)


#######################
## PARAMETRIC MODELS ##
#######################
create.glm = function(vars){
	formula = paste0("SL.glm.g_A1W.b = function(Y, X, newX, family, obsWeights, ...) {
					fit.glm.Tx = glm(Y ~ ", paste(vars, collapse = " + "), ", family=binomial, data=X, weights = obsWeights)
					pred = predict(fit.glm.Tx, newdata = newX, type='response')
					fit = list(object = fit.glm.Tx)
					class(fit) = 'SL.glm'
					out = list(pred = pred, fit = fit)
					return(out)
					}")
	model = eval(parse(text=formula))
	environment(model) = asNamespace("SuperLearner")
	return(model)
}
#Variable sets using in GLM
vars1 = words(ageb, cd4v_locf.cat.ge500.lt200, cd4v_locf.cat.ge500.200to350, cd4v_locf.cat.ge500.350to500, cd4_zenith.p100, cd4_nadir.p100, clintype.ruralhealthcenter..sub.districthospital, clintype.ruralhealthcenter.refhospital, daysfromenroll.p180, stage34_locf, urbanelig, male, onARV, tbtx_locf.l, ns(rank,4))
vars2 = words(ageb, cd4v_locf.cat.ge500.lt200, cd4v_locf.cat.ge500.200to350, cd4v_locf.cat.ge500.350to500, cd4_zenith.p100, cd4_nadir.p100, clintype.ruralhealthcenter..sub.districthospital, clintype.ruralhealthcenter.refhospital, daysfromenroll.p180, stage34_locf, male, tbtx_locf.l, ns(rank,4))
vars3 = words(ageb, cd4v_locf.cat.ge500.lt200, cd4v_locf.cat.ge500.200to350, cd4v_locf.cat.ge500.350to500, cd4_nadir.p100, stage34_locf, male, ns(rank,4))
#SL glm models
SL.glm.g_AW.1 = create.glm(vars1)
SL.glm.g_AW.2 = create.glm(vars2)
SL.glm.g_AW.3 = create.glm(vars3)
SL.glm.g_EW.1 = create.glm(intersect(vars1, c(base.vars, "ns(rank, 4)")))
SL.glm.g_EW.2 = create.glm(intersect(vars2, c(base.vars, "ns(rank, 4)")))
SL.glm.g_TW.1 = create.glm(c("avail", "enroll", vars1))
SL.glm.g_TW.2 = create.glm(c("avail", "enroll", vars2))
SL.glm.g_TW.3 = create.glm(c("avail", "enroll", vars3))


###############
## TIME ONLY ##
###############
SL.time = function (Y, X, newX, family, obsWeights, ...) {
	prob.t = tapply(Y, X$rank, mean)
	prob.t[max(X$rank):max(newX$rank)+2] = 0
	names(prob.t)[max(X$rank):max(newX$rank)+2] = max(X$rank):max(newX$rank)+1
	pred = prob.t[as.character(newX$rank)]
	out = list(pred = pred, fit = prob.t)
	class(out$fit) = c("SL.time")
	return(out)
}

