require(xgboost)
require(methods)

train = read.csv('train.csv',header=TRUE,stringsAsFactors = F)
test = read.csv('test.csv',header=TRUE,stringsAsFactors = F)
train = train[,-1]
test = test[,-1]

y = train[,ncol(train)]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(train[,-ncol(train)],test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

for (s in 1:5)
{
	# Set necessary parameter
	minsplitLoss=1.5
	param <- list("objective" = "multi:softprob",
				  "eval_metric" = "mlogloss",
				  "num_class" = 9,
				  "eta"=0.2,
				  "max.depth"=10,
				  "nthread" = 3,
				  "min_child_weight"=3,
				  "colsample_bytree"=0.8,
				  "min_split_loss"= minsplitLoss)

	# Run Cross Validation
	minsplitLoss=minsplitLoss+0.0
	cv.nround = 100
	#bst.cv = xgb.cv(param=param, data = x[trind,], label = y, 
					#nfold = 5, nrounds=cv.nround)

	# Train the model
	nround = 350
	set.seed(s)
	k<-sample(trind,0.9*length(trind),replace=FALSE)
	bst = xgboost(param=param, data = x[k,], label = y[k], nrounds=nround)

	# Make prediction
	pred = predict(bst,x[teind,])
	pred = matrix(pred,9,length(pred)/9)
	pred = t(pred)

	# Output submission
	pred = format(pred, digits=2,scientific=F) # shrink the size of submission
	pred = data.frame(1:nrow(pred),pred)
	names(pred) = c('id', paste0('Class_',1:9))
	filename<-paste('ensemble3/','en_',s,'.csv',sep='')
	write.csv(pred,file=filename, quote=FALSE,row.names=FALSE)
}
