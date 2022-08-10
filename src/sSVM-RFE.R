
library(e1071)
svmrfeFeatureRanking = function(x, y, cValue = 50, Scaled = FALSE){
  n = ncol(x)
  
  survivingFeaturesIndexes = seq(1:n)
  featureRankedList = vector(length=n)
  rankedFeatureIndex = n
  
  while(length(survivingFeaturesIndexes)>0){
    svmModel <- svm(x[, survivingFeaturesIndexes],
                    y,
                    cost = cValue,
                    cachesize=500,
                    scale=Scaled,
                    type="C-classification",
                    kernel="linear")
    w <- t(svmModel$coefs) %*% svmModel$SV
    rankingCriteria <- w * w
    ranking <- sort(rankingCriteria, index.return = TRUE)$ix
    featureRankedList[rankedFeatureIndex] <- survivingFeaturesIndexes[ranking[1]]
    rankedFeatureIndex <- rankedFeatureIndex - 1
    (survivingFeaturesIndexes <- survivingFeaturesIndexes[-ranking[1]])
  }
  return (featureRankedList)
}
