library(data.table)

out = fread('~/data/SkinHack/~out.csv')
features = fread('~/Downloads/foo.csv')

test_features = fread('~/data/SkinHack/test_features.csv')
test_out = fread('~/data/SkinHack/area_test.csv', col.names = c('V1', 'area'))

test_out$index = 1:nrow(test_out)
test_features$index = 1:nrow(test_features)
test_out[, V1 := NULL]
setkey(test_features, index)
setkey(test_out, index)

test_join = test_features[test_out]
test_join[, area := ifelse(is.na(area), 0, area)]
test_join[, area := as.numeric(as.factor(area))]
test_join[, `:=`(index = NULL)]

library(caret)
library(caretEnsemble)

out$index = 1:nrow(out)
features$index = 1:nrow(features)
out[, V1 := NULL]
setkey(features, index)
setkey(out, index)

join = features[out]
join[, gender := as.numeric(as.factor(gender))]

join[, area := ifelse(is.na(area), 0, area)]
join[, area := as.numeric(as.factor(area))]
join[, `:=`(index = NULL)]

set.seed(1337)

folds = 2
repeats = 1
resamp_index = createMultiFolds(join$Y, k = folds, times = repeats)
trControlEnsem = trainControl( method = 'repeatedcv'
                               , number = folds
                               , repeats = repeats
                               , index = resamp_index
                              # , savePredictions = TRUE
                              # , classProbs = TRUE
                              # , summaryFunction = twoClassSummary
                               , search = 'random'
)

join[, gender := NULL]

models = caretList( Y ~ .
                    , data = join
                    , trControl = trControlEnsem
                    #, metric = 'ROC'
                    , tuneList = list(
                      xgbTree = caretModelSpec( method = 'xgbTree'
                                                , tuneGrid = expand.grid( nrounds = 350
                                                                        , eta = 0.2
                                                                        , max_depth = 7
                                                                        , gamma = 6
                                                                        , colsample_bytree = 0.4
                                                                        , min_child_weight = 10
                                                                        , subsample = 1)
                                                #, objective = 'binary:logistic'
                                                , verbose = 1
                      )
                      # elnet = caretModelSpec(
                      #   method = 'enet'
                      # )
                      # glm = caretModelSpec( method = 'glmboost'
                      #                    #, tuneGrid = expand.grid()
                      # ),
                      # # svm = caretModelSpec( method = 'svmLinear2'),
                      # rf = caretModelSpec( method = 'rf'
                      #                    , tuneGrid = expand.grid( mtry = 4 )
                      # )
                      # nb = caretModelSpec( method = 'nb'
                      #                     , tuneGrid = expand.grid(fL = 0, usekernel = T, adjust = 1)
                      # )
                    )
)

test_predict = predict(models, test_join)

test_filtered = fread('cat ~/data/SkinHack/test_filtered.csv | cut -d, -f1', colClasses = 'character')

write.csv(data.frame(file = test_filtered$file, age = test_predict), file = '~/data/SkinHack/12_Пронькин_Возраст.csv', quote = F, row.names = F)
