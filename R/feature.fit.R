feature.fit <- function(target, feature){
  #' Analyze if the missingess of `target` can be predicted from `feature`.
  #' `Feature` can be a numeric or categorical vector, and the model is LDA with
  #' a binary decision tree as back up (rpart).
  #'
  #' @param target A vector with some values missing
  #' @param feature A vector we wish to predict missingess from.
  #'
  #'
  #' @export

  tts <- prepare.data(feature, target)

  training <- data.frame(x=tts$xTrain, y=tts$yTrain)
  test <- data.frame(x=tts$xTest, y=tts$yTest)

  naming.f <-  function(var, lvl, ordinal = FALSE, sep = "_"){
    lvl
  }

  rec <- recipe(y ~ x, data=training) %>%
    step_dummy(x, naming = naming.f, one_hot = T) %>%
    prep()

  embed <- juice(rec)
  embedTest <- bake(rec, new_data=test)


  model <- tryCatch(
    MASS::lda(y ~ -1 + ., data=embed),
    error = function(e) {
      embed$y <- as.character(embed$y)
      rpart::rpart(y ~ -1 + ., data=embed)
    }
  )

  phat <- tryCatch(
    predict(model, embedTest, type="response")$posterior ,
    error = function(e) {
      predict(model, embedTest)
    }
  )
  h <- as.data.frame(phat)
  phat <- h[["TRUE"]]

  result <- tibble(obs = embedTest$y, pred=phat) %>% roc_("obs", "pred")
  list(AUC = result$auc,
       model = model,
       naming_vector = tts$naming_vector)
}
