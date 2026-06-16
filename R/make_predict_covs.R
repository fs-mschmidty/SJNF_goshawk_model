make_predict_covs <- function(all_covs) {
  all_covs[["aspect"]] <- round(all_covs[["aspect"]])
  all_covs[["aspect"]] <- as.int(all_covs[["aspect"]])
  all_covs[["CANOPYPCT"]] <- round(all_covs[["CANOPYPCT"]])
  all_covs[["CANOPYPCT"]] <- as.int(all_covs[["CANOPYPCT"]])
  all_covs[["b200"]] <- round(all_covs[["b200"]])
  all_covs[["b200"]] <- as.int(all_covs[["b200"]])
  all_covs[["b30"]] <- round(all_covs[["b30"]])
  all_covs[["b30"]] <- as.int(all_covs[["b30"]])
  all_covs[["slope"]] <- round(all_covs[["slope"]])
  all_covs[["slope"]] <- as.int(all_covs[["slope"]])
  all_covs
}
