#' @importFrom MCDM MMOORA TOPSISVector TOPSISLinear VIKOR
#' @importFrom RankAggreg BruteAggreg RankAggreg
#'
MetaRanking_custom <- function(decision, weights, cb, lambda, v, AB, CD) {
  MMoora <- MCDM::MMOORA(decision, weights, cb)
  TopsisV <- MCDM::TOPSISVector(decision, weights, cb)
  TopsisL <- MCDM::TOPSISLinear(decision, weights, cb)
  Vikor <- VIKOR_custom(decision, weights, cb, v)
  Waspas <- MCDM::WASPAS(decision, weights, cb, lambda)
  if (Vikor[1, 5] == "-") {
    MetaR <- MMoora[, 8] + TopsisV[, 3] + TopsisL[, 3] + Waspas[, 5]
  } else {
    MetaR <- MMoora[, 8] + TopsisV[, 3] + TopsisL[, 3 ] + Vikor[, 5] + Waspas[, 5]
  }
  if (Vikor[1, 5] == "-") {
    ra <- rbind(MMoora[, 8], TopsisV[, 3], TopsisL[, 3], Waspas[, 5])
  } else {
    ra <- rbind(MMoora[, 8], TopsisV[, 3], TopsisL[, 3 ], Vikor[, 5], Waspas[, 5])
  }
  if (nrow(decision) <= 10) {
    RA <- RankAggreg::BruteAggreg(ra, nrow(decision), distance = "Spearman")
  } else {
    RA <- RankAggreg::RankAggreg(ra, nrow(decision),
      method = "GA",
      distance = "Spearman", verbose = FALSE
    )
  }

  res <-
    data.frame(
      Alternatives = 1:nrow(decision),
      MMOORA = MMoora[, 8],
      TOPSISVector = TopsisV[, 3],
      TOPSISLinear = TopsisL[, 3],
      VIKOR = Vikor[, 5],
      WASPAS = Waspas[, 5],
      MetaRanking_Sum = rank(MetaR, ties.method = "first"),
      MetaRanking_Aggreg = RA$top.list
    )

  return(res)
}
