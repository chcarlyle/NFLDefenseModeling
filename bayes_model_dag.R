library(ggdag)

dag <- dagify(
  Outcome ~ Weights_DL + Weights_LB + Weights_DB + DL_Effect + LB_Effect + DB_Effect + OffenseEffect + DownDistance,
  Weights_DL ~ PlayType,
  Weights_LB ~ PlayType,
  Weights_DB ~ PlayType,
  DownDistance ~ PlayType, # optional
  coords = list(
    x = c(PlayType = 0, Weights_DL = 1, Weights_LB = 1, Weights_DB = 1,
          DL_Effect = 2, LB_Effect = 2, DB_Effect = 2, 
          OffenseEffect = 2, DownDistance = 0, Outcome = 3),
    y = c(PlayType = 2, Weights_DL = 3, Weights_LB = 2, Weights_DB = 1,
          DL_Effect = 3, LB_Effect = 2, DB_Effect = 1, 
          OffenseEffect = 0, DownDistance = 0, Outcome = 2)
  )
)

ggdag(dag, text = TRUE, use_labels = "name") +
  theme_dag_blank()
