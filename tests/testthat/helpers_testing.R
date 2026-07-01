#-------------------------------------------------------------------------------
# Helper functions to create data for function tests
#-------------------------------------------------------------------------------

# Create input data for group_impute tests
.impute_data_creation <- function(groupings){

  if(groupings == 1) {
    df <- data.frame(
      loc = c(rep("A",50),rep("B",50)),
      wt = c(runif(50,1,10),runif(50,20,30))
    )
    df$wt_clean <- df$wt
    df$wt[c(1,25,51,90,100)] <- NA
  }

  if(groupings == 2) {
    df <- df <- data.frame(
      loc = c(rep("A",50),rep("B",50)),
      time = c(rep(1,25),rep(2,25),rep(1,50)),
      wt = c(runif(25,1,10),runif(25,20,30),runif(50,40,50))
    )
    df$wt_clean <- df$wt
    df$wt[c(1,25,50,90,100)] <- NA

  }

  if(groupings == 3) {
    df <- data.frame(
      loc = c(rep("A",50),rep("B",50)),
      time = c(rep(1,25),rep(2,25),rep(1,50)),
      sp = c(rep("sp1",75),rep("sp2",25)),
      wt = c(runif(25,1,10),runif(25,20,30),runif(25,40,50),runif(25,60,70))
    )
    df$wt_clean <- df$wt
    df$wt[c(1,10,25,50,60,90,100)] <- NA
  }
  df
}
