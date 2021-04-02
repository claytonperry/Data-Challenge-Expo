#COVID related unemployment rates
#estimates of no work by state per week
#HH Pulse Survey Data and Rep Weights

tapply(puf_df.$PWEIGHT,list(puf_df.$RSNNOWRK,puf_df.$EST_ST),sum)

x<-sum(puf_df$PWEIGHT)
puf_df$newvar <- sum(puf_df$PWEIGHT)

