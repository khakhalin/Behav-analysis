#
# Scripts used to analyze teaching data, and make figures for the teaching paper
#

# --- Volume and startles
# For now, manually reading from the summary paper (columns "id-volume-average")
d <- read.table("clipboard",header=TRUE,sep="\t")
ggplot(data=d,aes(volume, average,color=id)) + theme_bw() +
  geom_point(alpha=0.5,position=position_jitter(w=0.05,h=0.05)) +
  geom_smooth(method="glm", method.args=list(family="binomial"),se=F) + 
  guides(color=F)
