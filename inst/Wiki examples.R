png("~/Dropbox/github/lian0090.github.io/figures/GibbsFW/lm1.png")
plot(lm1)
dev.off()

png("~/Dropbox/github/lian0090.github.io/figures/GibbsFW/lm1_fewline.png")
plot(lm1,plotVAR=c("V1","V4","V5","V9"))
dev.off()

png("~/Dropbox/github/lian0090.github.io/figures/GibbsFW/samps.png")
plot(samps)
dev.off()