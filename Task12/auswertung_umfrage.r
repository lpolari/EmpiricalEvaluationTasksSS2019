
## Load data from csv file

data <- read.csv('empirie.csv', colClasses=rep('factor', 35))


# Calculate Correlation between categorical unordered survey answers
## Example  most used IDE vs best IDE

# extract corresponding columns from the dataframe
df = data[,c("Welche.IDE.findest.du.am.besten.", "Welche.IDE.nutzt.du.am.meisten.")]

# only look at the correlation between the most often mentioned IDE's
ides = c("Eclipse", "IntelliJ", "Xcode", "Microsoft Visual Studio", "Netbeans")
df = df[df$"Welche.IDE.findest.du.am.besten" %in% ides, ]
df = df[df$"Welche.IDE.nutzt.du.am.meisten" %in% ides, ]

# create a table of all pairs of mentioned IDE's
IDE_mostXbest = table(droplevels(df))

# Calculate the CHI correlation
chi2 = chisq.test(IDE_mostXbest, correct=F, simulate.p.value = TRUE)

# Print CHI value // In this case p = 1.460416e-80
# This indicates a high correlation between the answers of the two questions
# The Nullhyptothesis should be rejected
cat("Chi-Quadrat Test für Korrelation zwichen den meist und am liebsten verwendeten IDE's : \n")
cat("p = ", chi2$p.value, "\n\n")




# Calculate Correlation between ordered survey answers
# use spearman correlation (maybe bad results due to ties)

## Different levels of satisfaction in the answers 

atm = data$"Wie.bewertest.du.deine.Erfahrung.mit.diesen.Entwicklungsumgebungen...Atom."
atm_f = factor(atm, levels=c("Sehr gut", "Gut", "Mittelmäßig", "Schlecht", "Sehr schlecht"), ordered = TRUE)
atm_int = as.integer(atm_f)

ecl = data$"Wie.bewertest.du.deine.Erfahrung.mit.diesen.Entwicklungsumgebungen...Eclipse."
ecl_f = factor(ecl, levels=c("Sehr gut", "Gut", "Mittelmäßig", "Schlecht", "Sehr schlecht"), ordered = TRUE)
ecl_int = as.integer(ecl_f)

cat("Spearman Korrelation der Nutzerzufriedenheit zwischen Atom und Eclipse : \n")
cat("Rho = ", cor(rank(ecl_int), rank(atm_int), method = 'pearson'), "\n")