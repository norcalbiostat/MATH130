pacman::p_load(dplyr,
               knitr,
               tools)

# reads the intermediate tex file
#input = readLines("accessibility_example.tex")
input = readLines("syllabus.tex")

# adds selected tags
output = 
# The fixed commands turns off regular expression processing. (To learn more about regular expressions visit: http://www.regexone.com .)
    gsub(
  "\\includegraphics{accessibility_example_files/figure-latex/diamonds-1.pdf}",
  "\\AccTool{This is a beutiful plot of the prices of 5,000 round cut diamonds}{\\includegraphics{accessibility_example_files/figure-latex/diamonds-1.pdf}}",
  input,
  fixed = T
) %>%
  
# Notice the quadruple backslashes in some instances where regular expressions are in use.
  gsub("\\\\begin\\{longtable\\}",
       "\\\\AccTool{This is a table of distances between cities}\\{\\}\\\\begin{longtable}",
       .)

# You can mannually add accessibilty tags without tooltips using \BeginAccSupp and \EndAccSupp
#writeLines(output, "processed_accessibility_example.tex")
#texi2pdf("processed_accessibility_example.tex", clean = T)

writeLines(output, "math130_sp19_syllabus.tex")
texi2pdf("math130_sp19_syllabus.tex", clean = T)


