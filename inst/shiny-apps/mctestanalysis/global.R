library(shiny)

ak_file <- system.file("extdata", 'answer_key_example.csv', package = 'MCTestAnalysis')
if (ak_file != '') {
  answer_key_example <- read.csv(ak_file, stringsAsFactors = FALSE)
}

test_file <- system.file("extdata", "test_example.csv", package = "MCTestAnalysis")
if (test_file != '') {
  test_example <- read.csv(test_file, stringsAsFactors = FALSE)
}

REFERENCES_THEORY <- HTML(
  '<div id="ref-Baker2001">
  <p>Baker, F. B. (2001). <em>The basics of item response theory</em> (2nd ed.). ERIC Clearinghouse on Assessment; Evaluation. Retrieved from <a href="http://echo.edres.org:8080/irt/baker/" class="uri">http://echo.edres.org:8080/irt/baker/</a></p>
  </div>
  <div id="ref-Bond2007">
  <p>Bond, T. G., &amp; Fox, C. M. (2007). <em>Applying the rasch model: Fundamental measurement in the human sciences</em> (1st ed.). Mahwah, N.J.: Lawrence Erlbaum Associates Publishers.</p>
  </div>
  <div id="ref-DiBello2015">
  <p>DiBello, L. V., Henson, R. A., &amp; Stout, W. F. (2015). A family of generalized diagnostic classification models for multiple choice option-based scoring. <em>Applied Psychological Measurement</em>, <em>39</em>(1), 62–79. <a href="https://doi.org/10.1177/0146621614561315" class="uri">https://doi.org/10.1177/0146621614561315</a></p>
  </div>
  <div id="ref-Haertel2004">
  <p>Haertel, E. H., &amp; Lorie, W. A. (2004). Validating standards-based test score interpretations. <em>Measurement: Interdisciplinary Research and Perspectives</em>, <em>2</em>(2), 61–103. <a href="https://doi.org/10.1207/s15366359mea0202_1" class="uri">https://doi.org/10.1207/s15366359mea0202_1</a></p>
  </div>
  <div id="ref-Jorion2015">
  <p>Jorion, N., Gane, B. D., James, K., Schroeder, L., DiBello, L. V., &amp; Pellegrino, J. W. (2015). An analytic framework for evaluating the validity of concept inventory claims. <em>Journal of Engineering Education</em>, <em>104</em>(4), 454–496. <a href="https://doi.org/10.1002/jee.20104" class="uri">https://doi.org/10.1002/jee.20104</a></p>
  </div>
  <div id="ref-PersonalityProject">
  <p>Revelle, W. (2017). Northwestern University; <a href="http://personality-project.org/r/book/" class="uri">http://personality-project.org/r/book/</a>.</p>
  </div>
  <div id="ref-Sleeper2011">
  <p>Sleeper, R. (2011). Keep, toss or revise? Tips for post-exam item analysis. <a href="http://www.ttuhsc.edu/sop/administration/enhancement/documents/Sleeper_Handout.ppt" class="uri">http://www.ttuhsc.edu/sop/administration/enhancement/documents/Sleeper_Handout.ppt</a> (URL no longer valid).</p>
  </div>'
)

REFERENCES_PKGS <- HTML(
  '
<div id="ref-pkg:psych">
<p>Revelle, W. (2016). <em>Psych: Procedures for psychological, psychometric, and personality research</em>. Evanston, Illinois: Northwestern University; <a href="https://CRAN.R-project.org/package=psych" class="uri">https://CRAN.R-project.org/package=psych</a>. Retrieved from <a href="https://CRAN.R-project.org/package=psych" class="uri">https://CRAN.R-project.org/package=psych</a></p>
</div>
<div id="ref-pkg:ltm">
<p>Rizopoulos, D. (2006). Ltm: An r package for latent variable modelling and item response theory analyses. <em>Journal of Statistical Software</em>, <em>17</em>(5), 1–25. Retrieved from <a href="http://www.jstatsoft.org/v17/i05/" class="uri">http://www.jstatsoft.org/v17/i05/</a></p>
</div>
<div id="ref-pkg:psychometric">
<p>Fletcher, T. D. (2010). <em>Psychometric: Applied psychometric theory</em>. Retrieved from <a href="https://CRAN.R-project.org/package=psychometric" class="uri">https://CRAN.R-project.org/package=psychometric</a></p>
</div>
'
)

pkg_url_li <- function(pkg, url = NULL) {
  if (is.null(url)) {
    url <- packageDescription(pkg, field = 'URL')
    if (!is.na(url)) url <- strsplit(url, '\n|,')[[1]][1]
    else url <- citation(pkg)$url
  }
  tags$li(tags$a(href = url, pkg))
}
