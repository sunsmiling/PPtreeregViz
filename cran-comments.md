### 4th submission
---

* Version: 2.0.5

* Response to cran comments

> You still have no \value tag in:
Missing Rd-tags:
     summary.PPTreereg.Rd: \value

> Please make sure to also roxygenize() your .Rd-files. In the
corresponding .R-file the @return value actually exists. My guess is you
have not rendered the .Rd-file before submitting.

==> Thank you for comments!! I rendered again and checked '\value' tag in 'summary.PPTreereg.Rd'.  

#####  R CMD check results

── R CMD check results  PPtreeregViz 2.0.5 ───
Duration: 2m 5.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

* This is a 4th release.


### Third submission
---

* Version: 2.0.4

* Response to cran comments

> Please proof-read your description text.
e.g.: in your first sentence it should probably be "tool" instead of "tools"? in "... finding connections between the input variables and outputs of model." there should probably be a "the" between "of" and "model".

==>  Thanks!! I used grammer checker and rewrite description text!

> You missed to add \value in one of your .Rd-files. Please also add it there.
Missing Rd-tags:
     summary.PPTreereg.Rd: \value
     
==> I added Rd-tags again. Thank you!

 summary.PPTreereg.Rd: \value  (done)


Thank you for comments!


#####  R CMD check results

── R CMD check results  PPtreeregViz 2.0.4 ───

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

* This is a third release.
 
### Second submission
---

* Version: 2.0.3

* minor code fixes in subpick.R 

removed the '..feature_dict' variable in calling scope for clarity.

* Response to cran comments

> If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...> authors (year) <arXiv:...> authors (year, ISBN:...)
or if those are not available: <https:...> with no space after 
'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.

Thanks! I added references describing the methods in package in the form
authors (year) <doi:...> and authors (year) <arXiv:...> in DESCRIPTION file. 

> Please always write package names, software names and API (application
programming interface) names in single quotes in title and description.
e.g: --> 'R'

I fixed names enclosed in single quotation marks in the title and description.

> Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)

I added all missing Rd-tags to all these documentations:
     feature_exact.Rd: \value  (done)
     plot.PPTreereg.Rd: \value  (done)
     ppshapr.empirical.Rd: \value  (done)
     ppshapr.simple.Rd: \value  (done)
     predict.PPTreereg.Rd: \value  (done)
     print.PPTreereg.Rd: \value  (done)
     shapley_weights.Rd: \value  (done)
     summary.PPTreereg.Rd: \value  (done) 
     weight_matrix.Rd: \value  (done)

> Please always add all authors, contributors and copyright holders in the
Authors@R field with the appropriate roles. ... Where copyrights are held by an entity other than the package authors, this should preferably be indicated via ‘cph’ roles in the ‘Authors@R’ field, or using a ‘Copyright’ field (if necessary referring to an inst/COPYRIGHTS file)."

 I additionally mentioned those authors as 'ctb' and 'cph' roles to all contributors  in the DESCRIPTION file and added ‘Copyright’ of code in each 'R' files. 
 
Thank you for comments!


## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 notes ✔

* This is a second release.
This is a major release adding a range of substantial new features and fixing a large number of bugs.
