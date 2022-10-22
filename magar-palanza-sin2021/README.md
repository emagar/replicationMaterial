

# Description

Replication materials for the regression models, tables, and plots in "Presidents on the Fast Track: Fighting Floor Amendments with Restrictive Rules" by Magar, Palanza, and Sin forthcoming in *The Journal of Politics*. This file is documentation for the materials.


# Contact

Documentation prerared by Eric Magar (ITAM). Email: emagar AT itam DOT mx. This file prepared 14-jul-2019 (revised 15-jul-2019).


# Software

The Cámara de Diputados web page, our primary source, was scraped using *Python* (v2.7.12, van Rossum 1995) and the *Selenium* library (v3.141.0, Seleniumhq 2018). Analysis was done using *R* (v3.4.4, R Core Team 2018) for 64-bit Linux and the following libraries:

-   *arm* (v1.10-1, Gelman and Su 2018),
-   *DataCombine* (v0.2.21, Gandrud 2016),
-   *dplyr* (v0.7.6, Wickham et al 2018),
-   *lme4* (v1.1-18-1, Bates et al. 2015),
-   *lubridate* (v1.7.4, Garrett and Wickham 2011),
-   *MASS* (v7.3-50, Venables and Ripley 2002),
-   *Matrix* (v1.2-14, Bates and Maechler 2018),
-   *plyr* (v1.8.4, Wickman 2011),
-   *stargazer* (v5.2.2, Hlavac 2018),
-   *timeDate* (v3043.102, Wuertz et al. 2018), and
-   *xtable* (v1.8-2, Dahl 2016).

The full reproducible code is included. 


# List of files included in replication kit


## Scripts

-   code/code-for-replication-chilBill.r = main replication script. It consists of *R* code to load and manipulate data in order to get results in publication.
-   code/data-prep.r = script manipulating data from primary sources in *R*. Not needed if the purpose is replication of results only. Manipulate and run prior to to inspect our variable operationalization.
-   data/raw/boletines/1getBol.py and data/raw/boletines/1loopToGetBol.py = scripts to perform data scraping. They consist of Python code to interact with a java-rich dynamic web page, such as our primary source. Not needed unless additional information from bill histories is sought.


## Data

-   data/dataForUrgencyRegressions.RData = data for replication in R format.
-   data/raw/boletines.zip = folder with bill histories from primary source. Unzip it if you will use/manipulate the code/data-prep.r script.
-   data/raw/proyec3.csv = summary of all bills in primary source in comma separated values.
-   data/raw/sesionesCamara.csv and data/raw/sesionesSenado.csv= summary of Congressional sessions in the period in comma separated values.
-   data/raw/memoRollCallSum/ = roll call vote summaries by year 2002-2014 in comma separated values.
-   data/raw/comisiones1990-2014.csv = Cámara committee chairs and their parties in comma separated values.
-   data/raw/dip.csv and data/raw/sen.csv = names and parties of all deputies and senators in the period, respectively, and their parties in comma separated values.
-   data/raw/Base revisada.csv = includes bill importance measure for many bills in our data that we used in the online appendix (inquire to Valeria Palanza for coding detail, email vpalanza AT uc DOT cl).


## Other

-   paper/magar-palanza-sin-yr.pdf = a copy of the published paper in pdf format.


# Character encoding

In order to produce accented vowels and other Spanish characters (e.g. á, é, ñ, ü, &#x2026;) correctly on your system, all files, including this one, should be read with the UTF-8 character encoding. If the parenthesized characters in the previous sentence appear garbled, you will need to specify the correct encoding. See <http://kunststube.net/encoding/> for a primer on character encodings. 


# Descriptive statistics of variables in the models


## Dichotomous variables

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Variable</th>
<th scope="col" class="org-left">=0</th>
<th scope="col" class="org-right">=1</th>
<th scope="col" class="org-left">Total</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Fast-tracked Bill (Dep. Var.)</td>
<td class="org-left">927</td>
<td class="org-right">540</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">Co-partisan Comm. Chair</td>
<td class="org-left">832</td>
<td class="org-right">635</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">Coalition Comm. Chair</td>
<td class="org-left">99</td>
<td class="org-right">1,368</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">Multiple Referrals</td>
<td class="org-left">1,096</td>
<td class="org-right">371</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">Hacienda Referral</td>
<td class="org-left">732</td>
<td class="org-right">735</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">Introduced in Senate</td>
<td class="org-left">1,224</td>
<td class="org-right">243</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">Senate Majority</td>
<td class="org-left">512</td>
<td class="org-right">955</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">Relax Deadlines</td>
<td class="org-left">1,094</td>
<td class="org-right">373</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">1998&#x2013;2002</td>
<td class="org-left">1,195</td>
<td class="org-right">272</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">2002&#x2013;2006</td>
<td class="org-left">1,067</td>
<td class="org-right">400</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">2006&#x2013;2010</td>
<td class="org-left">1,075</td>
<td class="org-right">392</td>
<td class="org-left">1,467</td>
</tr>


<tr>
<td class="org-left">2010&#x2013;2014</td>
<td class="org-left">1,064</td>
<td class="org-right">403</td>
<td class="org-left">1,467</td>
</tr>
</tbody>
</table>


## Continuous variables

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Variable</th>
<th scope="col" class="org-right">Min.</th>
<th scope="col" class="org-right">Q1</th>
<th scope="col" class="org-right">Med.</th>
<th scope="col" class="org-right">Mean</th>
<th scope="col" class="org-right">Q3</th>
<th scope="col" class="org-right">Max.</th>
<th scope="col" class="org-right">sd</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Year Remaining</td>
<td class="org-right">0</td>
<td class="org-right">27</td>
<td class="org-right">51</td>
<td class="org-right">51.5</td>
<td class="org-right">75</td>
<td class="org-right">100</td>
<td class="org-right">27.1</td>
</tr>


<tr>
<td class="org-left">Pres. Approval</td>
<td class="org-right">-39.2</td>
<td class="org-right">-8</td>
<td class="org-right">10.7</td>
<td class="org-right">9.5</td>
<td class="org-right">22.3</td>
<td class="org-right">66.3</td>
<td class="org-right">24.2</td>
</tr>
</tbody>
</table>


# Primary source

The primary source can be visited at <https://www.camara.cl>. Follow the 'Proyectos de Ley' tab, input a boletin number (e.g. 1201-13). 


# Github repository

The data and code distributed here are part of a larger, ongoing project. More information will be added to the dataset in the future, the inevitable mistakes fixed. If you are interested in tracking new developments, or if you wish to contribute to the project, clone the full repository at <https://github.com/emagar/chileLeg>. 


# References

-   Bates, Douglas  Martin Maechler, Ben Bolker, and Steve Walker (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48. <10.18637/jss.v067.i01>.
-   Bates, Douglas and Martin Maechler (2018). Matrix: Sparse and Dense Matrix Classes and Methods. R package version 1.2-14. <https://CRAN.R-project.org/package=Matrix>.
-   Dahl, David B. (2016). xtable: Export Tables to LaTeX or HTML. R package version 1.8-2. <https://CRAN.R-project.org/package=xtable>.
-   Gandrud, Christopher (2016). DataCombine: Tools for Easily Combining and Cleaning Data Sets. R package version 0.2.21. <https://CRAN.R-project.org/package=DataCombine>.
-   Gelman, Andrew and Yu-Sung Su (2018). arm: Data Analysis Using Regression and Multilevel/Hierarchical Models. R package version 1.10-1. <https://CRAN.R-project.org/package=arm>
-   Grolemund, Garrett and Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. URL <http://www.jstatsoft.org/v40/i03/>.
-   Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.2. <https://CRAN.R-project.org/package=stargazer>.
-   Seleniumhq (2018). Selemium v3.141.0. URL <https://pypi.org/project/selenium/>.
-   R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <https://www.R-project.org/>.
-   Van Rossum, G. (1995) Python tutorial, Technical Report CS-R9526, Centrum voor Wiskunde en Informatica (CWI), Amsterdam, Nehterlands. URL <http://www.python.org>.
-   Venables, W.N. and B.D. Ripley (2002) Modern Applied Statistics with S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0.
-   Wickham, Hadley (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29. URL <http://www.jstatsoft.org/v40/i01/>.
-   Wickham, Hadley, Romain François, Lionel Henry and Kirill Müller (2018). dplyr: A Grammar of Data Manipulation. R package version 0.7.6. <https://CRAN.R-project.org/package=dplyr>.
-   Wuertz, Diethelm, Tobias Setz, Yohan Chalabi, Martin Maechler and Joe W. Byers (2018). timeDate: Rmetrics - Chronological and Calendar Objects. R package version 3043.102. <https://CRAN.R-project.org/package=timeDate>.

