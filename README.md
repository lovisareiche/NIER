# Codes for "Does the Bank of England need gendered communication?"

## Clean data:
### prepdata.R
recodes age and income, creates point estimates from perception and expectations bins, matches with actual inflaion data, removes missing inflation value and those who don't identify as either male or female

## Output
### gendergaps.R
creates timeseries plot with all gender gaps

### regressions.R
creates tables: boeknowreg, pireg, piregint, pibadreg, BOEsatreg, boesources;
table boeknowregsources can be found when looking at the output og model sourceboe_2 manually

### sources.R
creates tables: ggpisources and ggboesources which compare the sources men and women use to form their forecasts about inflation or the BoE respectively
