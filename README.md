# Related Project

Repository dedicated to "Related Project", whose goal is to provide <strong>items recommendation</strong> to both CRM and Site Management teams for customized editorial activities.

It takes user-level Google Analytics data from BigQuery (any kind of "strong" interaction with a single item, ie: "add-to-cart" and "item zoom") and it leverages "apriori" package to build association rules among items.

A function <strong>show_apriori()</strong> is provided and it takes a few argoments to provide both tabular and graphical representation of association rules:

* season = item's season (ie: "FW17")
* col1 = index column for item variable (depends on the dimensionality of the final table)
* support = minimum threshold for item frequency to be taken into account from apriori algorythm

<u>Please note that this function will write a .csv to your path ('final.csv') and then read it as "transaction" class object, so be sure not to overwrite any other files stored in the same path with the same name!!!</u>

