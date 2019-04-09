# Notes on the course

###############################################.
## Structure of the stats.gov database ----
###############################################.

# stst.gov.scot is an rdf database - graph/nodes database 
# links reflecect relationship between nodes: geography, included, etc..
# works with URIs(same as URL), 
# unique resource identified - code for each subject, property, etc
# similar to urls

# This is the structure of their database (triplets):
# subject     property    object
# EH2 OP2     <within>    Edinburgh
# Edinburgh   type        Council area


# this is a subject: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Falcohol-related-discharge%2Fgovernment-year%2F2016-2017%2FS12000033%2Fnumber-per-100-000%2Fratio
# all the metatdata titles are properties and their values objects

# Only one "table" containing all relations and data
# It even includes metadata, so you can query it as well.

###############################################.
## SPARQL queries ----
###############################################.
# Examples queries:
# https://github.com/Swirrl/sg-training-material/tree/master/sg-training-r-project
# https://github.com/LiamCavin/sparql-queries
# https://github.com/GregorBoyd/sparql-queries/tree/master/Queries

### Prefix are the same as alias, to save time/space typing
# Similar to SQL: select x, y, z where conditions macthed, no FROM, and the 
# where relates to the triplets
# variables start with a question mark 

# PREFIX dcat: <http://www.w3.org/ns/dcat#>
# PREFIX dcterms: <http://purl.org/dc/terms/>
# PREFIX owl: <http://www.w3.org/2002/07/owl#>
# PREFIX qb: <http://purl.org/linked-data/cube#>
# PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/concept#>
# PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
# PREFIX void: <http://rdfs.org/ns/void#>
# PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
# 
# this will retrieve the whole database as it will match any value in the 
# three values of the triplet
# SELECT *
#   WHERE {
#     ?s ?p ?o
#   }
# 
# This limits the number of results the query will bring back
# LIMIT 100 