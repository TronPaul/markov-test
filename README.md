# markov-text

A Clojure library to build text using markov chains and neo4j

## Usage

FIXME

## TODO

* unit tests
  * build w/ multiple lines added
  * tf-idf calcs
  * deal with casing issues casuing token inequality
* refactor
  * neo4j queries
  * move tf query into weighting func
  * split into 2 packages?
* weighting options for building
* build line relevant to string
  * find most important word in string
  * check if word from ^ in dict
  * if not get similar words, repeat
  * if not ^ get next most imporant word, repeat
