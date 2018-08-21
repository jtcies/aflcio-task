# Analysis plan

## Literature review

## Methods

- start with PCA?
- combine data from districts?
- parsimony vs. accuracy?
- data sources
  - compare demographics voterfile and ACS data
  - other data with gun information? NRA registration by district or something like that?
  - presidential approval ratings
  - personal income growth
  - length of term for each candidate
- modeling
  - model type
    - logitistic regression
    - decision-tree / random forest
    - KNN
    - neural network
  - focus
    - point of model should be voter turnout; accurately identify individuals who oppose recall and ensure they vote
    - alternative is to create parsimonious model that explains why folks voted the way they did
  - evaluation
    - accuracy and loss equally important
      - want to make sure that we don't miss anyone who could oppose recall and we don't 

## Products

- create a model for each of the voters
- also predic the outcome of the election if it were held?
- map that shows breakdown of of forecast by census tract

## next steps

- can't get presidential approval ratings, but would incorporate polling that included that information into final model