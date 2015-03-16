# Slope One implementation in Haskell

Slope One is a simple algorithm for making item-based predictions.

This is an implementation of it in Haskell.

The implementation is based on the tutorial here: http://guidetodatamining.com/.
Both use a lot of maps (dictionaries in python), but this one uses a
more functional style with maps, folds, and filters.

It uses the Bookcrossings dataset, available here:
http://www2.informatik.uni-freiburg.de/~cziegler/BX/ To use the
Book-Crossing dataset, download the CSV version from this site,
extract and save it. Then change the `bookFile` variable in Main.hs to
your path.


# Todo

## Evaluate

Divide data into training and test set, then evaluate predictions on it.

take random split of all the data.

calculate deviations based on the training dataset, use those
deviations to make predictions for the user-item pairs in the training
set, then compare how well they match.
