# elm-sliding-list

[![Build Status](https://travis-ci.org/fdbeirao/elm-sliding-list.svg?branch=master)](https://travis-ci.org/fdbeirao/elm-sliding-list)

This package gives you a sliding list. You can specify that you wish to have a list that will have at most `n` items. For instance, maybe you want a list that can have at most 3 items. The moment you add a 4th item to this list, the oldest item will be dropped, thus ensuring you will only have a maximum of 3 items.

The maximum number of items in a list must be greater than zero. If you specify a number smaller than 1 for the `new`, `fromList` or `resize` functions, the sliding list will default to a size of 1.

Feel free to take a look into the tests, to get a better understanding of this module.

This package only supports inserting items into the sliding list from one direction (right to left). If you wish for more flexibility, you can also check the [Bounded Double-ended queue](http://package.elm-lang.org/packages/folkertdev/elm-deque/latest/BoundedDeque) package (not owned by the author of this package).

Feedback is always greatly appreciated 🙂