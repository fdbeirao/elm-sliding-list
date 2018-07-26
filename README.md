# elm-sliding-list

[![Build Status](https://travis-ci.org/fdbeirao/elm-sliding-list.svg?branch=master)](https://travis-ci.org/fdbeirao/elm-sliding-list)

This package gives you a sliding list. You can specify that you wish to have a list that will have at most `n` items. For instance, maybe you want a list that can have at most 3 items. The moment you add a 4th item to this list, the oldest item will be dropped, thus ensuring you will only have a maximum of 3 items.

The maximum number of items in a list must be greater than zero, therefore you need to use the `positiveInt` function, which will return a `Just` if this criteria was met.

Feel free to take a look into the tests, to get a better understanding of this module.

A lot of the public interface of this module was inspired by Elm's core List module.

Feedback is always greatly appreciated 🙂