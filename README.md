
# NeuRacket

This is a fork of the [Racket](https://racket-lang.org/) language. 
It adds `defMLObject`, `defneuralslice`, `neural-field`, `abstract-neural-field`, `override-neural-field`, `external-neural-field`, and `label-field` to the Racket Class System. 

# How to use it

To use NeuRacket:
1. clone this repository
2. in `NeuRacket/` run `make`
3. in `NeuRacket/MLObject/` run `../racket/bin/raco pkg install`
4. run `..<pathToNeuRacket>../NeuRacket/racket/bin/raco pyffi configure <python>` with `<python>` a Python installation that has access to the libraries necessary for the machine learning models you will use in the NeuRacket application, so probably best a Python installation from a virtual environment.



Then `...path-to-NeuRacket.../NeuRacket/racket/bin/racket` can be used in the same way as the `racket` command from the [Racket](https://racket-lang.org/) language. 