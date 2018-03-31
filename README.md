# Volr language
Volr is a domain specific language (DSL)

The language is capable of modeling basic neurological properties, and evaluate them on the neuromorphic platform.

## Example

    stimulus input [2]
      file: examples/maze_x500.txt

    strategy b1 from input
      functions: 30

    strategy b2 from b1
      functions: 10

    strategy b3 from b2
      functions: 2

    response from b3
      file: examples/maze_y500.txt
      learning_rate: 0.5

## Contact
Jens Egholm Pedersen <jensegholm@protonmail.com>
