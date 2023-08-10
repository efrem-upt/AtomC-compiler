# AtomC-compiler

A simple compiler for a subset of C language, AtomC. AtomC uses some basic lexical and syntax rules that can be explored in the documentation (`docs`). Small project made for the Compilers lab during my third year of Computer Engineering studies at Polytechnic University of Timișoara.

## Running the compiler

Using `gcc`:

`gcc AtomC.c "alex\lexer.c" "parser\ad.c" "parser\at.c" "parser\gc.c" "parser\parser.c" "parser\vm.c" "utils\utils.c" -o atom`

And then simply call the `atom` executable. By default, it's going to show the results on the `testgc.c` file in the `tests` folder. Further support will be added in the future to specify input file via the command line.

## Documentation

Read more about the vision of this project by visiting the [documentation](https://github.com/efrem-upt/AtomC-compiler/tree/main/docs).

## License

[MIT](https://choosealicense.com/licenses/mit/)
