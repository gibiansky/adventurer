#!/usr/bin/env python
import sys

# The string to repeat to get the key.
KEY_STRING = "GLASSCEILING"

def xor((data_char, key_char)):
    """Given a pair (a, b) of chars, return their bitwise xor as a char."""

    # Convert from strings into integers, so we can use xor.
    data_int = ord(data_char)
    key_int = ord(key_char)

    # Get the output using Python's ^ bitwise exclusive or operator.
    out = data_int ^ key_int

    # Return the output as a string so we can write it back.
    return chr(out)

def decrypt(filename):
    """Given a filename, decrypt or encrypt the file data using the XOR cipher.

    Write the output data to filename.xor.""" 

    # Open the input and output files.
    with open(filename, "r") as reader:
        with open(filename + ".xor", "w") as writer:

            # Read all data from input.
            data = reader.read()

            # Generate key of sufficient length.
            key = KEY_STRING * (len(data)/len(KEY_STRING) + 1)

            # Combine the data and key into a list of (d, k) character pairs.
            pairs = zip(data, key)

            # 'xor' each pair and then write it to the output file.
            for x in map(xor, pairs):
                writer.write(x)

# Decrypt or encrypt every filename passed as a command-line argument.
for filename in sys.argv[1:]:
    decrypt(filename)
