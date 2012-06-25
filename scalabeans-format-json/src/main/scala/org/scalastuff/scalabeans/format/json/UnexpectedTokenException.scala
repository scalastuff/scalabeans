package org.scalastuff.scalabeans.format.json

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.JsonLocation

class UnexpectedTokenException(expected: String, parser: JsonParser) 
extends UnexpectedInputException(expected, String.valueOf(parser.getCurrentToken()), parser.getCurrentLocation())

class UnexpectedInputException(expected: String, found: String, location: JsonLocation) 
extends RuntimeException("Cannot read json input: %s expected, %s found at %s".
    format(expected, found, location))

class UnreadableInputException(msg: String, location: JsonLocation)
extends RuntimeException("Cannot read json input: %s at %s".
    format(msg, location))