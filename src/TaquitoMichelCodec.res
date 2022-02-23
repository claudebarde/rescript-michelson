type parser

@new @module("@taquito/michel-codec") external michel_codec_parser: unit => parser = "Parser"
@send external parse_micheline_expression: (parser, string) => Js.Json.t = "parseMichelineExpression"
