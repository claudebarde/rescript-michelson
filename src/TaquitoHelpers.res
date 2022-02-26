// @taquito/michel-codec
type parser

@new @module("@taquito/michel-codec") external michel_codec_parser: unit => parser = "Parser"
@send external parse_micheline_expression: (parser, string) => Js.Json.t = "parseMichelineExpression"

// @taquito/utils
@module("@taquito/utils") external validate_contract_address: string => int = "validateContractAddress"
@module("@taquito/utils") external validate_address: string => int = "validateAddress"
@module("@taquito/utils") external validate_key_hash: string => int = "validateKeyHash"
@module("@taquito/utils") external validate_operation: string => int = "validateOperation"
@module("@taquito/utils") external validate_public_key: string => int = "validatePublicKey"
@module("@taquito/utils") external validate_chain: string => int = "validateChain"
@module("@taquito/utils") external validate_signature: string => int = "validateSignature"