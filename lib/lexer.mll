{
  let reserved_words = []
}

let white = [' ' '\t']+

rule read = parse
  | white
    { read lexbuf }
  | eof
    { Parser.EOF }
