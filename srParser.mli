val parse_data_file: string -> (float * float) list

val parse_json_data: Yojson.Basic.json -> (float * float) list 

val fitness: (float * float) list -> string list -> float

val strip_x: string list -> float -> string list

