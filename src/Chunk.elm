module Chunk exposing (chunk)

import List
import Array exposing (Array)

chunk : Int -> List a -> List (List a)
chunk pageSize list =
  if pageSize < 1 then
    [list]
  else
    chunkInternal pageSize list Array.empty

chunkInternal : Int -> List a -> Array (List a) -> List (List a)
chunkInternal pageSize list accumulator =
    if List.isEmpty list then
        Array.toList accumulator
    else
        let
            removed = List.drop pageSize list
            latest = Array.push (List.take pageSize list) accumulator 
        in
            chunkInternal pageSize removed latest